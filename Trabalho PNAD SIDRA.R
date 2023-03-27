### dados de trabalho da PNADC obtidos no SIDRA-IBGE
### autor: Paulo Roberto Carneiro

### carregando pacotes

# iremos estabelecer algumas condições temporarias iniciais

names <- c('julho/20', 'julho/19', 'Variação (%)')
ultima <- as.Date('2020-07-01')
times = seq(as.Date('2019-07-01'), ultima, 
            by='month')

library(forecast)
library(stargazer)
library(ggplot2)
library(sidrar)
library(scales)
library(gridExtra)
library(reshape2)
library(xts)
library(plyr)

### Coletar dados no SIDRA IBGE
# os dados iniciam em jan-fev-mar 2012

populacao = get_sidra(api='/t/6022/n1/all/v/606/p/all')$Valor
t1 = get_sidra(api='/t/6318/n1/all/v/1641/p/all/c629/all')
po = get_sidra(api='/t/6320/n1/all/v/4090/p/all/c11913/allxt')
po2 = get_sidra(api='/t/6323/n1/all/v/4090/p/all/c693/allxt')
renda = get_sidra(api='/t/6390/n1/all/v/5929,5933/p/all')
massa = get_sidra(api='/t/6392/n1/all/v/6288,6293/p/all')

### Obter séries individuais (pode ser mais rápido)

pia = t1$Valor[t1$`Condição em relação à força de trabalho e condição de ocupação (Código)`==32385]
pea = t1$Valor[t1$`Condição em relação à força de trabalho e condição de ocupação (Código)`==32386]
ocupada = t1$Valor[t1$`Condição em relação à força de trabalho e condição de ocupação (Código)`==32387]
desocupada = t1$Valor[t1$`Condição em relação à força de trabalho e condição de ocupação (Código)`==32446]
pnea = t1$Valor[t1$`Condição em relação à força de trabalho e condição de ocupação (Código)`==32447]
carteira = po$Valor[po$`Posição na ocupação e categoria do emprego no trabalho principal (Código)`=='31722']
scarteira = po$Valor[po$`Posição na ocupação e categoria do emprego no trabalho principal (Código)`=='31723']
domestico = po$Valor[po$`Posição na ocupação e categoria do emprego no trabalho principal (Código)`=='31724']
publico = po$Valor[po$`Posição na ocupação e categoria do emprego no trabalho principal (Código)`=='31727']

empregador = po$Valor[po$`Posição na ocupação e categoria do emprego no trabalho principal (Código)`=='96170']
cpropria = po$Valor[po$`Posição na ocupação e categoria do emprego no trabalho principal (Código)`=='96171']
tfa = po$Valor[po$`Posição na ocupação e categoria do emprego no trabalho principal (Código)`=='31731']
agro = po2$Valor[po2$`Grupamento de atividades no trabalho principal - PNADC (Código)`==33357]
ind = po2$Valor[po2$`Grupamento de atividades no trabalho principal - PNADC (Código)`==33358]
const = po2$Valor[po2$`Grupamento de atividades no trabalho principal - PNADC (Código)`==33360]
comercio = po2$Valor[po2$`Grupamento de atividades no trabalho principal - PNADC (Código)`==33361]
transporte = po2$Valor[po2$`Grupamento de atividades no trabalho principal - PNADC (Código)`==33362]
alojamento = po2$Valor[po2$`Grupamento de atividades no trabalho principal - PNADC (Código)`==33363]
informacao = po2$Valor[po2$`Grupamento de atividades no trabalho principal - PNADC (Código)`==33364]
admpub = po2$Valor[po2$`Grupamento de atividades no trabalho principal - PNADC (Código)`==39325]
outserv = po2$Valor[po2$`Grupamento de atividades no trabalho principal - PNADC (Código)`==33367]
servdom = po2$Valor[po2$`Grupamento de atividades no trabalho principal - PNADC (Código)`==33368]
rendanominal = renda$Valor[renda$`Variável (Código)`==5929]
rendareal = renda$Valor[renda$`Variável (Código)`==5933]
massanominal = massa$Valor[massa$`Variável (Código)`==6288]
massareal = massa$Valor[massa$`Variável (Código)`==6293]

### Vou juntar os dados em forma de um data.frame com base temporal.

dados.df<-cbind(populacao,pia,pea,ocupada,desocupada,
                pnea,carteira,scarteira,
                domestico,publico,empregador,
                cpropria,tfa,agro,
                ind,const,comercio,
                transporte,alojamento,informacao,
                admpub,outserv,
                servdom,rendanominal,
                rendareal,massanominal,massareal)
data = dados.df
# dados iniciam em jan-fev-mar 2012 - trimestres moveis mensais
pnadcm = ts(data, start=c(2012,03), freq=12)
colnames(pnadcm) <- c('População', 'PIA', 'PEA', 'PO', "PD",
                      'PNEA', 'Carteira', 'Sem Carteira',
                      'Doméstico','Público', 'Empregador', 
                      'Conta Própria','TFA', 'Agropecuária', 
                      'Indústria', 'Construção', 'Comércio',
                      'Transporte', 'Alojamento', 'Informação', 
                      'Administração Pública','Outros Serviços', 
                      'Serviços Domésticos', 'Renda Nominal', 
                      'Renda Real', 'Massa Nominal', 'Massa Real')
writexl::write_xlsx(as.data.frame(pnadcm),path = "dados.xlsx")

#Os dados são então salvos em formato .xlsx e gerei o formato estruturado embeded, como no chunk a seguir.

pnadcm<-
  structure(c(197074, 197217, 197359, 197502, 197645, 197788, 197931, 
              198074, 198217, 198360, 198503, 198646, 198789, 198932, 199075, 
              199217, 199360, 199503, 199646, 199789, 199931, 200074, 200217, 
              200359, 200502, 200645, 200787, 200929, 201072, 201214, 201357, 
              201499, 201641, 201783, 201925, 202067, 202209, 202351, 202492, 
              202634, 202775, 202917, 203058, 203199, 203340, 203482, 203622, 
              203763, 203904, 204045, 204185, 204325, 204466, 204606, 204746, 
              204886, 205025, 205165, 205305, 205444, 205583, 205722, 205861, 
              206000, 206138, 206277, 206415, 206553, 206691, 206829, 206966, 
              207104, 207241, 207378, 207515, 207652, 207788, 207924, 208061, 
              208196, 208332, 208468, 208603, 208738, 208873, 209008, 209142, 
              209276, 209411, 209544, 209678, 209811, 209944, 210077, 210210, 
              210342, 210474, 210606, 210738, 210869, 211001, 155670, 155810, 
              155920, 156215, 156344, 156543, 156774, 156983, 157240, 157426, 
              157558, 157732, 158069, 158240, 158294, 158288, 158455, 158657, 
              158873, 159068, 159228, 159587, 159700, 159786, 159959, 160336, 
              160699, 160903, 161111, 161376, 161615, 161849, 162043, 162319, 
              162599, 162879, 162978, 163009, 163133, 163286, 163429, 163586, 
              163693, 163810, 163978, 164151, 164302, 164537, 164775, 165120, 
              165347, 165491, 165618, 165672, 165735, 165893, 166076, 166401, 
              166640, 166716, 166807, 166984, 167158, 167432, 167713, 167863, 
              168039, 168108, 168254, 168396, 168491, 168509, 168508, 168701, 
              168922, 169241, 169425, 169591, 169734, 169793, 169936, 170022, 
              170193, 170395, 170500, 170494, 170615, 170864, 170975, 171123, 
              171158, 171281, 171401, 171613, 171798, 171989, 172354, 172978, 
              173610, 173918, 174114, 95191, 95833, 96179, 96375, 96342, 96458, 
              96454, 96362, 96386, 96468, 96473, 96577, 96702, 97032, 97195, 
              97323, 97481, 97423, 97460, 97356, 97514, 97416, 97330, 97594, 
              97783, 98076, 98178, 98300, 98223, 98217, 98456, 98637, 98636, 
              98805, 98934, 99187, 99438, 99687, 99743, 100050, 100293, 100419, 
              100557, 100847, 100763, 100818, 100727, 101010, 101239, 101559, 
              101804, 101902, 101854, 101688, 101391, 101467, 101884, 102150, 
              102324, 102447, 102684, 102851, 103030, 103298, 103587, 103767, 
              103859, 103892, 104132, 104037, 104015, 103842, 103907, 103790, 
              103776, 103864, 104193, 104454, 104783, 104928, 105078, 104888, 
              104916, 104933, 105250, 105543, 105931, 106108, 106153, 106195, 
              106315, 106421, 106279, 106184, 106065, 106052, 105073, 102052, 
              98646, 96138, 95158, 87632, 88407, 88863, 89129, 89181, 89428, 
              89639, 89723, 89867, 89857, 89520, 89133, 88999, 89437, 89839, 
              90099, 90388, 90506, 90707, 90828, 91204, 91403, 91112, 91015, 
              90782, 91081, 91336, 91577, 91455, 91465, 91795, 92115, 92228, 
              92396, 92214, 91834, 91555, 91711, 91641, 91750, 91725, 91670, 
              91635, 91833, 91704, 91800, 91166, 90702, 90216, 90213, 90428, 
              90379, 90072, 89730, 89433, 89488, 89815, 89871, 89469, 88968, 
              88579, 88872, 89323, 89872, 90319, 90710, 90953, 91203, 91610, 
              91770, 91373, 90773, 90272, 90429, 90586, 90941, 91367, 91790, 
              92333, 92619, 92915, 92736, 92291, 91880, 91863, 92365, 92947, 
              93342, 93584, 93631, 93801, 94055, 94416, 94552, 94151, 93710, 
              92223, 89241, 85936, 83347, 82027, 7559, 7426, 7317, 7245, 7162, 
              7030, 6815, 6639, 6519, 6611, 6953, 7444, 7704, 7596, 7355, 7225, 
              7093, 6917, 6753, 6528, 6309, 6013, 6218, 6579, 7001, 6995, 6842, 
              6723, 6768, 6752, 6662, 6522, 6408, 6409, 6720, 7353, 7883, 7975, 
              8103, 8300, 8568, 8748, 8922, 9014, 9059, 9019, 9560, 10308, 
              11023, 11346, 11376, 11523, 11782, 11958, 11958, 11979, 12069, 
              12278, 12855, 13479, 14105, 13979, 13707, 13426, 13269, 13057, 
              12906, 12689, 12522, 12267, 12642, 13070, 13634, 13361, 13190, 
              12923, 12827, 12665, 12450, 12309, 12164, 12152, 12625, 13053, 
              13387, 13177, 12984, 12766, 12569, 12565, 12515, 12367, 11863, 
              11632, 11913, 12343, 12850, 12811, 12710, 12791, 13130, 60479, 
              59977, 59741, 59841, 60002, 60085, 60320, 60621, 60855, 60958, 
              61085, 61155, 61367, 61207, 61099, 60965, 60974, 61235, 61413, 
              61712, 61714, 62171, 62370, 62192, 62176, 62259, 62520, 62603, 
              62888, 63159, 63159, 63212, 63407, 63514, 63666, 63692, 63540, 
              63322, 63390, 63236, 63137, 63167, 63136, 62963, 63215, 63333, 
              63576, 63526, 63536, 63561, 63543, 63589, 63764, 63984, 64344, 
              64426, 64192, 64252, 64316, 64269, 64123, 64133, 64129, 64134, 
              64125, 64096, 64180, 64216, 64122, 64360, 64475, 64667, 64601, 
              64911, 65147, 65377, 65231, 65136, 64951, 64866, 64858, 65133, 
              65277, 65461, 65250, 64951, 64684, 64756, 64822, 64928, 64843, 
              64860, 65122, 65429, 65733, 65937, 67281, 70926, 74964, 77781, 
              78956, 33393, 33682, 33841, 34083, 34138, 34240, 34399, 34505, 
              34580, 34752, 34778, 34755, 34489, 34582, 34810, 34928, 35177, 
              35299, 35476, 35700, 35808, 35889, 35898, 36126, 36237, 36465, 
              36509, 36716, 36506, 36470, 36495, 36405, 36369, 36350, 36339, 
              36087, 35916, 35928, 35818, 35765, 35597, 35400, 35277, 35227, 
              35260, 35268, 35027, 34741, 34503, 34403, 34319, 34302, 34224, 
              34060, 33995, 33928, 33962, 33894, 33751, 33632, 33305, 33187, 
              33161, 33237, 33247, 33321, 33212, 33215, 33133, 33237, 33215, 
              33048, 32837, 32655, 32701, 32763, 32912, 32902, 32909, 32863, 
              32904, 32942, 32866, 32979, 32918, 33136, 33222, 33213, 33146, 
              33042, 33075, 33206, 33420, 33668, 33711, 33624, 33096, 32207, 
              31103, 30154, 29385, 10942, 10973, 11018, 11038, 11055, 11089, 
              11202, 11200, 11151, 10907, 10811, 10758, 10797, 10779, 10748, 
              10781, 10701, 10841, 10845, 10784, 10743, 10657, 10554, 10421, 
              10389, 10337, 10318, 10252, 10180, 10134, 10190, 10348, 10400, 
              10420, 10325, 10148, 9982, 9952, 10008, 10004, 10003, 10021, 
              10113, 10137, 10039, 9975, 9721, 9660, 9661, 9894, 10002, 10023, 
              10099, 10144, 10209, 10305, 10390, 10457, 10347, 10194, 10126, 
              10202, 10414, 10564, 10668, 10699, 10853, 10922, 11110, 11056, 
              10930, 10704, 10657, 10849, 11012, 10935, 11039, 11135, 11453, 
              11572, 11634, 11488, 11254, 11075, 11124, 11217, 11384, 11500, 
              11658, 11795, 11838, 11852, 11812, 11855, 11673, 11644, 11023, 
              10126, 9218, 8639, 8691, 6065, 6055, 6115, 6115, 6106, 6110, 
              6112, 6091, 6107, 6143, 6137, 6098, 6049, 6049, 6033, 5924, 5917, 
              5833, 5911, 5916, 5954, 5941, 5918, 5923, 5900, 5904, 5917, 5973, 
              5973, 5923, 5950, 5965, 5973, 5951, 5968, 6008, 5990, 5956, 5946, 
              5974, 6019, 6010, 5987, 6120, 6206, 6249, 6205, 6175, 6195, 6197, 
              6254, 6200, 6147, 6097, 6098, 6114, 6053, 6083, 6032, 6019, 6036, 
              6080, 6110, 6081, 6062, 6089, 6155, 6239, 6305, 6348, 6302, 6273, 
              6184, 6148, 6119, 6214, 6258, 6284, 6241, 6245, 6243, 6256, 6223, 
              6167, 6108, 6147, 6183, 6254, 6280, 6287, 6276, 6314, 6356, 6356, 
              6260, 6209, 5971, 5524, 5033, 4714, 4593, 10931, 11049, 11081, 
              11186, 11276, 11366, 11316, 11275, 11193, 11049, 10835, 10768, 
              10823, 11029, 11070, 11206, 11270, 11305, 11268, 11258, 11254, 
              11174, 11045, 11025, 11139, 11304, 11385, 11329, 11387, 11457, 
              11497, 11606, 11588, 11545, 11380, 11310, 11286, 11404, 11369, 
              11391, 11421, 11492, 11485, 11444, 11319, 11264, 11129, 10969, 
              10921, 11027, 11127, 11243, 11186, 11305, 11274, 11337, 11358, 
              11200, 10935, 10787, 10822, 10962, 11115, 11248, 11383, 11409, 
              11440, 11473, 11503, 11424, 11255, 11149, 11172, 11326, 11440, 
              11565, 11611, 11675, 11690, 11671, 11729, 11596, 11461, 11279, 
              11362, 11462, 11543, 11661, 11714, 11671, 11683, 11675, 11686, 
              11641, 11526, 11370, 11652, 11904, 12255, 12360, 12119, 3413, 
              3521, 3523, 3516, 3515, 3555, 3564, 3572, 3625, 3655, 3654, 3647, 
              3641, 3703, 3663, 3718, 3736, 3735, 3700, 3734, 3809, 3783, 3700, 
              3681, 3697, 3687, 3657, 3713, 3689, 3761, 3739, 3810, 3869, 3923, 
              3932, 3962, 4056, 4019, 3954, 3977, 3986, 4036, 4036, 4025, 3981, 
              3936, 3835, 3749, 3707, 3709, 3749, 3690, 3805, 3928, 4065, 4112, 
              4143, 4127, 4168, 4107, 4110, 4104, 4101, 4173, 4216, 4195, 4228, 
              4292, 4387, 4390, 4350, 4334, 4347, 4344, 4333, 4353, 4389, 4419, 
              4416, 4503, 4468, 4520, 4502, 4523, 4435, 4381, 4422, 4369, 4331, 
              4348, 4368, 4452, 4483, 4442, 4426, 4411, 4385, 4201, 4034, 3955, 
              3938, 20494, 20522, 20463, 20228, 20129, 20122, 20156, 20271, 
              20390, 20508, 20468, 20315, 20409, 20482, 20684, 20724, 20809, 
              20781, 20836, 20776, 20920, 21167, 21209, 21077, 20789, 20769, 
              20968, 20956, 21107, 21101, 21350, 21437, 21511, 21637, 21640, 
              21641, 21651, 21788, 21900, 21944, 21987, 22024, 22107, 22348, 
              22489, 22790, 22977, 23165, 23066, 22860, 22855, 22804, 22521, 
              22121, 21743, 21638, 21830, 22021, 22089, 22051, 22009, 22176, 
              22273, 22407, 22531, 22751, 22819, 22865, 22951, 23110, 23095, 
              23052, 22871, 22945, 22863, 22985, 23035, 23204, 23419, 23533, 
              23736, 23775, 23831, 23711, 23750, 23884, 24033, 24141, 24227, 
              24293, 24434, 24446, 24597, 24557, 24575, 24477, 24159, 23379, 
              22415, 21664, 21406, 2393, 2606, 2822, 2965, 2961, 2946, 2889, 
              2809, 2821, 2843, 2837, 2792, 2791, 2812, 2832, 2819, 2778, 2711, 
              2671, 2661, 2716, 2793, 2789, 2762, 2633, 2616, 2582, 2637, 2614, 
              2619, 2574, 2544, 2519, 2571, 2631, 2678, 2674, 2666, 2646, 2695, 
              2712, 2686, 2631, 2531, 2409, 2317, 2273, 2244, 2163, 2124, 2122, 
              2117, 2091, 2075, 2049, 2055, 2080, 2089, 2146, 2178, 2172, 2160, 
              2150, 2161, 2211, 2245, 2247, 2197, 2221, 2206, 2226, 2213, 2204, 
              2162, 2118, 2127, 2123, 2170, 2204, 2232, 2201, 2158, 2155, 2145, 
              2166, 2139, 2160, 2203, 2230, 2193, 2127, 2110, 2062, 2033, 1980, 
              1974, 1938, 1900, 1879, 1861, 1895, 10239, 10334, 10404, 10446, 
              10445, 10450, 10279, 10114, 10036, 10115, 10156, 10034, 9944, 
              9955, 10118, 10197, 10270, 10217, 10161, 10055, 10183, 10257, 
              10145, 9855, 9551, 9522, 9628, 9683, 9675, 9567, 9513, 9429, 
              9386, 9333, 9427, 9417, 9464, 9429, 9403, 9475, 9486, 9446, 9379, 
              9302, 9257, 9260, 9273, 9438, 9359, 9348, 9277, 9332, 9290, 9175, 
              8940, 8827, 8821, 8842, 8840, 8740, 8606, 8623, 8598, 8574, 8548, 
              8555, 8545, 8414, 8447, 8391, 8496, 8461, 8415, 8370, 8350, 8422, 
              8505, 8599, 8685, 8619, 8513, 8389, 8430, 8388, 8422, 8399, 8573, 
              8655, 8648, 8586, 8511, 8449, 8388, 8333, 8323, 8370, 8266, 8166, 
              7993, 7976, 8049, 12840, 13051, 13082, 13122, 13044, 12954, 13065, 
              13072, 13114, 13051, 12933, 12868, 12883, 12928, 13001, 12956, 
              12824, 12716, 12729, 12855, 12819, 12828, 12835, 12986, 12937, 
              12983, 13010, 13088, 13208, 13357, 13339, 13373, 13326, 13366, 
              13234, 13144, 13186, 13209, 13112, 13047, 12963, 12891, 12827, 
              12634, 12519, 12319, 12123, 11784, 11679, 11654, 11709, 11619, 
              11589, 11481, 11537, 11487, 11502, 11372, 11234, 11279, 11342, 
              11439, 11623, 11718, 11865, 11855, 11790, 11785, 11905, 11908, 
              11801, 11663, 11584, 11674, 11768, 11871, 11833, 11884, 11838, 
              11954, 11784, 11777, 11609, 11587, 11667, 11779, 11859, 11986, 
              12044, 12131, 12054, 12119, 12105, 12166, 12121, 12165, 11844, 
              11436, 10934, 10727, 10520, 7042, 7128, 7269, 7370, 7488, 7569, 
              7594, 7679, 7748, 7772, 7744, 7554, 7573, 7697, 7767, 7766, 7737, 
              7836, 7970, 7996, 8032, 8069, 8075, 8028, 8001, 8019, 7901, 7772, 
              7510, 7505, 7574, 7693, 7715, 7737, 7697, 7669, 7607, 7410, 7267, 
              7101, 7147, 7286, 7276, 7538, 7729, 7900, 7799, 7660, 7522, 7403, 
              7437, 7384, 7334, 7189, 7111, 7043, 7034, 7049, 7050, 6917, 6809, 
              6763, 6651, 6709, 6721, 6843, 6850, 6887, 6918, 6922, 6777, 6644, 
              6537, 6593, 6564, 6543, 6615, 6652, 6770, 6738, 6791, 6806, 6740, 
              6639, 6518, 6566, 6565, 6605, 6650, 6746, 6859, 6846, 6925, 6820, 
              6781, 6624, 6380, 5896, 5541, 5323, 5336, 16362, 16429, 16516, 
              16502, 16473, 16520, 16414, 16481, 16627, 16821, 16811, 16837, 
              16767, 16711, 16772, 16808, 17005, 17091, 17175, 17195, 17262, 
              17422, 17428, 17397, 17389, 17355, 17319, 17314, 17230, 17152, 
              17150, 17216, 17339, 17462, 17556, 17435, 17350, 17381, 17393, 
              17491, 17425, 17431, 17504, 17601, 17552, 17640, 17585, 17543, 
              17364, 17285, 17318, 17325, 17248, 17163, 17016, 17159, 17384, 
              17575, 17569, 17359, 17142, 17125, 17213, 17344, 17352, 17392, 
              17439, 17562, 17618, 17806, 17767, 17644, 17416, 17336, 17385, 
              17333, 17376, 17446, 17469, 17592, 17715, 17737, 17680, 17646, 
              17542, 17493, 17519, 17531, 17540, 17495, 17633, 17743, 17833, 
              18009, 17922, 17853, 17381, 16704, 15870, 15244, 15091, 4008, 
              4039, 4074, 4067, 4087, 4024, 4093, 4134, 4250, 4233, 4200, 4189, 
              4203, 4197, 4256, 4281, 4290, 4280, 4200, 4220, 4204, 4193, 4153, 
              4122, 4137, 4167, 4239, 4224, 4175, 4110, 4116, 4153, 4202, 4239, 
              4246, 4260, 4275, 4282, 4263, 4263, 4283, 4268, 4257, 4342, 4397, 
              4488, 4439, 4486, 4460, 4511, 4500, 4477, 4489, 4458, 4480, 4429, 
              4504, 4592, 4556, 4563, 4479, 4550, 4519, 4609, 4632, 4635, 4599, 
              4584, 4548, 4546, 4547, 4551, 4588, 4631, 4623, 4641, 4646, 4619, 
              4591, 4621, 4664, 4749, 4752, 4800, 4790, 4844, 4881, 4862, 4827, 
              4846, 4870, 4854, 4911, 4896, 4965, 4970, 4870, 4723, 4550, 4341, 
              4161, 3819, 3833, 3849, 3837, 3821, 3782, 3744, 3776, 3791, 3912, 
              3914, 3927, 3884, 3865, 3852, 3862, 3910, 3958, 4020, 4081, 4149, 
              4202, 4203, 4274, 4255, 4212, 4126, 4117, 4086, 4137, 4186, 4218, 
              4220, 4292, 4335, 4339, 4308, 4347, 4333, 4309, 4324, 4329, 4319, 
              4410, 4421, 4560, 4503, 4527, 4480, 4500, 4515, 4473, 4461, 4560, 
              4663, 4737, 4769, 4811, 4899, 4939, 4975, 5047, 5084, 5053, 5147, 
              5167, 5231, 5235, 5211, 5233, 5217, 5212, 5260, 5207, 5236, 5189, 
              5227, 5262, 5367, 5333, 5426, 5381, 5459, 5419, 5424, 5440, 5429, 
              5417, 5415, 5409, 5484, 5539, 5613, 5663, 5633, 5613, 5355, 4933, 
              4373, 4006, 3790, 9415, 9464, 9361, 9313, 9270, 9410, 9618, 9681, 
              9613, 9549, 9589, 9627, 9617, 9680, 9664, 9767, 9789, 9821, 9745, 
              9695, 9783, 9687, 9662, 9680, 9781, 9815, 9996, 10224, 10493, 
              10495, 10641, 10549, 10530, 10480, 10429, 10407, 10311, 10473, 
              10613, 10730, 10705, 10574, 10514, 10128, 9853, 9530, 9572, 9606, 
              9661, 9660, 9702, 9658, 9655, 9586, 9543, 9550, 9601, 9709, 9787, 
              9818, 9911, 9815, 9806, 9794, 9783, 9900, 10037, 10097, 10122, 
              10125, 10147, 10154, 10050, 10036, 9947, 9891, 9999, 10030, 10156, 
              10216, 10314, 10349, 10386, 10406, 10467, 10508, 10527, 10509, 
              10529, 10560, 10560, 10540, 10572, 10570, 10598, 10582, 10625, 
              10379, 10240, 10064, 9999, 14050, 14295, 14452, 14569, 14659, 
              14768, 14754, 14707, 14614, 14418, 14153, 14075, 14117, 14337, 
              14398, 14573, 14685, 14733, 14734, 14755, 14735, 14717, 14583, 
              14620, 14705, 14973, 15071, 15055, 15009, 15053, 15063, 15174, 
              15278, 15309, 15138, 14978, 14929, 15074, 15127, 15200, 15201, 
              15264, 15368, 15546, 15577, 15586, 15405, 15248, 15293, 15464, 
              15515, 15687, 15615, 15809, 15682, 15791, 15711, 15488, 15146, 
              15015, 15007, 15099, 15301, 15489, 15689, 15713, 15740, 15776, 
              15841, 15742, 15562, 15410, 15479, 15674, 15833, 16069, 16127, 
              16197, 16253, 16285, 16457, 16313, 16115, 15889, 15981, 16207, 
              16341, 16451, 16495, 16474, 16480, 16517, 16561, 16529, 16384, 
              16186, 16525, 16671, 16934, 16789, 16503, 3753, 3737, 3701, 3747, 
              3753, 3810, 3937, 3961, 3944, 3829, 3874, 3918, 3954, 4009, 3967, 
              3954, 3949, 4010, 4049, 4046, 4066, 4073, 4089, 4114, 4106, 4111, 
              4109, 4108, 4084, 4152, 4246, 4324, 4235, 4198, 4161, 4157, 4123, 
              4140, 4175, 4148, 4162, 4160, 4191, 4152, 4088, 4128, 4148, 4145, 
              4132, 4109, 4109, 4128, 4155, 4140, 4292, 4290, 4376, 4294, 4300, 
              4268, 4217, 4287, 4368, 4453, 4461, 4501, 4509, 4562, 4625, 4671, 
              4675, 4676, 4659, 4677, 4694, 4724, 4733, 4764, 4916, 4973, 4966, 
              4931, 4834, 4847, 4838, 4872, 4976, 4988, 5088, 5025, 5001, 5053, 
              5088, 5152, 5111, 5082, 4940, 4745, 4407, 4117, 3942, 6065, 6055, 
              6115, 6115, 6106, 6110, 6112, 6091, 6107, 6143, 6137, 6098, 6049, 
              6049, 6033, 5924, 5917, 5833, 5911, 5916, 5954, 5941, 5919, 5924, 
              5901, 5904, 5917, 5974, 5973, 5925, 5953, 5971, 5978, 5959, 5974, 
              6014, 5995, 5960, 5949, 5976, 6020, 6011, 5992, 6170, 6296, 6367, 
              6300, 6251, 6256, 6269, 6340, 6293, 6232, 6163, 6161, 6169, 6106, 
              6133, 6081, 6064, 6076, 6108, 6140, 6114, 6103, 6130, 6191, 6276, 
              6343, 6395, 6349, 6323, 6229, 6180, 6136, 6219, 6262, 6290, 6246, 
              6254, 6249, 6262, 6236, 6194, 6150, 6193, 6233, 6301, 6326, 6342, 
              6335, 6376, 6400, 6391, 6292, 6243, 6016, 5565, 5074, 4746, 4625, 
              1431, 1445, 1445, 1454, 1465, 1474, 1479, 1485, 1493, 1501, 1518, 
              1540, 1558, 1570, 1580, 1599, 1613, 1626, 1630, 1641, 1643, 1642, 
              1651, 1679, 1708, 1721, 1731, 1732, 1734, 1748, 1759, 1773, 1771, 
              1783, 1802, 1817, 1840, 1855, 1864, 1882, 1881, 1883, 1890, 1892, 
              1893, 1902, 1927, 1935, 1967, 1964, 1983, 1974, 1987, 2013, 2017, 
              2026, 2033, 2048, 2062, 2074, 2092, 2088, 2090, 2084, 2087, 2085, 
              2100, 2112, 2126, 2134, 2149, 2166, 2171, 2184, 2189, 2200, 2207, 
              2227, 2224, 2232, 2240, 2256, 2271, 2286, 2289, 2292, 2286, 2290, 
              2286, 2298, 2298, 2317, 2332, 2340, 2361, 2375, 2398, 2425, 2460, 
              2500, 2535, 2218, 2230, 2222, 2227, 2238, 2245, 2242, 2240, 2237, 
              2234, 2243, 2258, 2271, 2277, 2280, 2299, 2314, 2329, 2329, 2335, 
              2327, 2309, 2308, 2330, 2354, 2353, 2351, 2340, 2336, 2351, 2359, 
              2368, 2353, 2355, 2360, 2355, 2356, 2350, 2339, 2344, 2327, 2315, 
              2314, 2304, 2287, 2276, 2282, 2268, 2286, 2267, 2276, 2252, 2254, 
              2274, 2271, 2276, 2279, 2290, 2299, 2304, 2318, 2308, 2305, 2296, 
              2297, 2294, 2305, 2312, 2321, 2321, 2329, 2340, 2339, 2348, 2349, 
              2346, 2338, 2348, 2339, 2341, 2343, 2357, 2371, 2379, 2371, 2360, 
              2343, 2341, 2335, 2344, 2342, 2361, 2371, 2365, 2371, 2372, 2391, 
              2419, 2458, 2502, 2535, 121454, 123398, 123825, 124725, 125792, 
              126913, 127746, 128548, 129462, 130105, 131077, 132424, 133789, 
              135525, 136897, 138981, 140689, 142142, 142809, 143990, 144744, 
              144838, 145194, 147520, 149972, 151609, 152993, 153415, 153460, 
              154783, 156380, 158237, 158308, 159577, 160843, 161481, 163009, 
              164669, 165314, 167030, 166981, 167049, 167815, 168475, 168500, 
              169632, 170803, 170641, 172774, 172652, 174918, 174001, 174690, 
              176343, 176150, 177041, 178218, 179638, 179879, 179863, 180653, 
              180912, 182045, 182618, 183669, 184316, 186101, 187769, 189846, 
              190941, 191414, 191679, 191015, 192547, 193448, 195212, 196770, 
              199438, 200265, 201564, 203039, 204169, 204563, 204944, 205156, 
              206544, 207294, 208435, 208627, 209893, 210424, 212808, 215104, 
              216262, 217399, 217631, 216290, 211628, 206623, 203519, 203016, 
              188272, 190469, 190372, 191070, 192132, 193271, 193610, 193811, 
              194020, 193700, 193697, 194233, 194981, 196454, 197524, 199781, 
              201827, 203549, 204063, 204922, 204977, 203732, 202895, 204711, 
              206658, 207355, 207840, 207357, 206809, 208125, 209672, 211284, 
              210321, 210807, 210712, 209299, 208676, 208547, 207454, 208047, 
              206495, 205448, 205445, 205157, 203558, 203020, 202254, 199986, 
              200767, 199325, 200716, 198496, 198196, 199231, 198329, 198811, 
              199772, 200867, 200564, 199888, 200139, 199950, 200727, 201213, 
              202153, 202732, 204283, 205565, 207215, 207606, 207437, 207028, 
              205852, 207085, 207568, 208167, 208467, 210270, 210634, 211396, 
              212423, 213314, 213557, 213329, 212495, 212678, 212417, 213079, 
              213047, 214127, 214484, 216788, 218697, 218576, 218347, 217416, 
              215706, 211051, 206483, 203678, 203016), .Dim = c(101L, 27L), .Dimnames = list(
                NULL, c("População", "PIA", "PEA", "PO", "PD", "PNEA", "Carteira", 
                        "Sem Carteira", "Doméstico", "Público", "Empregador", "Conta Própria", 
                        "TFA", "Agropecuária", "Indústria", "Construção", "Comércio", 
                        "Transporte", "Alojamento", "Informação", "Administração Pública", 
                        "Outros Serviços", "Serviços Domésticos", "Renda Nominal", 
                        "Renda Real", "Massa Nominal", "Massa Real")), .Tsp = c(2012.16666666667, 
                                                                                2020.5, 12), class = c("mts", "ts", "matrix"))

### analise de dados

interanual <- (pnadcm/lag(pnadcm,-12)-1)*100
colnames(interanual) <- colnames(pnadcm)

dinamica <- pnadcm - lag(pnadcm,-1)
colnames(dinamica) <- colnames(pnadcm)

### 3.1 População Desocupada na PNAD Contínua

desemprego = data.frame(time=times,
                        pd=tail(pnadcm[,5],length(times)))

ggplot(desemprego, aes(x=time, y=pd/1000))+
  geom_line(size=.8, colour='blue')+
  scale_x_date(breaks = date_breaks("1 months"),
               labels = date_format("%b/%Y"))+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_point(size=9, shape=21, colour="yellow", fill="yellow")+
  geom_text(aes(label=round(pd/1000,2)), size=2.1, 
            hjust=0.5, vjust=0.5, shape=21, colour="black")+
  xlab('')+ylab('milhões de pessoas')+
  labs(title='População Desocupada na PNAD Contínua',
       subtitle = "Brasil, Jul./2019-Jul./2020",
       caption = "Fonte: dados básicos do SIDRA-IBGE
                  Elaboração: Paulo Roberto Carneiro")

### Variação interanual da População Desocupada

desemprego = data.frame(time=times, pd=tail(interanual[,5], 
                                            length(times)))

ggplot(desemprego, aes(x=time, y=pd))+
  geom_bar(stat='identity', colour='blue', fill='blue')+
  scale_x_date(breaks = date_breaks("1 months"),
               labels = date_format("%b/%Y"))+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_point(size=9, shape=21, colour="yellow", fill="yellow")+
  geom_text(aes(label=round(pd,2)), size=2.1, 
            hjust=0.5, vjust=0.5, shape=21, colour="black")+
  xlab('')+ylab('%')+
  labs(title='Variação interanual da População Desocupada',
       subtitle = "Brasil, Jul./2019-Jul./2020",
       caption = "Fonte: dados básicos do SIDRA-IBGE
                  Elaboração: Paulo Roberto Carneiro")

### População Economicamente Ativa - PEA

desemprego = data.frame(time=times, pea=tail(pnadcm[,3],
                                             length(times)))

ggplot(desemprego, aes(x=time, y=pea/1000))+
  geom_line(size=.8, colour='blue')+
  scale_x_date(breaks = date_breaks("1 months"),
               labels = date_format("%b/%Y"))+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_point(size=9, shape=21, colour="yellow", fill="yellow")+
  geom_text(aes(label=round(pea/1000,2)), size=2.1, 
            hjust=0.5, vjust=0.5, shape=21, colour="black")+
  xlab('')+ylab('milhões de pessoas')+
  labs(title='População Economicamente Ativa na PNAD Contínua',
       subtitle = "Brasil, Jul./2019-Jul./2020",
       caption = "Fonte: dados básicos do SIDRA-IBGE
                  Elaboração: Paulo Roberto Carneiro")

### Variação interanual da PEA - População Economicamente Ativa

desemprego = data.frame(time=times, pea=tail(interanual[,3], 
                                             length(times)))

ggplot(desemprego, aes(x=time, y=pea))+
  geom_bar(stat='identity', colour='blue', fill='blue')+
  scale_x_date(breaks = date_breaks("1 months"),
               labels = date_format("%b/%Y"))+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_point(size=9, shape=21, colour="yellow", fill="yellow")+
  geom_text(aes(label=round(pea,2)), size=2.1, 
            hjust=0.5, vjust=0.5, shape=21, colour="black")+
  xlab('')+ylab('%')+
  labs(title='Variação interanual da PEA - População Economicamente Ativa',
       subtitle = "Brasil, Jul./2019-Jul./2020",
       caption = "Fonte: dados básicos do SIDRA-IBGE
                  Elaboração: Paulo Roberto Carneiro")

### Comparação dos ocupados com e sem carteira, empregador e trabahadores por conta própria 

df3 = data.frame(time=times,
                 po=tail(pnadcm[,4],length(times)),
                 carteira=tail(pnadcm[,7],length(times)),
                 scarteira=tail(pnadcm[,8],length(times)), 
                 empregador=tail(pnadcm[,11],length(times)),
                 cpropria=tail(pnadcm[,12],length(times)))

g1 = ggplot(df3, aes(time, carteira))+
  geom_line(colour='blue')+
  scale_x_date(breaks = date_breaks("2 months"),
               labels = date_format("%b/%Y"))+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(x='', y='pessoas', 
       title='Ocupado com Carteira')

g2 = ggplot(df3, aes(time, scarteira))+
  geom_line(colour='red')+
  scale_x_date(breaks = date_breaks("2 months"),
               labels = date_format("%b/%Y"))+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(x='', y='pessoas', 
       title='Ocupado sem Carteira')

g3 = ggplot(df3, aes(time, empregador))+
  geom_line(colour='black')+
  scale_x_date(breaks = date_breaks("2 months"),
               labels = date_format("%b/%Y"))+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(x='', y='pessoas', 
       title='Empregador')

g4 = ggplot(df3, aes(time, cpropria))+
  geom_line(colour='green')+
  scale_x_date(breaks = date_breaks("2 months"),
               labels = date_format("%b/%Y"))+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(x='', y='pessoas', 
       title='Conta Própria')


grid.arrange(g1, g2, g3, g4, 
             top = "Comparação dos Ocupados com e sem Carteira,
            Empregador e Trabalhadores por conta própria,
             Brasil, Jul./2019-Jul./2020",
             ncol=2, nrow=2)

### Comparação das variações absolutas da PEA com Pop. Ocupada

df4 = data.frame(time=times, 
                 dpea=tail(dinamica[,3], length(times)),
                 dpo=tail(dinamica[,4], length(times)),
                 dpd=tail(dinamica[,5], length(times)))

g1 = ggplot(df4, aes(x=time, y=dpea))+
  geom_bar(stat='identity', colour='blue', fill='blue')+
  scale_x_date(breaks = date_breaks("1 month"),
               labels = date_format("%m/%Y"))+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  xlab('')+ylab('Variação')+
  geom_point(size=7, shape=21, colour='yellow', fill="yellow")+
  geom_text(aes(label=round(dpea,0)), vjust=0.5,
            colour='black', size=3)+
  labs(title='Variação da PEA')

g2 = ggplot(df4, aes(x=time, y=dpo))+
  geom_bar(stat='identity', colour='blue', fill='blue')+
  scale_x_date(breaks = date_breaks("1 month"),
               labels = date_format("%m/%Y"))+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  xlab('')+ylab('Variação')+
  geom_point(size=7, shape=21, colour='yellow', fill="yellow")+
  geom_text(aes(label=round(dpo,0)), vjust=0.5,
            colour='white', size=3)+
  labs(title='Variação da PO')


grid.arrange(g1, g2,
             top = "Comparação das variações absolutas da PEA com Pop. Ocupada,
             Brasil, Jul./2019-Jul./2020",
             layout_matrix = matrix(c(1,1,2,2), 
                                    ncol=2, byrow=TRUE))

### Rendas Real e Nominal
### renda Nominal mensal

renda.df = data.frame(time=times,
                      renda_nom=as.numeric(tail(pnadcm[,24],length(times))),
                      renda_real=as.numeric(tail(pnadcm[,25],length(times))))

ggplot(renda.df, aes(x=time, y=renda_nom))+
  geom_line(size=.8, colour='blue')+
  scale_x_date(breaks = date_breaks("1 months"),
               labels = date_format("%b/%Y"))+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_point(size=9, shape=21, colour="yellow", fill="yellow")+
  geom_text(aes(label=round(renda_nom,2)), size=2.1, 
            hjust=0.5, vjust=0.5, shape=21, colour="black")+
  xlab('')+ylab('pessoas')+
  labs(title='Renda Nominal mensal na PNAD Contínua',
       subtitle = "Brasil, Jul./2019-Jul./2020",
       caption = "Elaboração: Paulo Roberto Carneiro
                  Fonte: dados básicos do SIDRA-IBGE")

# renda real mensal

ggplot(renda.df, aes(x=time, y=renda_real))+
  geom_line(size=.8, colour='red')+
  scale_x_date(breaks = date_breaks("1 months"),
               labels = date_format("%b/%Y"))+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_point(size=9, shape=21, colour="yellow", fill="yellow")+
  geom_text(aes(label=round(renda_real,2)), size=2.1, 
            hjust=0.5, vjust=0.5, shape=21, colour="black")+
  xlab('')+ylab('pessoas')+
  labs(title='Renda Real mensal na PNAD Contínua',
       subtitle = "Brasil, Jul./2019-Jul./2020",
       caption = "Elaboração: Paulo Roberto Carneiro
                  Fonte: dados básicos do SIDRA-IBGE")

### referencias:

#FIGUEIREDO, Adriano Marcos Rodrigues. Exemplos para dados de trabalho da PNADC obtidos no SIDRA-IBGE. Campo Grande-MS,Brasil: RStudio/Rpubs, 2020. Disponível em http://rpubs.com/amrofi/exemplos_sidra.

#BORGES, Letícia Maria C. Mercado de Trabalho - PNAD Continínua (SIDRAR). Brasil: RStudio/Rpubs, 2020. Disponível em https://rpubs.com/LeMaria/608156.

#WOOLDRIDGE, J.M. Introdução à Econometria: uma abordagem moderna. São Paulo: Pioneira Thomson Learning, 2006.(tradução da segunda edição americana).
































































































