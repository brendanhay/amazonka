{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.LanguageCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.LanguageCode where

import Network.AWS.Prelude

-- | Specify the language, using the ISO 639-2 three-letter code listed at https://www.loc.gov/standards/iso639-2/php/code_list.php.
data LanguageCode
  = Aar
  | Abk
  | Afr
  | Aka
  | Amh
  | Ara
  | Arg
  | Asm
  | Ava
  | Ave
  | Aym
  | Aze
  | Bak
  | Bam
  | Bel
  | Ben
  | Bih
  | Bis
  | Bod
  | Bos
  | Bre
  | Bul
  | Cat
  | Ces
  | Cha
  | Che
  | Chu
  | Chv
  | Cor
  | Cos
  | Cre
  | Cym
  | Dan
  | Deu
  | Div
  | Dzo
  | Ell
  | Eng
  | Enm
  | Epo
  | Est
  | Eus
  | Ewe
  | Fao
  | Fas
  | Fij
  | Fin
  | Fra
  | Frm
  | Fry
  | Ful
  | Ger
  | Gla
  | Gle
  | Glg
  | Glv
  | Grn
  | Guj
  | Hat
  | Hau
  | Heb
  | Her
  | Hin
  | Hmo
  | Hrv
  | Hun
  | Hye
  | IPk
  | Ibo
  | Ido
  | Iii
  | Iku
  | Ile
  | Ina
  | Ind
  | Isl
  | Ita
  | Jav
  | Jpn
  | Kal
  | Kan
  | Kas
  | Kat
  | Kau
  | Kaz
  | Khm
  | Kik
  | Kin
  | Kir
  | Kom
  | Kon
  | Kor
  | Kua
  | Kur
  | Lao
  | Lat
  | Lav
  | Lim
  | Lin
  | Lit
  | Ltz
  | Lub
  | Lug
  | Mah
  | Mal
  | Mar
  | Mkd
  | Mlg
  | Mlt
  | Mon
  | Mri
  | Msa
  | Mya
  | Nau
  | Nav
  | Nbl
  | Nde
  | Ndo
  | Nep
  | Nld
  | Nno
  | Nob
  | Nor
  | Nya
  | OSs
  | Oci
  | Oji
  | Ori
  | Orj
  | Orm
  | Pan
  | Pli
  | Pol
  | Por
  | Pus
  | Qaa
  | Qpc
  | Que
  | Roh
  | Ron
  | Run
  | Rus
  | Sag
  | San
  | Sin
  | Slk
  | Slv
  | Sme
  | Smo
  | Sna
  | Snd
  | Som
  | Sot
  | Spa
  | Sqi
  | Srb
  | Srd
  | Ssw
  | Sun
  | Swa
  | Swe
  | Tah
  | Tam
  | Tat
  | Tel
  | Tgk
  | Tgl
  | Tha
  | Tir
  | Tng
  | Ton
  | Tsn
  | Tso
  | Tuk
  | Tur
  | Twi
  | Uig
  | Ukr
  | Urd
  | Uzb
  | Ven
  | Vie
  | Vol
  | Wln
  | Wol
  | Xho
  | Yid
  | Yor
  | Zha
  | Zho
  | Zul
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText LanguageCode where
  parser =
    takeLowerText >>= \case
      "aar" -> pure Aar
      "abk" -> pure Abk
      "afr" -> pure Afr
      "aka" -> pure Aka
      "amh" -> pure Amh
      "ara" -> pure Ara
      "arg" -> pure Arg
      "asm" -> pure Asm
      "ava" -> pure Ava
      "ave" -> pure Ave
      "aym" -> pure Aym
      "aze" -> pure Aze
      "bak" -> pure Bak
      "bam" -> pure Bam
      "bel" -> pure Bel
      "ben" -> pure Ben
      "bih" -> pure Bih
      "bis" -> pure Bis
      "bod" -> pure Bod
      "bos" -> pure Bos
      "bre" -> pure Bre
      "bul" -> pure Bul
      "cat" -> pure Cat
      "ces" -> pure Ces
      "cha" -> pure Cha
      "che" -> pure Che
      "chu" -> pure Chu
      "chv" -> pure Chv
      "cor" -> pure Cor
      "cos" -> pure Cos
      "cre" -> pure Cre
      "cym" -> pure Cym
      "dan" -> pure Dan
      "deu" -> pure Deu
      "div" -> pure Div
      "dzo" -> pure Dzo
      "ell" -> pure Ell
      "eng" -> pure Eng
      "enm" -> pure Enm
      "epo" -> pure Epo
      "est" -> pure Est
      "eus" -> pure Eus
      "ewe" -> pure Ewe
      "fao" -> pure Fao
      "fas" -> pure Fas
      "fij" -> pure Fij
      "fin" -> pure Fin
      "fra" -> pure Fra
      "frm" -> pure Frm
      "fry" -> pure Fry
      "ful" -> pure Ful
      "ger" -> pure Ger
      "gla" -> pure Gla
      "gle" -> pure Gle
      "glg" -> pure Glg
      "glv" -> pure Glv
      "grn" -> pure Grn
      "guj" -> pure Guj
      "hat" -> pure Hat
      "hau" -> pure Hau
      "heb" -> pure Heb
      "her" -> pure Her
      "hin" -> pure Hin
      "hmo" -> pure Hmo
      "hrv" -> pure Hrv
      "hun" -> pure Hun
      "hye" -> pure Hye
      "ipk" -> pure IPk
      "ibo" -> pure Ibo
      "ido" -> pure Ido
      "iii" -> pure Iii
      "iku" -> pure Iku
      "ile" -> pure Ile
      "ina" -> pure Ina
      "ind" -> pure Ind
      "isl" -> pure Isl
      "ita" -> pure Ita
      "jav" -> pure Jav
      "jpn" -> pure Jpn
      "kal" -> pure Kal
      "kan" -> pure Kan
      "kas" -> pure Kas
      "kat" -> pure Kat
      "kau" -> pure Kau
      "kaz" -> pure Kaz
      "khm" -> pure Khm
      "kik" -> pure Kik
      "kin" -> pure Kin
      "kir" -> pure Kir
      "kom" -> pure Kom
      "kon" -> pure Kon
      "kor" -> pure Kor
      "kua" -> pure Kua
      "kur" -> pure Kur
      "lao" -> pure Lao
      "lat" -> pure Lat
      "lav" -> pure Lav
      "lim" -> pure Lim
      "lin" -> pure Lin
      "lit" -> pure Lit
      "ltz" -> pure Ltz
      "lub" -> pure Lub
      "lug" -> pure Lug
      "mah" -> pure Mah
      "mal" -> pure Mal
      "mar" -> pure Mar
      "mkd" -> pure Mkd
      "mlg" -> pure Mlg
      "mlt" -> pure Mlt
      "mon" -> pure Mon
      "mri" -> pure Mri
      "msa" -> pure Msa
      "mya" -> pure Mya
      "nau" -> pure Nau
      "nav" -> pure Nav
      "nbl" -> pure Nbl
      "nde" -> pure Nde
      "ndo" -> pure Ndo
      "nep" -> pure Nep
      "nld" -> pure Nld
      "nno" -> pure Nno
      "nob" -> pure Nob
      "nor" -> pure Nor
      "nya" -> pure Nya
      "oss" -> pure OSs
      "oci" -> pure Oci
      "oji" -> pure Oji
      "ori" -> pure Ori
      "orj" -> pure Orj
      "orm" -> pure Orm
      "pan" -> pure Pan
      "pli" -> pure Pli
      "pol" -> pure Pol
      "por" -> pure Por
      "pus" -> pure Pus
      "qaa" -> pure Qaa
      "qpc" -> pure Qpc
      "que" -> pure Que
      "roh" -> pure Roh
      "ron" -> pure Ron
      "run" -> pure Run
      "rus" -> pure Rus
      "sag" -> pure Sag
      "san" -> pure San
      "sin" -> pure Sin
      "slk" -> pure Slk
      "slv" -> pure Slv
      "sme" -> pure Sme
      "smo" -> pure Smo
      "sna" -> pure Sna
      "snd" -> pure Snd
      "som" -> pure Som
      "sot" -> pure Sot
      "spa" -> pure Spa
      "sqi" -> pure Sqi
      "srb" -> pure Srb
      "srd" -> pure Srd
      "ssw" -> pure Ssw
      "sun" -> pure Sun
      "swa" -> pure Swa
      "swe" -> pure Swe
      "tah" -> pure Tah
      "tam" -> pure Tam
      "tat" -> pure Tat
      "tel" -> pure Tel
      "tgk" -> pure Tgk
      "tgl" -> pure Tgl
      "tha" -> pure Tha
      "tir" -> pure Tir
      "tng" -> pure Tng
      "ton" -> pure Ton
      "tsn" -> pure Tsn
      "tso" -> pure Tso
      "tuk" -> pure Tuk
      "tur" -> pure Tur
      "twi" -> pure Twi
      "uig" -> pure Uig
      "ukr" -> pure Ukr
      "urd" -> pure Urd
      "uzb" -> pure Uzb
      "ven" -> pure Ven
      "vie" -> pure Vie
      "vol" -> pure Vol
      "wln" -> pure Wln
      "wol" -> pure Wol
      "xho" -> pure Xho
      "yid" -> pure Yid
      "yor" -> pure Yor
      "zha" -> pure Zha
      "zho" -> pure Zho
      "zul" -> pure Zul
      e ->
        fromTextError $
          "Failure parsing LanguageCode from value: '" <> e
            <> "'. Accepted values: aar, abk, afr, aka, amh, ara, arg, asm, ava, ave, aym, aze, bak, bam, bel, ben, bih, bis, bod, bos, bre, bul, cat, ces, cha, che, chu, chv, cor, cos, cre, cym, dan, deu, div, dzo, ell, eng, enm, epo, est, eus, ewe, fao, fas, fij, fin, fra, frm, fry, ful, ger, gla, gle, glg, glv, grn, guj, hat, hau, heb, her, hin, hmo, hrv, hun, hye, ipk, ibo, ido, iii, iku, ile, ina, ind, isl, ita, jav, jpn, kal, kan, kas, kat, kau, kaz, khm, kik, kin, kir, kom, kon, kor, kua, kur, lao, lat, lav, lim, lin, lit, ltz, lub, lug, mah, mal, mar, mkd, mlg, mlt, mon, mri, msa, mya, nau, nav, nbl, nde, ndo, nep, nld, nno, nob, nor, nya, oss, oci, oji, ori, orj, orm, pan, pli, pol, por, pus, qaa, qpc, que, roh, ron, run, rus, sag, san, sin, slk, slv, sme, smo, sna, snd, som, sot, spa, sqi, srb, srd, ssw, sun, swa, swe, tah, tam, tat, tel, tgk, tgl, tha, tir, tng, ton, tsn, tso, tuk, tur, twi, uig, ukr, urd, uzb, ven, vie, vol, wln, wol, xho, yid, yor, zha, zho, zul"

instance ToText LanguageCode where
  toText = \case
    Aar -> "AAR"
    Abk -> "ABK"
    Afr -> "AFR"
    Aka -> "AKA"
    Amh -> "AMH"
    Ara -> "ARA"
    Arg -> "ARG"
    Asm -> "ASM"
    Ava -> "AVA"
    Ave -> "AVE"
    Aym -> "AYM"
    Aze -> "AZE"
    Bak -> "BAK"
    Bam -> "BAM"
    Bel -> "BEL"
    Ben -> "BEN"
    Bih -> "BIH"
    Bis -> "BIS"
    Bod -> "BOD"
    Bos -> "BOS"
    Bre -> "BRE"
    Bul -> "BUL"
    Cat -> "CAT"
    Ces -> "CES"
    Cha -> "CHA"
    Che -> "CHE"
    Chu -> "CHU"
    Chv -> "CHV"
    Cor -> "COR"
    Cos -> "COS"
    Cre -> "CRE"
    Cym -> "CYM"
    Dan -> "DAN"
    Deu -> "DEU"
    Div -> "DIV"
    Dzo -> "DZO"
    Ell -> "ELL"
    Eng -> "ENG"
    Enm -> "ENM"
    Epo -> "EPO"
    Est -> "EST"
    Eus -> "EUS"
    Ewe -> "EWE"
    Fao -> "FAO"
    Fas -> "FAS"
    Fij -> "FIJ"
    Fin -> "FIN"
    Fra -> "FRA"
    Frm -> "FRM"
    Fry -> "FRY"
    Ful -> "FUL"
    Ger -> "GER"
    Gla -> "GLA"
    Gle -> "GLE"
    Glg -> "GLG"
    Glv -> "GLV"
    Grn -> "GRN"
    Guj -> "GUJ"
    Hat -> "HAT"
    Hau -> "HAU"
    Heb -> "HEB"
    Her -> "HER"
    Hin -> "HIN"
    Hmo -> "HMO"
    Hrv -> "HRV"
    Hun -> "HUN"
    Hye -> "HYE"
    IPk -> "IPK"
    Ibo -> "IBO"
    Ido -> "IDO"
    Iii -> "III"
    Iku -> "IKU"
    Ile -> "ILE"
    Ina -> "INA"
    Ind -> "IND"
    Isl -> "ISL"
    Ita -> "ITA"
    Jav -> "JAV"
    Jpn -> "JPN"
    Kal -> "KAL"
    Kan -> "KAN"
    Kas -> "KAS"
    Kat -> "KAT"
    Kau -> "KAU"
    Kaz -> "KAZ"
    Khm -> "KHM"
    Kik -> "KIK"
    Kin -> "KIN"
    Kir -> "KIR"
    Kom -> "KOM"
    Kon -> "KON"
    Kor -> "KOR"
    Kua -> "KUA"
    Kur -> "KUR"
    Lao -> "LAO"
    Lat -> "LAT"
    Lav -> "LAV"
    Lim -> "LIM"
    Lin -> "LIN"
    Lit -> "LIT"
    Ltz -> "LTZ"
    Lub -> "LUB"
    Lug -> "LUG"
    Mah -> "MAH"
    Mal -> "MAL"
    Mar -> "MAR"
    Mkd -> "MKD"
    Mlg -> "MLG"
    Mlt -> "MLT"
    Mon -> "MON"
    Mri -> "MRI"
    Msa -> "MSA"
    Mya -> "MYA"
    Nau -> "NAU"
    Nav -> "NAV"
    Nbl -> "NBL"
    Nde -> "NDE"
    Ndo -> "NDO"
    Nep -> "NEP"
    Nld -> "NLD"
    Nno -> "NNO"
    Nob -> "NOB"
    Nor -> "NOR"
    Nya -> "NYA"
    OSs -> "OSS"
    Oci -> "OCI"
    Oji -> "OJI"
    Ori -> "ORI"
    Orj -> "ORJ"
    Orm -> "ORM"
    Pan -> "PAN"
    Pli -> "PLI"
    Pol -> "POL"
    Por -> "POR"
    Pus -> "PUS"
    Qaa -> "QAA"
    Qpc -> "QPC"
    Que -> "QUE"
    Roh -> "ROH"
    Ron -> "RON"
    Run -> "RUN"
    Rus -> "RUS"
    Sag -> "SAG"
    San -> "SAN"
    Sin -> "SIN"
    Slk -> "SLK"
    Slv -> "SLV"
    Sme -> "SME"
    Smo -> "SMO"
    Sna -> "SNA"
    Snd -> "SND"
    Som -> "SOM"
    Sot -> "SOT"
    Spa -> "SPA"
    Sqi -> "SQI"
    Srb -> "SRB"
    Srd -> "SRD"
    Ssw -> "SSW"
    Sun -> "SUN"
    Swa -> "SWA"
    Swe -> "SWE"
    Tah -> "TAH"
    Tam -> "TAM"
    Tat -> "TAT"
    Tel -> "TEL"
    Tgk -> "TGK"
    Tgl -> "TGL"
    Tha -> "THA"
    Tir -> "TIR"
    Tng -> "TNG"
    Ton -> "TON"
    Tsn -> "TSN"
    Tso -> "TSO"
    Tuk -> "TUK"
    Tur -> "TUR"
    Twi -> "TWI"
    Uig -> "UIG"
    Ukr -> "UKR"
    Urd -> "URD"
    Uzb -> "UZB"
    Ven -> "VEN"
    Vie -> "VIE"
    Vol -> "VOL"
    Wln -> "WLN"
    Wol -> "WOL"
    Xho -> "XHO"
    Yid -> "YID"
    Yor -> "YOR"
    Zha -> "ZHA"
    Zho -> "ZHO"
    Zul -> "ZUL"

instance Hashable LanguageCode

instance NFData LanguageCode

instance ToByteString LanguageCode

instance ToQuery LanguageCode

instance ToHeader LanguageCode

instance ToJSON LanguageCode where
  toJSON = toJSONText

instance FromJSON LanguageCode where
  parseJSON = parseJSONText "LanguageCode"
