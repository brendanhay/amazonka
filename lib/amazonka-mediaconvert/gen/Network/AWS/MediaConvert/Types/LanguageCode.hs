{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.LanguageCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.LanguageCode
  ( LanguageCode
      ( LanguageCode',
        Aar,
        Abk,
        Afr,
        Aka,
        Amh,
        Ara,
        Arg,
        Asm,
        Ava,
        Ave,
        Aym,
        Aze,
        Bak,
        Bam,
        Bel,
        Ben,
        Bih,
        Bis,
        Bod,
        Bos,
        Bre,
        Bul,
        Cat,
        Ces,
        Cha,
        Che,
        Chu,
        Chv,
        Cor,
        Cos,
        Cre,
        Cym,
        Dan,
        Deu,
        Div,
        Dzo,
        Ell,
        Eng,
        Enm,
        Epo,
        Est,
        Eus,
        Ewe,
        Fao,
        Fas,
        Fij,
        Fin,
        Fra,
        Frm,
        Fry,
        Ful,
        Ger,
        Gla,
        Gle,
        Glg,
        Glv,
        Grn,
        Guj,
        Hat,
        Hau,
        Heb,
        Her,
        Hin,
        Hmo,
        Hrv,
        Hun,
        Hye,
        IPk,
        Ibo,
        Ido,
        Iii,
        Iku,
        Ile,
        Ina,
        Ind,
        Isl,
        Ita,
        Jav,
        Jpn,
        Kal,
        Kan,
        Kas,
        Kat,
        Kau,
        Kaz,
        Khm,
        Kik,
        Kin,
        Kir,
        Kom,
        Kon,
        Kor,
        Kua,
        Kur,
        Lao,
        Lat,
        Lav,
        Lim,
        Lin,
        Lit,
        Ltz,
        Lub,
        Lug,
        Mah,
        Mal,
        Mar,
        Mkd,
        Mlg,
        Mlt,
        Mon,
        Mri,
        Msa,
        Mya,
        Nau,
        Nav,
        Nbl,
        Nde,
        Ndo,
        Nep,
        Nld,
        Nno,
        Nob,
        Nor,
        Nya,
        OSs,
        Oci,
        Oji,
        Ori,
        Orj,
        Orm,
        Pan,
        Pli,
        Pol,
        Por,
        Pus,
        Qaa,
        Qpc,
        Que,
        Roh,
        Ron,
        Run,
        Rus,
        Sag,
        San,
        Sin,
        Slk,
        Slv,
        Sme,
        Smo,
        Sna,
        Snd,
        Som,
        Sot,
        Spa,
        Sqi,
        Srb,
        Srd,
        Ssw,
        Sun,
        Swa,
        Swe,
        Tah,
        Tam,
        Tat,
        Tel,
        Tgk,
        Tgl,
        Tha,
        Tir,
        Tng,
        Ton,
        Tsn,
        Tso,
        Tuk,
        Tur,
        Twi,
        Uig,
        Ukr,
        Urd,
        Uzb,
        Ven,
        Vie,
        Vol,
        Wln,
        Wol,
        Xho,
        Yid,
        Yor,
        Zha,
        Zho,
        Zul
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specify the language, using the ISO 639-2 three-letter code listed at https://www.loc.gov/standards/iso639-2/php/code_list.php.
newtype LanguageCode = LanguageCode' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Aar :: LanguageCode
pattern Aar = LanguageCode' "AAR"

pattern Abk :: LanguageCode
pattern Abk = LanguageCode' "ABK"

pattern Afr :: LanguageCode
pattern Afr = LanguageCode' "AFR"

pattern Aka :: LanguageCode
pattern Aka = LanguageCode' "AKA"

pattern Amh :: LanguageCode
pattern Amh = LanguageCode' "AMH"

pattern Ara :: LanguageCode
pattern Ara = LanguageCode' "ARA"

pattern Arg :: LanguageCode
pattern Arg = LanguageCode' "ARG"

pattern Asm :: LanguageCode
pattern Asm = LanguageCode' "ASM"

pattern Ava :: LanguageCode
pattern Ava = LanguageCode' "AVA"

pattern Ave :: LanguageCode
pattern Ave = LanguageCode' "AVE"

pattern Aym :: LanguageCode
pattern Aym = LanguageCode' "AYM"

pattern Aze :: LanguageCode
pattern Aze = LanguageCode' "AZE"

pattern Bak :: LanguageCode
pattern Bak = LanguageCode' "BAK"

pattern Bam :: LanguageCode
pattern Bam = LanguageCode' "BAM"

pattern Bel :: LanguageCode
pattern Bel = LanguageCode' "BEL"

pattern Ben :: LanguageCode
pattern Ben = LanguageCode' "BEN"

pattern Bih :: LanguageCode
pattern Bih = LanguageCode' "BIH"

pattern Bis :: LanguageCode
pattern Bis = LanguageCode' "BIS"

pattern Bod :: LanguageCode
pattern Bod = LanguageCode' "BOD"

pattern Bos :: LanguageCode
pattern Bos = LanguageCode' "BOS"

pattern Bre :: LanguageCode
pattern Bre = LanguageCode' "BRE"

pattern Bul :: LanguageCode
pattern Bul = LanguageCode' "BUL"

pattern Cat :: LanguageCode
pattern Cat = LanguageCode' "CAT"

pattern Ces :: LanguageCode
pattern Ces = LanguageCode' "CES"

pattern Cha :: LanguageCode
pattern Cha = LanguageCode' "CHA"

pattern Che :: LanguageCode
pattern Che = LanguageCode' "CHE"

pattern Chu :: LanguageCode
pattern Chu = LanguageCode' "CHU"

pattern Chv :: LanguageCode
pattern Chv = LanguageCode' "CHV"

pattern Cor :: LanguageCode
pattern Cor = LanguageCode' "COR"

pattern Cos :: LanguageCode
pattern Cos = LanguageCode' "COS"

pattern Cre :: LanguageCode
pattern Cre = LanguageCode' "CRE"

pattern Cym :: LanguageCode
pattern Cym = LanguageCode' "CYM"

pattern Dan :: LanguageCode
pattern Dan = LanguageCode' "DAN"

pattern Deu :: LanguageCode
pattern Deu = LanguageCode' "DEU"

pattern Div :: LanguageCode
pattern Div = LanguageCode' "DIV"

pattern Dzo :: LanguageCode
pattern Dzo = LanguageCode' "DZO"

pattern Ell :: LanguageCode
pattern Ell = LanguageCode' "ELL"

pattern Eng :: LanguageCode
pattern Eng = LanguageCode' "ENG"

pattern Enm :: LanguageCode
pattern Enm = LanguageCode' "ENM"

pattern Epo :: LanguageCode
pattern Epo = LanguageCode' "EPO"

pattern Est :: LanguageCode
pattern Est = LanguageCode' "EST"

pattern Eus :: LanguageCode
pattern Eus = LanguageCode' "EUS"

pattern Ewe :: LanguageCode
pattern Ewe = LanguageCode' "EWE"

pattern Fao :: LanguageCode
pattern Fao = LanguageCode' "FAO"

pattern Fas :: LanguageCode
pattern Fas = LanguageCode' "FAS"

pattern Fij :: LanguageCode
pattern Fij = LanguageCode' "FIJ"

pattern Fin :: LanguageCode
pattern Fin = LanguageCode' "FIN"

pattern Fra :: LanguageCode
pattern Fra = LanguageCode' "FRA"

pattern Frm :: LanguageCode
pattern Frm = LanguageCode' "FRM"

pattern Fry :: LanguageCode
pattern Fry = LanguageCode' "FRY"

pattern Ful :: LanguageCode
pattern Ful = LanguageCode' "FUL"

pattern Ger :: LanguageCode
pattern Ger = LanguageCode' "GER"

pattern Gla :: LanguageCode
pattern Gla = LanguageCode' "GLA"

pattern Gle :: LanguageCode
pattern Gle = LanguageCode' "GLE"

pattern Glg :: LanguageCode
pattern Glg = LanguageCode' "GLG"

pattern Glv :: LanguageCode
pattern Glv = LanguageCode' "GLV"

pattern Grn :: LanguageCode
pattern Grn = LanguageCode' "GRN"

pattern Guj :: LanguageCode
pattern Guj = LanguageCode' "GUJ"

pattern Hat :: LanguageCode
pattern Hat = LanguageCode' "HAT"

pattern Hau :: LanguageCode
pattern Hau = LanguageCode' "HAU"

pattern Heb :: LanguageCode
pattern Heb = LanguageCode' "HEB"

pattern Her :: LanguageCode
pattern Her = LanguageCode' "HER"

pattern Hin :: LanguageCode
pattern Hin = LanguageCode' "HIN"

pattern Hmo :: LanguageCode
pattern Hmo = LanguageCode' "HMO"

pattern Hrv :: LanguageCode
pattern Hrv = LanguageCode' "HRV"

pattern Hun :: LanguageCode
pattern Hun = LanguageCode' "HUN"

pattern Hye :: LanguageCode
pattern Hye = LanguageCode' "HYE"

pattern IPk :: LanguageCode
pattern IPk = LanguageCode' "IPK"

pattern Ibo :: LanguageCode
pattern Ibo = LanguageCode' "IBO"

pattern Ido :: LanguageCode
pattern Ido = LanguageCode' "IDO"

pattern Iii :: LanguageCode
pattern Iii = LanguageCode' "III"

pattern Iku :: LanguageCode
pattern Iku = LanguageCode' "IKU"

pattern Ile :: LanguageCode
pattern Ile = LanguageCode' "ILE"

pattern Ina :: LanguageCode
pattern Ina = LanguageCode' "INA"

pattern Ind :: LanguageCode
pattern Ind = LanguageCode' "IND"

pattern Isl :: LanguageCode
pattern Isl = LanguageCode' "ISL"

pattern Ita :: LanguageCode
pattern Ita = LanguageCode' "ITA"

pattern Jav :: LanguageCode
pattern Jav = LanguageCode' "JAV"

pattern Jpn :: LanguageCode
pattern Jpn = LanguageCode' "JPN"

pattern Kal :: LanguageCode
pattern Kal = LanguageCode' "KAL"

pattern Kan :: LanguageCode
pattern Kan = LanguageCode' "KAN"

pattern Kas :: LanguageCode
pattern Kas = LanguageCode' "KAS"

pattern Kat :: LanguageCode
pattern Kat = LanguageCode' "KAT"

pattern Kau :: LanguageCode
pattern Kau = LanguageCode' "KAU"

pattern Kaz :: LanguageCode
pattern Kaz = LanguageCode' "KAZ"

pattern Khm :: LanguageCode
pattern Khm = LanguageCode' "KHM"

pattern Kik :: LanguageCode
pattern Kik = LanguageCode' "KIK"

pattern Kin :: LanguageCode
pattern Kin = LanguageCode' "KIN"

pattern Kir :: LanguageCode
pattern Kir = LanguageCode' "KIR"

pattern Kom :: LanguageCode
pattern Kom = LanguageCode' "KOM"

pattern Kon :: LanguageCode
pattern Kon = LanguageCode' "KON"

pattern Kor :: LanguageCode
pattern Kor = LanguageCode' "KOR"

pattern Kua :: LanguageCode
pattern Kua = LanguageCode' "KUA"

pattern Kur :: LanguageCode
pattern Kur = LanguageCode' "KUR"

pattern Lao :: LanguageCode
pattern Lao = LanguageCode' "LAO"

pattern Lat :: LanguageCode
pattern Lat = LanguageCode' "LAT"

pattern Lav :: LanguageCode
pattern Lav = LanguageCode' "LAV"

pattern Lim :: LanguageCode
pattern Lim = LanguageCode' "LIM"

pattern Lin :: LanguageCode
pattern Lin = LanguageCode' "LIN"

pattern Lit :: LanguageCode
pattern Lit = LanguageCode' "LIT"

pattern Ltz :: LanguageCode
pattern Ltz = LanguageCode' "LTZ"

pattern Lub :: LanguageCode
pattern Lub = LanguageCode' "LUB"

pattern Lug :: LanguageCode
pattern Lug = LanguageCode' "LUG"

pattern Mah :: LanguageCode
pattern Mah = LanguageCode' "MAH"

pattern Mal :: LanguageCode
pattern Mal = LanguageCode' "MAL"

pattern Mar :: LanguageCode
pattern Mar = LanguageCode' "MAR"

pattern Mkd :: LanguageCode
pattern Mkd = LanguageCode' "MKD"

pattern Mlg :: LanguageCode
pattern Mlg = LanguageCode' "MLG"

pattern Mlt :: LanguageCode
pattern Mlt = LanguageCode' "MLT"

pattern Mon :: LanguageCode
pattern Mon = LanguageCode' "MON"

pattern Mri :: LanguageCode
pattern Mri = LanguageCode' "MRI"

pattern Msa :: LanguageCode
pattern Msa = LanguageCode' "MSA"

pattern Mya :: LanguageCode
pattern Mya = LanguageCode' "MYA"

pattern Nau :: LanguageCode
pattern Nau = LanguageCode' "NAU"

pattern Nav :: LanguageCode
pattern Nav = LanguageCode' "NAV"

pattern Nbl :: LanguageCode
pattern Nbl = LanguageCode' "NBL"

pattern Nde :: LanguageCode
pattern Nde = LanguageCode' "NDE"

pattern Ndo :: LanguageCode
pattern Ndo = LanguageCode' "NDO"

pattern Nep :: LanguageCode
pattern Nep = LanguageCode' "NEP"

pattern Nld :: LanguageCode
pattern Nld = LanguageCode' "NLD"

pattern Nno :: LanguageCode
pattern Nno = LanguageCode' "NNO"

pattern Nob :: LanguageCode
pattern Nob = LanguageCode' "NOB"

pattern Nor :: LanguageCode
pattern Nor = LanguageCode' "NOR"

pattern Nya :: LanguageCode
pattern Nya = LanguageCode' "NYA"

pattern OSs :: LanguageCode
pattern OSs = LanguageCode' "OSS"

pattern Oci :: LanguageCode
pattern Oci = LanguageCode' "OCI"

pattern Oji :: LanguageCode
pattern Oji = LanguageCode' "OJI"

pattern Ori :: LanguageCode
pattern Ori = LanguageCode' "ORI"

pattern Orj :: LanguageCode
pattern Orj = LanguageCode' "ORJ"

pattern Orm :: LanguageCode
pattern Orm = LanguageCode' "ORM"

pattern Pan :: LanguageCode
pattern Pan = LanguageCode' "PAN"

pattern Pli :: LanguageCode
pattern Pli = LanguageCode' "PLI"

pattern Pol :: LanguageCode
pattern Pol = LanguageCode' "POL"

pattern Por :: LanguageCode
pattern Por = LanguageCode' "POR"

pattern Pus :: LanguageCode
pattern Pus = LanguageCode' "PUS"

pattern Qaa :: LanguageCode
pattern Qaa = LanguageCode' "QAA"

pattern Qpc :: LanguageCode
pattern Qpc = LanguageCode' "QPC"

pattern Que :: LanguageCode
pattern Que = LanguageCode' "QUE"

pattern Roh :: LanguageCode
pattern Roh = LanguageCode' "ROH"

pattern Ron :: LanguageCode
pattern Ron = LanguageCode' "RON"

pattern Run :: LanguageCode
pattern Run = LanguageCode' "RUN"

pattern Rus :: LanguageCode
pattern Rus = LanguageCode' "RUS"

pattern Sag :: LanguageCode
pattern Sag = LanguageCode' "SAG"

pattern San :: LanguageCode
pattern San = LanguageCode' "SAN"

pattern Sin :: LanguageCode
pattern Sin = LanguageCode' "SIN"

pattern Slk :: LanguageCode
pattern Slk = LanguageCode' "SLK"

pattern Slv :: LanguageCode
pattern Slv = LanguageCode' "SLV"

pattern Sme :: LanguageCode
pattern Sme = LanguageCode' "SME"

pattern Smo :: LanguageCode
pattern Smo = LanguageCode' "SMO"

pattern Sna :: LanguageCode
pattern Sna = LanguageCode' "SNA"

pattern Snd :: LanguageCode
pattern Snd = LanguageCode' "SND"

pattern Som :: LanguageCode
pattern Som = LanguageCode' "SOM"

pattern Sot :: LanguageCode
pattern Sot = LanguageCode' "SOT"

pattern Spa :: LanguageCode
pattern Spa = LanguageCode' "SPA"

pattern Sqi :: LanguageCode
pattern Sqi = LanguageCode' "SQI"

pattern Srb :: LanguageCode
pattern Srb = LanguageCode' "SRB"

pattern Srd :: LanguageCode
pattern Srd = LanguageCode' "SRD"

pattern Ssw :: LanguageCode
pattern Ssw = LanguageCode' "SSW"

pattern Sun :: LanguageCode
pattern Sun = LanguageCode' "SUN"

pattern Swa :: LanguageCode
pattern Swa = LanguageCode' "SWA"

pattern Swe :: LanguageCode
pattern Swe = LanguageCode' "SWE"

pattern Tah :: LanguageCode
pattern Tah = LanguageCode' "TAH"

pattern Tam :: LanguageCode
pattern Tam = LanguageCode' "TAM"

pattern Tat :: LanguageCode
pattern Tat = LanguageCode' "TAT"

pattern Tel :: LanguageCode
pattern Tel = LanguageCode' "TEL"

pattern Tgk :: LanguageCode
pattern Tgk = LanguageCode' "TGK"

pattern Tgl :: LanguageCode
pattern Tgl = LanguageCode' "TGL"

pattern Tha :: LanguageCode
pattern Tha = LanguageCode' "THA"

pattern Tir :: LanguageCode
pattern Tir = LanguageCode' "TIR"

pattern Tng :: LanguageCode
pattern Tng = LanguageCode' "TNG"

pattern Ton :: LanguageCode
pattern Ton = LanguageCode' "TON"

pattern Tsn :: LanguageCode
pattern Tsn = LanguageCode' "TSN"

pattern Tso :: LanguageCode
pattern Tso = LanguageCode' "TSO"

pattern Tuk :: LanguageCode
pattern Tuk = LanguageCode' "TUK"

pattern Tur :: LanguageCode
pattern Tur = LanguageCode' "TUR"

pattern Twi :: LanguageCode
pattern Twi = LanguageCode' "TWI"

pattern Uig :: LanguageCode
pattern Uig = LanguageCode' "UIG"

pattern Ukr :: LanguageCode
pattern Ukr = LanguageCode' "UKR"

pattern Urd :: LanguageCode
pattern Urd = LanguageCode' "URD"

pattern Uzb :: LanguageCode
pattern Uzb = LanguageCode' "UZB"

pattern Ven :: LanguageCode
pattern Ven = LanguageCode' "VEN"

pattern Vie :: LanguageCode
pattern Vie = LanguageCode' "VIE"

pattern Vol :: LanguageCode
pattern Vol = LanguageCode' "VOL"

pattern Wln :: LanguageCode
pattern Wln = LanguageCode' "WLN"

pattern Wol :: LanguageCode
pattern Wol = LanguageCode' "WOL"

pattern Xho :: LanguageCode
pattern Xho = LanguageCode' "XHO"

pattern Yid :: LanguageCode
pattern Yid = LanguageCode' "YID"

pattern Yor :: LanguageCode
pattern Yor = LanguageCode' "YOR"

pattern Zha :: LanguageCode
pattern Zha = LanguageCode' "ZHA"

pattern Zho :: LanguageCode
pattern Zho = LanguageCode' "ZHO"

pattern Zul :: LanguageCode
pattern Zul = LanguageCode' "ZUL"

{-# COMPLETE
  Aar,
  Abk,
  Afr,
  Aka,
  Amh,
  Ara,
  Arg,
  Asm,
  Ava,
  Ave,
  Aym,
  Aze,
  Bak,
  Bam,
  Bel,
  Ben,
  Bih,
  Bis,
  Bod,
  Bos,
  Bre,
  Bul,
  Cat,
  Ces,
  Cha,
  Che,
  Chu,
  Chv,
  Cor,
  Cos,
  Cre,
  Cym,
  Dan,
  Deu,
  Div,
  Dzo,
  Ell,
  Eng,
  Enm,
  Epo,
  Est,
  Eus,
  Ewe,
  Fao,
  Fas,
  Fij,
  Fin,
  Fra,
  Frm,
  Fry,
  Ful,
  Ger,
  Gla,
  Gle,
  Glg,
  Glv,
  Grn,
  Guj,
  Hat,
  Hau,
  Heb,
  Her,
  Hin,
  Hmo,
  Hrv,
  Hun,
  Hye,
  IPk,
  Ibo,
  Ido,
  Iii,
  Iku,
  Ile,
  Ina,
  Ind,
  Isl,
  Ita,
  Jav,
  Jpn,
  Kal,
  Kan,
  Kas,
  Kat,
  Kau,
  Kaz,
  Khm,
  Kik,
  Kin,
  Kir,
  Kom,
  Kon,
  Kor,
  Kua,
  Kur,
  Lao,
  Lat,
  Lav,
  Lim,
  Lin,
  Lit,
  Ltz,
  Lub,
  Lug,
  Mah,
  Mal,
  Mar,
  Mkd,
  Mlg,
  Mlt,
  Mon,
  Mri,
  Msa,
  Mya,
  Nau,
  Nav,
  Nbl,
  Nde,
  Ndo,
  Nep,
  Nld,
  Nno,
  Nob,
  Nor,
  Nya,
  OSs,
  Oci,
  Oji,
  Ori,
  Orj,
  Orm,
  Pan,
  Pli,
  Pol,
  Por,
  Pus,
  Qaa,
  Qpc,
  Que,
  Roh,
  Ron,
  Run,
  Rus,
  Sag,
  San,
  Sin,
  Slk,
  Slv,
  Sme,
  Smo,
  Sna,
  Snd,
  Som,
  Sot,
  Spa,
  Sqi,
  Srb,
  Srd,
  Ssw,
  Sun,
  Swa,
  Swe,
  Tah,
  Tam,
  Tat,
  Tel,
  Tgk,
  Tgl,
  Tha,
  Tir,
  Tng,
  Ton,
  Tsn,
  Tso,
  Tuk,
  Tur,
  Twi,
  Uig,
  Ukr,
  Urd,
  Uzb,
  Ven,
  Vie,
  Vol,
  Wln,
  Wol,
  Xho,
  Yid,
  Yor,
  Zha,
  Zho,
  Zul,
  LanguageCode'
  #-}
