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
        Eng,
        Spa,
        Fra,
        Deu,
        Ger,
        Zho,
        Ara,
        Hin,
        Jpn,
        Rus,
        Por,
        Ita,
        Urd,
        Vie,
        Kor,
        Pan,
        Abk,
        Aar,
        Afr,
        Aka,
        Sqi,
        Amh,
        Arg,
        Hye,
        Asm,
        Ava,
        Ave,
        Aym,
        Aze,
        Bam,
        Bak,
        Eus,
        Bel,
        Ben,
        Bih,
        Bis,
        Bos,
        Bre,
        Bul,
        Mya,
        Cat,
        Khm,
        Cha,
        Che,
        Nya,
        Chu,
        Chv,
        Cor,
        Cos,
        Cre,
        Hrv,
        Ces,
        Dan,
        Div,
        Nld,
        Dzo,
        Enm,
        Epo,
        Est,
        Ewe,
        Fao,
        Fij,
        Fin,
        Frm,
        Ful,
        Gla,
        Glg,
        Lug,
        Kat,
        Ell,
        Grn,
        Guj,
        Hat,
        Hau,
        Heb,
        Her,
        Hmo,
        Hun,
        Isl,
        Ido,
        Ibo,
        Ind,
        Ina,
        Ile,
        Iku,
        IPk,
        Gle,
        Jav,
        Kal,
        Kan,
        Kau,
        Kas,
        Kaz,
        Kik,
        Kin,
        Kir,
        Kom,
        Kon,
        Kua,
        Kur,
        Lao,
        Lat,
        Lav,
        Lim,
        Lin,
        Lit,
        Lub,
        Ltz,
        Mkd,
        Mlg,
        Msa,
        Mal,
        Mlt,
        Glv,
        Mri,
        Mar,
        Mah,
        Mon,
        Nau,
        Nav,
        Nde,
        Nbl,
        Ndo,
        Nep,
        Sme,
        Nor,
        Nob,
        Nno,
        Oci,
        Oji,
        Ori,
        Orm,
        OSs,
        Pli,
        Fas,
        Pol,
        Pus,
        Que,
        Qaa,
        Ron,
        Roh,
        Run,
        Smo,
        Sag,
        San,
        Srd,
        Srb,
        Sna,
        Iii,
        Snd,
        Sin,
        Slk,
        Slv,
        Som,
        Sot,
        Sun,
        Swa,
        Ssw,
        Swe,
        Tgl,
        Tah,
        Tgk,
        Tam,
        Tat,
        Tel,
        Tha,
        Bod,
        Tir,
        Ton,
        Tso,
        Tsn,
        Tur,
        Tuk,
        Twi,
        Uig,
        Ukr,
        Uzb,
        Ven,
        Vol,
        Wln,
        Cym,
        Fry,
        Wol,
        Xho,
        Yid,
        Yor,
        Zha,
        Zul,
        Orj,
        Qpc,
        Tng
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

pattern Eng :: LanguageCode
pattern Eng = LanguageCode' "ENG"

pattern Spa :: LanguageCode
pattern Spa = LanguageCode' "SPA"

pattern Fra :: LanguageCode
pattern Fra = LanguageCode' "FRA"

pattern Deu :: LanguageCode
pattern Deu = LanguageCode' "DEU"

pattern Ger :: LanguageCode
pattern Ger = LanguageCode' "GER"

pattern Zho :: LanguageCode
pattern Zho = LanguageCode' "ZHO"

pattern Ara :: LanguageCode
pattern Ara = LanguageCode' "ARA"

pattern Hin :: LanguageCode
pattern Hin = LanguageCode' "HIN"

pattern Jpn :: LanguageCode
pattern Jpn = LanguageCode' "JPN"

pattern Rus :: LanguageCode
pattern Rus = LanguageCode' "RUS"

pattern Por :: LanguageCode
pattern Por = LanguageCode' "POR"

pattern Ita :: LanguageCode
pattern Ita = LanguageCode' "ITA"

pattern Urd :: LanguageCode
pattern Urd = LanguageCode' "URD"

pattern Vie :: LanguageCode
pattern Vie = LanguageCode' "VIE"

pattern Kor :: LanguageCode
pattern Kor = LanguageCode' "KOR"

pattern Pan :: LanguageCode
pattern Pan = LanguageCode' "PAN"

pattern Abk :: LanguageCode
pattern Abk = LanguageCode' "ABK"

pattern Aar :: LanguageCode
pattern Aar = LanguageCode' "AAR"

pattern Afr :: LanguageCode
pattern Afr = LanguageCode' "AFR"

pattern Aka :: LanguageCode
pattern Aka = LanguageCode' "AKA"

pattern Sqi :: LanguageCode
pattern Sqi = LanguageCode' "SQI"

pattern Amh :: LanguageCode
pattern Amh = LanguageCode' "AMH"

pattern Arg :: LanguageCode
pattern Arg = LanguageCode' "ARG"

pattern Hye :: LanguageCode
pattern Hye = LanguageCode' "HYE"

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

pattern Bam :: LanguageCode
pattern Bam = LanguageCode' "BAM"

pattern Bak :: LanguageCode
pattern Bak = LanguageCode' "BAK"

pattern Eus :: LanguageCode
pattern Eus = LanguageCode' "EUS"

pattern Bel :: LanguageCode
pattern Bel = LanguageCode' "BEL"

pattern Ben :: LanguageCode
pattern Ben = LanguageCode' "BEN"

pattern Bih :: LanguageCode
pattern Bih = LanguageCode' "BIH"

pattern Bis :: LanguageCode
pattern Bis = LanguageCode' "BIS"

pattern Bos :: LanguageCode
pattern Bos = LanguageCode' "BOS"

pattern Bre :: LanguageCode
pattern Bre = LanguageCode' "BRE"

pattern Bul :: LanguageCode
pattern Bul = LanguageCode' "BUL"

pattern Mya :: LanguageCode
pattern Mya = LanguageCode' "MYA"

pattern Cat :: LanguageCode
pattern Cat = LanguageCode' "CAT"

pattern Khm :: LanguageCode
pattern Khm = LanguageCode' "KHM"

pattern Cha :: LanguageCode
pattern Cha = LanguageCode' "CHA"

pattern Che :: LanguageCode
pattern Che = LanguageCode' "CHE"

pattern Nya :: LanguageCode
pattern Nya = LanguageCode' "NYA"

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

pattern Hrv :: LanguageCode
pattern Hrv = LanguageCode' "HRV"

pattern Ces :: LanguageCode
pattern Ces = LanguageCode' "CES"

pattern Dan :: LanguageCode
pattern Dan = LanguageCode' "DAN"

pattern Div :: LanguageCode
pattern Div = LanguageCode' "DIV"

pattern Nld :: LanguageCode
pattern Nld = LanguageCode' "NLD"

pattern Dzo :: LanguageCode
pattern Dzo = LanguageCode' "DZO"

pattern Enm :: LanguageCode
pattern Enm = LanguageCode' "ENM"

pattern Epo :: LanguageCode
pattern Epo = LanguageCode' "EPO"

pattern Est :: LanguageCode
pattern Est = LanguageCode' "EST"

pattern Ewe :: LanguageCode
pattern Ewe = LanguageCode' "EWE"

pattern Fao :: LanguageCode
pattern Fao = LanguageCode' "FAO"

pattern Fij :: LanguageCode
pattern Fij = LanguageCode' "FIJ"

pattern Fin :: LanguageCode
pattern Fin = LanguageCode' "FIN"

pattern Frm :: LanguageCode
pattern Frm = LanguageCode' "FRM"

pattern Ful :: LanguageCode
pattern Ful = LanguageCode' "FUL"

pattern Gla :: LanguageCode
pattern Gla = LanguageCode' "GLA"

pattern Glg :: LanguageCode
pattern Glg = LanguageCode' "GLG"

pattern Lug :: LanguageCode
pattern Lug = LanguageCode' "LUG"

pattern Kat :: LanguageCode
pattern Kat = LanguageCode' "KAT"

pattern Ell :: LanguageCode
pattern Ell = LanguageCode' "ELL"

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

pattern Hmo :: LanguageCode
pattern Hmo = LanguageCode' "HMO"

pattern Hun :: LanguageCode
pattern Hun = LanguageCode' "HUN"

pattern Isl :: LanguageCode
pattern Isl = LanguageCode' "ISL"

pattern Ido :: LanguageCode
pattern Ido = LanguageCode' "IDO"

pattern Ibo :: LanguageCode
pattern Ibo = LanguageCode' "IBO"

pattern Ind :: LanguageCode
pattern Ind = LanguageCode' "IND"

pattern Ina :: LanguageCode
pattern Ina = LanguageCode' "INA"

pattern Ile :: LanguageCode
pattern Ile = LanguageCode' "ILE"

pattern Iku :: LanguageCode
pattern Iku = LanguageCode' "IKU"

pattern IPk :: LanguageCode
pattern IPk = LanguageCode' "IPK"

pattern Gle :: LanguageCode
pattern Gle = LanguageCode' "GLE"

pattern Jav :: LanguageCode
pattern Jav = LanguageCode' "JAV"

pattern Kal :: LanguageCode
pattern Kal = LanguageCode' "KAL"

pattern Kan :: LanguageCode
pattern Kan = LanguageCode' "KAN"

pattern Kau :: LanguageCode
pattern Kau = LanguageCode' "KAU"

pattern Kas :: LanguageCode
pattern Kas = LanguageCode' "KAS"

pattern Kaz :: LanguageCode
pattern Kaz = LanguageCode' "KAZ"

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

pattern Lub :: LanguageCode
pattern Lub = LanguageCode' "LUB"

pattern Ltz :: LanguageCode
pattern Ltz = LanguageCode' "LTZ"

pattern Mkd :: LanguageCode
pattern Mkd = LanguageCode' "MKD"

pattern Mlg :: LanguageCode
pattern Mlg = LanguageCode' "MLG"

pattern Msa :: LanguageCode
pattern Msa = LanguageCode' "MSA"

pattern Mal :: LanguageCode
pattern Mal = LanguageCode' "MAL"

pattern Mlt :: LanguageCode
pattern Mlt = LanguageCode' "MLT"

pattern Glv :: LanguageCode
pattern Glv = LanguageCode' "GLV"

pattern Mri :: LanguageCode
pattern Mri = LanguageCode' "MRI"

pattern Mar :: LanguageCode
pattern Mar = LanguageCode' "MAR"

pattern Mah :: LanguageCode
pattern Mah = LanguageCode' "MAH"

pattern Mon :: LanguageCode
pattern Mon = LanguageCode' "MON"

pattern Nau :: LanguageCode
pattern Nau = LanguageCode' "NAU"

pattern Nav :: LanguageCode
pattern Nav = LanguageCode' "NAV"

pattern Nde :: LanguageCode
pattern Nde = LanguageCode' "NDE"

pattern Nbl :: LanguageCode
pattern Nbl = LanguageCode' "NBL"

pattern Ndo :: LanguageCode
pattern Ndo = LanguageCode' "NDO"

pattern Nep :: LanguageCode
pattern Nep = LanguageCode' "NEP"

pattern Sme :: LanguageCode
pattern Sme = LanguageCode' "SME"

pattern Nor :: LanguageCode
pattern Nor = LanguageCode' "NOR"

pattern Nob :: LanguageCode
pattern Nob = LanguageCode' "NOB"

pattern Nno :: LanguageCode
pattern Nno = LanguageCode' "NNO"

pattern Oci :: LanguageCode
pattern Oci = LanguageCode' "OCI"

pattern Oji :: LanguageCode
pattern Oji = LanguageCode' "OJI"

pattern Ori :: LanguageCode
pattern Ori = LanguageCode' "ORI"

pattern Orm :: LanguageCode
pattern Orm = LanguageCode' "ORM"

pattern OSs :: LanguageCode
pattern OSs = LanguageCode' "OSS"

pattern Pli :: LanguageCode
pattern Pli = LanguageCode' "PLI"

pattern Fas :: LanguageCode
pattern Fas = LanguageCode' "FAS"

pattern Pol :: LanguageCode
pattern Pol = LanguageCode' "POL"

pattern Pus :: LanguageCode
pattern Pus = LanguageCode' "PUS"

pattern Que :: LanguageCode
pattern Que = LanguageCode' "QUE"

pattern Qaa :: LanguageCode
pattern Qaa = LanguageCode' "QAA"

pattern Ron :: LanguageCode
pattern Ron = LanguageCode' "RON"

pattern Roh :: LanguageCode
pattern Roh = LanguageCode' "ROH"

pattern Run :: LanguageCode
pattern Run = LanguageCode' "RUN"

pattern Smo :: LanguageCode
pattern Smo = LanguageCode' "SMO"

pattern Sag :: LanguageCode
pattern Sag = LanguageCode' "SAG"

pattern San :: LanguageCode
pattern San = LanguageCode' "SAN"

pattern Srd :: LanguageCode
pattern Srd = LanguageCode' "SRD"

pattern Srb :: LanguageCode
pattern Srb = LanguageCode' "SRB"

pattern Sna :: LanguageCode
pattern Sna = LanguageCode' "SNA"

pattern Iii :: LanguageCode
pattern Iii = LanguageCode' "III"

pattern Snd :: LanguageCode
pattern Snd = LanguageCode' "SND"

pattern Sin :: LanguageCode
pattern Sin = LanguageCode' "SIN"

pattern Slk :: LanguageCode
pattern Slk = LanguageCode' "SLK"

pattern Slv :: LanguageCode
pattern Slv = LanguageCode' "SLV"

pattern Som :: LanguageCode
pattern Som = LanguageCode' "SOM"

pattern Sot :: LanguageCode
pattern Sot = LanguageCode' "SOT"

pattern Sun :: LanguageCode
pattern Sun = LanguageCode' "SUN"

pattern Swa :: LanguageCode
pattern Swa = LanguageCode' "SWA"

pattern Ssw :: LanguageCode
pattern Ssw = LanguageCode' "SSW"

pattern Swe :: LanguageCode
pattern Swe = LanguageCode' "SWE"

pattern Tgl :: LanguageCode
pattern Tgl = LanguageCode' "TGL"

pattern Tah :: LanguageCode
pattern Tah = LanguageCode' "TAH"

pattern Tgk :: LanguageCode
pattern Tgk = LanguageCode' "TGK"

pattern Tam :: LanguageCode
pattern Tam = LanguageCode' "TAM"

pattern Tat :: LanguageCode
pattern Tat = LanguageCode' "TAT"

pattern Tel :: LanguageCode
pattern Tel = LanguageCode' "TEL"

pattern Tha :: LanguageCode
pattern Tha = LanguageCode' "THA"

pattern Bod :: LanguageCode
pattern Bod = LanguageCode' "BOD"

pattern Tir :: LanguageCode
pattern Tir = LanguageCode' "TIR"

pattern Ton :: LanguageCode
pattern Ton = LanguageCode' "TON"

pattern Tso :: LanguageCode
pattern Tso = LanguageCode' "TSO"

pattern Tsn :: LanguageCode
pattern Tsn = LanguageCode' "TSN"

pattern Tur :: LanguageCode
pattern Tur = LanguageCode' "TUR"

pattern Tuk :: LanguageCode
pattern Tuk = LanguageCode' "TUK"

pattern Twi :: LanguageCode
pattern Twi = LanguageCode' "TWI"

pattern Uig :: LanguageCode
pattern Uig = LanguageCode' "UIG"

pattern Ukr :: LanguageCode
pattern Ukr = LanguageCode' "UKR"

pattern Uzb :: LanguageCode
pattern Uzb = LanguageCode' "UZB"

pattern Ven :: LanguageCode
pattern Ven = LanguageCode' "VEN"

pattern Vol :: LanguageCode
pattern Vol = LanguageCode' "VOL"

pattern Wln :: LanguageCode
pattern Wln = LanguageCode' "WLN"

pattern Cym :: LanguageCode
pattern Cym = LanguageCode' "CYM"

pattern Fry :: LanguageCode
pattern Fry = LanguageCode' "FRY"

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

pattern Zul :: LanguageCode
pattern Zul = LanguageCode' "ZUL"

pattern Orj :: LanguageCode
pattern Orj = LanguageCode' "ORJ"

pattern Qpc :: LanguageCode
pattern Qpc = LanguageCode' "QPC"

pattern Tng :: LanguageCode
pattern Tng = LanguageCode' "TNG"

{-# COMPLETE
  Eng,
  Spa,
  Fra,
  Deu,
  Ger,
  Zho,
  Ara,
  Hin,
  Jpn,
  Rus,
  Por,
  Ita,
  Urd,
  Vie,
  Kor,
  Pan,
  Abk,
  Aar,
  Afr,
  Aka,
  Sqi,
  Amh,
  Arg,
  Hye,
  Asm,
  Ava,
  Ave,
  Aym,
  Aze,
  Bam,
  Bak,
  Eus,
  Bel,
  Ben,
  Bih,
  Bis,
  Bos,
  Bre,
  Bul,
  Mya,
  Cat,
  Khm,
  Cha,
  Che,
  Nya,
  Chu,
  Chv,
  Cor,
  Cos,
  Cre,
  Hrv,
  Ces,
  Dan,
  Div,
  Nld,
  Dzo,
  Enm,
  Epo,
  Est,
  Ewe,
  Fao,
  Fij,
  Fin,
  Frm,
  Ful,
  Gla,
  Glg,
  Lug,
  Kat,
  Ell,
  Grn,
  Guj,
  Hat,
  Hau,
  Heb,
  Her,
  Hmo,
  Hun,
  Isl,
  Ido,
  Ibo,
  Ind,
  Ina,
  Ile,
  Iku,
  IPk,
  Gle,
  Jav,
  Kal,
  Kan,
  Kau,
  Kas,
  Kaz,
  Kik,
  Kin,
  Kir,
  Kom,
  Kon,
  Kua,
  Kur,
  Lao,
  Lat,
  Lav,
  Lim,
  Lin,
  Lit,
  Lub,
  Ltz,
  Mkd,
  Mlg,
  Msa,
  Mal,
  Mlt,
  Glv,
  Mri,
  Mar,
  Mah,
  Mon,
  Nau,
  Nav,
  Nde,
  Nbl,
  Ndo,
  Nep,
  Sme,
  Nor,
  Nob,
  Nno,
  Oci,
  Oji,
  Ori,
  Orm,
  OSs,
  Pli,
  Fas,
  Pol,
  Pus,
  Que,
  Qaa,
  Ron,
  Roh,
  Run,
  Smo,
  Sag,
  San,
  Srd,
  Srb,
  Sna,
  Iii,
  Snd,
  Sin,
  Slk,
  Slv,
  Som,
  Sot,
  Sun,
  Swa,
  Ssw,
  Swe,
  Tgl,
  Tah,
  Tgk,
  Tam,
  Tat,
  Tel,
  Tha,
  Bod,
  Tir,
  Ton,
  Tso,
  Tsn,
  Tur,
  Tuk,
  Twi,
  Uig,
  Ukr,
  Uzb,
  Ven,
  Vol,
  Wln,
  Cym,
  Fry,
  Wol,
  Xho,
  Yid,
  Yor,
  Zha,
  Zul,
  Orj,
  Qpc,
  Tng,
  LanguageCode'
  #-}
