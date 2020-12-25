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
        LanguageCodeEng,
        LanguageCodeSpa,
        LanguageCodeFra,
        LanguageCodeDeu,
        LanguageCodeGer,
        LanguageCodeZho,
        LanguageCodeAra,
        LanguageCodeHin,
        LanguageCodeJpn,
        LanguageCodeRus,
        LanguageCodePor,
        LanguageCodeIta,
        LanguageCodeUrd,
        LanguageCodeVie,
        LanguageCodeKor,
        LanguageCodePan,
        LanguageCodeAbk,
        LanguageCodeAar,
        LanguageCodeAfr,
        LanguageCodeAka,
        LanguageCodeSqi,
        LanguageCodeAmh,
        LanguageCodeArg,
        LanguageCodeHye,
        LanguageCodeAsm,
        LanguageCodeAva,
        LanguageCodeAve,
        LanguageCodeAym,
        LanguageCodeAze,
        LanguageCodeBam,
        LanguageCodeBak,
        LanguageCodeEus,
        LanguageCodeBel,
        LanguageCodeBen,
        LanguageCodeBih,
        LanguageCodeBis,
        LanguageCodeBos,
        LanguageCodeBre,
        LanguageCodeBul,
        LanguageCodeMya,
        LanguageCodeCat,
        LanguageCodeKhm,
        LanguageCodeCha,
        LanguageCodeChe,
        LanguageCodeNya,
        LanguageCodeChu,
        LanguageCodeChv,
        LanguageCodeCor,
        LanguageCodeCos,
        LanguageCodeCre,
        LanguageCodeHrv,
        LanguageCodeCes,
        LanguageCodeDan,
        LanguageCodeDiv,
        LanguageCodeNld,
        LanguageCodeDzo,
        LanguageCodeEnm,
        LanguageCodeEpo,
        LanguageCodeEst,
        LanguageCodeEwe,
        LanguageCodeFao,
        LanguageCodeFij,
        LanguageCodeFin,
        LanguageCodeFrm,
        LanguageCodeFul,
        LanguageCodeGla,
        LanguageCodeGlg,
        LanguageCodeLug,
        LanguageCodeKat,
        LanguageCodeEll,
        LanguageCodeGrn,
        LanguageCodeGuj,
        LanguageCodeHat,
        LanguageCodeHau,
        LanguageCodeHeb,
        LanguageCodeHer,
        LanguageCodeHmo,
        LanguageCodeHun,
        LanguageCodeIsl,
        LanguageCodeIdo,
        LanguageCodeIbo,
        LanguageCodeInd,
        LanguageCodeIna,
        LanguageCodeIle,
        LanguageCodeIku,
        LanguageCodeIpk,
        LanguageCodeGle,
        LanguageCodeJav,
        LanguageCodeKal,
        LanguageCodeKan,
        LanguageCodeKau,
        LanguageCodeKas,
        LanguageCodeKaz,
        LanguageCodeKik,
        LanguageCodeKin,
        LanguageCodeKir,
        LanguageCodeKom,
        LanguageCodeKon,
        LanguageCodeKua,
        LanguageCodeKur,
        LanguageCodeLao,
        LanguageCodeLat,
        LanguageCodeLav,
        LanguageCodeLim,
        LanguageCodeLin,
        LanguageCodeLit,
        LanguageCodeLub,
        LanguageCodeLtz,
        LanguageCodeMkd,
        LanguageCodeMlg,
        LanguageCodeMsa,
        LanguageCodeMal,
        LanguageCodeMlt,
        LanguageCodeGlv,
        LanguageCodeMri,
        LanguageCodeMar,
        LanguageCodeMah,
        LanguageCodeMon,
        LanguageCodeNau,
        LanguageCodeNav,
        LanguageCodeNde,
        LanguageCodeNbl,
        LanguageCodeNdo,
        LanguageCodeNep,
        LanguageCodeSme,
        LanguageCodeNor,
        LanguageCodeNob,
        LanguageCodeNno,
        LanguageCodeOci,
        LanguageCodeOji,
        LanguageCodeOri,
        LanguageCodeOrm,
        LanguageCodeOss,
        LanguageCodePli,
        LanguageCodeFas,
        LanguageCodePol,
        LanguageCodePus,
        LanguageCodeQue,
        LanguageCodeQaa,
        LanguageCodeRon,
        LanguageCodeRoh,
        LanguageCodeRun,
        LanguageCodeSmo,
        LanguageCodeSag,
        LanguageCodeSan,
        LanguageCodeSrd,
        LanguageCodeSrb,
        LanguageCodeSna,
        LanguageCodeIii,
        LanguageCodeSnd,
        LanguageCodeSin,
        LanguageCodeSlk,
        LanguageCodeSlv,
        LanguageCodeSom,
        LanguageCodeSot,
        LanguageCodeSun,
        LanguageCodeSwa,
        LanguageCodeSsw,
        LanguageCodeSwe,
        LanguageCodeTgl,
        LanguageCodeTah,
        LanguageCodeTgk,
        LanguageCodeTam,
        LanguageCodeTat,
        LanguageCodeTel,
        LanguageCodeTha,
        LanguageCodeBod,
        LanguageCodeTir,
        LanguageCodeTon,
        LanguageCodeTso,
        LanguageCodeTsn,
        LanguageCodeTur,
        LanguageCodeTuk,
        LanguageCodeTwi,
        LanguageCodeUig,
        LanguageCodeUkr,
        LanguageCodeUzb,
        LanguageCodeVen,
        LanguageCodeVol,
        LanguageCodeWln,
        LanguageCodeCym,
        LanguageCodeFry,
        LanguageCodeWol,
        LanguageCodeXho,
        LanguageCodeYid,
        LanguageCodeYor,
        LanguageCodeZha,
        LanguageCodeZul,
        LanguageCodeOrj,
        LanguageCodeQpc,
        LanguageCodeTng,
        fromLanguageCode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Specify the language, using the ISO 639-2 three-letter code listed at https://www.loc.gov/standards/iso639-2/php/code_list.php.
newtype LanguageCode = LanguageCode' {fromLanguageCode :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern LanguageCodeEng :: LanguageCode
pattern LanguageCodeEng = LanguageCode' "ENG"

pattern LanguageCodeSpa :: LanguageCode
pattern LanguageCodeSpa = LanguageCode' "SPA"

pattern LanguageCodeFra :: LanguageCode
pattern LanguageCodeFra = LanguageCode' "FRA"

pattern LanguageCodeDeu :: LanguageCode
pattern LanguageCodeDeu = LanguageCode' "DEU"

pattern LanguageCodeGer :: LanguageCode
pattern LanguageCodeGer = LanguageCode' "GER"

pattern LanguageCodeZho :: LanguageCode
pattern LanguageCodeZho = LanguageCode' "ZHO"

pattern LanguageCodeAra :: LanguageCode
pattern LanguageCodeAra = LanguageCode' "ARA"

pattern LanguageCodeHin :: LanguageCode
pattern LanguageCodeHin = LanguageCode' "HIN"

pattern LanguageCodeJpn :: LanguageCode
pattern LanguageCodeJpn = LanguageCode' "JPN"

pattern LanguageCodeRus :: LanguageCode
pattern LanguageCodeRus = LanguageCode' "RUS"

pattern LanguageCodePor :: LanguageCode
pattern LanguageCodePor = LanguageCode' "POR"

pattern LanguageCodeIta :: LanguageCode
pattern LanguageCodeIta = LanguageCode' "ITA"

pattern LanguageCodeUrd :: LanguageCode
pattern LanguageCodeUrd = LanguageCode' "URD"

pattern LanguageCodeVie :: LanguageCode
pattern LanguageCodeVie = LanguageCode' "VIE"

pattern LanguageCodeKor :: LanguageCode
pattern LanguageCodeKor = LanguageCode' "KOR"

pattern LanguageCodePan :: LanguageCode
pattern LanguageCodePan = LanguageCode' "PAN"

pattern LanguageCodeAbk :: LanguageCode
pattern LanguageCodeAbk = LanguageCode' "ABK"

pattern LanguageCodeAar :: LanguageCode
pattern LanguageCodeAar = LanguageCode' "AAR"

pattern LanguageCodeAfr :: LanguageCode
pattern LanguageCodeAfr = LanguageCode' "AFR"

pattern LanguageCodeAka :: LanguageCode
pattern LanguageCodeAka = LanguageCode' "AKA"

pattern LanguageCodeSqi :: LanguageCode
pattern LanguageCodeSqi = LanguageCode' "SQI"

pattern LanguageCodeAmh :: LanguageCode
pattern LanguageCodeAmh = LanguageCode' "AMH"

pattern LanguageCodeArg :: LanguageCode
pattern LanguageCodeArg = LanguageCode' "ARG"

pattern LanguageCodeHye :: LanguageCode
pattern LanguageCodeHye = LanguageCode' "HYE"

pattern LanguageCodeAsm :: LanguageCode
pattern LanguageCodeAsm = LanguageCode' "ASM"

pattern LanguageCodeAva :: LanguageCode
pattern LanguageCodeAva = LanguageCode' "AVA"

pattern LanguageCodeAve :: LanguageCode
pattern LanguageCodeAve = LanguageCode' "AVE"

pattern LanguageCodeAym :: LanguageCode
pattern LanguageCodeAym = LanguageCode' "AYM"

pattern LanguageCodeAze :: LanguageCode
pattern LanguageCodeAze = LanguageCode' "AZE"

pattern LanguageCodeBam :: LanguageCode
pattern LanguageCodeBam = LanguageCode' "BAM"

pattern LanguageCodeBak :: LanguageCode
pattern LanguageCodeBak = LanguageCode' "BAK"

pattern LanguageCodeEus :: LanguageCode
pattern LanguageCodeEus = LanguageCode' "EUS"

pattern LanguageCodeBel :: LanguageCode
pattern LanguageCodeBel = LanguageCode' "BEL"

pattern LanguageCodeBen :: LanguageCode
pattern LanguageCodeBen = LanguageCode' "BEN"

pattern LanguageCodeBih :: LanguageCode
pattern LanguageCodeBih = LanguageCode' "BIH"

pattern LanguageCodeBis :: LanguageCode
pattern LanguageCodeBis = LanguageCode' "BIS"

pattern LanguageCodeBos :: LanguageCode
pattern LanguageCodeBos = LanguageCode' "BOS"

pattern LanguageCodeBre :: LanguageCode
pattern LanguageCodeBre = LanguageCode' "BRE"

pattern LanguageCodeBul :: LanguageCode
pattern LanguageCodeBul = LanguageCode' "BUL"

pattern LanguageCodeMya :: LanguageCode
pattern LanguageCodeMya = LanguageCode' "MYA"

pattern LanguageCodeCat :: LanguageCode
pattern LanguageCodeCat = LanguageCode' "CAT"

pattern LanguageCodeKhm :: LanguageCode
pattern LanguageCodeKhm = LanguageCode' "KHM"

pattern LanguageCodeCha :: LanguageCode
pattern LanguageCodeCha = LanguageCode' "CHA"

pattern LanguageCodeChe :: LanguageCode
pattern LanguageCodeChe = LanguageCode' "CHE"

pattern LanguageCodeNya :: LanguageCode
pattern LanguageCodeNya = LanguageCode' "NYA"

pattern LanguageCodeChu :: LanguageCode
pattern LanguageCodeChu = LanguageCode' "CHU"

pattern LanguageCodeChv :: LanguageCode
pattern LanguageCodeChv = LanguageCode' "CHV"

pattern LanguageCodeCor :: LanguageCode
pattern LanguageCodeCor = LanguageCode' "COR"

pattern LanguageCodeCos :: LanguageCode
pattern LanguageCodeCos = LanguageCode' "COS"

pattern LanguageCodeCre :: LanguageCode
pattern LanguageCodeCre = LanguageCode' "CRE"

pattern LanguageCodeHrv :: LanguageCode
pattern LanguageCodeHrv = LanguageCode' "HRV"

pattern LanguageCodeCes :: LanguageCode
pattern LanguageCodeCes = LanguageCode' "CES"

pattern LanguageCodeDan :: LanguageCode
pattern LanguageCodeDan = LanguageCode' "DAN"

pattern LanguageCodeDiv :: LanguageCode
pattern LanguageCodeDiv = LanguageCode' "DIV"

pattern LanguageCodeNld :: LanguageCode
pattern LanguageCodeNld = LanguageCode' "NLD"

pattern LanguageCodeDzo :: LanguageCode
pattern LanguageCodeDzo = LanguageCode' "DZO"

pattern LanguageCodeEnm :: LanguageCode
pattern LanguageCodeEnm = LanguageCode' "ENM"

pattern LanguageCodeEpo :: LanguageCode
pattern LanguageCodeEpo = LanguageCode' "EPO"

pattern LanguageCodeEst :: LanguageCode
pattern LanguageCodeEst = LanguageCode' "EST"

pattern LanguageCodeEwe :: LanguageCode
pattern LanguageCodeEwe = LanguageCode' "EWE"

pattern LanguageCodeFao :: LanguageCode
pattern LanguageCodeFao = LanguageCode' "FAO"

pattern LanguageCodeFij :: LanguageCode
pattern LanguageCodeFij = LanguageCode' "FIJ"

pattern LanguageCodeFin :: LanguageCode
pattern LanguageCodeFin = LanguageCode' "FIN"

pattern LanguageCodeFrm :: LanguageCode
pattern LanguageCodeFrm = LanguageCode' "FRM"

pattern LanguageCodeFul :: LanguageCode
pattern LanguageCodeFul = LanguageCode' "FUL"

pattern LanguageCodeGla :: LanguageCode
pattern LanguageCodeGla = LanguageCode' "GLA"

pattern LanguageCodeGlg :: LanguageCode
pattern LanguageCodeGlg = LanguageCode' "GLG"

pattern LanguageCodeLug :: LanguageCode
pattern LanguageCodeLug = LanguageCode' "LUG"

pattern LanguageCodeKat :: LanguageCode
pattern LanguageCodeKat = LanguageCode' "KAT"

pattern LanguageCodeEll :: LanguageCode
pattern LanguageCodeEll = LanguageCode' "ELL"

pattern LanguageCodeGrn :: LanguageCode
pattern LanguageCodeGrn = LanguageCode' "GRN"

pattern LanguageCodeGuj :: LanguageCode
pattern LanguageCodeGuj = LanguageCode' "GUJ"

pattern LanguageCodeHat :: LanguageCode
pattern LanguageCodeHat = LanguageCode' "HAT"

pattern LanguageCodeHau :: LanguageCode
pattern LanguageCodeHau = LanguageCode' "HAU"

pattern LanguageCodeHeb :: LanguageCode
pattern LanguageCodeHeb = LanguageCode' "HEB"

pattern LanguageCodeHer :: LanguageCode
pattern LanguageCodeHer = LanguageCode' "HER"

pattern LanguageCodeHmo :: LanguageCode
pattern LanguageCodeHmo = LanguageCode' "HMO"

pattern LanguageCodeHun :: LanguageCode
pattern LanguageCodeHun = LanguageCode' "HUN"

pattern LanguageCodeIsl :: LanguageCode
pattern LanguageCodeIsl = LanguageCode' "ISL"

pattern LanguageCodeIdo :: LanguageCode
pattern LanguageCodeIdo = LanguageCode' "IDO"

pattern LanguageCodeIbo :: LanguageCode
pattern LanguageCodeIbo = LanguageCode' "IBO"

pattern LanguageCodeInd :: LanguageCode
pattern LanguageCodeInd = LanguageCode' "IND"

pattern LanguageCodeIna :: LanguageCode
pattern LanguageCodeIna = LanguageCode' "INA"

pattern LanguageCodeIle :: LanguageCode
pattern LanguageCodeIle = LanguageCode' "ILE"

pattern LanguageCodeIku :: LanguageCode
pattern LanguageCodeIku = LanguageCode' "IKU"

pattern LanguageCodeIpk :: LanguageCode
pattern LanguageCodeIpk = LanguageCode' "IPK"

pattern LanguageCodeGle :: LanguageCode
pattern LanguageCodeGle = LanguageCode' "GLE"

pattern LanguageCodeJav :: LanguageCode
pattern LanguageCodeJav = LanguageCode' "JAV"

pattern LanguageCodeKal :: LanguageCode
pattern LanguageCodeKal = LanguageCode' "KAL"

pattern LanguageCodeKan :: LanguageCode
pattern LanguageCodeKan = LanguageCode' "KAN"

pattern LanguageCodeKau :: LanguageCode
pattern LanguageCodeKau = LanguageCode' "KAU"

pattern LanguageCodeKas :: LanguageCode
pattern LanguageCodeKas = LanguageCode' "KAS"

pattern LanguageCodeKaz :: LanguageCode
pattern LanguageCodeKaz = LanguageCode' "KAZ"

pattern LanguageCodeKik :: LanguageCode
pattern LanguageCodeKik = LanguageCode' "KIK"

pattern LanguageCodeKin :: LanguageCode
pattern LanguageCodeKin = LanguageCode' "KIN"

pattern LanguageCodeKir :: LanguageCode
pattern LanguageCodeKir = LanguageCode' "KIR"

pattern LanguageCodeKom :: LanguageCode
pattern LanguageCodeKom = LanguageCode' "KOM"

pattern LanguageCodeKon :: LanguageCode
pattern LanguageCodeKon = LanguageCode' "KON"

pattern LanguageCodeKua :: LanguageCode
pattern LanguageCodeKua = LanguageCode' "KUA"

pattern LanguageCodeKur :: LanguageCode
pattern LanguageCodeKur = LanguageCode' "KUR"

pattern LanguageCodeLao :: LanguageCode
pattern LanguageCodeLao = LanguageCode' "LAO"

pattern LanguageCodeLat :: LanguageCode
pattern LanguageCodeLat = LanguageCode' "LAT"

pattern LanguageCodeLav :: LanguageCode
pattern LanguageCodeLav = LanguageCode' "LAV"

pattern LanguageCodeLim :: LanguageCode
pattern LanguageCodeLim = LanguageCode' "LIM"

pattern LanguageCodeLin :: LanguageCode
pattern LanguageCodeLin = LanguageCode' "LIN"

pattern LanguageCodeLit :: LanguageCode
pattern LanguageCodeLit = LanguageCode' "LIT"

pattern LanguageCodeLub :: LanguageCode
pattern LanguageCodeLub = LanguageCode' "LUB"

pattern LanguageCodeLtz :: LanguageCode
pattern LanguageCodeLtz = LanguageCode' "LTZ"

pattern LanguageCodeMkd :: LanguageCode
pattern LanguageCodeMkd = LanguageCode' "MKD"

pattern LanguageCodeMlg :: LanguageCode
pattern LanguageCodeMlg = LanguageCode' "MLG"

pattern LanguageCodeMsa :: LanguageCode
pattern LanguageCodeMsa = LanguageCode' "MSA"

pattern LanguageCodeMal :: LanguageCode
pattern LanguageCodeMal = LanguageCode' "MAL"

pattern LanguageCodeMlt :: LanguageCode
pattern LanguageCodeMlt = LanguageCode' "MLT"

pattern LanguageCodeGlv :: LanguageCode
pattern LanguageCodeGlv = LanguageCode' "GLV"

pattern LanguageCodeMri :: LanguageCode
pattern LanguageCodeMri = LanguageCode' "MRI"

pattern LanguageCodeMar :: LanguageCode
pattern LanguageCodeMar = LanguageCode' "MAR"

pattern LanguageCodeMah :: LanguageCode
pattern LanguageCodeMah = LanguageCode' "MAH"

pattern LanguageCodeMon :: LanguageCode
pattern LanguageCodeMon = LanguageCode' "MON"

pattern LanguageCodeNau :: LanguageCode
pattern LanguageCodeNau = LanguageCode' "NAU"

pattern LanguageCodeNav :: LanguageCode
pattern LanguageCodeNav = LanguageCode' "NAV"

pattern LanguageCodeNde :: LanguageCode
pattern LanguageCodeNde = LanguageCode' "NDE"

pattern LanguageCodeNbl :: LanguageCode
pattern LanguageCodeNbl = LanguageCode' "NBL"

pattern LanguageCodeNdo :: LanguageCode
pattern LanguageCodeNdo = LanguageCode' "NDO"

pattern LanguageCodeNep :: LanguageCode
pattern LanguageCodeNep = LanguageCode' "NEP"

pattern LanguageCodeSme :: LanguageCode
pattern LanguageCodeSme = LanguageCode' "SME"

pattern LanguageCodeNor :: LanguageCode
pattern LanguageCodeNor = LanguageCode' "NOR"

pattern LanguageCodeNob :: LanguageCode
pattern LanguageCodeNob = LanguageCode' "NOB"

pattern LanguageCodeNno :: LanguageCode
pattern LanguageCodeNno = LanguageCode' "NNO"

pattern LanguageCodeOci :: LanguageCode
pattern LanguageCodeOci = LanguageCode' "OCI"

pattern LanguageCodeOji :: LanguageCode
pattern LanguageCodeOji = LanguageCode' "OJI"

pattern LanguageCodeOri :: LanguageCode
pattern LanguageCodeOri = LanguageCode' "ORI"

pattern LanguageCodeOrm :: LanguageCode
pattern LanguageCodeOrm = LanguageCode' "ORM"

pattern LanguageCodeOss :: LanguageCode
pattern LanguageCodeOss = LanguageCode' "OSS"

pattern LanguageCodePli :: LanguageCode
pattern LanguageCodePli = LanguageCode' "PLI"

pattern LanguageCodeFas :: LanguageCode
pattern LanguageCodeFas = LanguageCode' "FAS"

pattern LanguageCodePol :: LanguageCode
pattern LanguageCodePol = LanguageCode' "POL"

pattern LanguageCodePus :: LanguageCode
pattern LanguageCodePus = LanguageCode' "PUS"

pattern LanguageCodeQue :: LanguageCode
pattern LanguageCodeQue = LanguageCode' "QUE"

pattern LanguageCodeQaa :: LanguageCode
pattern LanguageCodeQaa = LanguageCode' "QAA"

pattern LanguageCodeRon :: LanguageCode
pattern LanguageCodeRon = LanguageCode' "RON"

pattern LanguageCodeRoh :: LanguageCode
pattern LanguageCodeRoh = LanguageCode' "ROH"

pattern LanguageCodeRun :: LanguageCode
pattern LanguageCodeRun = LanguageCode' "RUN"

pattern LanguageCodeSmo :: LanguageCode
pattern LanguageCodeSmo = LanguageCode' "SMO"

pattern LanguageCodeSag :: LanguageCode
pattern LanguageCodeSag = LanguageCode' "SAG"

pattern LanguageCodeSan :: LanguageCode
pattern LanguageCodeSan = LanguageCode' "SAN"

pattern LanguageCodeSrd :: LanguageCode
pattern LanguageCodeSrd = LanguageCode' "SRD"

pattern LanguageCodeSrb :: LanguageCode
pattern LanguageCodeSrb = LanguageCode' "SRB"

pattern LanguageCodeSna :: LanguageCode
pattern LanguageCodeSna = LanguageCode' "SNA"

pattern LanguageCodeIii :: LanguageCode
pattern LanguageCodeIii = LanguageCode' "III"

pattern LanguageCodeSnd :: LanguageCode
pattern LanguageCodeSnd = LanguageCode' "SND"

pattern LanguageCodeSin :: LanguageCode
pattern LanguageCodeSin = LanguageCode' "SIN"

pattern LanguageCodeSlk :: LanguageCode
pattern LanguageCodeSlk = LanguageCode' "SLK"

pattern LanguageCodeSlv :: LanguageCode
pattern LanguageCodeSlv = LanguageCode' "SLV"

pattern LanguageCodeSom :: LanguageCode
pattern LanguageCodeSom = LanguageCode' "SOM"

pattern LanguageCodeSot :: LanguageCode
pattern LanguageCodeSot = LanguageCode' "SOT"

pattern LanguageCodeSun :: LanguageCode
pattern LanguageCodeSun = LanguageCode' "SUN"

pattern LanguageCodeSwa :: LanguageCode
pattern LanguageCodeSwa = LanguageCode' "SWA"

pattern LanguageCodeSsw :: LanguageCode
pattern LanguageCodeSsw = LanguageCode' "SSW"

pattern LanguageCodeSwe :: LanguageCode
pattern LanguageCodeSwe = LanguageCode' "SWE"

pattern LanguageCodeTgl :: LanguageCode
pattern LanguageCodeTgl = LanguageCode' "TGL"

pattern LanguageCodeTah :: LanguageCode
pattern LanguageCodeTah = LanguageCode' "TAH"

pattern LanguageCodeTgk :: LanguageCode
pattern LanguageCodeTgk = LanguageCode' "TGK"

pattern LanguageCodeTam :: LanguageCode
pattern LanguageCodeTam = LanguageCode' "TAM"

pattern LanguageCodeTat :: LanguageCode
pattern LanguageCodeTat = LanguageCode' "TAT"

pattern LanguageCodeTel :: LanguageCode
pattern LanguageCodeTel = LanguageCode' "TEL"

pattern LanguageCodeTha :: LanguageCode
pattern LanguageCodeTha = LanguageCode' "THA"

pattern LanguageCodeBod :: LanguageCode
pattern LanguageCodeBod = LanguageCode' "BOD"

pattern LanguageCodeTir :: LanguageCode
pattern LanguageCodeTir = LanguageCode' "TIR"

pattern LanguageCodeTon :: LanguageCode
pattern LanguageCodeTon = LanguageCode' "TON"

pattern LanguageCodeTso :: LanguageCode
pattern LanguageCodeTso = LanguageCode' "TSO"

pattern LanguageCodeTsn :: LanguageCode
pattern LanguageCodeTsn = LanguageCode' "TSN"

pattern LanguageCodeTur :: LanguageCode
pattern LanguageCodeTur = LanguageCode' "TUR"

pattern LanguageCodeTuk :: LanguageCode
pattern LanguageCodeTuk = LanguageCode' "TUK"

pattern LanguageCodeTwi :: LanguageCode
pattern LanguageCodeTwi = LanguageCode' "TWI"

pattern LanguageCodeUig :: LanguageCode
pattern LanguageCodeUig = LanguageCode' "UIG"

pattern LanguageCodeUkr :: LanguageCode
pattern LanguageCodeUkr = LanguageCode' "UKR"

pattern LanguageCodeUzb :: LanguageCode
pattern LanguageCodeUzb = LanguageCode' "UZB"

pattern LanguageCodeVen :: LanguageCode
pattern LanguageCodeVen = LanguageCode' "VEN"

pattern LanguageCodeVol :: LanguageCode
pattern LanguageCodeVol = LanguageCode' "VOL"

pattern LanguageCodeWln :: LanguageCode
pattern LanguageCodeWln = LanguageCode' "WLN"

pattern LanguageCodeCym :: LanguageCode
pattern LanguageCodeCym = LanguageCode' "CYM"

pattern LanguageCodeFry :: LanguageCode
pattern LanguageCodeFry = LanguageCode' "FRY"

pattern LanguageCodeWol :: LanguageCode
pattern LanguageCodeWol = LanguageCode' "WOL"

pattern LanguageCodeXho :: LanguageCode
pattern LanguageCodeXho = LanguageCode' "XHO"

pattern LanguageCodeYid :: LanguageCode
pattern LanguageCodeYid = LanguageCode' "YID"

pattern LanguageCodeYor :: LanguageCode
pattern LanguageCodeYor = LanguageCode' "YOR"

pattern LanguageCodeZha :: LanguageCode
pattern LanguageCodeZha = LanguageCode' "ZHA"

pattern LanguageCodeZul :: LanguageCode
pattern LanguageCodeZul = LanguageCode' "ZUL"

pattern LanguageCodeOrj :: LanguageCode
pattern LanguageCodeOrj = LanguageCode' "ORJ"

pattern LanguageCodeQpc :: LanguageCode
pattern LanguageCodeQpc = LanguageCode' "QPC"

pattern LanguageCodeTng :: LanguageCode
pattern LanguageCodeTng = LanguageCode' "TNG"

{-# COMPLETE
  LanguageCodeEng,
  LanguageCodeSpa,
  LanguageCodeFra,
  LanguageCodeDeu,
  LanguageCodeGer,
  LanguageCodeZho,
  LanguageCodeAra,
  LanguageCodeHin,
  LanguageCodeJpn,
  LanguageCodeRus,
  LanguageCodePor,
  LanguageCodeIta,
  LanguageCodeUrd,
  LanguageCodeVie,
  LanguageCodeKor,
  LanguageCodePan,
  LanguageCodeAbk,
  LanguageCodeAar,
  LanguageCodeAfr,
  LanguageCodeAka,
  LanguageCodeSqi,
  LanguageCodeAmh,
  LanguageCodeArg,
  LanguageCodeHye,
  LanguageCodeAsm,
  LanguageCodeAva,
  LanguageCodeAve,
  LanguageCodeAym,
  LanguageCodeAze,
  LanguageCodeBam,
  LanguageCodeBak,
  LanguageCodeEus,
  LanguageCodeBel,
  LanguageCodeBen,
  LanguageCodeBih,
  LanguageCodeBis,
  LanguageCodeBos,
  LanguageCodeBre,
  LanguageCodeBul,
  LanguageCodeMya,
  LanguageCodeCat,
  LanguageCodeKhm,
  LanguageCodeCha,
  LanguageCodeChe,
  LanguageCodeNya,
  LanguageCodeChu,
  LanguageCodeChv,
  LanguageCodeCor,
  LanguageCodeCos,
  LanguageCodeCre,
  LanguageCodeHrv,
  LanguageCodeCes,
  LanguageCodeDan,
  LanguageCodeDiv,
  LanguageCodeNld,
  LanguageCodeDzo,
  LanguageCodeEnm,
  LanguageCodeEpo,
  LanguageCodeEst,
  LanguageCodeEwe,
  LanguageCodeFao,
  LanguageCodeFij,
  LanguageCodeFin,
  LanguageCodeFrm,
  LanguageCodeFul,
  LanguageCodeGla,
  LanguageCodeGlg,
  LanguageCodeLug,
  LanguageCodeKat,
  LanguageCodeEll,
  LanguageCodeGrn,
  LanguageCodeGuj,
  LanguageCodeHat,
  LanguageCodeHau,
  LanguageCodeHeb,
  LanguageCodeHer,
  LanguageCodeHmo,
  LanguageCodeHun,
  LanguageCodeIsl,
  LanguageCodeIdo,
  LanguageCodeIbo,
  LanguageCodeInd,
  LanguageCodeIna,
  LanguageCodeIle,
  LanguageCodeIku,
  LanguageCodeIpk,
  LanguageCodeGle,
  LanguageCodeJav,
  LanguageCodeKal,
  LanguageCodeKan,
  LanguageCodeKau,
  LanguageCodeKas,
  LanguageCodeKaz,
  LanguageCodeKik,
  LanguageCodeKin,
  LanguageCodeKir,
  LanguageCodeKom,
  LanguageCodeKon,
  LanguageCodeKua,
  LanguageCodeKur,
  LanguageCodeLao,
  LanguageCodeLat,
  LanguageCodeLav,
  LanguageCodeLim,
  LanguageCodeLin,
  LanguageCodeLit,
  LanguageCodeLub,
  LanguageCodeLtz,
  LanguageCodeMkd,
  LanguageCodeMlg,
  LanguageCodeMsa,
  LanguageCodeMal,
  LanguageCodeMlt,
  LanguageCodeGlv,
  LanguageCodeMri,
  LanguageCodeMar,
  LanguageCodeMah,
  LanguageCodeMon,
  LanguageCodeNau,
  LanguageCodeNav,
  LanguageCodeNde,
  LanguageCodeNbl,
  LanguageCodeNdo,
  LanguageCodeNep,
  LanguageCodeSme,
  LanguageCodeNor,
  LanguageCodeNob,
  LanguageCodeNno,
  LanguageCodeOci,
  LanguageCodeOji,
  LanguageCodeOri,
  LanguageCodeOrm,
  LanguageCodeOss,
  LanguageCodePli,
  LanguageCodeFas,
  LanguageCodePol,
  LanguageCodePus,
  LanguageCodeQue,
  LanguageCodeQaa,
  LanguageCodeRon,
  LanguageCodeRoh,
  LanguageCodeRun,
  LanguageCodeSmo,
  LanguageCodeSag,
  LanguageCodeSan,
  LanguageCodeSrd,
  LanguageCodeSrb,
  LanguageCodeSna,
  LanguageCodeIii,
  LanguageCodeSnd,
  LanguageCodeSin,
  LanguageCodeSlk,
  LanguageCodeSlv,
  LanguageCodeSom,
  LanguageCodeSot,
  LanguageCodeSun,
  LanguageCodeSwa,
  LanguageCodeSsw,
  LanguageCodeSwe,
  LanguageCodeTgl,
  LanguageCodeTah,
  LanguageCodeTgk,
  LanguageCodeTam,
  LanguageCodeTat,
  LanguageCodeTel,
  LanguageCodeTha,
  LanguageCodeBod,
  LanguageCodeTir,
  LanguageCodeTon,
  LanguageCodeTso,
  LanguageCodeTsn,
  LanguageCodeTur,
  LanguageCodeTuk,
  LanguageCodeTwi,
  LanguageCodeUig,
  LanguageCodeUkr,
  LanguageCodeUzb,
  LanguageCodeVen,
  LanguageCodeVol,
  LanguageCodeWln,
  LanguageCodeCym,
  LanguageCodeFry,
  LanguageCodeWol,
  LanguageCodeXho,
  LanguageCodeYid,
  LanguageCodeYor,
  LanguageCodeZha,
  LanguageCodeZul,
  LanguageCodeOrj,
  LanguageCodeQpc,
  LanguageCodeTng,
  LanguageCode'
  #-}
