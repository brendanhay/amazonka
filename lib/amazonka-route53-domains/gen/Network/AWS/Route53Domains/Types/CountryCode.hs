{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.CountryCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.CountryCode
  ( CountryCode
      ( CountryCode',
        CountryCodeAD,
        CountryCodeAE,
        CountryCodeAF,
        CountryCodeAG,
        CountryCodeAI,
        CountryCodeAL,
        CountryCodeAM,
        CountryCodeAN,
        CountryCodeAO,
        CountryCodeAQ,
        CountryCodeAR,
        CountryCodeAS,
        CountryCodeAT,
        CountryCodeAU,
        CountryCodeAW,
        CountryCodeAZ,
        CountryCodeBA,
        CountryCodeBB,
        CountryCodeBD,
        CountryCodeBE,
        CountryCodeBF,
        CountryCodeBG,
        CountryCodeBH,
        CountryCodeBI,
        CountryCodeBJ,
        CountryCodeBL,
        CountryCodeBM,
        CountryCodeBN,
        CountryCodeBO,
        CountryCodeBR,
        CountryCodeBS,
        CountryCodeBT,
        CountryCodeBW,
        CountryCodeBY,
        CountryCodeBZ,
        CountryCodeCA,
        CountryCodeCC,
        CountryCodeCD,
        CountryCodeCF,
        CountryCodeCG,
        CountryCodeCH,
        CountryCodeCI,
        CountryCodeCK,
        CountryCodeCL,
        CountryCodeCM,
        CountryCodeCN,
        CountryCodeCO,
        CountryCodeCR,
        CountryCodeCU,
        CountryCodeCV,
        CountryCodeCX,
        CountryCodeCY,
        CountryCodeCZ,
        CountryCodeDE,
        CountryCodeDJ,
        CountryCodeDK,
        CountryCodeDM,
        CountryCodeDO,
        CountryCodeDZ,
        CountryCodeEC,
        CountryCodeEE,
        CountryCodeEG,
        CountryCodeER,
        CountryCodeES,
        CountryCodeET,
        CountryCodeFI,
        CountryCodeFJ,
        CountryCodeFK,
        CountryCodeFM,
        CountryCodeFO,
        CountryCodeFR,
        CountryCodeGA,
        CountryCodeGB,
        CountryCodeGD,
        CountryCodeGE,
        CountryCodeGH,
        CountryCodeGI,
        CountryCodeGL,
        CountryCodeGM,
        CountryCodeGN,
        CountryCodeGQ,
        CountryCodeGR,
        CountryCodeGT,
        CountryCodeGU,
        CountryCodeGW,
        CountryCodeGY,
        CountryCodeHK,
        CountryCodeHN,
        CountryCodeHR,
        CountryCodeHT,
        CountryCodeHU,
        CountryCodeID,
        CountryCodeIE,
        CountryCodeIL,
        CountryCodeIM,
        CountryCodeIN,
        CountryCodeIQ,
        CountryCodeIR,
        CountryCodeIS,
        CountryCodeIT,
        CountryCodeJM,
        CountryCodeJO,
        CountryCodeJP,
        CountryCodeKE,
        CountryCodeKG,
        CountryCodeKH,
        CountryCodeKI,
        CountryCodeKM,
        CountryCodeKN,
        CountryCodeKP,
        CountryCodeKR,
        CountryCodeKW,
        CountryCodeKY,
        CountryCodeKZ,
        CountryCodeLA,
        CountryCodeLB,
        CountryCodeLC,
        CountryCodeLI,
        CountryCodeLK,
        CountryCodeLR,
        CountryCodeLS,
        CountryCodeLT,
        CountryCodeLU,
        CountryCodeLV,
        CountryCodeLY,
        CountryCodeMA,
        CountryCodeMC,
        CountryCodeMD,
        CountryCodeME,
        CountryCodeMF,
        CountryCodeMG,
        CountryCodeMH,
        CountryCodeMK,
        CountryCodeML,
        CountryCodeMM,
        CountryCodeMN,
        CountryCodeMO,
        CountryCodeMP,
        CountryCodeMR,
        CountryCodeMS,
        CountryCodeMT,
        CountryCodeMU,
        CountryCodeMV,
        CountryCodeMW,
        CountryCodeMX,
        CountryCodeMY,
        CountryCodeMZ,
        CountryCodeNA,
        CountryCodeNC,
        CountryCodeNE,
        CountryCodeNG,
        CountryCodeNI,
        CountryCodeNL,
        CountryCodeNO,
        CountryCodeNP,
        CountryCodeNR,
        CountryCodeNU,
        CountryCodeNZ,
        CountryCodeOM,
        CountryCodePA,
        CountryCodePE,
        CountryCodePF,
        CountryCodePG,
        CountryCodePH,
        CountryCodePK,
        CountryCodePL,
        CountryCodePM,
        CountryCodePN,
        CountryCodePR,
        CountryCodePT,
        CountryCodePW,
        CountryCodePY,
        CountryCodeQA,
        CountryCodeRO,
        CountryCodeRS,
        CountryCodeRU,
        CountryCodeRW,
        CountryCodeSA,
        CountryCodeSB,
        CountryCodeSC,
        CountryCodeSD,
        CountryCodeSE,
        CountryCodeSG,
        CountryCodeSH,
        CountryCodeSI,
        CountryCodeSK,
        CountryCodeSL,
        CountryCodeSM,
        CountryCodeSN,
        CountryCodeSO,
        CountryCodeSR,
        CountryCodeST,
        CountryCodeSV,
        CountryCodeSY,
        CountryCodeSZ,
        CountryCodeTC,
        CountryCodeTD,
        CountryCodeTG,
        CountryCodeTH,
        CountryCodeTJ,
        CountryCodeTK,
        CountryCodeTL,
        CountryCodeTM,
        CountryCodeTN,
        CountryCodeTO,
        CountryCodeTR,
        CountryCodeTT,
        CountryCodeTV,
        CountryCodeTW,
        CountryCodeTZ,
        CountryCodeUA,
        CountryCodeUG,
        CountryCodeUS,
        CountryCodeUY,
        CountryCodeUZ,
        CountryCodeVA,
        CountryCodeVC,
        CountryCodeVE,
        CountryCodeVG,
        CountryCodeVI,
        CountryCodeVN,
        CountryCodeVU,
        CountryCodeWF,
        CountryCodeWS,
        CountryCodeYE,
        CountryCodeYT,
        CountryCodeZA,
        CountryCodeZM,
        CountryCodeZW,
        fromCountryCode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype CountryCode = CountryCode' {fromCountryCode :: Core.Text}
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

pattern CountryCodeAD :: CountryCode
pattern CountryCodeAD = CountryCode' "AD"

pattern CountryCodeAE :: CountryCode
pattern CountryCodeAE = CountryCode' "AE"

pattern CountryCodeAF :: CountryCode
pattern CountryCodeAF = CountryCode' "AF"

pattern CountryCodeAG :: CountryCode
pattern CountryCodeAG = CountryCode' "AG"

pattern CountryCodeAI :: CountryCode
pattern CountryCodeAI = CountryCode' "AI"

pattern CountryCodeAL :: CountryCode
pattern CountryCodeAL = CountryCode' "AL"

pattern CountryCodeAM :: CountryCode
pattern CountryCodeAM = CountryCode' "AM"

pattern CountryCodeAN :: CountryCode
pattern CountryCodeAN = CountryCode' "AN"

pattern CountryCodeAO :: CountryCode
pattern CountryCodeAO = CountryCode' "AO"

pattern CountryCodeAQ :: CountryCode
pattern CountryCodeAQ = CountryCode' "AQ"

pattern CountryCodeAR :: CountryCode
pattern CountryCodeAR = CountryCode' "AR"

pattern CountryCodeAS :: CountryCode
pattern CountryCodeAS = CountryCode' "AS"

pattern CountryCodeAT :: CountryCode
pattern CountryCodeAT = CountryCode' "AT"

pattern CountryCodeAU :: CountryCode
pattern CountryCodeAU = CountryCode' "AU"

pattern CountryCodeAW :: CountryCode
pattern CountryCodeAW = CountryCode' "AW"

pattern CountryCodeAZ :: CountryCode
pattern CountryCodeAZ = CountryCode' "AZ"

pattern CountryCodeBA :: CountryCode
pattern CountryCodeBA = CountryCode' "BA"

pattern CountryCodeBB :: CountryCode
pattern CountryCodeBB = CountryCode' "BB"

pattern CountryCodeBD :: CountryCode
pattern CountryCodeBD = CountryCode' "BD"

pattern CountryCodeBE :: CountryCode
pattern CountryCodeBE = CountryCode' "BE"

pattern CountryCodeBF :: CountryCode
pattern CountryCodeBF = CountryCode' "BF"

pattern CountryCodeBG :: CountryCode
pattern CountryCodeBG = CountryCode' "BG"

pattern CountryCodeBH :: CountryCode
pattern CountryCodeBH = CountryCode' "BH"

pattern CountryCodeBI :: CountryCode
pattern CountryCodeBI = CountryCode' "BI"

pattern CountryCodeBJ :: CountryCode
pattern CountryCodeBJ = CountryCode' "BJ"

pattern CountryCodeBL :: CountryCode
pattern CountryCodeBL = CountryCode' "BL"

pattern CountryCodeBM :: CountryCode
pattern CountryCodeBM = CountryCode' "BM"

pattern CountryCodeBN :: CountryCode
pattern CountryCodeBN = CountryCode' "BN"

pattern CountryCodeBO :: CountryCode
pattern CountryCodeBO = CountryCode' "BO"

pattern CountryCodeBR :: CountryCode
pattern CountryCodeBR = CountryCode' "BR"

pattern CountryCodeBS :: CountryCode
pattern CountryCodeBS = CountryCode' "BS"

pattern CountryCodeBT :: CountryCode
pattern CountryCodeBT = CountryCode' "BT"

pattern CountryCodeBW :: CountryCode
pattern CountryCodeBW = CountryCode' "BW"

pattern CountryCodeBY :: CountryCode
pattern CountryCodeBY = CountryCode' "BY"

pattern CountryCodeBZ :: CountryCode
pattern CountryCodeBZ = CountryCode' "BZ"

pattern CountryCodeCA :: CountryCode
pattern CountryCodeCA = CountryCode' "CA"

pattern CountryCodeCC :: CountryCode
pattern CountryCodeCC = CountryCode' "CC"

pattern CountryCodeCD :: CountryCode
pattern CountryCodeCD = CountryCode' "CD"

pattern CountryCodeCF :: CountryCode
pattern CountryCodeCF = CountryCode' "CF"

pattern CountryCodeCG :: CountryCode
pattern CountryCodeCG = CountryCode' "CG"

pattern CountryCodeCH :: CountryCode
pattern CountryCodeCH = CountryCode' "CH"

pattern CountryCodeCI :: CountryCode
pattern CountryCodeCI = CountryCode' "CI"

pattern CountryCodeCK :: CountryCode
pattern CountryCodeCK = CountryCode' "CK"

pattern CountryCodeCL :: CountryCode
pattern CountryCodeCL = CountryCode' "CL"

pattern CountryCodeCM :: CountryCode
pattern CountryCodeCM = CountryCode' "CM"

pattern CountryCodeCN :: CountryCode
pattern CountryCodeCN = CountryCode' "CN"

pattern CountryCodeCO :: CountryCode
pattern CountryCodeCO = CountryCode' "CO"

pattern CountryCodeCR :: CountryCode
pattern CountryCodeCR = CountryCode' "CR"

pattern CountryCodeCU :: CountryCode
pattern CountryCodeCU = CountryCode' "CU"

pattern CountryCodeCV :: CountryCode
pattern CountryCodeCV = CountryCode' "CV"

pattern CountryCodeCX :: CountryCode
pattern CountryCodeCX = CountryCode' "CX"

pattern CountryCodeCY :: CountryCode
pattern CountryCodeCY = CountryCode' "CY"

pattern CountryCodeCZ :: CountryCode
pattern CountryCodeCZ = CountryCode' "CZ"

pattern CountryCodeDE :: CountryCode
pattern CountryCodeDE = CountryCode' "DE"

pattern CountryCodeDJ :: CountryCode
pattern CountryCodeDJ = CountryCode' "DJ"

pattern CountryCodeDK :: CountryCode
pattern CountryCodeDK = CountryCode' "DK"

pattern CountryCodeDM :: CountryCode
pattern CountryCodeDM = CountryCode' "DM"

pattern CountryCodeDO :: CountryCode
pattern CountryCodeDO = CountryCode' "DO"

pattern CountryCodeDZ :: CountryCode
pattern CountryCodeDZ = CountryCode' "DZ"

pattern CountryCodeEC :: CountryCode
pattern CountryCodeEC = CountryCode' "EC"

pattern CountryCodeEE :: CountryCode
pattern CountryCodeEE = CountryCode' "EE"

pattern CountryCodeEG :: CountryCode
pattern CountryCodeEG = CountryCode' "EG"

pattern CountryCodeER :: CountryCode
pattern CountryCodeER = CountryCode' "ER"

pattern CountryCodeES :: CountryCode
pattern CountryCodeES = CountryCode' "ES"

pattern CountryCodeET :: CountryCode
pattern CountryCodeET = CountryCode' "ET"

pattern CountryCodeFI :: CountryCode
pattern CountryCodeFI = CountryCode' "FI"

pattern CountryCodeFJ :: CountryCode
pattern CountryCodeFJ = CountryCode' "FJ"

pattern CountryCodeFK :: CountryCode
pattern CountryCodeFK = CountryCode' "FK"

pattern CountryCodeFM :: CountryCode
pattern CountryCodeFM = CountryCode' "FM"

pattern CountryCodeFO :: CountryCode
pattern CountryCodeFO = CountryCode' "FO"

pattern CountryCodeFR :: CountryCode
pattern CountryCodeFR = CountryCode' "FR"

pattern CountryCodeGA :: CountryCode
pattern CountryCodeGA = CountryCode' "GA"

pattern CountryCodeGB :: CountryCode
pattern CountryCodeGB = CountryCode' "GB"

pattern CountryCodeGD :: CountryCode
pattern CountryCodeGD = CountryCode' "GD"

pattern CountryCodeGE :: CountryCode
pattern CountryCodeGE = CountryCode' "GE"

pattern CountryCodeGH :: CountryCode
pattern CountryCodeGH = CountryCode' "GH"

pattern CountryCodeGI :: CountryCode
pattern CountryCodeGI = CountryCode' "GI"

pattern CountryCodeGL :: CountryCode
pattern CountryCodeGL = CountryCode' "GL"

pattern CountryCodeGM :: CountryCode
pattern CountryCodeGM = CountryCode' "GM"

pattern CountryCodeGN :: CountryCode
pattern CountryCodeGN = CountryCode' "GN"

pattern CountryCodeGQ :: CountryCode
pattern CountryCodeGQ = CountryCode' "GQ"

pattern CountryCodeGR :: CountryCode
pattern CountryCodeGR = CountryCode' "GR"

pattern CountryCodeGT :: CountryCode
pattern CountryCodeGT = CountryCode' "GT"

pattern CountryCodeGU :: CountryCode
pattern CountryCodeGU = CountryCode' "GU"

pattern CountryCodeGW :: CountryCode
pattern CountryCodeGW = CountryCode' "GW"

pattern CountryCodeGY :: CountryCode
pattern CountryCodeGY = CountryCode' "GY"

pattern CountryCodeHK :: CountryCode
pattern CountryCodeHK = CountryCode' "HK"

pattern CountryCodeHN :: CountryCode
pattern CountryCodeHN = CountryCode' "HN"

pattern CountryCodeHR :: CountryCode
pattern CountryCodeHR = CountryCode' "HR"

pattern CountryCodeHT :: CountryCode
pattern CountryCodeHT = CountryCode' "HT"

pattern CountryCodeHU :: CountryCode
pattern CountryCodeHU = CountryCode' "HU"

pattern CountryCodeID :: CountryCode
pattern CountryCodeID = CountryCode' "ID"

pattern CountryCodeIE :: CountryCode
pattern CountryCodeIE = CountryCode' "IE"

pattern CountryCodeIL :: CountryCode
pattern CountryCodeIL = CountryCode' "IL"

pattern CountryCodeIM :: CountryCode
pattern CountryCodeIM = CountryCode' "IM"

pattern CountryCodeIN :: CountryCode
pattern CountryCodeIN = CountryCode' "IN"

pattern CountryCodeIQ :: CountryCode
pattern CountryCodeIQ = CountryCode' "IQ"

pattern CountryCodeIR :: CountryCode
pattern CountryCodeIR = CountryCode' "IR"

pattern CountryCodeIS :: CountryCode
pattern CountryCodeIS = CountryCode' "IS"

pattern CountryCodeIT :: CountryCode
pattern CountryCodeIT = CountryCode' "IT"

pattern CountryCodeJM :: CountryCode
pattern CountryCodeJM = CountryCode' "JM"

pattern CountryCodeJO :: CountryCode
pattern CountryCodeJO = CountryCode' "JO"

pattern CountryCodeJP :: CountryCode
pattern CountryCodeJP = CountryCode' "JP"

pattern CountryCodeKE :: CountryCode
pattern CountryCodeKE = CountryCode' "KE"

pattern CountryCodeKG :: CountryCode
pattern CountryCodeKG = CountryCode' "KG"

pattern CountryCodeKH :: CountryCode
pattern CountryCodeKH = CountryCode' "KH"

pattern CountryCodeKI :: CountryCode
pattern CountryCodeKI = CountryCode' "KI"

pattern CountryCodeKM :: CountryCode
pattern CountryCodeKM = CountryCode' "KM"

pattern CountryCodeKN :: CountryCode
pattern CountryCodeKN = CountryCode' "KN"

pattern CountryCodeKP :: CountryCode
pattern CountryCodeKP = CountryCode' "KP"

pattern CountryCodeKR :: CountryCode
pattern CountryCodeKR = CountryCode' "KR"

pattern CountryCodeKW :: CountryCode
pattern CountryCodeKW = CountryCode' "KW"

pattern CountryCodeKY :: CountryCode
pattern CountryCodeKY = CountryCode' "KY"

pattern CountryCodeKZ :: CountryCode
pattern CountryCodeKZ = CountryCode' "KZ"

pattern CountryCodeLA :: CountryCode
pattern CountryCodeLA = CountryCode' "LA"

pattern CountryCodeLB :: CountryCode
pattern CountryCodeLB = CountryCode' "LB"

pattern CountryCodeLC :: CountryCode
pattern CountryCodeLC = CountryCode' "LC"

pattern CountryCodeLI :: CountryCode
pattern CountryCodeLI = CountryCode' "LI"

pattern CountryCodeLK :: CountryCode
pattern CountryCodeLK = CountryCode' "LK"

pattern CountryCodeLR :: CountryCode
pattern CountryCodeLR = CountryCode' "LR"

pattern CountryCodeLS :: CountryCode
pattern CountryCodeLS = CountryCode' "LS"

pattern CountryCodeLT :: CountryCode
pattern CountryCodeLT = CountryCode' "LT"

pattern CountryCodeLU :: CountryCode
pattern CountryCodeLU = CountryCode' "LU"

pattern CountryCodeLV :: CountryCode
pattern CountryCodeLV = CountryCode' "LV"

pattern CountryCodeLY :: CountryCode
pattern CountryCodeLY = CountryCode' "LY"

pattern CountryCodeMA :: CountryCode
pattern CountryCodeMA = CountryCode' "MA"

pattern CountryCodeMC :: CountryCode
pattern CountryCodeMC = CountryCode' "MC"

pattern CountryCodeMD :: CountryCode
pattern CountryCodeMD = CountryCode' "MD"

pattern CountryCodeME :: CountryCode
pattern CountryCodeME = CountryCode' "ME"

pattern CountryCodeMF :: CountryCode
pattern CountryCodeMF = CountryCode' "MF"

pattern CountryCodeMG :: CountryCode
pattern CountryCodeMG = CountryCode' "MG"

pattern CountryCodeMH :: CountryCode
pattern CountryCodeMH = CountryCode' "MH"

pattern CountryCodeMK :: CountryCode
pattern CountryCodeMK = CountryCode' "MK"

pattern CountryCodeML :: CountryCode
pattern CountryCodeML = CountryCode' "ML"

pattern CountryCodeMM :: CountryCode
pattern CountryCodeMM = CountryCode' "MM"

pattern CountryCodeMN :: CountryCode
pattern CountryCodeMN = CountryCode' "MN"

pattern CountryCodeMO :: CountryCode
pattern CountryCodeMO = CountryCode' "MO"

pattern CountryCodeMP :: CountryCode
pattern CountryCodeMP = CountryCode' "MP"

pattern CountryCodeMR :: CountryCode
pattern CountryCodeMR = CountryCode' "MR"

pattern CountryCodeMS :: CountryCode
pattern CountryCodeMS = CountryCode' "MS"

pattern CountryCodeMT :: CountryCode
pattern CountryCodeMT = CountryCode' "MT"

pattern CountryCodeMU :: CountryCode
pattern CountryCodeMU = CountryCode' "MU"

pattern CountryCodeMV :: CountryCode
pattern CountryCodeMV = CountryCode' "MV"

pattern CountryCodeMW :: CountryCode
pattern CountryCodeMW = CountryCode' "MW"

pattern CountryCodeMX :: CountryCode
pattern CountryCodeMX = CountryCode' "MX"

pattern CountryCodeMY :: CountryCode
pattern CountryCodeMY = CountryCode' "MY"

pattern CountryCodeMZ :: CountryCode
pattern CountryCodeMZ = CountryCode' "MZ"

pattern CountryCodeNA :: CountryCode
pattern CountryCodeNA = CountryCode' "NA"

pattern CountryCodeNC :: CountryCode
pattern CountryCodeNC = CountryCode' "NC"

pattern CountryCodeNE :: CountryCode
pattern CountryCodeNE = CountryCode' "NE"

pattern CountryCodeNG :: CountryCode
pattern CountryCodeNG = CountryCode' "NG"

pattern CountryCodeNI :: CountryCode
pattern CountryCodeNI = CountryCode' "NI"

pattern CountryCodeNL :: CountryCode
pattern CountryCodeNL = CountryCode' "NL"

pattern CountryCodeNO :: CountryCode
pattern CountryCodeNO = CountryCode' "NO"

pattern CountryCodeNP :: CountryCode
pattern CountryCodeNP = CountryCode' "NP"

pattern CountryCodeNR :: CountryCode
pattern CountryCodeNR = CountryCode' "NR"

pattern CountryCodeNU :: CountryCode
pattern CountryCodeNU = CountryCode' "NU"

pattern CountryCodeNZ :: CountryCode
pattern CountryCodeNZ = CountryCode' "NZ"

pattern CountryCodeOM :: CountryCode
pattern CountryCodeOM = CountryCode' "OM"

pattern CountryCodePA :: CountryCode
pattern CountryCodePA = CountryCode' "PA"

pattern CountryCodePE :: CountryCode
pattern CountryCodePE = CountryCode' "PE"

pattern CountryCodePF :: CountryCode
pattern CountryCodePF = CountryCode' "PF"

pattern CountryCodePG :: CountryCode
pattern CountryCodePG = CountryCode' "PG"

pattern CountryCodePH :: CountryCode
pattern CountryCodePH = CountryCode' "PH"

pattern CountryCodePK :: CountryCode
pattern CountryCodePK = CountryCode' "PK"

pattern CountryCodePL :: CountryCode
pattern CountryCodePL = CountryCode' "PL"

pattern CountryCodePM :: CountryCode
pattern CountryCodePM = CountryCode' "PM"

pattern CountryCodePN :: CountryCode
pattern CountryCodePN = CountryCode' "PN"

pattern CountryCodePR :: CountryCode
pattern CountryCodePR = CountryCode' "PR"

pattern CountryCodePT :: CountryCode
pattern CountryCodePT = CountryCode' "PT"

pattern CountryCodePW :: CountryCode
pattern CountryCodePW = CountryCode' "PW"

pattern CountryCodePY :: CountryCode
pattern CountryCodePY = CountryCode' "PY"

pattern CountryCodeQA :: CountryCode
pattern CountryCodeQA = CountryCode' "QA"

pattern CountryCodeRO :: CountryCode
pattern CountryCodeRO = CountryCode' "RO"

pattern CountryCodeRS :: CountryCode
pattern CountryCodeRS = CountryCode' "RS"

pattern CountryCodeRU :: CountryCode
pattern CountryCodeRU = CountryCode' "RU"

pattern CountryCodeRW :: CountryCode
pattern CountryCodeRW = CountryCode' "RW"

pattern CountryCodeSA :: CountryCode
pattern CountryCodeSA = CountryCode' "SA"

pattern CountryCodeSB :: CountryCode
pattern CountryCodeSB = CountryCode' "SB"

pattern CountryCodeSC :: CountryCode
pattern CountryCodeSC = CountryCode' "SC"

pattern CountryCodeSD :: CountryCode
pattern CountryCodeSD = CountryCode' "SD"

pattern CountryCodeSE :: CountryCode
pattern CountryCodeSE = CountryCode' "SE"

pattern CountryCodeSG :: CountryCode
pattern CountryCodeSG = CountryCode' "SG"

pattern CountryCodeSH :: CountryCode
pattern CountryCodeSH = CountryCode' "SH"

pattern CountryCodeSI :: CountryCode
pattern CountryCodeSI = CountryCode' "SI"

pattern CountryCodeSK :: CountryCode
pattern CountryCodeSK = CountryCode' "SK"

pattern CountryCodeSL :: CountryCode
pattern CountryCodeSL = CountryCode' "SL"

pattern CountryCodeSM :: CountryCode
pattern CountryCodeSM = CountryCode' "SM"

pattern CountryCodeSN :: CountryCode
pattern CountryCodeSN = CountryCode' "SN"

pattern CountryCodeSO :: CountryCode
pattern CountryCodeSO = CountryCode' "SO"

pattern CountryCodeSR :: CountryCode
pattern CountryCodeSR = CountryCode' "SR"

pattern CountryCodeST :: CountryCode
pattern CountryCodeST = CountryCode' "ST"

pattern CountryCodeSV :: CountryCode
pattern CountryCodeSV = CountryCode' "SV"

pattern CountryCodeSY :: CountryCode
pattern CountryCodeSY = CountryCode' "SY"

pattern CountryCodeSZ :: CountryCode
pattern CountryCodeSZ = CountryCode' "SZ"

pattern CountryCodeTC :: CountryCode
pattern CountryCodeTC = CountryCode' "TC"

pattern CountryCodeTD :: CountryCode
pattern CountryCodeTD = CountryCode' "TD"

pattern CountryCodeTG :: CountryCode
pattern CountryCodeTG = CountryCode' "TG"

pattern CountryCodeTH :: CountryCode
pattern CountryCodeTH = CountryCode' "TH"

pattern CountryCodeTJ :: CountryCode
pattern CountryCodeTJ = CountryCode' "TJ"

pattern CountryCodeTK :: CountryCode
pattern CountryCodeTK = CountryCode' "TK"

pattern CountryCodeTL :: CountryCode
pattern CountryCodeTL = CountryCode' "TL"

pattern CountryCodeTM :: CountryCode
pattern CountryCodeTM = CountryCode' "TM"

pattern CountryCodeTN :: CountryCode
pattern CountryCodeTN = CountryCode' "TN"

pattern CountryCodeTO :: CountryCode
pattern CountryCodeTO = CountryCode' "TO"

pattern CountryCodeTR :: CountryCode
pattern CountryCodeTR = CountryCode' "TR"

pattern CountryCodeTT :: CountryCode
pattern CountryCodeTT = CountryCode' "TT"

pattern CountryCodeTV :: CountryCode
pattern CountryCodeTV = CountryCode' "TV"

pattern CountryCodeTW :: CountryCode
pattern CountryCodeTW = CountryCode' "TW"

pattern CountryCodeTZ :: CountryCode
pattern CountryCodeTZ = CountryCode' "TZ"

pattern CountryCodeUA :: CountryCode
pattern CountryCodeUA = CountryCode' "UA"

pattern CountryCodeUG :: CountryCode
pattern CountryCodeUG = CountryCode' "UG"

pattern CountryCodeUS :: CountryCode
pattern CountryCodeUS = CountryCode' "US"

pattern CountryCodeUY :: CountryCode
pattern CountryCodeUY = CountryCode' "UY"

pattern CountryCodeUZ :: CountryCode
pattern CountryCodeUZ = CountryCode' "UZ"

pattern CountryCodeVA :: CountryCode
pattern CountryCodeVA = CountryCode' "VA"

pattern CountryCodeVC :: CountryCode
pattern CountryCodeVC = CountryCode' "VC"

pattern CountryCodeVE :: CountryCode
pattern CountryCodeVE = CountryCode' "VE"

pattern CountryCodeVG :: CountryCode
pattern CountryCodeVG = CountryCode' "VG"

pattern CountryCodeVI :: CountryCode
pattern CountryCodeVI = CountryCode' "VI"

pattern CountryCodeVN :: CountryCode
pattern CountryCodeVN = CountryCode' "VN"

pattern CountryCodeVU :: CountryCode
pattern CountryCodeVU = CountryCode' "VU"

pattern CountryCodeWF :: CountryCode
pattern CountryCodeWF = CountryCode' "WF"

pattern CountryCodeWS :: CountryCode
pattern CountryCodeWS = CountryCode' "WS"

pattern CountryCodeYE :: CountryCode
pattern CountryCodeYE = CountryCode' "YE"

pattern CountryCodeYT :: CountryCode
pattern CountryCodeYT = CountryCode' "YT"

pattern CountryCodeZA :: CountryCode
pattern CountryCodeZA = CountryCode' "ZA"

pattern CountryCodeZM :: CountryCode
pattern CountryCodeZM = CountryCode' "ZM"

pattern CountryCodeZW :: CountryCode
pattern CountryCodeZW = CountryCode' "ZW"

{-# COMPLETE
  CountryCodeAD,
  CountryCodeAE,
  CountryCodeAF,
  CountryCodeAG,
  CountryCodeAI,
  CountryCodeAL,
  CountryCodeAM,
  CountryCodeAN,
  CountryCodeAO,
  CountryCodeAQ,
  CountryCodeAR,
  CountryCodeAS,
  CountryCodeAT,
  CountryCodeAU,
  CountryCodeAW,
  CountryCodeAZ,
  CountryCodeBA,
  CountryCodeBB,
  CountryCodeBD,
  CountryCodeBE,
  CountryCodeBF,
  CountryCodeBG,
  CountryCodeBH,
  CountryCodeBI,
  CountryCodeBJ,
  CountryCodeBL,
  CountryCodeBM,
  CountryCodeBN,
  CountryCodeBO,
  CountryCodeBR,
  CountryCodeBS,
  CountryCodeBT,
  CountryCodeBW,
  CountryCodeBY,
  CountryCodeBZ,
  CountryCodeCA,
  CountryCodeCC,
  CountryCodeCD,
  CountryCodeCF,
  CountryCodeCG,
  CountryCodeCH,
  CountryCodeCI,
  CountryCodeCK,
  CountryCodeCL,
  CountryCodeCM,
  CountryCodeCN,
  CountryCodeCO,
  CountryCodeCR,
  CountryCodeCU,
  CountryCodeCV,
  CountryCodeCX,
  CountryCodeCY,
  CountryCodeCZ,
  CountryCodeDE,
  CountryCodeDJ,
  CountryCodeDK,
  CountryCodeDM,
  CountryCodeDO,
  CountryCodeDZ,
  CountryCodeEC,
  CountryCodeEE,
  CountryCodeEG,
  CountryCodeER,
  CountryCodeES,
  CountryCodeET,
  CountryCodeFI,
  CountryCodeFJ,
  CountryCodeFK,
  CountryCodeFM,
  CountryCodeFO,
  CountryCodeFR,
  CountryCodeGA,
  CountryCodeGB,
  CountryCodeGD,
  CountryCodeGE,
  CountryCodeGH,
  CountryCodeGI,
  CountryCodeGL,
  CountryCodeGM,
  CountryCodeGN,
  CountryCodeGQ,
  CountryCodeGR,
  CountryCodeGT,
  CountryCodeGU,
  CountryCodeGW,
  CountryCodeGY,
  CountryCodeHK,
  CountryCodeHN,
  CountryCodeHR,
  CountryCodeHT,
  CountryCodeHU,
  CountryCodeID,
  CountryCodeIE,
  CountryCodeIL,
  CountryCodeIM,
  CountryCodeIN,
  CountryCodeIQ,
  CountryCodeIR,
  CountryCodeIS,
  CountryCodeIT,
  CountryCodeJM,
  CountryCodeJO,
  CountryCodeJP,
  CountryCodeKE,
  CountryCodeKG,
  CountryCodeKH,
  CountryCodeKI,
  CountryCodeKM,
  CountryCodeKN,
  CountryCodeKP,
  CountryCodeKR,
  CountryCodeKW,
  CountryCodeKY,
  CountryCodeKZ,
  CountryCodeLA,
  CountryCodeLB,
  CountryCodeLC,
  CountryCodeLI,
  CountryCodeLK,
  CountryCodeLR,
  CountryCodeLS,
  CountryCodeLT,
  CountryCodeLU,
  CountryCodeLV,
  CountryCodeLY,
  CountryCodeMA,
  CountryCodeMC,
  CountryCodeMD,
  CountryCodeME,
  CountryCodeMF,
  CountryCodeMG,
  CountryCodeMH,
  CountryCodeMK,
  CountryCodeML,
  CountryCodeMM,
  CountryCodeMN,
  CountryCodeMO,
  CountryCodeMP,
  CountryCodeMR,
  CountryCodeMS,
  CountryCodeMT,
  CountryCodeMU,
  CountryCodeMV,
  CountryCodeMW,
  CountryCodeMX,
  CountryCodeMY,
  CountryCodeMZ,
  CountryCodeNA,
  CountryCodeNC,
  CountryCodeNE,
  CountryCodeNG,
  CountryCodeNI,
  CountryCodeNL,
  CountryCodeNO,
  CountryCodeNP,
  CountryCodeNR,
  CountryCodeNU,
  CountryCodeNZ,
  CountryCodeOM,
  CountryCodePA,
  CountryCodePE,
  CountryCodePF,
  CountryCodePG,
  CountryCodePH,
  CountryCodePK,
  CountryCodePL,
  CountryCodePM,
  CountryCodePN,
  CountryCodePR,
  CountryCodePT,
  CountryCodePW,
  CountryCodePY,
  CountryCodeQA,
  CountryCodeRO,
  CountryCodeRS,
  CountryCodeRU,
  CountryCodeRW,
  CountryCodeSA,
  CountryCodeSB,
  CountryCodeSC,
  CountryCodeSD,
  CountryCodeSE,
  CountryCodeSG,
  CountryCodeSH,
  CountryCodeSI,
  CountryCodeSK,
  CountryCodeSL,
  CountryCodeSM,
  CountryCodeSN,
  CountryCodeSO,
  CountryCodeSR,
  CountryCodeST,
  CountryCodeSV,
  CountryCodeSY,
  CountryCodeSZ,
  CountryCodeTC,
  CountryCodeTD,
  CountryCodeTG,
  CountryCodeTH,
  CountryCodeTJ,
  CountryCodeTK,
  CountryCodeTL,
  CountryCodeTM,
  CountryCodeTN,
  CountryCodeTO,
  CountryCodeTR,
  CountryCodeTT,
  CountryCodeTV,
  CountryCodeTW,
  CountryCodeTZ,
  CountryCodeUA,
  CountryCodeUG,
  CountryCodeUS,
  CountryCodeUY,
  CountryCodeUZ,
  CountryCodeVA,
  CountryCodeVC,
  CountryCodeVE,
  CountryCodeVG,
  CountryCodeVI,
  CountryCodeVN,
  CountryCodeVU,
  CountryCodeWF,
  CountryCodeWS,
  CountryCodeYE,
  CountryCodeYT,
  CountryCodeZA,
  CountryCodeZM,
  CountryCodeZW,
  CountryCode'
  #-}
