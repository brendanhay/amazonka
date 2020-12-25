{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.GeoMatchConstraintValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.GeoMatchConstraintValue
  ( GeoMatchConstraintValue
      ( GeoMatchConstraintValue',
        GeoMatchConstraintValueAF,
        GeoMatchConstraintValueAX,
        GeoMatchConstraintValueAL,
        GeoMatchConstraintValueDZ,
        GeoMatchConstraintValueAS,
        GeoMatchConstraintValueAD,
        GeoMatchConstraintValueAO,
        GeoMatchConstraintValueAI,
        GeoMatchConstraintValueAQ,
        GeoMatchConstraintValueAG,
        GeoMatchConstraintValueAR,
        GeoMatchConstraintValueAM,
        GeoMatchConstraintValueAW,
        GeoMatchConstraintValueAU,
        GeoMatchConstraintValueAT,
        GeoMatchConstraintValueAZ,
        GeoMatchConstraintValueBS,
        GeoMatchConstraintValueBH,
        GeoMatchConstraintValueBD,
        GeoMatchConstraintValueBB,
        GeoMatchConstraintValueBY,
        GeoMatchConstraintValueBE,
        GeoMatchConstraintValueBZ,
        GeoMatchConstraintValueBJ,
        GeoMatchConstraintValueBM,
        GeoMatchConstraintValueBT,
        GeoMatchConstraintValueBO,
        GeoMatchConstraintValueBQ,
        GeoMatchConstraintValueBA,
        GeoMatchConstraintValueBW,
        GeoMatchConstraintValueBV,
        GeoMatchConstraintValueBR,
        GeoMatchConstraintValueIO,
        GeoMatchConstraintValueBN,
        GeoMatchConstraintValueBG,
        GeoMatchConstraintValueBF,
        GeoMatchConstraintValueBI,
        GeoMatchConstraintValueKH,
        GeoMatchConstraintValueCM,
        GeoMatchConstraintValueCA,
        GeoMatchConstraintValueCV,
        GeoMatchConstraintValueKY,
        GeoMatchConstraintValueCF,
        GeoMatchConstraintValueTD,
        GeoMatchConstraintValueCL,
        GeoMatchConstraintValueCN,
        GeoMatchConstraintValueCX,
        GeoMatchConstraintValueCC,
        GeoMatchConstraintValueCO,
        GeoMatchConstraintValueKM,
        GeoMatchConstraintValueCG,
        GeoMatchConstraintValueCD,
        GeoMatchConstraintValueCK,
        GeoMatchConstraintValueCR,
        GeoMatchConstraintValueCI,
        GeoMatchConstraintValueHR,
        GeoMatchConstraintValueCU,
        GeoMatchConstraintValueCW,
        GeoMatchConstraintValueCY,
        GeoMatchConstraintValueCZ,
        GeoMatchConstraintValueDK,
        GeoMatchConstraintValueDJ,
        GeoMatchConstraintValueDM,
        GeoMatchConstraintValueDO,
        GeoMatchConstraintValueEC,
        GeoMatchConstraintValueEG,
        GeoMatchConstraintValueSV,
        GeoMatchConstraintValueGQ,
        GeoMatchConstraintValueER,
        GeoMatchConstraintValueEE,
        GeoMatchConstraintValueET,
        GeoMatchConstraintValueFK,
        GeoMatchConstraintValueFO,
        GeoMatchConstraintValueFJ,
        GeoMatchConstraintValueFI,
        GeoMatchConstraintValueFR,
        GeoMatchConstraintValueGF,
        GeoMatchConstraintValuePF,
        GeoMatchConstraintValueTF,
        GeoMatchConstraintValueGA,
        GeoMatchConstraintValueGM,
        GeoMatchConstraintValueGE,
        GeoMatchConstraintValueDE,
        GeoMatchConstraintValueGH,
        GeoMatchConstraintValueGI,
        GeoMatchConstraintValueGR,
        GeoMatchConstraintValueGL,
        GeoMatchConstraintValueGD,
        GeoMatchConstraintValueGP,
        GeoMatchConstraintValueGU,
        GeoMatchConstraintValueGT,
        GeoMatchConstraintValueGG,
        GeoMatchConstraintValueGN,
        GeoMatchConstraintValueGW,
        GeoMatchConstraintValueGY,
        GeoMatchConstraintValueHT,
        GeoMatchConstraintValueHM,
        GeoMatchConstraintValueVA,
        GeoMatchConstraintValueHN,
        GeoMatchConstraintValueHK,
        GeoMatchConstraintValueHU,
        GeoMatchConstraintValueIS,
        GeoMatchConstraintValueIN,
        GeoMatchConstraintValueID,
        GeoMatchConstraintValueIR,
        GeoMatchConstraintValueIQ,
        GeoMatchConstraintValueIE,
        GeoMatchConstraintValueIM,
        GeoMatchConstraintValueIL,
        GeoMatchConstraintValueIT,
        GeoMatchConstraintValueJM,
        GeoMatchConstraintValueJP,
        GeoMatchConstraintValueJE,
        GeoMatchConstraintValueJO,
        GeoMatchConstraintValueKZ,
        GeoMatchConstraintValueKE,
        GeoMatchConstraintValueKI,
        GeoMatchConstraintValueKP,
        GeoMatchConstraintValueKR,
        GeoMatchConstraintValueKW,
        GeoMatchConstraintValueKG,
        GeoMatchConstraintValueLA,
        GeoMatchConstraintValueLV,
        GeoMatchConstraintValueLB,
        GeoMatchConstraintValueLS,
        GeoMatchConstraintValueLR,
        GeoMatchConstraintValueLY,
        GeoMatchConstraintValueLI,
        GeoMatchConstraintValueLT,
        GeoMatchConstraintValueLU,
        GeoMatchConstraintValueMO,
        GeoMatchConstraintValueMK,
        GeoMatchConstraintValueMG,
        GeoMatchConstraintValueMW,
        GeoMatchConstraintValueMY,
        GeoMatchConstraintValueMV,
        GeoMatchConstraintValueML,
        GeoMatchConstraintValueMT,
        GeoMatchConstraintValueMH,
        GeoMatchConstraintValueMQ,
        GeoMatchConstraintValueMR,
        GeoMatchConstraintValueMU,
        GeoMatchConstraintValueYT,
        GeoMatchConstraintValueMX,
        GeoMatchConstraintValueFM,
        GeoMatchConstraintValueMD,
        GeoMatchConstraintValueMC,
        GeoMatchConstraintValueMN,
        GeoMatchConstraintValueME,
        GeoMatchConstraintValueMS,
        GeoMatchConstraintValueMA,
        GeoMatchConstraintValueMZ,
        GeoMatchConstraintValueMM,
        GeoMatchConstraintValueNA,
        GeoMatchConstraintValueNR,
        GeoMatchConstraintValueNP,
        GeoMatchConstraintValueNL,
        GeoMatchConstraintValueNC,
        GeoMatchConstraintValueNZ,
        GeoMatchConstraintValueNI,
        GeoMatchConstraintValueNE,
        GeoMatchConstraintValueNG,
        GeoMatchConstraintValueNU,
        GeoMatchConstraintValueNF,
        GeoMatchConstraintValueMP,
        GeoMatchConstraintValueNO,
        GeoMatchConstraintValueOM,
        GeoMatchConstraintValuePK,
        GeoMatchConstraintValuePW,
        GeoMatchConstraintValuePS,
        GeoMatchConstraintValuePA,
        GeoMatchConstraintValuePG,
        GeoMatchConstraintValuePY,
        GeoMatchConstraintValuePE,
        GeoMatchConstraintValuePH,
        GeoMatchConstraintValuePN,
        GeoMatchConstraintValuePL,
        GeoMatchConstraintValuePT,
        GeoMatchConstraintValuePR,
        GeoMatchConstraintValueQA,
        GeoMatchConstraintValueRE,
        GeoMatchConstraintValueRO,
        GeoMatchConstraintValueRU,
        GeoMatchConstraintValueRW,
        GeoMatchConstraintValueBL,
        GeoMatchConstraintValueSH,
        GeoMatchConstraintValueKN,
        GeoMatchConstraintValueLC,
        GeoMatchConstraintValueMF,
        GeoMatchConstraintValuePM,
        GeoMatchConstraintValueVC,
        GeoMatchConstraintValueWS,
        GeoMatchConstraintValueSM,
        GeoMatchConstraintValueST,
        GeoMatchConstraintValueSA,
        GeoMatchConstraintValueSN,
        GeoMatchConstraintValueRS,
        GeoMatchConstraintValueSC,
        GeoMatchConstraintValueSL,
        GeoMatchConstraintValueSG,
        GeoMatchConstraintValueSX,
        GeoMatchConstraintValueSK,
        GeoMatchConstraintValueSI,
        GeoMatchConstraintValueSB,
        GeoMatchConstraintValueSO,
        GeoMatchConstraintValueZA,
        GeoMatchConstraintValueGS,
        GeoMatchConstraintValueSS,
        GeoMatchConstraintValueES,
        GeoMatchConstraintValueLK,
        GeoMatchConstraintValueSD,
        GeoMatchConstraintValueSR,
        GeoMatchConstraintValueSJ,
        GeoMatchConstraintValueSZ,
        GeoMatchConstraintValueSE,
        GeoMatchConstraintValueCH,
        GeoMatchConstraintValueSY,
        GeoMatchConstraintValueTW,
        GeoMatchConstraintValueTJ,
        GeoMatchConstraintValueTZ,
        GeoMatchConstraintValueTH,
        GeoMatchConstraintValueTL,
        GeoMatchConstraintValueTG,
        GeoMatchConstraintValueTK,
        GeoMatchConstraintValueTO,
        GeoMatchConstraintValueTT,
        GeoMatchConstraintValueTN,
        GeoMatchConstraintValueTR,
        GeoMatchConstraintValueTM,
        GeoMatchConstraintValueTC,
        GeoMatchConstraintValueTV,
        GeoMatchConstraintValueUG,
        GeoMatchConstraintValueUA,
        GeoMatchConstraintValueAE,
        GeoMatchConstraintValueGB,
        GeoMatchConstraintValueUS,
        GeoMatchConstraintValueUM,
        GeoMatchConstraintValueUY,
        GeoMatchConstraintValueUZ,
        GeoMatchConstraintValueVU,
        GeoMatchConstraintValueVE,
        GeoMatchConstraintValueVN,
        GeoMatchConstraintValueVG,
        GeoMatchConstraintValueVI,
        GeoMatchConstraintValueWF,
        GeoMatchConstraintValueEH,
        GeoMatchConstraintValueYE,
        GeoMatchConstraintValueZM,
        GeoMatchConstraintValueZW,
        fromGeoMatchConstraintValue
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype GeoMatchConstraintValue = GeoMatchConstraintValue'
  { fromGeoMatchConstraintValue ::
      Core.Text
  }
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

pattern GeoMatchConstraintValueAF :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueAF = GeoMatchConstraintValue' "AF"

pattern GeoMatchConstraintValueAX :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueAX = GeoMatchConstraintValue' "AX"

pattern GeoMatchConstraintValueAL :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueAL = GeoMatchConstraintValue' "AL"

pattern GeoMatchConstraintValueDZ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueDZ = GeoMatchConstraintValue' "DZ"

pattern GeoMatchConstraintValueAS :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueAS = GeoMatchConstraintValue' "AS"

pattern GeoMatchConstraintValueAD :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueAD = GeoMatchConstraintValue' "AD"

pattern GeoMatchConstraintValueAO :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueAO = GeoMatchConstraintValue' "AO"

pattern GeoMatchConstraintValueAI :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueAI = GeoMatchConstraintValue' "AI"

pattern GeoMatchConstraintValueAQ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueAQ = GeoMatchConstraintValue' "AQ"

pattern GeoMatchConstraintValueAG :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueAG = GeoMatchConstraintValue' "AG"

pattern GeoMatchConstraintValueAR :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueAR = GeoMatchConstraintValue' "AR"

pattern GeoMatchConstraintValueAM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueAM = GeoMatchConstraintValue' "AM"

pattern GeoMatchConstraintValueAW :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueAW = GeoMatchConstraintValue' "AW"

pattern GeoMatchConstraintValueAU :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueAU = GeoMatchConstraintValue' "AU"

pattern GeoMatchConstraintValueAT :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueAT = GeoMatchConstraintValue' "AT"

pattern GeoMatchConstraintValueAZ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueAZ = GeoMatchConstraintValue' "AZ"

pattern GeoMatchConstraintValueBS :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueBS = GeoMatchConstraintValue' "BS"

pattern GeoMatchConstraintValueBH :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueBH = GeoMatchConstraintValue' "BH"

pattern GeoMatchConstraintValueBD :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueBD = GeoMatchConstraintValue' "BD"

pattern GeoMatchConstraintValueBB :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueBB = GeoMatchConstraintValue' "BB"

pattern GeoMatchConstraintValueBY :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueBY = GeoMatchConstraintValue' "BY"

pattern GeoMatchConstraintValueBE :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueBE = GeoMatchConstraintValue' "BE"

pattern GeoMatchConstraintValueBZ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueBZ = GeoMatchConstraintValue' "BZ"

pattern GeoMatchConstraintValueBJ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueBJ = GeoMatchConstraintValue' "BJ"

pattern GeoMatchConstraintValueBM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueBM = GeoMatchConstraintValue' "BM"

pattern GeoMatchConstraintValueBT :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueBT = GeoMatchConstraintValue' "BT"

pattern GeoMatchConstraintValueBO :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueBO = GeoMatchConstraintValue' "BO"

pattern GeoMatchConstraintValueBQ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueBQ = GeoMatchConstraintValue' "BQ"

pattern GeoMatchConstraintValueBA :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueBA = GeoMatchConstraintValue' "BA"

pattern GeoMatchConstraintValueBW :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueBW = GeoMatchConstraintValue' "BW"

pattern GeoMatchConstraintValueBV :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueBV = GeoMatchConstraintValue' "BV"

pattern GeoMatchConstraintValueBR :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueBR = GeoMatchConstraintValue' "BR"

pattern GeoMatchConstraintValueIO :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueIO = GeoMatchConstraintValue' "IO"

pattern GeoMatchConstraintValueBN :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueBN = GeoMatchConstraintValue' "BN"

pattern GeoMatchConstraintValueBG :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueBG = GeoMatchConstraintValue' "BG"

pattern GeoMatchConstraintValueBF :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueBF = GeoMatchConstraintValue' "BF"

pattern GeoMatchConstraintValueBI :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueBI = GeoMatchConstraintValue' "BI"

pattern GeoMatchConstraintValueKH :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueKH = GeoMatchConstraintValue' "KH"

pattern GeoMatchConstraintValueCM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueCM = GeoMatchConstraintValue' "CM"

pattern GeoMatchConstraintValueCA :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueCA = GeoMatchConstraintValue' "CA"

pattern GeoMatchConstraintValueCV :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueCV = GeoMatchConstraintValue' "CV"

pattern GeoMatchConstraintValueKY :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueKY = GeoMatchConstraintValue' "KY"

pattern GeoMatchConstraintValueCF :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueCF = GeoMatchConstraintValue' "CF"

pattern GeoMatchConstraintValueTD :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueTD = GeoMatchConstraintValue' "TD"

pattern GeoMatchConstraintValueCL :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueCL = GeoMatchConstraintValue' "CL"

pattern GeoMatchConstraintValueCN :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueCN = GeoMatchConstraintValue' "CN"

pattern GeoMatchConstraintValueCX :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueCX = GeoMatchConstraintValue' "CX"

pattern GeoMatchConstraintValueCC :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueCC = GeoMatchConstraintValue' "CC"

pattern GeoMatchConstraintValueCO :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueCO = GeoMatchConstraintValue' "CO"

pattern GeoMatchConstraintValueKM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueKM = GeoMatchConstraintValue' "KM"

pattern GeoMatchConstraintValueCG :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueCG = GeoMatchConstraintValue' "CG"

pattern GeoMatchConstraintValueCD :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueCD = GeoMatchConstraintValue' "CD"

pattern GeoMatchConstraintValueCK :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueCK = GeoMatchConstraintValue' "CK"

pattern GeoMatchConstraintValueCR :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueCR = GeoMatchConstraintValue' "CR"

pattern GeoMatchConstraintValueCI :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueCI = GeoMatchConstraintValue' "CI"

pattern GeoMatchConstraintValueHR :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueHR = GeoMatchConstraintValue' "HR"

pattern GeoMatchConstraintValueCU :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueCU = GeoMatchConstraintValue' "CU"

pattern GeoMatchConstraintValueCW :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueCW = GeoMatchConstraintValue' "CW"

pattern GeoMatchConstraintValueCY :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueCY = GeoMatchConstraintValue' "CY"

pattern GeoMatchConstraintValueCZ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueCZ = GeoMatchConstraintValue' "CZ"

pattern GeoMatchConstraintValueDK :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueDK = GeoMatchConstraintValue' "DK"

pattern GeoMatchConstraintValueDJ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueDJ = GeoMatchConstraintValue' "DJ"

pattern GeoMatchConstraintValueDM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueDM = GeoMatchConstraintValue' "DM"

pattern GeoMatchConstraintValueDO :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueDO = GeoMatchConstraintValue' "DO"

pattern GeoMatchConstraintValueEC :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueEC = GeoMatchConstraintValue' "EC"

pattern GeoMatchConstraintValueEG :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueEG = GeoMatchConstraintValue' "EG"

pattern GeoMatchConstraintValueSV :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueSV = GeoMatchConstraintValue' "SV"

pattern GeoMatchConstraintValueGQ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueGQ = GeoMatchConstraintValue' "GQ"

pattern GeoMatchConstraintValueER :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueER = GeoMatchConstraintValue' "ER"

pattern GeoMatchConstraintValueEE :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueEE = GeoMatchConstraintValue' "EE"

pattern GeoMatchConstraintValueET :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueET = GeoMatchConstraintValue' "ET"

pattern GeoMatchConstraintValueFK :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueFK = GeoMatchConstraintValue' "FK"

pattern GeoMatchConstraintValueFO :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueFO = GeoMatchConstraintValue' "FO"

pattern GeoMatchConstraintValueFJ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueFJ = GeoMatchConstraintValue' "FJ"

pattern GeoMatchConstraintValueFI :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueFI = GeoMatchConstraintValue' "FI"

pattern GeoMatchConstraintValueFR :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueFR = GeoMatchConstraintValue' "FR"

pattern GeoMatchConstraintValueGF :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueGF = GeoMatchConstraintValue' "GF"

pattern GeoMatchConstraintValuePF :: GeoMatchConstraintValue
pattern GeoMatchConstraintValuePF = GeoMatchConstraintValue' "PF"

pattern GeoMatchConstraintValueTF :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueTF = GeoMatchConstraintValue' "TF"

pattern GeoMatchConstraintValueGA :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueGA = GeoMatchConstraintValue' "GA"

pattern GeoMatchConstraintValueGM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueGM = GeoMatchConstraintValue' "GM"

pattern GeoMatchConstraintValueGE :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueGE = GeoMatchConstraintValue' "GE"

pattern GeoMatchConstraintValueDE :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueDE = GeoMatchConstraintValue' "DE"

pattern GeoMatchConstraintValueGH :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueGH = GeoMatchConstraintValue' "GH"

pattern GeoMatchConstraintValueGI :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueGI = GeoMatchConstraintValue' "GI"

pattern GeoMatchConstraintValueGR :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueGR = GeoMatchConstraintValue' "GR"

pattern GeoMatchConstraintValueGL :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueGL = GeoMatchConstraintValue' "GL"

pattern GeoMatchConstraintValueGD :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueGD = GeoMatchConstraintValue' "GD"

pattern GeoMatchConstraintValueGP :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueGP = GeoMatchConstraintValue' "GP"

pattern GeoMatchConstraintValueGU :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueGU = GeoMatchConstraintValue' "GU"

pattern GeoMatchConstraintValueGT :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueGT = GeoMatchConstraintValue' "GT"

pattern GeoMatchConstraintValueGG :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueGG = GeoMatchConstraintValue' "GG"

pattern GeoMatchConstraintValueGN :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueGN = GeoMatchConstraintValue' "GN"

pattern GeoMatchConstraintValueGW :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueGW = GeoMatchConstraintValue' "GW"

pattern GeoMatchConstraintValueGY :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueGY = GeoMatchConstraintValue' "GY"

pattern GeoMatchConstraintValueHT :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueHT = GeoMatchConstraintValue' "HT"

pattern GeoMatchConstraintValueHM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueHM = GeoMatchConstraintValue' "HM"

pattern GeoMatchConstraintValueVA :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueVA = GeoMatchConstraintValue' "VA"

pattern GeoMatchConstraintValueHN :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueHN = GeoMatchConstraintValue' "HN"

pattern GeoMatchConstraintValueHK :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueHK = GeoMatchConstraintValue' "HK"

pattern GeoMatchConstraintValueHU :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueHU = GeoMatchConstraintValue' "HU"

pattern GeoMatchConstraintValueIS :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueIS = GeoMatchConstraintValue' "IS"

pattern GeoMatchConstraintValueIN :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueIN = GeoMatchConstraintValue' "IN"

pattern GeoMatchConstraintValueID :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueID = GeoMatchConstraintValue' "ID"

pattern GeoMatchConstraintValueIR :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueIR = GeoMatchConstraintValue' "IR"

pattern GeoMatchConstraintValueIQ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueIQ = GeoMatchConstraintValue' "IQ"

pattern GeoMatchConstraintValueIE :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueIE = GeoMatchConstraintValue' "IE"

pattern GeoMatchConstraintValueIM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueIM = GeoMatchConstraintValue' "IM"

pattern GeoMatchConstraintValueIL :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueIL = GeoMatchConstraintValue' "IL"

pattern GeoMatchConstraintValueIT :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueIT = GeoMatchConstraintValue' "IT"

pattern GeoMatchConstraintValueJM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueJM = GeoMatchConstraintValue' "JM"

pattern GeoMatchConstraintValueJP :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueJP = GeoMatchConstraintValue' "JP"

pattern GeoMatchConstraintValueJE :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueJE = GeoMatchConstraintValue' "JE"

pattern GeoMatchConstraintValueJO :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueJO = GeoMatchConstraintValue' "JO"

pattern GeoMatchConstraintValueKZ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueKZ = GeoMatchConstraintValue' "KZ"

pattern GeoMatchConstraintValueKE :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueKE = GeoMatchConstraintValue' "KE"

pattern GeoMatchConstraintValueKI :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueKI = GeoMatchConstraintValue' "KI"

pattern GeoMatchConstraintValueKP :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueKP = GeoMatchConstraintValue' "KP"

pattern GeoMatchConstraintValueKR :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueKR = GeoMatchConstraintValue' "KR"

pattern GeoMatchConstraintValueKW :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueKW = GeoMatchConstraintValue' "KW"

pattern GeoMatchConstraintValueKG :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueKG = GeoMatchConstraintValue' "KG"

pattern GeoMatchConstraintValueLA :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueLA = GeoMatchConstraintValue' "LA"

pattern GeoMatchConstraintValueLV :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueLV = GeoMatchConstraintValue' "LV"

pattern GeoMatchConstraintValueLB :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueLB = GeoMatchConstraintValue' "LB"

pattern GeoMatchConstraintValueLS :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueLS = GeoMatchConstraintValue' "LS"

pattern GeoMatchConstraintValueLR :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueLR = GeoMatchConstraintValue' "LR"

pattern GeoMatchConstraintValueLY :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueLY = GeoMatchConstraintValue' "LY"

pattern GeoMatchConstraintValueLI :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueLI = GeoMatchConstraintValue' "LI"

pattern GeoMatchConstraintValueLT :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueLT = GeoMatchConstraintValue' "LT"

pattern GeoMatchConstraintValueLU :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueLU = GeoMatchConstraintValue' "LU"

pattern GeoMatchConstraintValueMO :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueMO = GeoMatchConstraintValue' "MO"

pattern GeoMatchConstraintValueMK :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueMK = GeoMatchConstraintValue' "MK"

pattern GeoMatchConstraintValueMG :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueMG = GeoMatchConstraintValue' "MG"

pattern GeoMatchConstraintValueMW :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueMW = GeoMatchConstraintValue' "MW"

pattern GeoMatchConstraintValueMY :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueMY = GeoMatchConstraintValue' "MY"

pattern GeoMatchConstraintValueMV :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueMV = GeoMatchConstraintValue' "MV"

pattern GeoMatchConstraintValueML :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueML = GeoMatchConstraintValue' "ML"

pattern GeoMatchConstraintValueMT :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueMT = GeoMatchConstraintValue' "MT"

pattern GeoMatchConstraintValueMH :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueMH = GeoMatchConstraintValue' "MH"

pattern GeoMatchConstraintValueMQ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueMQ = GeoMatchConstraintValue' "MQ"

pattern GeoMatchConstraintValueMR :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueMR = GeoMatchConstraintValue' "MR"

pattern GeoMatchConstraintValueMU :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueMU = GeoMatchConstraintValue' "MU"

pattern GeoMatchConstraintValueYT :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueYT = GeoMatchConstraintValue' "YT"

pattern GeoMatchConstraintValueMX :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueMX = GeoMatchConstraintValue' "MX"

pattern GeoMatchConstraintValueFM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueFM = GeoMatchConstraintValue' "FM"

pattern GeoMatchConstraintValueMD :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueMD = GeoMatchConstraintValue' "MD"

pattern GeoMatchConstraintValueMC :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueMC = GeoMatchConstraintValue' "MC"

pattern GeoMatchConstraintValueMN :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueMN = GeoMatchConstraintValue' "MN"

pattern GeoMatchConstraintValueME :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueME = GeoMatchConstraintValue' "ME"

pattern GeoMatchConstraintValueMS :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueMS = GeoMatchConstraintValue' "MS"

pattern GeoMatchConstraintValueMA :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueMA = GeoMatchConstraintValue' "MA"

pattern GeoMatchConstraintValueMZ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueMZ = GeoMatchConstraintValue' "MZ"

pattern GeoMatchConstraintValueMM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueMM = GeoMatchConstraintValue' "MM"

pattern GeoMatchConstraintValueNA :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueNA = GeoMatchConstraintValue' "NA"

pattern GeoMatchConstraintValueNR :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueNR = GeoMatchConstraintValue' "NR"

pattern GeoMatchConstraintValueNP :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueNP = GeoMatchConstraintValue' "NP"

pattern GeoMatchConstraintValueNL :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueNL = GeoMatchConstraintValue' "NL"

pattern GeoMatchConstraintValueNC :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueNC = GeoMatchConstraintValue' "NC"

pattern GeoMatchConstraintValueNZ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueNZ = GeoMatchConstraintValue' "NZ"

pattern GeoMatchConstraintValueNI :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueNI = GeoMatchConstraintValue' "NI"

pattern GeoMatchConstraintValueNE :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueNE = GeoMatchConstraintValue' "NE"

pattern GeoMatchConstraintValueNG :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueNG = GeoMatchConstraintValue' "NG"

pattern GeoMatchConstraintValueNU :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueNU = GeoMatchConstraintValue' "NU"

pattern GeoMatchConstraintValueNF :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueNF = GeoMatchConstraintValue' "NF"

pattern GeoMatchConstraintValueMP :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueMP = GeoMatchConstraintValue' "MP"

pattern GeoMatchConstraintValueNO :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueNO = GeoMatchConstraintValue' "NO"

pattern GeoMatchConstraintValueOM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueOM = GeoMatchConstraintValue' "OM"

pattern GeoMatchConstraintValuePK :: GeoMatchConstraintValue
pattern GeoMatchConstraintValuePK = GeoMatchConstraintValue' "PK"

pattern GeoMatchConstraintValuePW :: GeoMatchConstraintValue
pattern GeoMatchConstraintValuePW = GeoMatchConstraintValue' "PW"

pattern GeoMatchConstraintValuePS :: GeoMatchConstraintValue
pattern GeoMatchConstraintValuePS = GeoMatchConstraintValue' "PS"

pattern GeoMatchConstraintValuePA :: GeoMatchConstraintValue
pattern GeoMatchConstraintValuePA = GeoMatchConstraintValue' "PA"

pattern GeoMatchConstraintValuePG :: GeoMatchConstraintValue
pattern GeoMatchConstraintValuePG = GeoMatchConstraintValue' "PG"

pattern GeoMatchConstraintValuePY :: GeoMatchConstraintValue
pattern GeoMatchConstraintValuePY = GeoMatchConstraintValue' "PY"

pattern GeoMatchConstraintValuePE :: GeoMatchConstraintValue
pattern GeoMatchConstraintValuePE = GeoMatchConstraintValue' "PE"

pattern GeoMatchConstraintValuePH :: GeoMatchConstraintValue
pattern GeoMatchConstraintValuePH = GeoMatchConstraintValue' "PH"

pattern GeoMatchConstraintValuePN :: GeoMatchConstraintValue
pattern GeoMatchConstraintValuePN = GeoMatchConstraintValue' "PN"

pattern GeoMatchConstraintValuePL :: GeoMatchConstraintValue
pattern GeoMatchConstraintValuePL = GeoMatchConstraintValue' "PL"

pattern GeoMatchConstraintValuePT :: GeoMatchConstraintValue
pattern GeoMatchConstraintValuePT = GeoMatchConstraintValue' "PT"

pattern GeoMatchConstraintValuePR :: GeoMatchConstraintValue
pattern GeoMatchConstraintValuePR = GeoMatchConstraintValue' "PR"

pattern GeoMatchConstraintValueQA :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueQA = GeoMatchConstraintValue' "QA"

pattern GeoMatchConstraintValueRE :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueRE = GeoMatchConstraintValue' "RE"

pattern GeoMatchConstraintValueRO :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueRO = GeoMatchConstraintValue' "RO"

pattern GeoMatchConstraintValueRU :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueRU = GeoMatchConstraintValue' "RU"

pattern GeoMatchConstraintValueRW :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueRW = GeoMatchConstraintValue' "RW"

pattern GeoMatchConstraintValueBL :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueBL = GeoMatchConstraintValue' "BL"

pattern GeoMatchConstraintValueSH :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueSH = GeoMatchConstraintValue' "SH"

pattern GeoMatchConstraintValueKN :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueKN = GeoMatchConstraintValue' "KN"

pattern GeoMatchConstraintValueLC :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueLC = GeoMatchConstraintValue' "LC"

pattern GeoMatchConstraintValueMF :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueMF = GeoMatchConstraintValue' "MF"

pattern GeoMatchConstraintValuePM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValuePM = GeoMatchConstraintValue' "PM"

pattern GeoMatchConstraintValueVC :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueVC = GeoMatchConstraintValue' "VC"

pattern GeoMatchConstraintValueWS :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueWS = GeoMatchConstraintValue' "WS"

pattern GeoMatchConstraintValueSM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueSM = GeoMatchConstraintValue' "SM"

pattern GeoMatchConstraintValueST :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueST = GeoMatchConstraintValue' "ST"

pattern GeoMatchConstraintValueSA :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueSA = GeoMatchConstraintValue' "SA"

pattern GeoMatchConstraintValueSN :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueSN = GeoMatchConstraintValue' "SN"

pattern GeoMatchConstraintValueRS :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueRS = GeoMatchConstraintValue' "RS"

pattern GeoMatchConstraintValueSC :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueSC = GeoMatchConstraintValue' "SC"

pattern GeoMatchConstraintValueSL :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueSL = GeoMatchConstraintValue' "SL"

pattern GeoMatchConstraintValueSG :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueSG = GeoMatchConstraintValue' "SG"

pattern GeoMatchConstraintValueSX :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueSX = GeoMatchConstraintValue' "SX"

pattern GeoMatchConstraintValueSK :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueSK = GeoMatchConstraintValue' "SK"

pattern GeoMatchConstraintValueSI :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueSI = GeoMatchConstraintValue' "SI"

pattern GeoMatchConstraintValueSB :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueSB = GeoMatchConstraintValue' "SB"

pattern GeoMatchConstraintValueSO :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueSO = GeoMatchConstraintValue' "SO"

pattern GeoMatchConstraintValueZA :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueZA = GeoMatchConstraintValue' "ZA"

pattern GeoMatchConstraintValueGS :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueGS = GeoMatchConstraintValue' "GS"

pattern GeoMatchConstraintValueSS :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueSS = GeoMatchConstraintValue' "SS"

pattern GeoMatchConstraintValueES :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueES = GeoMatchConstraintValue' "ES"

pattern GeoMatchConstraintValueLK :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueLK = GeoMatchConstraintValue' "LK"

pattern GeoMatchConstraintValueSD :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueSD = GeoMatchConstraintValue' "SD"

pattern GeoMatchConstraintValueSR :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueSR = GeoMatchConstraintValue' "SR"

pattern GeoMatchConstraintValueSJ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueSJ = GeoMatchConstraintValue' "SJ"

pattern GeoMatchConstraintValueSZ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueSZ = GeoMatchConstraintValue' "SZ"

pattern GeoMatchConstraintValueSE :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueSE = GeoMatchConstraintValue' "SE"

pattern GeoMatchConstraintValueCH :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueCH = GeoMatchConstraintValue' "CH"

pattern GeoMatchConstraintValueSY :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueSY = GeoMatchConstraintValue' "SY"

pattern GeoMatchConstraintValueTW :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueTW = GeoMatchConstraintValue' "TW"

pattern GeoMatchConstraintValueTJ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueTJ = GeoMatchConstraintValue' "TJ"

pattern GeoMatchConstraintValueTZ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueTZ = GeoMatchConstraintValue' "TZ"

pattern GeoMatchConstraintValueTH :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueTH = GeoMatchConstraintValue' "TH"

pattern GeoMatchConstraintValueTL :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueTL = GeoMatchConstraintValue' "TL"

pattern GeoMatchConstraintValueTG :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueTG = GeoMatchConstraintValue' "TG"

pattern GeoMatchConstraintValueTK :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueTK = GeoMatchConstraintValue' "TK"

pattern GeoMatchConstraintValueTO :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueTO = GeoMatchConstraintValue' "TO"

pattern GeoMatchConstraintValueTT :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueTT = GeoMatchConstraintValue' "TT"

pattern GeoMatchConstraintValueTN :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueTN = GeoMatchConstraintValue' "TN"

pattern GeoMatchConstraintValueTR :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueTR = GeoMatchConstraintValue' "TR"

pattern GeoMatchConstraintValueTM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueTM = GeoMatchConstraintValue' "TM"

pattern GeoMatchConstraintValueTC :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueTC = GeoMatchConstraintValue' "TC"

pattern GeoMatchConstraintValueTV :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueTV = GeoMatchConstraintValue' "TV"

pattern GeoMatchConstraintValueUG :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueUG = GeoMatchConstraintValue' "UG"

pattern GeoMatchConstraintValueUA :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueUA = GeoMatchConstraintValue' "UA"

pattern GeoMatchConstraintValueAE :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueAE = GeoMatchConstraintValue' "AE"

pattern GeoMatchConstraintValueGB :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueGB = GeoMatchConstraintValue' "GB"

pattern GeoMatchConstraintValueUS :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueUS = GeoMatchConstraintValue' "US"

pattern GeoMatchConstraintValueUM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueUM = GeoMatchConstraintValue' "UM"

pattern GeoMatchConstraintValueUY :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueUY = GeoMatchConstraintValue' "UY"

pattern GeoMatchConstraintValueUZ :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueUZ = GeoMatchConstraintValue' "UZ"

pattern GeoMatchConstraintValueVU :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueVU = GeoMatchConstraintValue' "VU"

pattern GeoMatchConstraintValueVE :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueVE = GeoMatchConstraintValue' "VE"

pattern GeoMatchConstraintValueVN :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueVN = GeoMatchConstraintValue' "VN"

pattern GeoMatchConstraintValueVG :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueVG = GeoMatchConstraintValue' "VG"

pattern GeoMatchConstraintValueVI :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueVI = GeoMatchConstraintValue' "VI"

pattern GeoMatchConstraintValueWF :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueWF = GeoMatchConstraintValue' "WF"

pattern GeoMatchConstraintValueEH :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueEH = GeoMatchConstraintValue' "EH"

pattern GeoMatchConstraintValueYE :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueYE = GeoMatchConstraintValue' "YE"

pattern GeoMatchConstraintValueZM :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueZM = GeoMatchConstraintValue' "ZM"

pattern GeoMatchConstraintValueZW :: GeoMatchConstraintValue
pattern GeoMatchConstraintValueZW = GeoMatchConstraintValue' "ZW"

{-# COMPLETE
  GeoMatchConstraintValueAF,
  GeoMatchConstraintValueAX,
  GeoMatchConstraintValueAL,
  GeoMatchConstraintValueDZ,
  GeoMatchConstraintValueAS,
  GeoMatchConstraintValueAD,
  GeoMatchConstraintValueAO,
  GeoMatchConstraintValueAI,
  GeoMatchConstraintValueAQ,
  GeoMatchConstraintValueAG,
  GeoMatchConstraintValueAR,
  GeoMatchConstraintValueAM,
  GeoMatchConstraintValueAW,
  GeoMatchConstraintValueAU,
  GeoMatchConstraintValueAT,
  GeoMatchConstraintValueAZ,
  GeoMatchConstraintValueBS,
  GeoMatchConstraintValueBH,
  GeoMatchConstraintValueBD,
  GeoMatchConstraintValueBB,
  GeoMatchConstraintValueBY,
  GeoMatchConstraintValueBE,
  GeoMatchConstraintValueBZ,
  GeoMatchConstraintValueBJ,
  GeoMatchConstraintValueBM,
  GeoMatchConstraintValueBT,
  GeoMatchConstraintValueBO,
  GeoMatchConstraintValueBQ,
  GeoMatchConstraintValueBA,
  GeoMatchConstraintValueBW,
  GeoMatchConstraintValueBV,
  GeoMatchConstraintValueBR,
  GeoMatchConstraintValueIO,
  GeoMatchConstraintValueBN,
  GeoMatchConstraintValueBG,
  GeoMatchConstraintValueBF,
  GeoMatchConstraintValueBI,
  GeoMatchConstraintValueKH,
  GeoMatchConstraintValueCM,
  GeoMatchConstraintValueCA,
  GeoMatchConstraintValueCV,
  GeoMatchConstraintValueKY,
  GeoMatchConstraintValueCF,
  GeoMatchConstraintValueTD,
  GeoMatchConstraintValueCL,
  GeoMatchConstraintValueCN,
  GeoMatchConstraintValueCX,
  GeoMatchConstraintValueCC,
  GeoMatchConstraintValueCO,
  GeoMatchConstraintValueKM,
  GeoMatchConstraintValueCG,
  GeoMatchConstraintValueCD,
  GeoMatchConstraintValueCK,
  GeoMatchConstraintValueCR,
  GeoMatchConstraintValueCI,
  GeoMatchConstraintValueHR,
  GeoMatchConstraintValueCU,
  GeoMatchConstraintValueCW,
  GeoMatchConstraintValueCY,
  GeoMatchConstraintValueCZ,
  GeoMatchConstraintValueDK,
  GeoMatchConstraintValueDJ,
  GeoMatchConstraintValueDM,
  GeoMatchConstraintValueDO,
  GeoMatchConstraintValueEC,
  GeoMatchConstraintValueEG,
  GeoMatchConstraintValueSV,
  GeoMatchConstraintValueGQ,
  GeoMatchConstraintValueER,
  GeoMatchConstraintValueEE,
  GeoMatchConstraintValueET,
  GeoMatchConstraintValueFK,
  GeoMatchConstraintValueFO,
  GeoMatchConstraintValueFJ,
  GeoMatchConstraintValueFI,
  GeoMatchConstraintValueFR,
  GeoMatchConstraintValueGF,
  GeoMatchConstraintValuePF,
  GeoMatchConstraintValueTF,
  GeoMatchConstraintValueGA,
  GeoMatchConstraintValueGM,
  GeoMatchConstraintValueGE,
  GeoMatchConstraintValueDE,
  GeoMatchConstraintValueGH,
  GeoMatchConstraintValueGI,
  GeoMatchConstraintValueGR,
  GeoMatchConstraintValueGL,
  GeoMatchConstraintValueGD,
  GeoMatchConstraintValueGP,
  GeoMatchConstraintValueGU,
  GeoMatchConstraintValueGT,
  GeoMatchConstraintValueGG,
  GeoMatchConstraintValueGN,
  GeoMatchConstraintValueGW,
  GeoMatchConstraintValueGY,
  GeoMatchConstraintValueHT,
  GeoMatchConstraintValueHM,
  GeoMatchConstraintValueVA,
  GeoMatchConstraintValueHN,
  GeoMatchConstraintValueHK,
  GeoMatchConstraintValueHU,
  GeoMatchConstraintValueIS,
  GeoMatchConstraintValueIN,
  GeoMatchConstraintValueID,
  GeoMatchConstraintValueIR,
  GeoMatchConstraintValueIQ,
  GeoMatchConstraintValueIE,
  GeoMatchConstraintValueIM,
  GeoMatchConstraintValueIL,
  GeoMatchConstraintValueIT,
  GeoMatchConstraintValueJM,
  GeoMatchConstraintValueJP,
  GeoMatchConstraintValueJE,
  GeoMatchConstraintValueJO,
  GeoMatchConstraintValueKZ,
  GeoMatchConstraintValueKE,
  GeoMatchConstraintValueKI,
  GeoMatchConstraintValueKP,
  GeoMatchConstraintValueKR,
  GeoMatchConstraintValueKW,
  GeoMatchConstraintValueKG,
  GeoMatchConstraintValueLA,
  GeoMatchConstraintValueLV,
  GeoMatchConstraintValueLB,
  GeoMatchConstraintValueLS,
  GeoMatchConstraintValueLR,
  GeoMatchConstraintValueLY,
  GeoMatchConstraintValueLI,
  GeoMatchConstraintValueLT,
  GeoMatchConstraintValueLU,
  GeoMatchConstraintValueMO,
  GeoMatchConstraintValueMK,
  GeoMatchConstraintValueMG,
  GeoMatchConstraintValueMW,
  GeoMatchConstraintValueMY,
  GeoMatchConstraintValueMV,
  GeoMatchConstraintValueML,
  GeoMatchConstraintValueMT,
  GeoMatchConstraintValueMH,
  GeoMatchConstraintValueMQ,
  GeoMatchConstraintValueMR,
  GeoMatchConstraintValueMU,
  GeoMatchConstraintValueYT,
  GeoMatchConstraintValueMX,
  GeoMatchConstraintValueFM,
  GeoMatchConstraintValueMD,
  GeoMatchConstraintValueMC,
  GeoMatchConstraintValueMN,
  GeoMatchConstraintValueME,
  GeoMatchConstraintValueMS,
  GeoMatchConstraintValueMA,
  GeoMatchConstraintValueMZ,
  GeoMatchConstraintValueMM,
  GeoMatchConstraintValueNA,
  GeoMatchConstraintValueNR,
  GeoMatchConstraintValueNP,
  GeoMatchConstraintValueNL,
  GeoMatchConstraintValueNC,
  GeoMatchConstraintValueNZ,
  GeoMatchConstraintValueNI,
  GeoMatchConstraintValueNE,
  GeoMatchConstraintValueNG,
  GeoMatchConstraintValueNU,
  GeoMatchConstraintValueNF,
  GeoMatchConstraintValueMP,
  GeoMatchConstraintValueNO,
  GeoMatchConstraintValueOM,
  GeoMatchConstraintValuePK,
  GeoMatchConstraintValuePW,
  GeoMatchConstraintValuePS,
  GeoMatchConstraintValuePA,
  GeoMatchConstraintValuePG,
  GeoMatchConstraintValuePY,
  GeoMatchConstraintValuePE,
  GeoMatchConstraintValuePH,
  GeoMatchConstraintValuePN,
  GeoMatchConstraintValuePL,
  GeoMatchConstraintValuePT,
  GeoMatchConstraintValuePR,
  GeoMatchConstraintValueQA,
  GeoMatchConstraintValueRE,
  GeoMatchConstraintValueRO,
  GeoMatchConstraintValueRU,
  GeoMatchConstraintValueRW,
  GeoMatchConstraintValueBL,
  GeoMatchConstraintValueSH,
  GeoMatchConstraintValueKN,
  GeoMatchConstraintValueLC,
  GeoMatchConstraintValueMF,
  GeoMatchConstraintValuePM,
  GeoMatchConstraintValueVC,
  GeoMatchConstraintValueWS,
  GeoMatchConstraintValueSM,
  GeoMatchConstraintValueST,
  GeoMatchConstraintValueSA,
  GeoMatchConstraintValueSN,
  GeoMatchConstraintValueRS,
  GeoMatchConstraintValueSC,
  GeoMatchConstraintValueSL,
  GeoMatchConstraintValueSG,
  GeoMatchConstraintValueSX,
  GeoMatchConstraintValueSK,
  GeoMatchConstraintValueSI,
  GeoMatchConstraintValueSB,
  GeoMatchConstraintValueSO,
  GeoMatchConstraintValueZA,
  GeoMatchConstraintValueGS,
  GeoMatchConstraintValueSS,
  GeoMatchConstraintValueES,
  GeoMatchConstraintValueLK,
  GeoMatchConstraintValueSD,
  GeoMatchConstraintValueSR,
  GeoMatchConstraintValueSJ,
  GeoMatchConstraintValueSZ,
  GeoMatchConstraintValueSE,
  GeoMatchConstraintValueCH,
  GeoMatchConstraintValueSY,
  GeoMatchConstraintValueTW,
  GeoMatchConstraintValueTJ,
  GeoMatchConstraintValueTZ,
  GeoMatchConstraintValueTH,
  GeoMatchConstraintValueTL,
  GeoMatchConstraintValueTG,
  GeoMatchConstraintValueTK,
  GeoMatchConstraintValueTO,
  GeoMatchConstraintValueTT,
  GeoMatchConstraintValueTN,
  GeoMatchConstraintValueTR,
  GeoMatchConstraintValueTM,
  GeoMatchConstraintValueTC,
  GeoMatchConstraintValueTV,
  GeoMatchConstraintValueUG,
  GeoMatchConstraintValueUA,
  GeoMatchConstraintValueAE,
  GeoMatchConstraintValueGB,
  GeoMatchConstraintValueUS,
  GeoMatchConstraintValueUM,
  GeoMatchConstraintValueUY,
  GeoMatchConstraintValueUZ,
  GeoMatchConstraintValueVU,
  GeoMatchConstraintValueVE,
  GeoMatchConstraintValueVN,
  GeoMatchConstraintValueVG,
  GeoMatchConstraintValueVI,
  GeoMatchConstraintValueWF,
  GeoMatchConstraintValueEH,
  GeoMatchConstraintValueYE,
  GeoMatchConstraintValueZM,
  GeoMatchConstraintValueZW,
  GeoMatchConstraintValue'
  #-}
