{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.PhoneNumberCountryCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.PhoneNumberCountryCode
  ( PhoneNumberCountryCode
    ( PhoneNumberCountryCode'
    , PhoneNumberCountryCodeAF
    , PhoneNumberCountryCodeAL
    , PhoneNumberCountryCodeDZ
    , PhoneNumberCountryCodeAS
    , PhoneNumberCountryCodeAD
    , PhoneNumberCountryCodeAO
    , PhoneNumberCountryCodeAI
    , PhoneNumberCountryCodeAQ
    , PhoneNumberCountryCodeAG
    , PhoneNumberCountryCodeAR
    , PhoneNumberCountryCodeAM
    , PhoneNumberCountryCodeAW
    , PhoneNumberCountryCodeAU
    , PhoneNumberCountryCodeAT
    , PhoneNumberCountryCodeAZ
    , PhoneNumberCountryCodeBS
    , PhoneNumberCountryCodeBH
    , PhoneNumberCountryCodeBD
    , PhoneNumberCountryCodeBB
    , PhoneNumberCountryCodeBY
    , PhoneNumberCountryCodeBE
    , PhoneNumberCountryCodeBZ
    , PhoneNumberCountryCodeBJ
    , PhoneNumberCountryCodeBM
    , PhoneNumberCountryCodeBT
    , PhoneNumberCountryCodeBO
    , PhoneNumberCountryCodeBA
    , PhoneNumberCountryCodeBW
    , PhoneNumberCountryCodeBR
    , PhoneNumberCountryCodeIO
    , PhoneNumberCountryCodeVG
    , PhoneNumberCountryCodeBN
    , PhoneNumberCountryCodeBG
    , PhoneNumberCountryCodeBF
    , PhoneNumberCountryCodeBI
    , PhoneNumberCountryCodeKH
    , PhoneNumberCountryCodeCM
    , PhoneNumberCountryCodeCA
    , PhoneNumberCountryCodeCV
    , PhoneNumberCountryCodeKY
    , PhoneNumberCountryCodeCF
    , PhoneNumberCountryCodeTD
    , PhoneNumberCountryCodeCL
    , PhoneNumberCountryCodeCN
    , PhoneNumberCountryCodeCX
    , PhoneNumberCountryCodeCC
    , PhoneNumberCountryCodeCO
    , PhoneNumberCountryCodeKM
    , PhoneNumberCountryCodeCK
    , PhoneNumberCountryCodeCR
    , PhoneNumberCountryCodeHR
    , PhoneNumberCountryCodeCU
    , PhoneNumberCountryCodeCW
    , PhoneNumberCountryCodeCY
    , PhoneNumberCountryCodeCZ
    , PhoneNumberCountryCodeCD
    , PhoneNumberCountryCodeDK
    , PhoneNumberCountryCodeDJ
    , PhoneNumberCountryCodeDM
    , PhoneNumberCountryCodeDO
    , PhoneNumberCountryCodeTL
    , PhoneNumberCountryCodeEC
    , PhoneNumberCountryCodeEG
    , PhoneNumberCountryCodeSV
    , PhoneNumberCountryCodeGQ
    , PhoneNumberCountryCodeER
    , PhoneNumberCountryCodeEE
    , PhoneNumberCountryCodeET
    , PhoneNumberCountryCodeFK
    , PhoneNumberCountryCodeFO
    , PhoneNumberCountryCodeFJ
    , PhoneNumberCountryCodeFI
    , PhoneNumberCountryCodeFR
    , PhoneNumberCountryCodePF
    , PhoneNumberCountryCodeGA
    , PhoneNumberCountryCodeGM
    , PhoneNumberCountryCodeGE
    , PhoneNumberCountryCodeDE
    , PhoneNumberCountryCodeGH
    , PhoneNumberCountryCodeGI
    , PhoneNumberCountryCodeGR
    , PhoneNumberCountryCodeGL
    , PhoneNumberCountryCodeGD
    , PhoneNumberCountryCodeGU
    , PhoneNumberCountryCodeGT
    , PhoneNumberCountryCodeGG
    , PhoneNumberCountryCodeGN
    , PhoneNumberCountryCodeGW
    , PhoneNumberCountryCodeGY
    , PhoneNumberCountryCodeHT
    , PhoneNumberCountryCodeHN
    , PhoneNumberCountryCodeHK
    , PhoneNumberCountryCodeHU
    , PhoneNumberCountryCodeIS
    , PhoneNumberCountryCodeIN
    , PhoneNumberCountryCodeID
    , PhoneNumberCountryCodeIR
    , PhoneNumberCountryCodeIQ
    , PhoneNumberCountryCodeIE
    , PhoneNumberCountryCodeIM
    , PhoneNumberCountryCodeIL
    , PhoneNumberCountryCodeIT
    , PhoneNumberCountryCodeCI
    , PhoneNumberCountryCodeJM
    , PhoneNumberCountryCodeJP
    , PhoneNumberCountryCodeJE
    , PhoneNumberCountryCodeJO
    , PhoneNumberCountryCodeKZ
    , PhoneNumberCountryCodeKE
    , PhoneNumberCountryCodeKI
    , PhoneNumberCountryCodeKW
    , PhoneNumberCountryCodeKG
    , PhoneNumberCountryCodeLA
    , PhoneNumberCountryCodeLV
    , PhoneNumberCountryCodeLB
    , PhoneNumberCountryCodeLS
    , PhoneNumberCountryCodeLR
    , PhoneNumberCountryCodeLY
    , PhoneNumberCountryCodeLI
    , PhoneNumberCountryCodeLT
    , PhoneNumberCountryCodeLU
    , PhoneNumberCountryCodeMO
    , PhoneNumberCountryCodeMK
    , PhoneNumberCountryCodeMG
    , PhoneNumberCountryCodeMW
    , PhoneNumberCountryCodeMY
    , PhoneNumberCountryCodeMV
    , PhoneNumberCountryCodeML
    , PhoneNumberCountryCodeMT
    , PhoneNumberCountryCodeMH
    , PhoneNumberCountryCodeMR
    , PhoneNumberCountryCodeMU
    , PhoneNumberCountryCodeYT
    , PhoneNumberCountryCodeMX
    , PhoneNumberCountryCodeFM
    , PhoneNumberCountryCodeMD
    , PhoneNumberCountryCodeMC
    , PhoneNumberCountryCodeMN
    , PhoneNumberCountryCodeME
    , PhoneNumberCountryCodeMS
    , PhoneNumberCountryCodeMA
    , PhoneNumberCountryCodeMZ
    , PhoneNumberCountryCodeMM
    , PhoneNumberCountryCodeNA
    , PhoneNumberCountryCodeNR
    , PhoneNumberCountryCodeNP
    , PhoneNumberCountryCodeNL
    , PhoneNumberCountryCodeAN
    , PhoneNumberCountryCodeNC
    , PhoneNumberCountryCodeNZ
    , PhoneNumberCountryCodeNI
    , PhoneNumberCountryCodeNE
    , PhoneNumberCountryCodeNG
    , PhoneNumberCountryCodeNU
    , PhoneNumberCountryCodeKP
    , PhoneNumberCountryCodeMP
    , PhoneNumberCountryCodeNO
    , PhoneNumberCountryCodeOM
    , PhoneNumberCountryCodePK
    , PhoneNumberCountryCodePW
    , PhoneNumberCountryCodePA
    , PhoneNumberCountryCodePG
    , PhoneNumberCountryCodePY
    , PhoneNumberCountryCodePE
    , PhoneNumberCountryCodePH
    , PhoneNumberCountryCodePN
    , PhoneNumberCountryCodePL
    , PhoneNumberCountryCodePT
    , PhoneNumberCountryCodePR
    , PhoneNumberCountryCodeQA
    , PhoneNumberCountryCodeCG
    , PhoneNumberCountryCodeRE
    , PhoneNumberCountryCodeRO
    , PhoneNumberCountryCodeRU
    , PhoneNumberCountryCodeRW
    , PhoneNumberCountryCodeBL
    , PhoneNumberCountryCodeSH
    , PhoneNumberCountryCodeKN
    , PhoneNumberCountryCodeLC
    , PhoneNumberCountryCodeMF
    , PhoneNumberCountryCodePM
    , PhoneNumberCountryCodeVC
    , PhoneNumberCountryCodeWS
    , PhoneNumberCountryCodeSM
    , PhoneNumberCountryCodeST
    , PhoneNumberCountryCodeSA
    , PhoneNumberCountryCodeSN
    , PhoneNumberCountryCodeRS
    , PhoneNumberCountryCodeSC
    , PhoneNumberCountryCodeSL
    , PhoneNumberCountryCodeSG
    , PhoneNumberCountryCodeSX
    , PhoneNumberCountryCodeSK
    , PhoneNumberCountryCodeSI
    , PhoneNumberCountryCodeSB
    , PhoneNumberCountryCodeSO
    , PhoneNumberCountryCodeZA
    , PhoneNumberCountryCodeKR
    , PhoneNumberCountryCodeES
    , PhoneNumberCountryCodeLK
    , PhoneNumberCountryCodeSD
    , PhoneNumberCountryCodeSR
    , PhoneNumberCountryCodeSJ
    , PhoneNumberCountryCodeSZ
    , PhoneNumberCountryCodeSE
    , PhoneNumberCountryCodeCH
    , PhoneNumberCountryCodeSY
    , PhoneNumberCountryCodeTW
    , PhoneNumberCountryCodeTJ
    , PhoneNumberCountryCodeTZ
    , PhoneNumberCountryCodeTH
    , PhoneNumberCountryCodeTG
    , PhoneNumberCountryCodeTK
    , PhoneNumberCountryCodeTO
    , PhoneNumberCountryCodeTT
    , PhoneNumberCountryCodeTN
    , PhoneNumberCountryCodeTR
    , PhoneNumberCountryCodeTM
    , PhoneNumberCountryCodeTC
    , PhoneNumberCountryCodeTV
    , PhoneNumberCountryCodeVI
    , PhoneNumberCountryCodeUG
    , PhoneNumberCountryCodeUA
    , PhoneNumberCountryCodeAE
    , PhoneNumberCountryCodeGB
    , PhoneNumberCountryCodeUS
    , PhoneNumberCountryCodeUY
    , PhoneNumberCountryCodeUZ
    , PhoneNumberCountryCodeVU
    , PhoneNumberCountryCodeVA
    , PhoneNumberCountryCodeVE
    , PhoneNumberCountryCodeVN
    , PhoneNumberCountryCodeWF
    , PhoneNumberCountryCodeEH
    , PhoneNumberCountryCodeYE
    , PhoneNumberCountryCodeZM
    , PhoneNumberCountryCodeZW
    , fromPhoneNumberCountryCode
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype PhoneNumberCountryCode = PhoneNumberCountryCode'{fromPhoneNumberCountryCode
                                                         :: Core.Text}
                                   deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                   Core.Generic)
                                   deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                     Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                     Core.FromJSON, Core.ToXML, Core.FromXML,
                                                     Core.ToText, Core.FromText, Core.ToByteString,
                                                     Core.ToQuery, Core.ToHeader)

pattern PhoneNumberCountryCodeAF :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeAF = PhoneNumberCountryCode' "AF"

pattern PhoneNumberCountryCodeAL :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeAL = PhoneNumberCountryCode' "AL"

pattern PhoneNumberCountryCodeDZ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeDZ = PhoneNumberCountryCode' "DZ"

pattern PhoneNumberCountryCodeAS :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeAS = PhoneNumberCountryCode' "AS"

pattern PhoneNumberCountryCodeAD :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeAD = PhoneNumberCountryCode' "AD"

pattern PhoneNumberCountryCodeAO :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeAO = PhoneNumberCountryCode' "AO"

pattern PhoneNumberCountryCodeAI :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeAI = PhoneNumberCountryCode' "AI"

pattern PhoneNumberCountryCodeAQ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeAQ = PhoneNumberCountryCode' "AQ"

pattern PhoneNumberCountryCodeAG :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeAG = PhoneNumberCountryCode' "AG"

pattern PhoneNumberCountryCodeAR :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeAR = PhoneNumberCountryCode' "AR"

pattern PhoneNumberCountryCodeAM :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeAM = PhoneNumberCountryCode' "AM"

pattern PhoneNumberCountryCodeAW :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeAW = PhoneNumberCountryCode' "AW"

pattern PhoneNumberCountryCodeAU :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeAU = PhoneNumberCountryCode' "AU"

pattern PhoneNumberCountryCodeAT :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeAT = PhoneNumberCountryCode' "AT"

pattern PhoneNumberCountryCodeAZ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeAZ = PhoneNumberCountryCode' "AZ"

pattern PhoneNumberCountryCodeBS :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeBS = PhoneNumberCountryCode' "BS"

pattern PhoneNumberCountryCodeBH :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeBH = PhoneNumberCountryCode' "BH"

pattern PhoneNumberCountryCodeBD :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeBD = PhoneNumberCountryCode' "BD"

pattern PhoneNumberCountryCodeBB :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeBB = PhoneNumberCountryCode' "BB"

pattern PhoneNumberCountryCodeBY :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeBY = PhoneNumberCountryCode' "BY"

pattern PhoneNumberCountryCodeBE :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeBE = PhoneNumberCountryCode' "BE"

pattern PhoneNumberCountryCodeBZ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeBZ = PhoneNumberCountryCode' "BZ"

pattern PhoneNumberCountryCodeBJ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeBJ = PhoneNumberCountryCode' "BJ"

pattern PhoneNumberCountryCodeBM :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeBM = PhoneNumberCountryCode' "BM"

pattern PhoneNumberCountryCodeBT :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeBT = PhoneNumberCountryCode' "BT"

pattern PhoneNumberCountryCodeBO :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeBO = PhoneNumberCountryCode' "BO"

pattern PhoneNumberCountryCodeBA :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeBA = PhoneNumberCountryCode' "BA"

pattern PhoneNumberCountryCodeBW :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeBW = PhoneNumberCountryCode' "BW"

pattern PhoneNumberCountryCodeBR :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeBR = PhoneNumberCountryCode' "BR"

pattern PhoneNumberCountryCodeIO :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeIO = PhoneNumberCountryCode' "IO"

pattern PhoneNumberCountryCodeVG :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeVG = PhoneNumberCountryCode' "VG"

pattern PhoneNumberCountryCodeBN :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeBN = PhoneNumberCountryCode' "BN"

pattern PhoneNumberCountryCodeBG :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeBG = PhoneNumberCountryCode' "BG"

pattern PhoneNumberCountryCodeBF :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeBF = PhoneNumberCountryCode' "BF"

pattern PhoneNumberCountryCodeBI :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeBI = PhoneNumberCountryCode' "BI"

pattern PhoneNumberCountryCodeKH :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeKH = PhoneNumberCountryCode' "KH"

pattern PhoneNumberCountryCodeCM :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeCM = PhoneNumberCountryCode' "CM"

pattern PhoneNumberCountryCodeCA :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeCA = PhoneNumberCountryCode' "CA"

pattern PhoneNumberCountryCodeCV :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeCV = PhoneNumberCountryCode' "CV"

pattern PhoneNumberCountryCodeKY :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeKY = PhoneNumberCountryCode' "KY"

pattern PhoneNumberCountryCodeCF :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeCF = PhoneNumberCountryCode' "CF"

pattern PhoneNumberCountryCodeTD :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeTD = PhoneNumberCountryCode' "TD"

pattern PhoneNumberCountryCodeCL :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeCL = PhoneNumberCountryCode' "CL"

pattern PhoneNumberCountryCodeCN :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeCN = PhoneNumberCountryCode' "CN"

pattern PhoneNumberCountryCodeCX :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeCX = PhoneNumberCountryCode' "CX"

pattern PhoneNumberCountryCodeCC :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeCC = PhoneNumberCountryCode' "CC"

pattern PhoneNumberCountryCodeCO :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeCO = PhoneNumberCountryCode' "CO"

pattern PhoneNumberCountryCodeKM :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeKM = PhoneNumberCountryCode' "KM"

pattern PhoneNumberCountryCodeCK :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeCK = PhoneNumberCountryCode' "CK"

pattern PhoneNumberCountryCodeCR :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeCR = PhoneNumberCountryCode' "CR"

pattern PhoneNumberCountryCodeHR :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeHR = PhoneNumberCountryCode' "HR"

pattern PhoneNumberCountryCodeCU :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeCU = PhoneNumberCountryCode' "CU"

pattern PhoneNumberCountryCodeCW :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeCW = PhoneNumberCountryCode' "CW"

pattern PhoneNumberCountryCodeCY :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeCY = PhoneNumberCountryCode' "CY"

pattern PhoneNumberCountryCodeCZ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeCZ = PhoneNumberCountryCode' "CZ"

pattern PhoneNumberCountryCodeCD :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeCD = PhoneNumberCountryCode' "CD"

pattern PhoneNumberCountryCodeDK :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeDK = PhoneNumberCountryCode' "DK"

pattern PhoneNumberCountryCodeDJ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeDJ = PhoneNumberCountryCode' "DJ"

pattern PhoneNumberCountryCodeDM :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeDM = PhoneNumberCountryCode' "DM"

pattern PhoneNumberCountryCodeDO :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeDO = PhoneNumberCountryCode' "DO"

pattern PhoneNumberCountryCodeTL :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeTL = PhoneNumberCountryCode' "TL"

pattern PhoneNumberCountryCodeEC :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeEC = PhoneNumberCountryCode' "EC"

pattern PhoneNumberCountryCodeEG :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeEG = PhoneNumberCountryCode' "EG"

pattern PhoneNumberCountryCodeSV :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeSV = PhoneNumberCountryCode' "SV"

pattern PhoneNumberCountryCodeGQ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeGQ = PhoneNumberCountryCode' "GQ"

pattern PhoneNumberCountryCodeER :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeER = PhoneNumberCountryCode' "ER"

pattern PhoneNumberCountryCodeEE :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeEE = PhoneNumberCountryCode' "EE"

pattern PhoneNumberCountryCodeET :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeET = PhoneNumberCountryCode' "ET"

pattern PhoneNumberCountryCodeFK :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeFK = PhoneNumberCountryCode' "FK"

pattern PhoneNumberCountryCodeFO :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeFO = PhoneNumberCountryCode' "FO"

pattern PhoneNumberCountryCodeFJ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeFJ = PhoneNumberCountryCode' "FJ"

pattern PhoneNumberCountryCodeFI :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeFI = PhoneNumberCountryCode' "FI"

pattern PhoneNumberCountryCodeFR :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeFR = PhoneNumberCountryCode' "FR"

pattern PhoneNumberCountryCodePF :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodePF = PhoneNumberCountryCode' "PF"

pattern PhoneNumberCountryCodeGA :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeGA = PhoneNumberCountryCode' "GA"

pattern PhoneNumberCountryCodeGM :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeGM = PhoneNumberCountryCode' "GM"

pattern PhoneNumberCountryCodeGE :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeGE = PhoneNumberCountryCode' "GE"

pattern PhoneNumberCountryCodeDE :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeDE = PhoneNumberCountryCode' "DE"

pattern PhoneNumberCountryCodeGH :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeGH = PhoneNumberCountryCode' "GH"

pattern PhoneNumberCountryCodeGI :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeGI = PhoneNumberCountryCode' "GI"

pattern PhoneNumberCountryCodeGR :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeGR = PhoneNumberCountryCode' "GR"

pattern PhoneNumberCountryCodeGL :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeGL = PhoneNumberCountryCode' "GL"

pattern PhoneNumberCountryCodeGD :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeGD = PhoneNumberCountryCode' "GD"

pattern PhoneNumberCountryCodeGU :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeGU = PhoneNumberCountryCode' "GU"

pattern PhoneNumberCountryCodeGT :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeGT = PhoneNumberCountryCode' "GT"

pattern PhoneNumberCountryCodeGG :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeGG = PhoneNumberCountryCode' "GG"

pattern PhoneNumberCountryCodeGN :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeGN = PhoneNumberCountryCode' "GN"

pattern PhoneNumberCountryCodeGW :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeGW = PhoneNumberCountryCode' "GW"

pattern PhoneNumberCountryCodeGY :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeGY = PhoneNumberCountryCode' "GY"

pattern PhoneNumberCountryCodeHT :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeHT = PhoneNumberCountryCode' "HT"

pattern PhoneNumberCountryCodeHN :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeHN = PhoneNumberCountryCode' "HN"

pattern PhoneNumberCountryCodeHK :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeHK = PhoneNumberCountryCode' "HK"

pattern PhoneNumberCountryCodeHU :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeHU = PhoneNumberCountryCode' "HU"

pattern PhoneNumberCountryCodeIS :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeIS = PhoneNumberCountryCode' "IS"

pattern PhoneNumberCountryCodeIN :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeIN = PhoneNumberCountryCode' "IN"

pattern PhoneNumberCountryCodeID :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeID = PhoneNumberCountryCode' "ID"

pattern PhoneNumberCountryCodeIR :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeIR = PhoneNumberCountryCode' "IR"

pattern PhoneNumberCountryCodeIQ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeIQ = PhoneNumberCountryCode' "IQ"

pattern PhoneNumberCountryCodeIE :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeIE = PhoneNumberCountryCode' "IE"

pattern PhoneNumberCountryCodeIM :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeIM = PhoneNumberCountryCode' "IM"

pattern PhoneNumberCountryCodeIL :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeIL = PhoneNumberCountryCode' "IL"

pattern PhoneNumberCountryCodeIT :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeIT = PhoneNumberCountryCode' "IT"

pattern PhoneNumberCountryCodeCI :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeCI = PhoneNumberCountryCode' "CI"

pattern PhoneNumberCountryCodeJM :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeJM = PhoneNumberCountryCode' "JM"

pattern PhoneNumberCountryCodeJP :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeJP = PhoneNumberCountryCode' "JP"

pattern PhoneNumberCountryCodeJE :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeJE = PhoneNumberCountryCode' "JE"

pattern PhoneNumberCountryCodeJO :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeJO = PhoneNumberCountryCode' "JO"

pattern PhoneNumberCountryCodeKZ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeKZ = PhoneNumberCountryCode' "KZ"

pattern PhoneNumberCountryCodeKE :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeKE = PhoneNumberCountryCode' "KE"

pattern PhoneNumberCountryCodeKI :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeKI = PhoneNumberCountryCode' "KI"

pattern PhoneNumberCountryCodeKW :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeKW = PhoneNumberCountryCode' "KW"

pattern PhoneNumberCountryCodeKG :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeKG = PhoneNumberCountryCode' "KG"

pattern PhoneNumberCountryCodeLA :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeLA = PhoneNumberCountryCode' "LA"

pattern PhoneNumberCountryCodeLV :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeLV = PhoneNumberCountryCode' "LV"

pattern PhoneNumberCountryCodeLB :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeLB = PhoneNumberCountryCode' "LB"

pattern PhoneNumberCountryCodeLS :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeLS = PhoneNumberCountryCode' "LS"

pattern PhoneNumberCountryCodeLR :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeLR = PhoneNumberCountryCode' "LR"

pattern PhoneNumberCountryCodeLY :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeLY = PhoneNumberCountryCode' "LY"

pattern PhoneNumberCountryCodeLI :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeLI = PhoneNumberCountryCode' "LI"

pattern PhoneNumberCountryCodeLT :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeLT = PhoneNumberCountryCode' "LT"

pattern PhoneNumberCountryCodeLU :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeLU = PhoneNumberCountryCode' "LU"

pattern PhoneNumberCountryCodeMO :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeMO = PhoneNumberCountryCode' "MO"

pattern PhoneNumberCountryCodeMK :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeMK = PhoneNumberCountryCode' "MK"

pattern PhoneNumberCountryCodeMG :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeMG = PhoneNumberCountryCode' "MG"

pattern PhoneNumberCountryCodeMW :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeMW = PhoneNumberCountryCode' "MW"

pattern PhoneNumberCountryCodeMY :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeMY = PhoneNumberCountryCode' "MY"

pattern PhoneNumberCountryCodeMV :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeMV = PhoneNumberCountryCode' "MV"

pattern PhoneNumberCountryCodeML :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeML = PhoneNumberCountryCode' "ML"

pattern PhoneNumberCountryCodeMT :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeMT = PhoneNumberCountryCode' "MT"

pattern PhoneNumberCountryCodeMH :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeMH = PhoneNumberCountryCode' "MH"

pattern PhoneNumberCountryCodeMR :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeMR = PhoneNumberCountryCode' "MR"

pattern PhoneNumberCountryCodeMU :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeMU = PhoneNumberCountryCode' "MU"

pattern PhoneNumberCountryCodeYT :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeYT = PhoneNumberCountryCode' "YT"

pattern PhoneNumberCountryCodeMX :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeMX = PhoneNumberCountryCode' "MX"

pattern PhoneNumberCountryCodeFM :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeFM = PhoneNumberCountryCode' "FM"

pattern PhoneNumberCountryCodeMD :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeMD = PhoneNumberCountryCode' "MD"

pattern PhoneNumberCountryCodeMC :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeMC = PhoneNumberCountryCode' "MC"

pattern PhoneNumberCountryCodeMN :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeMN = PhoneNumberCountryCode' "MN"

pattern PhoneNumberCountryCodeME :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeME = PhoneNumberCountryCode' "ME"

pattern PhoneNumberCountryCodeMS :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeMS = PhoneNumberCountryCode' "MS"

pattern PhoneNumberCountryCodeMA :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeMA = PhoneNumberCountryCode' "MA"

pattern PhoneNumberCountryCodeMZ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeMZ = PhoneNumberCountryCode' "MZ"

pattern PhoneNumberCountryCodeMM :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeMM = PhoneNumberCountryCode' "MM"

pattern PhoneNumberCountryCodeNA :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeNA = PhoneNumberCountryCode' "NA"

pattern PhoneNumberCountryCodeNR :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeNR = PhoneNumberCountryCode' "NR"

pattern PhoneNumberCountryCodeNP :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeNP = PhoneNumberCountryCode' "NP"

pattern PhoneNumberCountryCodeNL :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeNL = PhoneNumberCountryCode' "NL"

pattern PhoneNumberCountryCodeAN :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeAN = PhoneNumberCountryCode' "AN"

pattern PhoneNumberCountryCodeNC :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeNC = PhoneNumberCountryCode' "NC"

pattern PhoneNumberCountryCodeNZ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeNZ = PhoneNumberCountryCode' "NZ"

pattern PhoneNumberCountryCodeNI :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeNI = PhoneNumberCountryCode' "NI"

pattern PhoneNumberCountryCodeNE :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeNE = PhoneNumberCountryCode' "NE"

pattern PhoneNumberCountryCodeNG :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeNG = PhoneNumberCountryCode' "NG"

pattern PhoneNumberCountryCodeNU :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeNU = PhoneNumberCountryCode' "NU"

pattern PhoneNumberCountryCodeKP :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeKP = PhoneNumberCountryCode' "KP"

pattern PhoneNumberCountryCodeMP :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeMP = PhoneNumberCountryCode' "MP"

pattern PhoneNumberCountryCodeNO :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeNO = PhoneNumberCountryCode' "NO"

pattern PhoneNumberCountryCodeOM :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeOM = PhoneNumberCountryCode' "OM"

pattern PhoneNumberCountryCodePK :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodePK = PhoneNumberCountryCode' "PK"

pattern PhoneNumberCountryCodePW :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodePW = PhoneNumberCountryCode' "PW"

pattern PhoneNumberCountryCodePA :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodePA = PhoneNumberCountryCode' "PA"

pattern PhoneNumberCountryCodePG :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodePG = PhoneNumberCountryCode' "PG"

pattern PhoneNumberCountryCodePY :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodePY = PhoneNumberCountryCode' "PY"

pattern PhoneNumberCountryCodePE :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodePE = PhoneNumberCountryCode' "PE"

pattern PhoneNumberCountryCodePH :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodePH = PhoneNumberCountryCode' "PH"

pattern PhoneNumberCountryCodePN :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodePN = PhoneNumberCountryCode' "PN"

pattern PhoneNumberCountryCodePL :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodePL = PhoneNumberCountryCode' "PL"

pattern PhoneNumberCountryCodePT :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodePT = PhoneNumberCountryCode' "PT"

pattern PhoneNumberCountryCodePR :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodePR = PhoneNumberCountryCode' "PR"

pattern PhoneNumberCountryCodeQA :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeQA = PhoneNumberCountryCode' "QA"

pattern PhoneNumberCountryCodeCG :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeCG = PhoneNumberCountryCode' "CG"

pattern PhoneNumberCountryCodeRE :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeRE = PhoneNumberCountryCode' "RE"

pattern PhoneNumberCountryCodeRO :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeRO = PhoneNumberCountryCode' "RO"

pattern PhoneNumberCountryCodeRU :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeRU = PhoneNumberCountryCode' "RU"

pattern PhoneNumberCountryCodeRW :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeRW = PhoneNumberCountryCode' "RW"

pattern PhoneNumberCountryCodeBL :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeBL = PhoneNumberCountryCode' "BL"

pattern PhoneNumberCountryCodeSH :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeSH = PhoneNumberCountryCode' "SH"

pattern PhoneNumberCountryCodeKN :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeKN = PhoneNumberCountryCode' "KN"

pattern PhoneNumberCountryCodeLC :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeLC = PhoneNumberCountryCode' "LC"

pattern PhoneNumberCountryCodeMF :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeMF = PhoneNumberCountryCode' "MF"

pattern PhoneNumberCountryCodePM :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodePM = PhoneNumberCountryCode' "PM"

pattern PhoneNumberCountryCodeVC :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeVC = PhoneNumberCountryCode' "VC"

pattern PhoneNumberCountryCodeWS :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeWS = PhoneNumberCountryCode' "WS"

pattern PhoneNumberCountryCodeSM :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeSM = PhoneNumberCountryCode' "SM"

pattern PhoneNumberCountryCodeST :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeST = PhoneNumberCountryCode' "ST"

pattern PhoneNumberCountryCodeSA :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeSA = PhoneNumberCountryCode' "SA"

pattern PhoneNumberCountryCodeSN :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeSN = PhoneNumberCountryCode' "SN"

pattern PhoneNumberCountryCodeRS :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeRS = PhoneNumberCountryCode' "RS"

pattern PhoneNumberCountryCodeSC :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeSC = PhoneNumberCountryCode' "SC"

pattern PhoneNumberCountryCodeSL :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeSL = PhoneNumberCountryCode' "SL"

pattern PhoneNumberCountryCodeSG :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeSG = PhoneNumberCountryCode' "SG"

pattern PhoneNumberCountryCodeSX :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeSX = PhoneNumberCountryCode' "SX"

pattern PhoneNumberCountryCodeSK :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeSK = PhoneNumberCountryCode' "SK"

pattern PhoneNumberCountryCodeSI :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeSI = PhoneNumberCountryCode' "SI"

pattern PhoneNumberCountryCodeSB :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeSB = PhoneNumberCountryCode' "SB"

pattern PhoneNumberCountryCodeSO :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeSO = PhoneNumberCountryCode' "SO"

pattern PhoneNumberCountryCodeZA :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeZA = PhoneNumberCountryCode' "ZA"

pattern PhoneNumberCountryCodeKR :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeKR = PhoneNumberCountryCode' "KR"

pattern PhoneNumberCountryCodeES :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeES = PhoneNumberCountryCode' "ES"

pattern PhoneNumberCountryCodeLK :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeLK = PhoneNumberCountryCode' "LK"

pattern PhoneNumberCountryCodeSD :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeSD = PhoneNumberCountryCode' "SD"

pattern PhoneNumberCountryCodeSR :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeSR = PhoneNumberCountryCode' "SR"

pattern PhoneNumberCountryCodeSJ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeSJ = PhoneNumberCountryCode' "SJ"

pattern PhoneNumberCountryCodeSZ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeSZ = PhoneNumberCountryCode' "SZ"

pattern PhoneNumberCountryCodeSE :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeSE = PhoneNumberCountryCode' "SE"

pattern PhoneNumberCountryCodeCH :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeCH = PhoneNumberCountryCode' "CH"

pattern PhoneNumberCountryCodeSY :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeSY = PhoneNumberCountryCode' "SY"

pattern PhoneNumberCountryCodeTW :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeTW = PhoneNumberCountryCode' "TW"

pattern PhoneNumberCountryCodeTJ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeTJ = PhoneNumberCountryCode' "TJ"

pattern PhoneNumberCountryCodeTZ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeTZ = PhoneNumberCountryCode' "TZ"

pattern PhoneNumberCountryCodeTH :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeTH = PhoneNumberCountryCode' "TH"

pattern PhoneNumberCountryCodeTG :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeTG = PhoneNumberCountryCode' "TG"

pattern PhoneNumberCountryCodeTK :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeTK = PhoneNumberCountryCode' "TK"

pattern PhoneNumberCountryCodeTO :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeTO = PhoneNumberCountryCode' "TO"

pattern PhoneNumberCountryCodeTT :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeTT = PhoneNumberCountryCode' "TT"

pattern PhoneNumberCountryCodeTN :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeTN = PhoneNumberCountryCode' "TN"

pattern PhoneNumberCountryCodeTR :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeTR = PhoneNumberCountryCode' "TR"

pattern PhoneNumberCountryCodeTM :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeTM = PhoneNumberCountryCode' "TM"

pattern PhoneNumberCountryCodeTC :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeTC = PhoneNumberCountryCode' "TC"

pattern PhoneNumberCountryCodeTV :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeTV = PhoneNumberCountryCode' "TV"

pattern PhoneNumberCountryCodeVI :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeVI = PhoneNumberCountryCode' "VI"

pattern PhoneNumberCountryCodeUG :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeUG = PhoneNumberCountryCode' "UG"

pattern PhoneNumberCountryCodeUA :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeUA = PhoneNumberCountryCode' "UA"

pattern PhoneNumberCountryCodeAE :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeAE = PhoneNumberCountryCode' "AE"

pattern PhoneNumberCountryCodeGB :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeGB = PhoneNumberCountryCode' "GB"

pattern PhoneNumberCountryCodeUS :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeUS = PhoneNumberCountryCode' "US"

pattern PhoneNumberCountryCodeUY :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeUY = PhoneNumberCountryCode' "UY"

pattern PhoneNumberCountryCodeUZ :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeUZ = PhoneNumberCountryCode' "UZ"

pattern PhoneNumberCountryCodeVU :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeVU = PhoneNumberCountryCode' "VU"

pattern PhoneNumberCountryCodeVA :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeVA = PhoneNumberCountryCode' "VA"

pattern PhoneNumberCountryCodeVE :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeVE = PhoneNumberCountryCode' "VE"

pattern PhoneNumberCountryCodeVN :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeVN = PhoneNumberCountryCode' "VN"

pattern PhoneNumberCountryCodeWF :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeWF = PhoneNumberCountryCode' "WF"

pattern PhoneNumberCountryCodeEH :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeEH = PhoneNumberCountryCode' "EH"

pattern PhoneNumberCountryCodeYE :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeYE = PhoneNumberCountryCode' "YE"

pattern PhoneNumberCountryCodeZM :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeZM = PhoneNumberCountryCode' "ZM"

pattern PhoneNumberCountryCodeZW :: PhoneNumberCountryCode
pattern PhoneNumberCountryCodeZW = PhoneNumberCountryCode' "ZW"

{-# COMPLETE 
  PhoneNumberCountryCodeAF,

  PhoneNumberCountryCodeAL,

  PhoneNumberCountryCodeDZ,

  PhoneNumberCountryCodeAS,

  PhoneNumberCountryCodeAD,

  PhoneNumberCountryCodeAO,

  PhoneNumberCountryCodeAI,

  PhoneNumberCountryCodeAQ,

  PhoneNumberCountryCodeAG,

  PhoneNumberCountryCodeAR,

  PhoneNumberCountryCodeAM,

  PhoneNumberCountryCodeAW,

  PhoneNumberCountryCodeAU,

  PhoneNumberCountryCodeAT,

  PhoneNumberCountryCodeAZ,

  PhoneNumberCountryCodeBS,

  PhoneNumberCountryCodeBH,

  PhoneNumberCountryCodeBD,

  PhoneNumberCountryCodeBB,

  PhoneNumberCountryCodeBY,

  PhoneNumberCountryCodeBE,

  PhoneNumberCountryCodeBZ,

  PhoneNumberCountryCodeBJ,

  PhoneNumberCountryCodeBM,

  PhoneNumberCountryCodeBT,

  PhoneNumberCountryCodeBO,

  PhoneNumberCountryCodeBA,

  PhoneNumberCountryCodeBW,

  PhoneNumberCountryCodeBR,

  PhoneNumberCountryCodeIO,

  PhoneNumberCountryCodeVG,

  PhoneNumberCountryCodeBN,

  PhoneNumberCountryCodeBG,

  PhoneNumberCountryCodeBF,

  PhoneNumberCountryCodeBI,

  PhoneNumberCountryCodeKH,

  PhoneNumberCountryCodeCM,

  PhoneNumberCountryCodeCA,

  PhoneNumberCountryCodeCV,

  PhoneNumberCountryCodeKY,

  PhoneNumberCountryCodeCF,

  PhoneNumberCountryCodeTD,

  PhoneNumberCountryCodeCL,

  PhoneNumberCountryCodeCN,

  PhoneNumberCountryCodeCX,

  PhoneNumberCountryCodeCC,

  PhoneNumberCountryCodeCO,

  PhoneNumberCountryCodeKM,

  PhoneNumberCountryCodeCK,

  PhoneNumberCountryCodeCR,

  PhoneNumberCountryCodeHR,

  PhoneNumberCountryCodeCU,

  PhoneNumberCountryCodeCW,

  PhoneNumberCountryCodeCY,

  PhoneNumberCountryCodeCZ,

  PhoneNumberCountryCodeCD,

  PhoneNumberCountryCodeDK,

  PhoneNumberCountryCodeDJ,

  PhoneNumberCountryCodeDM,

  PhoneNumberCountryCodeDO,

  PhoneNumberCountryCodeTL,

  PhoneNumberCountryCodeEC,

  PhoneNumberCountryCodeEG,

  PhoneNumberCountryCodeSV,

  PhoneNumberCountryCodeGQ,

  PhoneNumberCountryCodeER,

  PhoneNumberCountryCodeEE,

  PhoneNumberCountryCodeET,

  PhoneNumberCountryCodeFK,

  PhoneNumberCountryCodeFO,

  PhoneNumberCountryCodeFJ,

  PhoneNumberCountryCodeFI,

  PhoneNumberCountryCodeFR,

  PhoneNumberCountryCodePF,

  PhoneNumberCountryCodeGA,

  PhoneNumberCountryCodeGM,

  PhoneNumberCountryCodeGE,

  PhoneNumberCountryCodeDE,

  PhoneNumberCountryCodeGH,

  PhoneNumberCountryCodeGI,

  PhoneNumberCountryCodeGR,

  PhoneNumberCountryCodeGL,

  PhoneNumberCountryCodeGD,

  PhoneNumberCountryCodeGU,

  PhoneNumberCountryCodeGT,

  PhoneNumberCountryCodeGG,

  PhoneNumberCountryCodeGN,

  PhoneNumberCountryCodeGW,

  PhoneNumberCountryCodeGY,

  PhoneNumberCountryCodeHT,

  PhoneNumberCountryCodeHN,

  PhoneNumberCountryCodeHK,

  PhoneNumberCountryCodeHU,

  PhoneNumberCountryCodeIS,

  PhoneNumberCountryCodeIN,

  PhoneNumberCountryCodeID,

  PhoneNumberCountryCodeIR,

  PhoneNumberCountryCodeIQ,

  PhoneNumberCountryCodeIE,

  PhoneNumberCountryCodeIM,

  PhoneNumberCountryCodeIL,

  PhoneNumberCountryCodeIT,

  PhoneNumberCountryCodeCI,

  PhoneNumberCountryCodeJM,

  PhoneNumberCountryCodeJP,

  PhoneNumberCountryCodeJE,

  PhoneNumberCountryCodeJO,

  PhoneNumberCountryCodeKZ,

  PhoneNumberCountryCodeKE,

  PhoneNumberCountryCodeKI,

  PhoneNumberCountryCodeKW,

  PhoneNumberCountryCodeKG,

  PhoneNumberCountryCodeLA,

  PhoneNumberCountryCodeLV,

  PhoneNumberCountryCodeLB,

  PhoneNumberCountryCodeLS,

  PhoneNumberCountryCodeLR,

  PhoneNumberCountryCodeLY,

  PhoneNumberCountryCodeLI,

  PhoneNumberCountryCodeLT,

  PhoneNumberCountryCodeLU,

  PhoneNumberCountryCodeMO,

  PhoneNumberCountryCodeMK,

  PhoneNumberCountryCodeMG,

  PhoneNumberCountryCodeMW,

  PhoneNumberCountryCodeMY,

  PhoneNumberCountryCodeMV,

  PhoneNumberCountryCodeML,

  PhoneNumberCountryCodeMT,

  PhoneNumberCountryCodeMH,

  PhoneNumberCountryCodeMR,

  PhoneNumberCountryCodeMU,

  PhoneNumberCountryCodeYT,

  PhoneNumberCountryCodeMX,

  PhoneNumberCountryCodeFM,

  PhoneNumberCountryCodeMD,

  PhoneNumberCountryCodeMC,

  PhoneNumberCountryCodeMN,

  PhoneNumberCountryCodeME,

  PhoneNumberCountryCodeMS,

  PhoneNumberCountryCodeMA,

  PhoneNumberCountryCodeMZ,

  PhoneNumberCountryCodeMM,

  PhoneNumberCountryCodeNA,

  PhoneNumberCountryCodeNR,

  PhoneNumberCountryCodeNP,

  PhoneNumberCountryCodeNL,

  PhoneNumberCountryCodeAN,

  PhoneNumberCountryCodeNC,

  PhoneNumberCountryCodeNZ,

  PhoneNumberCountryCodeNI,

  PhoneNumberCountryCodeNE,

  PhoneNumberCountryCodeNG,

  PhoneNumberCountryCodeNU,

  PhoneNumberCountryCodeKP,

  PhoneNumberCountryCodeMP,

  PhoneNumberCountryCodeNO,

  PhoneNumberCountryCodeOM,

  PhoneNumberCountryCodePK,

  PhoneNumberCountryCodePW,

  PhoneNumberCountryCodePA,

  PhoneNumberCountryCodePG,

  PhoneNumberCountryCodePY,

  PhoneNumberCountryCodePE,

  PhoneNumberCountryCodePH,

  PhoneNumberCountryCodePN,

  PhoneNumberCountryCodePL,

  PhoneNumberCountryCodePT,

  PhoneNumberCountryCodePR,

  PhoneNumberCountryCodeQA,

  PhoneNumberCountryCodeCG,

  PhoneNumberCountryCodeRE,

  PhoneNumberCountryCodeRO,

  PhoneNumberCountryCodeRU,

  PhoneNumberCountryCodeRW,

  PhoneNumberCountryCodeBL,

  PhoneNumberCountryCodeSH,

  PhoneNumberCountryCodeKN,

  PhoneNumberCountryCodeLC,

  PhoneNumberCountryCodeMF,

  PhoneNumberCountryCodePM,

  PhoneNumberCountryCodeVC,

  PhoneNumberCountryCodeWS,

  PhoneNumberCountryCodeSM,

  PhoneNumberCountryCodeST,

  PhoneNumberCountryCodeSA,

  PhoneNumberCountryCodeSN,

  PhoneNumberCountryCodeRS,

  PhoneNumberCountryCodeSC,

  PhoneNumberCountryCodeSL,

  PhoneNumberCountryCodeSG,

  PhoneNumberCountryCodeSX,

  PhoneNumberCountryCodeSK,

  PhoneNumberCountryCodeSI,

  PhoneNumberCountryCodeSB,

  PhoneNumberCountryCodeSO,

  PhoneNumberCountryCodeZA,

  PhoneNumberCountryCodeKR,

  PhoneNumberCountryCodeES,

  PhoneNumberCountryCodeLK,

  PhoneNumberCountryCodeSD,

  PhoneNumberCountryCodeSR,

  PhoneNumberCountryCodeSJ,

  PhoneNumberCountryCodeSZ,

  PhoneNumberCountryCodeSE,

  PhoneNumberCountryCodeCH,

  PhoneNumberCountryCodeSY,

  PhoneNumberCountryCodeTW,

  PhoneNumberCountryCodeTJ,

  PhoneNumberCountryCodeTZ,

  PhoneNumberCountryCodeTH,

  PhoneNumberCountryCodeTG,

  PhoneNumberCountryCodeTK,

  PhoneNumberCountryCodeTO,

  PhoneNumberCountryCodeTT,

  PhoneNumberCountryCodeTN,

  PhoneNumberCountryCodeTR,

  PhoneNumberCountryCodeTM,

  PhoneNumberCountryCodeTC,

  PhoneNumberCountryCodeTV,

  PhoneNumberCountryCodeVI,

  PhoneNumberCountryCodeUG,

  PhoneNumberCountryCodeUA,

  PhoneNumberCountryCodeAE,

  PhoneNumberCountryCodeGB,

  PhoneNumberCountryCodeUS,

  PhoneNumberCountryCodeUY,

  PhoneNumberCountryCodeUZ,

  PhoneNumberCountryCodeVU,

  PhoneNumberCountryCodeVA,

  PhoneNumberCountryCodeVE,

  PhoneNumberCountryCodeVN,

  PhoneNumberCountryCodeWF,

  PhoneNumberCountryCodeEH,

  PhoneNumberCountryCodeYE,

  PhoneNumberCountryCodeZM,

  PhoneNumberCountryCodeZW,
  PhoneNumberCountryCode'
  #-}
