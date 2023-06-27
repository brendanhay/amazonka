{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.PaymentCryptography.Types.KeyUsage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptography.Types.KeyUsage
  ( KeyUsage
      ( ..,
        KeyUsage_TR31_B0_BASE_DERIVATION_KEY,
        KeyUsage_TR31_C0_CARD_VERIFICATION_KEY,
        KeyUsage_TR31_D0_SYMMETRIC_DATA_ENCRYPTION_KEY,
        KeyUsage_TR31_D1_ASYMMETRIC_KEY_FOR_DATA_ENCRYPTION,
        KeyUsage_TR31_E0_EMV_MKEY_APP_CRYPTOGRAMS,
        KeyUsage_TR31_E1_EMV_MKEY_CONFIDENTIALITY,
        KeyUsage_TR31_E2_EMV_MKEY_INTEGRITY,
        KeyUsage_TR31_E4_EMV_MKEY_DYNAMIC_NUMBERS,
        KeyUsage_TR31_E5_EMV_MKEY_CARD_PERSONALIZATION,
        KeyUsage_TR31_E6_EMV_MKEY_OTHER,
        KeyUsage_TR31_K0_KEY_ENCRYPTION_KEY,
        KeyUsage_TR31_K1_KEY_BLOCK_PROTECTION_KEY,
        KeyUsage_TR31_K2_TR34_ASYMMETRIC_KEY,
        KeyUsage_TR31_K3_ASYMMETRIC_KEY_FOR_KEY_AGREEMENT,
        KeyUsage_TR31_M3_ISO_9797_3_MAC_KEY,
        KeyUsage_TR31_M6_ISO_9797_5_CMAC_KEY,
        KeyUsage_TR31_M7_HMAC_KEY,
        KeyUsage_TR31_P0_PIN_ENCRYPTION_KEY,
        KeyUsage_TR31_P1_PIN_GENERATION_KEY,
        KeyUsage_TR31_S0_ASYMMETRIC_KEY_FOR_DIGITAL_SIGNATURE,
        KeyUsage_TR31_V1_IBM3624_PIN_VERIFICATION_KEY,
        KeyUsage_TR31_V2_VISA_PIN_VERIFICATION_KEY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype KeyUsage = KeyUsage'
  { fromKeyUsage ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern KeyUsage_TR31_B0_BASE_DERIVATION_KEY :: KeyUsage
pattern KeyUsage_TR31_B0_BASE_DERIVATION_KEY = KeyUsage' "TR31_B0_BASE_DERIVATION_KEY"

pattern KeyUsage_TR31_C0_CARD_VERIFICATION_KEY :: KeyUsage
pattern KeyUsage_TR31_C0_CARD_VERIFICATION_KEY = KeyUsage' "TR31_C0_CARD_VERIFICATION_KEY"

pattern KeyUsage_TR31_D0_SYMMETRIC_DATA_ENCRYPTION_KEY :: KeyUsage
pattern KeyUsage_TR31_D0_SYMMETRIC_DATA_ENCRYPTION_KEY = KeyUsage' "TR31_D0_SYMMETRIC_DATA_ENCRYPTION_KEY"

pattern KeyUsage_TR31_D1_ASYMMETRIC_KEY_FOR_DATA_ENCRYPTION :: KeyUsage
pattern KeyUsage_TR31_D1_ASYMMETRIC_KEY_FOR_DATA_ENCRYPTION = KeyUsage' "TR31_D1_ASYMMETRIC_KEY_FOR_DATA_ENCRYPTION"

pattern KeyUsage_TR31_E0_EMV_MKEY_APP_CRYPTOGRAMS :: KeyUsage
pattern KeyUsage_TR31_E0_EMV_MKEY_APP_CRYPTOGRAMS = KeyUsage' "TR31_E0_EMV_MKEY_APP_CRYPTOGRAMS"

pattern KeyUsage_TR31_E1_EMV_MKEY_CONFIDENTIALITY :: KeyUsage
pattern KeyUsage_TR31_E1_EMV_MKEY_CONFIDENTIALITY = KeyUsage' "TR31_E1_EMV_MKEY_CONFIDENTIALITY"

pattern KeyUsage_TR31_E2_EMV_MKEY_INTEGRITY :: KeyUsage
pattern KeyUsage_TR31_E2_EMV_MKEY_INTEGRITY = KeyUsage' "TR31_E2_EMV_MKEY_INTEGRITY"

pattern KeyUsage_TR31_E4_EMV_MKEY_DYNAMIC_NUMBERS :: KeyUsage
pattern KeyUsage_TR31_E4_EMV_MKEY_DYNAMIC_NUMBERS = KeyUsage' "TR31_E4_EMV_MKEY_DYNAMIC_NUMBERS"

pattern KeyUsage_TR31_E5_EMV_MKEY_CARD_PERSONALIZATION :: KeyUsage
pattern KeyUsage_TR31_E5_EMV_MKEY_CARD_PERSONALIZATION = KeyUsage' "TR31_E5_EMV_MKEY_CARD_PERSONALIZATION"

pattern KeyUsage_TR31_E6_EMV_MKEY_OTHER :: KeyUsage
pattern KeyUsage_TR31_E6_EMV_MKEY_OTHER = KeyUsage' "TR31_E6_EMV_MKEY_OTHER"

pattern KeyUsage_TR31_K0_KEY_ENCRYPTION_KEY :: KeyUsage
pattern KeyUsage_TR31_K0_KEY_ENCRYPTION_KEY = KeyUsage' "TR31_K0_KEY_ENCRYPTION_KEY"

pattern KeyUsage_TR31_K1_KEY_BLOCK_PROTECTION_KEY :: KeyUsage
pattern KeyUsage_TR31_K1_KEY_BLOCK_PROTECTION_KEY = KeyUsage' "TR31_K1_KEY_BLOCK_PROTECTION_KEY"

pattern KeyUsage_TR31_K2_TR34_ASYMMETRIC_KEY :: KeyUsage
pattern KeyUsage_TR31_K2_TR34_ASYMMETRIC_KEY = KeyUsage' "TR31_K2_TR34_ASYMMETRIC_KEY"

pattern KeyUsage_TR31_K3_ASYMMETRIC_KEY_FOR_KEY_AGREEMENT :: KeyUsage
pattern KeyUsage_TR31_K3_ASYMMETRIC_KEY_FOR_KEY_AGREEMENT = KeyUsage' "TR31_K3_ASYMMETRIC_KEY_FOR_KEY_AGREEMENT"

pattern KeyUsage_TR31_M3_ISO_9797_3_MAC_KEY :: KeyUsage
pattern KeyUsage_TR31_M3_ISO_9797_3_MAC_KEY = KeyUsage' "TR31_M3_ISO_9797_3_MAC_KEY"

pattern KeyUsage_TR31_M6_ISO_9797_5_CMAC_KEY :: KeyUsage
pattern KeyUsage_TR31_M6_ISO_9797_5_CMAC_KEY = KeyUsage' "TR31_M6_ISO_9797_5_CMAC_KEY"

pattern KeyUsage_TR31_M7_HMAC_KEY :: KeyUsage
pattern KeyUsage_TR31_M7_HMAC_KEY = KeyUsage' "TR31_M7_HMAC_KEY"

pattern KeyUsage_TR31_P0_PIN_ENCRYPTION_KEY :: KeyUsage
pattern KeyUsage_TR31_P0_PIN_ENCRYPTION_KEY = KeyUsage' "TR31_P0_PIN_ENCRYPTION_KEY"

pattern KeyUsage_TR31_P1_PIN_GENERATION_KEY :: KeyUsage
pattern KeyUsage_TR31_P1_PIN_GENERATION_KEY = KeyUsage' "TR31_P1_PIN_GENERATION_KEY"

pattern KeyUsage_TR31_S0_ASYMMETRIC_KEY_FOR_DIGITAL_SIGNATURE :: KeyUsage
pattern KeyUsage_TR31_S0_ASYMMETRIC_KEY_FOR_DIGITAL_SIGNATURE = KeyUsage' "TR31_S0_ASYMMETRIC_KEY_FOR_DIGITAL_SIGNATURE"

pattern KeyUsage_TR31_V1_IBM3624_PIN_VERIFICATION_KEY :: KeyUsage
pattern KeyUsage_TR31_V1_IBM3624_PIN_VERIFICATION_KEY = KeyUsage' "TR31_V1_IBM3624_PIN_VERIFICATION_KEY"

pattern KeyUsage_TR31_V2_VISA_PIN_VERIFICATION_KEY :: KeyUsage
pattern KeyUsage_TR31_V2_VISA_PIN_VERIFICATION_KEY = KeyUsage' "TR31_V2_VISA_PIN_VERIFICATION_KEY"

{-# COMPLETE
  KeyUsage_TR31_B0_BASE_DERIVATION_KEY,
  KeyUsage_TR31_C0_CARD_VERIFICATION_KEY,
  KeyUsage_TR31_D0_SYMMETRIC_DATA_ENCRYPTION_KEY,
  KeyUsage_TR31_D1_ASYMMETRIC_KEY_FOR_DATA_ENCRYPTION,
  KeyUsage_TR31_E0_EMV_MKEY_APP_CRYPTOGRAMS,
  KeyUsage_TR31_E1_EMV_MKEY_CONFIDENTIALITY,
  KeyUsage_TR31_E2_EMV_MKEY_INTEGRITY,
  KeyUsage_TR31_E4_EMV_MKEY_DYNAMIC_NUMBERS,
  KeyUsage_TR31_E5_EMV_MKEY_CARD_PERSONALIZATION,
  KeyUsage_TR31_E6_EMV_MKEY_OTHER,
  KeyUsage_TR31_K0_KEY_ENCRYPTION_KEY,
  KeyUsage_TR31_K1_KEY_BLOCK_PROTECTION_KEY,
  KeyUsage_TR31_K2_TR34_ASYMMETRIC_KEY,
  KeyUsage_TR31_K3_ASYMMETRIC_KEY_FOR_KEY_AGREEMENT,
  KeyUsage_TR31_M3_ISO_9797_3_MAC_KEY,
  KeyUsage_TR31_M6_ISO_9797_5_CMAC_KEY,
  KeyUsage_TR31_M7_HMAC_KEY,
  KeyUsage_TR31_P0_PIN_ENCRYPTION_KEY,
  KeyUsage_TR31_P1_PIN_GENERATION_KEY,
  KeyUsage_TR31_S0_ASYMMETRIC_KEY_FOR_DIGITAL_SIGNATURE,
  KeyUsage_TR31_V1_IBM3624_PIN_VERIFICATION_KEY,
  KeyUsage_TR31_V2_VISA_PIN_VERIFICATION_KEY,
  KeyUsage'
  #-}
