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
-- Module      : Amazonka.PaymentCryptography.Types.KeyAlgorithm
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptography.Types.KeyAlgorithm
  ( KeyAlgorithm
      ( ..,
        KeyAlgorithm_AES_128,
        KeyAlgorithm_AES_192,
        KeyAlgorithm_AES_256,
        KeyAlgorithm_RSA_2048,
        KeyAlgorithm_RSA_3072,
        KeyAlgorithm_RSA_4096,
        KeyAlgorithm_TDES_2KEY,
        KeyAlgorithm_TDES_3KEY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype KeyAlgorithm = KeyAlgorithm'
  { fromKeyAlgorithm ::
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

pattern KeyAlgorithm_AES_128 :: KeyAlgorithm
pattern KeyAlgorithm_AES_128 = KeyAlgorithm' "AES_128"

pattern KeyAlgorithm_AES_192 :: KeyAlgorithm
pattern KeyAlgorithm_AES_192 = KeyAlgorithm' "AES_192"

pattern KeyAlgorithm_AES_256 :: KeyAlgorithm
pattern KeyAlgorithm_AES_256 = KeyAlgorithm' "AES_256"

pattern KeyAlgorithm_RSA_2048 :: KeyAlgorithm
pattern KeyAlgorithm_RSA_2048 = KeyAlgorithm' "RSA_2048"

pattern KeyAlgorithm_RSA_3072 :: KeyAlgorithm
pattern KeyAlgorithm_RSA_3072 = KeyAlgorithm' "RSA_3072"

pattern KeyAlgorithm_RSA_4096 :: KeyAlgorithm
pattern KeyAlgorithm_RSA_4096 = KeyAlgorithm' "RSA_4096"

pattern KeyAlgorithm_TDES_2KEY :: KeyAlgorithm
pattern KeyAlgorithm_TDES_2KEY = KeyAlgorithm' "TDES_2KEY"

pattern KeyAlgorithm_TDES_3KEY :: KeyAlgorithm
pattern KeyAlgorithm_TDES_3KEY = KeyAlgorithm' "TDES_3KEY"

{-# COMPLETE
  KeyAlgorithm_AES_128,
  KeyAlgorithm_AES_192,
  KeyAlgorithm_AES_256,
  KeyAlgorithm_RSA_2048,
  KeyAlgorithm_RSA_3072,
  KeyAlgorithm_RSA_4096,
  KeyAlgorithm_TDES_2KEY,
  KeyAlgorithm_TDES_3KEY,
  KeyAlgorithm'
  #-}
