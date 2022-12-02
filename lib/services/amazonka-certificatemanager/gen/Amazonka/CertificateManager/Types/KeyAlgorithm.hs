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
-- Module      : Amazonka.CertificateManager.Types.KeyAlgorithm
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManager.Types.KeyAlgorithm
  ( KeyAlgorithm
      ( ..,
        KeyAlgorithm_EC_prime256v1,
        KeyAlgorithm_EC_secp384r1,
        KeyAlgorithm_EC_secp521r1,
        KeyAlgorithm_RSA_1024,
        KeyAlgorithm_RSA_2048,
        KeyAlgorithm_RSA_3072,
        KeyAlgorithm_RSA_4096
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

pattern KeyAlgorithm_EC_prime256v1 :: KeyAlgorithm
pattern KeyAlgorithm_EC_prime256v1 = KeyAlgorithm' "EC_prime256v1"

pattern KeyAlgorithm_EC_secp384r1 :: KeyAlgorithm
pattern KeyAlgorithm_EC_secp384r1 = KeyAlgorithm' "EC_secp384r1"

pattern KeyAlgorithm_EC_secp521r1 :: KeyAlgorithm
pattern KeyAlgorithm_EC_secp521r1 = KeyAlgorithm' "EC_secp521r1"

pattern KeyAlgorithm_RSA_1024 :: KeyAlgorithm
pattern KeyAlgorithm_RSA_1024 = KeyAlgorithm' "RSA_1024"

pattern KeyAlgorithm_RSA_2048 :: KeyAlgorithm
pattern KeyAlgorithm_RSA_2048 = KeyAlgorithm' "RSA_2048"

pattern KeyAlgorithm_RSA_3072 :: KeyAlgorithm
pattern KeyAlgorithm_RSA_3072 = KeyAlgorithm' "RSA_3072"

pattern KeyAlgorithm_RSA_4096 :: KeyAlgorithm
pattern KeyAlgorithm_RSA_4096 = KeyAlgorithm' "RSA_4096"

{-# COMPLETE
  KeyAlgorithm_EC_prime256v1,
  KeyAlgorithm_EC_secp384r1,
  KeyAlgorithm_EC_secp521r1,
  KeyAlgorithm_RSA_1024,
  KeyAlgorithm_RSA_2048,
  KeyAlgorithm_RSA_3072,
  KeyAlgorithm_RSA_4096,
  KeyAlgorithm'
  #-}
