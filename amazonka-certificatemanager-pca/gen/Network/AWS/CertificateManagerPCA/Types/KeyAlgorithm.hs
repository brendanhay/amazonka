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
-- Module      : Network.AWS.CertificateManagerPCA.Types.KeyAlgorithm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.KeyAlgorithm
  ( KeyAlgorithm
      ( ..,
        KeyAlgorithm_EC_prime256v1,
        KeyAlgorithm_EC_secp384r1,
        KeyAlgorithm_RSA_2048,
        KeyAlgorithm_RSA_4096
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype KeyAlgorithm = KeyAlgorithm'
  { fromKeyAlgorithm ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern KeyAlgorithm_EC_prime256v1 :: KeyAlgorithm
pattern KeyAlgorithm_EC_prime256v1 = KeyAlgorithm' "EC_prime256v1"

pattern KeyAlgorithm_EC_secp384r1 :: KeyAlgorithm
pattern KeyAlgorithm_EC_secp384r1 = KeyAlgorithm' "EC_secp384r1"

pattern KeyAlgorithm_RSA_2048 :: KeyAlgorithm
pattern KeyAlgorithm_RSA_2048 = KeyAlgorithm' "RSA_2048"

pattern KeyAlgorithm_RSA_4096 :: KeyAlgorithm
pattern KeyAlgorithm_RSA_4096 = KeyAlgorithm' "RSA_4096"

{-# COMPLETE
  KeyAlgorithm_EC_prime256v1,
  KeyAlgorithm_EC_secp384r1,
  KeyAlgorithm_RSA_2048,
  KeyAlgorithm_RSA_4096,
  KeyAlgorithm'
  #-}
