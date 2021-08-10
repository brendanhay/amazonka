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
-- Module      : Network.AWS.KMS.Types.SigningAlgorithmSpec
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.SigningAlgorithmSpec
  ( SigningAlgorithmSpec
      ( ..,
        SigningAlgorithmSpec_ECDSA_SHA_256,
        SigningAlgorithmSpec_ECDSA_SHA_384,
        SigningAlgorithmSpec_ECDSA_SHA_512,
        SigningAlgorithmSpec_RSASSA_PKCS1_V1_5_SHA_256,
        SigningAlgorithmSpec_RSASSA_PKCS1_V1_5_SHA_384,
        SigningAlgorithmSpec_RSASSA_PKCS1_V1_5_SHA_512,
        SigningAlgorithmSpec_RSASSA_PSS_SHA_256,
        SigningAlgorithmSpec_RSASSA_PSS_SHA_384,
        SigningAlgorithmSpec_RSASSA_PSS_SHA_512
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype SigningAlgorithmSpec = SigningAlgorithmSpec'
  { fromSigningAlgorithmSpec ::
      Core.Text
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

pattern SigningAlgorithmSpec_ECDSA_SHA_256 :: SigningAlgorithmSpec
pattern SigningAlgorithmSpec_ECDSA_SHA_256 = SigningAlgorithmSpec' "ECDSA_SHA_256"

pattern SigningAlgorithmSpec_ECDSA_SHA_384 :: SigningAlgorithmSpec
pattern SigningAlgorithmSpec_ECDSA_SHA_384 = SigningAlgorithmSpec' "ECDSA_SHA_384"

pattern SigningAlgorithmSpec_ECDSA_SHA_512 :: SigningAlgorithmSpec
pattern SigningAlgorithmSpec_ECDSA_SHA_512 = SigningAlgorithmSpec' "ECDSA_SHA_512"

pattern SigningAlgorithmSpec_RSASSA_PKCS1_V1_5_SHA_256 :: SigningAlgorithmSpec
pattern SigningAlgorithmSpec_RSASSA_PKCS1_V1_5_SHA_256 = SigningAlgorithmSpec' "RSASSA_PKCS1_V1_5_SHA_256"

pattern SigningAlgorithmSpec_RSASSA_PKCS1_V1_5_SHA_384 :: SigningAlgorithmSpec
pattern SigningAlgorithmSpec_RSASSA_PKCS1_V1_5_SHA_384 = SigningAlgorithmSpec' "RSASSA_PKCS1_V1_5_SHA_384"

pattern SigningAlgorithmSpec_RSASSA_PKCS1_V1_5_SHA_512 :: SigningAlgorithmSpec
pattern SigningAlgorithmSpec_RSASSA_PKCS1_V1_5_SHA_512 = SigningAlgorithmSpec' "RSASSA_PKCS1_V1_5_SHA_512"

pattern SigningAlgorithmSpec_RSASSA_PSS_SHA_256 :: SigningAlgorithmSpec
pattern SigningAlgorithmSpec_RSASSA_PSS_SHA_256 = SigningAlgorithmSpec' "RSASSA_PSS_SHA_256"

pattern SigningAlgorithmSpec_RSASSA_PSS_SHA_384 :: SigningAlgorithmSpec
pattern SigningAlgorithmSpec_RSASSA_PSS_SHA_384 = SigningAlgorithmSpec' "RSASSA_PSS_SHA_384"

pattern SigningAlgorithmSpec_RSASSA_PSS_SHA_512 :: SigningAlgorithmSpec
pattern SigningAlgorithmSpec_RSASSA_PSS_SHA_512 = SigningAlgorithmSpec' "RSASSA_PSS_SHA_512"

{-# COMPLETE
  SigningAlgorithmSpec_ECDSA_SHA_256,
  SigningAlgorithmSpec_ECDSA_SHA_384,
  SigningAlgorithmSpec_ECDSA_SHA_512,
  SigningAlgorithmSpec_RSASSA_PKCS1_V1_5_SHA_256,
  SigningAlgorithmSpec_RSASSA_PKCS1_V1_5_SHA_384,
  SigningAlgorithmSpec_RSASSA_PKCS1_V1_5_SHA_512,
  SigningAlgorithmSpec_RSASSA_PSS_SHA_256,
  SigningAlgorithmSpec_RSASSA_PSS_SHA_384,
  SigningAlgorithmSpec_RSASSA_PSS_SHA_512,
  SigningAlgorithmSpec'
  #-}
