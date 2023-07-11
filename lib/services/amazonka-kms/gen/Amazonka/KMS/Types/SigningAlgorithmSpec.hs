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
-- Module      : Amazonka.KMS.Types.SigningAlgorithmSpec
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Types.SigningAlgorithmSpec
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
        SigningAlgorithmSpec_RSASSA_PSS_SHA_512,
        SigningAlgorithmSpec_SM2DSA
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SigningAlgorithmSpec = SigningAlgorithmSpec'
  { fromSigningAlgorithmSpec ::
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

pattern SigningAlgorithmSpec_SM2DSA :: SigningAlgorithmSpec
pattern SigningAlgorithmSpec_SM2DSA = SigningAlgorithmSpec' "SM2DSA"

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
  SigningAlgorithmSpec_SM2DSA,
  SigningAlgorithmSpec'
  #-}
