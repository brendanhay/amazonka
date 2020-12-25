{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.SigningAlgorithmSpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.SigningAlgorithmSpec
  ( SigningAlgorithmSpec
      ( SigningAlgorithmSpec',
        SigningAlgorithmSpecRsassaPssSha256,
        SigningAlgorithmSpecRsassaPssSha384,
        SigningAlgorithmSpecRsassaPssSha512,
        SigningAlgorithmSpecRsassaPKCS1V15Sha256,
        SigningAlgorithmSpecRsassaPKCS1V15Sha384,
        SigningAlgorithmSpecRsassaPKCS1V15Sha512,
        SigningAlgorithmSpecEcdsaSha256,
        SigningAlgorithmSpecEcdsaSha384,
        SigningAlgorithmSpecEcdsaSha512,
        fromSigningAlgorithmSpec
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype SigningAlgorithmSpec = SigningAlgorithmSpec'
  { fromSigningAlgorithmSpec ::
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

pattern SigningAlgorithmSpecRsassaPssSha256 :: SigningAlgorithmSpec
pattern SigningAlgorithmSpecRsassaPssSha256 = SigningAlgorithmSpec' "RSASSA_PSS_SHA_256"

pattern SigningAlgorithmSpecRsassaPssSha384 :: SigningAlgorithmSpec
pattern SigningAlgorithmSpecRsassaPssSha384 = SigningAlgorithmSpec' "RSASSA_PSS_SHA_384"

pattern SigningAlgorithmSpecRsassaPssSha512 :: SigningAlgorithmSpec
pattern SigningAlgorithmSpecRsassaPssSha512 = SigningAlgorithmSpec' "RSASSA_PSS_SHA_512"

pattern SigningAlgorithmSpecRsassaPKCS1V15Sha256 :: SigningAlgorithmSpec
pattern SigningAlgorithmSpecRsassaPKCS1V15Sha256 = SigningAlgorithmSpec' "RSASSA_PKCS1_V1_5_SHA_256"

pattern SigningAlgorithmSpecRsassaPKCS1V15Sha384 :: SigningAlgorithmSpec
pattern SigningAlgorithmSpecRsassaPKCS1V15Sha384 = SigningAlgorithmSpec' "RSASSA_PKCS1_V1_5_SHA_384"

pattern SigningAlgorithmSpecRsassaPKCS1V15Sha512 :: SigningAlgorithmSpec
pattern SigningAlgorithmSpecRsassaPKCS1V15Sha512 = SigningAlgorithmSpec' "RSASSA_PKCS1_V1_5_SHA_512"

pattern SigningAlgorithmSpecEcdsaSha256 :: SigningAlgorithmSpec
pattern SigningAlgorithmSpecEcdsaSha256 = SigningAlgorithmSpec' "ECDSA_SHA_256"

pattern SigningAlgorithmSpecEcdsaSha384 :: SigningAlgorithmSpec
pattern SigningAlgorithmSpecEcdsaSha384 = SigningAlgorithmSpec' "ECDSA_SHA_384"

pattern SigningAlgorithmSpecEcdsaSha512 :: SigningAlgorithmSpec
pattern SigningAlgorithmSpecEcdsaSha512 = SigningAlgorithmSpec' "ECDSA_SHA_512"

{-# COMPLETE
  SigningAlgorithmSpecRsassaPssSha256,
  SigningAlgorithmSpecRsassaPssSha384,
  SigningAlgorithmSpecRsassaPssSha512,
  SigningAlgorithmSpecRsassaPKCS1V15Sha256,
  SigningAlgorithmSpecRsassaPKCS1V15Sha384,
  SigningAlgorithmSpecRsassaPKCS1V15Sha512,
  SigningAlgorithmSpecEcdsaSha256,
  SigningAlgorithmSpecEcdsaSha384,
  SigningAlgorithmSpecEcdsaSha512,
  SigningAlgorithmSpec'
  #-}
