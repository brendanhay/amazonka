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
        RsassaPssSha256,
        RsassaPssSha384,
        RsassaPssSha512,
        RsassaPKCS1V15Sha256,
        RsassaPKCS1V15Sha384,
        RsassaPKCS1V15Sha512,
        EcdsaSha256,
        EcdsaSha384,
        EcdsaSha512
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype SigningAlgorithmSpec = SigningAlgorithmSpec' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern RsassaPssSha256 :: SigningAlgorithmSpec
pattern RsassaPssSha256 = SigningAlgorithmSpec' "RSASSA_PSS_SHA_256"

pattern RsassaPssSha384 :: SigningAlgorithmSpec
pattern RsassaPssSha384 = SigningAlgorithmSpec' "RSASSA_PSS_SHA_384"

pattern RsassaPssSha512 :: SigningAlgorithmSpec
pattern RsassaPssSha512 = SigningAlgorithmSpec' "RSASSA_PSS_SHA_512"

pattern RsassaPKCS1V15Sha256 :: SigningAlgorithmSpec
pattern RsassaPKCS1V15Sha256 = SigningAlgorithmSpec' "RSASSA_PKCS1_V1_5_SHA_256"

pattern RsassaPKCS1V15Sha384 :: SigningAlgorithmSpec
pattern RsassaPKCS1V15Sha384 = SigningAlgorithmSpec' "RSASSA_PKCS1_V1_5_SHA_384"

pattern RsassaPKCS1V15Sha512 :: SigningAlgorithmSpec
pattern RsassaPKCS1V15Sha512 = SigningAlgorithmSpec' "RSASSA_PKCS1_V1_5_SHA_512"

pattern EcdsaSha256 :: SigningAlgorithmSpec
pattern EcdsaSha256 = SigningAlgorithmSpec' "ECDSA_SHA_256"

pattern EcdsaSha384 :: SigningAlgorithmSpec
pattern EcdsaSha384 = SigningAlgorithmSpec' "ECDSA_SHA_384"

pattern EcdsaSha512 :: SigningAlgorithmSpec
pattern EcdsaSha512 = SigningAlgorithmSpec' "ECDSA_SHA_512"

{-# COMPLETE
  RsassaPssSha256,
  RsassaPssSha384,
  RsassaPssSha512,
  RsassaPKCS1V15Sha256,
  RsassaPKCS1V15Sha384,
  RsassaPKCS1V15Sha512,
  EcdsaSha256,
  EcdsaSha384,
  EcdsaSha512,
  SigningAlgorithmSpec'
  #-}
