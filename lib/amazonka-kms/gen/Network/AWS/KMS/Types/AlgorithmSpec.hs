-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.AlgorithmSpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.AlgorithmSpec
  ( AlgorithmSpec
      ( AlgorithmSpec',
        ASRsaesOaepSha1,
        ASRsaesOaepSha256,
        ASRsaesPKCS1V15
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AlgorithmSpec = AlgorithmSpec' Lude.Text
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

pattern ASRsaesOaepSha1 :: AlgorithmSpec
pattern ASRsaesOaepSha1 = AlgorithmSpec' "RSAES_OAEP_SHA_1"

pattern ASRsaesOaepSha256 :: AlgorithmSpec
pattern ASRsaesOaepSha256 = AlgorithmSpec' "RSAES_OAEP_SHA_256"

pattern ASRsaesPKCS1V15 :: AlgorithmSpec
pattern ASRsaesPKCS1V15 = AlgorithmSpec' "RSAES_PKCS1_V1_5"

{-# COMPLETE
  ASRsaesOaepSha1,
  ASRsaesOaepSha256,
  ASRsaesPKCS1V15,
  AlgorithmSpec'
  #-}
