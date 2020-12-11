-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.EncryptionAlgorithmSpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.EncryptionAlgorithmSpec
  ( EncryptionAlgorithmSpec
      ( EncryptionAlgorithmSpec',
        RsaesOaepSha1,
        RsaesOaepSha256,
        SymmetricDefault
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EncryptionAlgorithmSpec = EncryptionAlgorithmSpec' Lude.Text
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

pattern RsaesOaepSha1 :: EncryptionAlgorithmSpec
pattern RsaesOaepSha1 = EncryptionAlgorithmSpec' "RSAES_OAEP_SHA_1"

pattern RsaesOaepSha256 :: EncryptionAlgorithmSpec
pattern RsaesOaepSha256 = EncryptionAlgorithmSpec' "RSAES_OAEP_SHA_256"

pattern SymmetricDefault :: EncryptionAlgorithmSpec
pattern SymmetricDefault = EncryptionAlgorithmSpec' "SYMMETRIC_DEFAULT"

{-# COMPLETE
  RsaesOaepSha1,
  RsaesOaepSha256,
  SymmetricDefault,
  EncryptionAlgorithmSpec'
  #-}
