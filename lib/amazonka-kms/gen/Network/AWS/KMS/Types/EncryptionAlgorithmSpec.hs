{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.EncryptionAlgorithmSpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KMS.Types.EncryptionAlgorithmSpec
  ( EncryptionAlgorithmSpec
    ( EncryptionAlgorithmSpec'
    , EncryptionAlgorithmSpecSymmetricDefault
    , EncryptionAlgorithmSpecRsaesOaepSha1
    , EncryptionAlgorithmSpecRsaesOaepSha256
    , fromEncryptionAlgorithmSpec
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype EncryptionAlgorithmSpec = EncryptionAlgorithmSpec'{fromEncryptionAlgorithmSpec
                                                           :: Core.Text}
                                    deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                    Core.Generic)
                                    deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                      Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                      Core.FromJSON, Core.ToXML, Core.FromXML,
                                                      Core.ToText, Core.FromText, Core.ToByteString,
                                                      Core.ToQuery, Core.ToHeader)

pattern EncryptionAlgorithmSpecSymmetricDefault :: EncryptionAlgorithmSpec
pattern EncryptionAlgorithmSpecSymmetricDefault = EncryptionAlgorithmSpec' "SYMMETRIC_DEFAULT"

pattern EncryptionAlgorithmSpecRsaesOaepSha1 :: EncryptionAlgorithmSpec
pattern EncryptionAlgorithmSpecRsaesOaepSha1 = EncryptionAlgorithmSpec' "RSAES_OAEP_SHA_1"

pattern EncryptionAlgorithmSpecRsaesOaepSha256 :: EncryptionAlgorithmSpec
pattern EncryptionAlgorithmSpecRsaesOaepSha256 = EncryptionAlgorithmSpec' "RSAES_OAEP_SHA_256"

{-# COMPLETE 
  EncryptionAlgorithmSpecSymmetricDefault,

  EncryptionAlgorithmSpecRsaesOaepSha1,

  EncryptionAlgorithmSpecRsaesOaepSha256,
  EncryptionAlgorithmSpec'
  #-}
