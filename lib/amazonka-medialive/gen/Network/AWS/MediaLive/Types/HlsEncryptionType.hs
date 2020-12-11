-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsEncryptionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsEncryptionType
  ( HlsEncryptionType
      ( HlsEncryptionType',
        AES128,
        SampleAES
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Hls Encryption Type
newtype HlsEncryptionType = HlsEncryptionType' Lude.Text
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

pattern AES128 :: HlsEncryptionType
pattern AES128 = HlsEncryptionType' "AES128"

pattern SampleAES :: HlsEncryptionType
pattern SampleAES = HlsEncryptionType' "SAMPLE_AES"

{-# COMPLETE
  AES128,
  SampleAES,
  HlsEncryptionType'
  #-}
