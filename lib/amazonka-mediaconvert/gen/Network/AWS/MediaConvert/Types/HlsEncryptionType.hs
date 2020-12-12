{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsEncryptionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsEncryptionType
  ( HlsEncryptionType
      ( HlsEncryptionType',
        HETAES128,
        HETSampleAES
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Encrypts the segments with the given encryption scheme. Leave blank to disable. Selecting 'Disabled' in the web interface also disables encryption.
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

pattern HETAES128 :: HlsEncryptionType
pattern HETAES128 = HlsEncryptionType' "AES128"

pattern HETSampleAES :: HlsEncryptionType
pattern HETSampleAES = HlsEncryptionType' "SAMPLE_AES"

{-# COMPLETE
  HETAES128,
  HETSampleAES,
  HlsEncryptionType'
  #-}
