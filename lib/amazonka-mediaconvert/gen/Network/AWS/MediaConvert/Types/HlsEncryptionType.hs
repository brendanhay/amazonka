{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsEncryptionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.HlsEncryptionType
  ( HlsEncryptionType
    ( HlsEncryptionType'
    , HlsEncryptionTypeAES128
    , HlsEncryptionTypeSampleAes
    , fromHlsEncryptionType
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Encrypts the segments with the given encryption scheme. Leave blank to disable. Selecting 'Disabled' in the web interface also disables encryption.
newtype HlsEncryptionType = HlsEncryptionType'{fromHlsEncryptionType
                                               :: Core.Text}
                              deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                              Core.Generic)
                              deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                Core.FromJSON, Core.ToXML, Core.FromXML,
                                                Core.ToText, Core.FromText, Core.ToByteString,
                                                Core.ToQuery, Core.ToHeader)

pattern HlsEncryptionTypeAES128 :: HlsEncryptionType
pattern HlsEncryptionTypeAES128 = HlsEncryptionType' "AES128"

pattern HlsEncryptionTypeSampleAes :: HlsEncryptionType
pattern HlsEncryptionTypeSampleAes = HlsEncryptionType' "SAMPLE_AES"

{-# COMPLETE 
  HlsEncryptionTypeAES128,

  HlsEncryptionTypeSampleAes,
  HlsEncryptionType'
  #-}
