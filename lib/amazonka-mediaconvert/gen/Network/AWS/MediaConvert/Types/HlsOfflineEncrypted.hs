{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsOfflineEncrypted
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsOfflineEncrypted
  ( HlsOfflineEncrypted
      ( HlsOfflineEncrypted',
        HlsOfflineEncryptedEnabled,
        HlsOfflineEncryptedDisabled,
        fromHlsOfflineEncrypted
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Enable this setting to insert the EXT-X-SESSION-KEY element into the master playlist. This allows for offline Apple HLS FairPlay content protection.
newtype HlsOfflineEncrypted = HlsOfflineEncrypted'
  { fromHlsOfflineEncrypted ::
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

pattern HlsOfflineEncryptedEnabled :: HlsOfflineEncrypted
pattern HlsOfflineEncryptedEnabled = HlsOfflineEncrypted' "ENABLED"

pattern HlsOfflineEncryptedDisabled :: HlsOfflineEncrypted
pattern HlsOfflineEncryptedDisabled = HlsOfflineEncrypted' "DISABLED"

{-# COMPLETE
  HlsOfflineEncryptedEnabled,
  HlsOfflineEncryptedDisabled,
  HlsOfflineEncrypted'
  #-}
