{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DashIsoPlaybackDeviceCompatibility
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DashIsoPlaybackDeviceCompatibility
  ( DashIsoPlaybackDeviceCompatibility
      ( DashIsoPlaybackDeviceCompatibility',
        DashIsoPlaybackDeviceCompatibilityCencV1,
        DashIsoPlaybackDeviceCompatibilityUnencryptedSei,
        fromDashIsoPlaybackDeviceCompatibility
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | This setting can improve the compatibility of your output with video players on obsolete devices. It applies only to DASH H.264 outputs with DRM encryption. Choose Unencrypted SEI (UNENCRYPTED_SEI) only to correct problems with playback on older devices. Otherwise, keep the default setting CENC v1 (CENC_V1). If you choose Unencrypted SEI, for that output, the service will exclude the access unit delimiter and will leave the SEI NAL units unencrypted.
newtype DashIsoPlaybackDeviceCompatibility = DashIsoPlaybackDeviceCompatibility'
  { fromDashIsoPlaybackDeviceCompatibility ::
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

pattern DashIsoPlaybackDeviceCompatibilityCencV1 :: DashIsoPlaybackDeviceCompatibility
pattern DashIsoPlaybackDeviceCompatibilityCencV1 = DashIsoPlaybackDeviceCompatibility' "CENC_V1"

pattern DashIsoPlaybackDeviceCompatibilityUnencryptedSei :: DashIsoPlaybackDeviceCompatibility
pattern DashIsoPlaybackDeviceCompatibilityUnencryptedSei = DashIsoPlaybackDeviceCompatibility' "UNENCRYPTED_SEI"

{-# COMPLETE
  DashIsoPlaybackDeviceCompatibilityCencV1,
  DashIsoPlaybackDeviceCompatibilityUnencryptedSei,
  DashIsoPlaybackDeviceCompatibility'
  #-}
