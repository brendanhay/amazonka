{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MxfAfdSignaling
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MxfAfdSignaling
  ( MxfAfdSignaling
      ( MxfAfdSignaling',
        MxfAfdSignalingNoCopy,
        MxfAfdSignalingCopyFromVideo,
        fromMxfAfdSignaling
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Optional. When you have AFD signaling set up in your output video stream, use this setting to choose whether to also include it in the MXF wrapper. Choose Don't copy (NO_COPY) to exclude AFD signaling from the MXF wrapper. Choose Copy from video stream (COPY_FROM_VIDEO) to copy the AFD values from the video stream for this output to the MXF wrapper. Regardless of which option you choose, the AFD values remain in the video stream. Related settings: To set up your output to include or exclude AFD values, see AfdSignaling, under VideoDescription. On the console, find AFD signaling under the output's video encoding settings.
newtype MxfAfdSignaling = MxfAfdSignaling'
  { fromMxfAfdSignaling ::
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

pattern MxfAfdSignalingNoCopy :: MxfAfdSignaling
pattern MxfAfdSignalingNoCopy = MxfAfdSignaling' "NO_COPY"

pattern MxfAfdSignalingCopyFromVideo :: MxfAfdSignaling
pattern MxfAfdSignalingCopyFromVideo = MxfAfdSignaling' "COPY_FROM_VIDEO"

{-# COMPLETE
  MxfAfdSignalingNoCopy,
  MxfAfdSignalingCopyFromVideo,
  MxfAfdSignaling'
  #-}
