{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.HLSPlaybackMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.HLSPlaybackMode
  ( HLSPlaybackMode
      ( HLSPlaybackMode',
        HLSPlaybackModeLive,
        HLSPlaybackModeLiveReplay,
        HLSPlaybackModeOnDemand,
        fromHLSPlaybackMode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype HLSPlaybackMode = HLSPlaybackMode'
  { fromHLSPlaybackMode ::
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

pattern HLSPlaybackModeLive :: HLSPlaybackMode
pattern HLSPlaybackModeLive = HLSPlaybackMode' "LIVE"

pattern HLSPlaybackModeLiveReplay :: HLSPlaybackMode
pattern HLSPlaybackModeLiveReplay = HLSPlaybackMode' "LIVE_REPLAY"

pattern HLSPlaybackModeOnDemand :: HLSPlaybackMode
pattern HLSPlaybackModeOnDemand = HLSPlaybackMode' "ON_DEMAND"

{-# COMPLETE
  HLSPlaybackModeLive,
  HLSPlaybackModeLiveReplay,
  HLSPlaybackModeOnDemand,
  HLSPlaybackMode'
  #-}
