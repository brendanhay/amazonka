{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DropFrameTimecode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DropFrameTimecode
  ( DropFrameTimecode
      ( DropFrameTimecode',
        DropFrameTimecodeDisabled,
        DropFrameTimecodeEnabled,
        fromDropFrameTimecode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Applies only to 29.97 fps outputs. When this feature is enabled, the service will use drop-frame timecode on outputs. If it is not possible to use drop-frame timecode, the system will fall back to non-drop-frame. This setting is enabled by default when Timecode insertion (TimecodeInsertion) is enabled.
newtype DropFrameTimecode = DropFrameTimecode'
  { fromDropFrameTimecode ::
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

pattern DropFrameTimecodeDisabled :: DropFrameTimecode
pattern DropFrameTimecodeDisabled = DropFrameTimecode' "DISABLED"

pattern DropFrameTimecodeEnabled :: DropFrameTimecode
pattern DropFrameTimecodeEnabled = DropFrameTimecode' "ENABLED"

{-# COMPLETE
  DropFrameTimecodeDisabled,
  DropFrameTimecodeEnabled,
  DropFrameTimecode'
  #-}
