{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264TimecodeInsertionBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264TimecodeInsertionBehavior
  ( H264TimecodeInsertionBehavior
      ( H264TimecodeInsertionBehavior',
        H264TimecodeInsertionBehaviorDisabled,
        H264TimecodeInsertionBehaviorPicTimingSei,
        fromH264TimecodeInsertionBehavior
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | H264 Timecode Insertion Behavior
newtype H264TimecodeInsertionBehavior = H264TimecodeInsertionBehavior'
  { fromH264TimecodeInsertionBehavior ::
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

pattern H264TimecodeInsertionBehaviorDisabled :: H264TimecodeInsertionBehavior
pattern H264TimecodeInsertionBehaviorDisabled = H264TimecodeInsertionBehavior' "DISABLED"

pattern H264TimecodeInsertionBehaviorPicTimingSei :: H264TimecodeInsertionBehavior
pattern H264TimecodeInsertionBehaviorPicTimingSei = H264TimecodeInsertionBehavior' "PIC_TIMING_SEI"

{-# COMPLETE
  H264TimecodeInsertionBehaviorDisabled,
  H264TimecodeInsertionBehaviorPicTimingSei,
  H264TimecodeInsertionBehavior'
  #-}
