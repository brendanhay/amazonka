{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265TimecodeInsertionBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265TimecodeInsertionBehavior
  ( H265TimecodeInsertionBehavior
      ( H265TimecodeInsertionBehavior',
        H265TimecodeInsertionBehaviorDisabled,
        H265TimecodeInsertionBehaviorPicTimingSei,
        fromH265TimecodeInsertionBehavior
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | H265 Timecode Insertion Behavior
newtype H265TimecodeInsertionBehavior = H265TimecodeInsertionBehavior'
  { fromH265TimecodeInsertionBehavior ::
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

pattern H265TimecodeInsertionBehaviorDisabled :: H265TimecodeInsertionBehavior
pattern H265TimecodeInsertionBehaviorDisabled = H265TimecodeInsertionBehavior' "DISABLED"

pattern H265TimecodeInsertionBehaviorPicTimingSei :: H265TimecodeInsertionBehavior
pattern H265TimecodeInsertionBehaviorPicTimingSei = H265TimecodeInsertionBehavior' "PIC_TIMING_SEI"

{-# COMPLETE
  H265TimecodeInsertionBehaviorDisabled,
  H265TimecodeInsertionBehaviorPicTimingSei,
  H265TimecodeInsertionBehavior'
  #-}
