{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MotionImageInsertionMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MotionImageInsertionMode
  ( MotionImageInsertionMode
      ( MotionImageInsertionMode',
        MotionImageInsertionModeMov,
        MotionImageInsertionModePng,
        fromMotionImageInsertionMode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Choose the type of motion graphic asset that you are providing for your overlay. You can choose either a .mov file or a series of .png files.
newtype MotionImageInsertionMode = MotionImageInsertionMode'
  { fromMotionImageInsertionMode ::
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

pattern MotionImageInsertionModeMov :: MotionImageInsertionMode
pattern MotionImageInsertionModeMov = MotionImageInsertionMode' "MOV"

pattern MotionImageInsertionModePng :: MotionImageInsertionMode
pattern MotionImageInsertionModePng = MotionImageInsertionMode' "PNG"

{-# COMPLETE
  MotionImageInsertionModeMov,
  MotionImageInsertionModePng,
  MotionImageInsertionMode'
  #-}
