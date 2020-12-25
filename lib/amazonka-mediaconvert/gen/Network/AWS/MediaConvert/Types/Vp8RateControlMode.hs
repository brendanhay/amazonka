{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Vp8RateControlMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Vp8RateControlMode
  ( Vp8RateControlMode
      ( Vp8RateControlMode',
        Vp8RateControlModeVbr,
        fromVp8RateControlMode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | With the VP8 codec, you can use only the variable bitrate (VBR) rate control mode.
newtype Vp8RateControlMode = Vp8RateControlMode'
  { fromVp8RateControlMode ::
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

pattern Vp8RateControlModeVbr :: Vp8RateControlMode
pattern Vp8RateControlModeVbr = Vp8RateControlMode' "VBR"

{-# COMPLETE
  Vp8RateControlModeVbr,
  Vp8RateControlMode'
  #-}
