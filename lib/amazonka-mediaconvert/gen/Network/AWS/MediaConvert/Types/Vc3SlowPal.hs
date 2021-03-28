{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Vc3SlowPal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Vc3SlowPal
  ( Vc3SlowPal
    ( Vc3SlowPal'
    , Vc3SlowPalDisabled
    , Vc3SlowPalEnabled
    , fromVc3SlowPal
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output by relabeling the video frames and resampling your audio. Note that enabling this setting will slightly reduce the duration of your video. Related settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
newtype Vc3SlowPal = Vc3SlowPal'{fromVc3SlowPal :: Core.Text}
                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                       Core.Generic)
                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                         Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                         Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                         Core.FromText, Core.ToByteString, Core.ToQuery,
                                         Core.ToHeader)

pattern Vc3SlowPalDisabled :: Vc3SlowPal
pattern Vc3SlowPalDisabled = Vc3SlowPal' "DISABLED"

pattern Vc3SlowPalEnabled :: Vc3SlowPal
pattern Vc3SlowPalEnabled = Vc3SlowPal' "ENABLED"

{-# COMPLETE 
  Vc3SlowPalDisabled,

  Vc3SlowPalEnabled,
  Vc3SlowPal'
  #-}
