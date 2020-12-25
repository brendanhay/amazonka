{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Vc3Class
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Vc3Class
  ( Vc3Class
      ( Vc3Class',
        Vc3ClassClass1458BIT,
        Vc3ClassClass2208BIT,
        Vc3ClassClass22010BIT,
        fromVc3Class
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Specify the VC3 class to choose the quality characteristics for this output. VC3 class, together with the settings Framerate (framerateNumerator and framerateDenominator) and Resolution (height and width), determine your output bitrate. For example, say that your video resolution is 1920x1080 and your framerate is 29.97. Then Class 145 (CLASS_145) gives you an output with a bitrate of approximately 145 Mbps and Class 220 (CLASS_220) gives you and output with a bitrate of approximately 220 Mbps. VC3 class also specifies the color bit depth of your output.
newtype Vc3Class = Vc3Class' {fromVc3Class :: Core.Text}
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

pattern Vc3ClassClass1458BIT :: Vc3Class
pattern Vc3ClassClass1458BIT = Vc3Class' "CLASS_145_8BIT"

pattern Vc3ClassClass2208BIT :: Vc3Class
pattern Vc3ClassClass2208BIT = Vc3Class' "CLASS_220_8BIT"

pattern Vc3ClassClass22010BIT :: Vc3Class
pattern Vc3ClassClass22010BIT = Vc3Class' "CLASS_220_10BIT"

{-# COMPLETE
  Vc3ClassClass1458BIT,
  Vc3ClassClass2208BIT,
  Vc3ClassClass22010BIT,
  Vc3Class'
  #-}
