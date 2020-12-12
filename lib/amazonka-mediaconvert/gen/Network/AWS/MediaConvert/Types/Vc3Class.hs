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
        Class1458BIT,
        Class22010BIT,
        Class2208BIT
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specify the VC3 class to choose the quality characteristics for this output. VC3 class, together with the settings Framerate (framerateNumerator and framerateDenominator) and Resolution (height and width), determine your output bitrate. For example, say that your video resolution is 1920x1080 and your framerate is 29.97. Then Class 145 (CLASS_145) gives you an output with a bitrate of approximately 145 Mbps and Class 220 (CLASS_220) gives you and output with a bitrate of approximately 220 Mbps. VC3 class also specifies the color bit depth of your output.
newtype Vc3Class = Vc3Class' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Class1458BIT :: Vc3Class
pattern Class1458BIT = Vc3Class' "CLASS_145_8BIT"

pattern Class22010BIT :: Vc3Class
pattern Class22010BIT = Vc3Class' "CLASS_220_10BIT"

pattern Class2208BIT :: Vc3Class
pattern Class2208BIT = Vc3Class' "CLASS_220_8BIT"

{-# COMPLETE
  Class1458BIT,
  Class22010BIT,
  Class2208BIT,
  Vc3Class'
  #-}
