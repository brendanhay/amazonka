{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Vc3Class
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Vc3Class
  ( Vc3Class
      ( ..,
        Vc3Class_CLASS_145_8BIT,
        Vc3Class_CLASS_220_10BIT,
        Vc3Class_CLASS_220_8BIT
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Specify the VC3 class to choose the quality characteristics for this
-- output. VC3 class, together with the settings Framerate
-- (framerateNumerator and framerateDenominator) and Resolution (height and
-- width), determine your output bitrate. For example, say that your video
-- resolution is 1920x1080 and your framerate is 29.97. Then Class 145
-- (CLASS_145) gives you an output with a bitrate of approximately 145 Mbps
-- and Class 220 (CLASS_220) gives you and output with a bitrate of
-- approximately 220 Mbps. VC3 class also specifies the color bit depth of
-- your output.
newtype Vc3Class = Vc3Class'
  { fromVc3Class ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern Vc3Class_CLASS_145_8BIT :: Vc3Class
pattern Vc3Class_CLASS_145_8BIT = Vc3Class' "CLASS_145_8BIT"

pattern Vc3Class_CLASS_220_10BIT :: Vc3Class
pattern Vc3Class_CLASS_220_10BIT = Vc3Class' "CLASS_220_10BIT"

pattern Vc3Class_CLASS_220_8BIT :: Vc3Class
pattern Vc3Class_CLASS_220_8BIT = Vc3Class' "CLASS_220_8BIT"

{-# COMPLETE
  Vc3Class_CLASS_145_8BIT,
  Vc3Class_CLASS_220_10BIT,
  Vc3Class_CLASS_220_8BIT,
  Vc3Class'
  #-}
