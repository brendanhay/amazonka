{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Vc3Class
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Vc3Class where

import Network.AWS.Prelude

-- | Specify the VC3 class to choose the quality characteristics for this output. VC3 class, together with the settings Framerate (framerateNumerator and framerateDenominator) and Resolution (height and width), determine your output bitrate. For example, say that your video resolution is 1920x1080 and your framerate is 29.97. Then Class 145 (CLASS_145) gives you an output with a bitrate of approximately 145 Mbps and Class 220 (CLASS_220) gives you and output with a bitrate of approximately 220 Mbps. VC3 class also specifies the color bit depth of your output.
data Vc3Class
  = Class1458BIT
  | Class22010BIT
  | Class2208BIT
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText Vc3Class where
  parser =
    takeLowerText >>= \case
      "class_145_8bit" -> pure Class1458BIT
      "class_220_10bit" -> pure Class22010BIT
      "class_220_8bit" -> pure Class2208BIT
      e ->
        fromTextError $
          "Failure parsing Vc3Class from value: '" <> e
            <> "'. Accepted values: class_145_8bit, class_220_10bit, class_220_8bit"

instance ToText Vc3Class where
  toText = \case
    Class1458BIT -> "CLASS_145_8BIT"
    Class22010BIT -> "CLASS_220_10BIT"
    Class2208BIT -> "CLASS_220_8BIT"

instance Hashable Vc3Class

instance NFData Vc3Class

instance ToByteString Vc3Class

instance ToQuery Vc3Class

instance ToHeader Vc3Class

instance ToJSON Vc3Class where
  toJSON = toJSONText

instance FromJSON Vc3Class where
  parseJSON = parseJSONText "Vc3Class"
