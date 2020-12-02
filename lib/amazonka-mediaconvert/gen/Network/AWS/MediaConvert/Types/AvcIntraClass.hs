{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AvcIntraClass
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AvcIntraClass where

import Network.AWS.Prelude

-- | Specify the AVC-Intra class of your output. The AVC-Intra class selection determines the output video bit rate depending on the frame rate of the output. Outputs with higher class values have higher bitrates and improved image quality.
data AvcIntraClass
  = Class100
  | Class200
  | Class50
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

instance FromText AvcIntraClass where
  parser =
    takeLowerText >>= \case
      "class_100" -> pure Class100
      "class_200" -> pure Class200
      "class_50" -> pure Class50
      e ->
        fromTextError $
          "Failure parsing AvcIntraClass from value: '" <> e
            <> "'. Accepted values: class_100, class_200, class_50"

instance ToText AvcIntraClass where
  toText = \case
    Class100 -> "CLASS_100"
    Class200 -> "CLASS_200"
    Class50 -> "CLASS_50"

instance Hashable AvcIntraClass

instance NFData AvcIntraClass

instance ToByteString AvcIntraClass

instance ToQuery AvcIntraClass

instance ToHeader AvcIntraClass

instance ToJSON AvcIntraClass where
  toJSON = toJSONText

instance FromJSON AvcIntraClass where
  parseJSON = parseJSONText "AvcIntraClass"
