{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.InputTimecodeSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.InputTimecodeSource where

import Network.AWS.Prelude

-- | Use this Timecode source setting, located under the input settings (InputTimecodeSource), to specify how the service counts input video frames. This input frame count affects only the behavior of features that apply to a single input at a time, such as input clipping and synchronizing some captions formats. Choose Embedded (EMBEDDED) to use the timecodes in your input video. Choose Start at zero (ZEROBASED) to start the first frame at zero. Choose Specified start (SPECIFIEDSTART) to start the first frame at the timecode that you specify in the setting Start timecode (timecodeStart). If you don't specify a value for Timecode source, the service will use Embedded by default. For more information about timecodes, see https://docs.aws.amazon.com/console/mediaconvert/timecode.
data InputTimecodeSource
  = ITSEmbedded
  | ITSSpecifiedstart
  | ITSZerobased
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

instance FromText InputTimecodeSource where
  parser =
    takeLowerText >>= \case
      "embedded" -> pure ITSEmbedded
      "specifiedstart" -> pure ITSSpecifiedstart
      "zerobased" -> pure ITSZerobased
      e ->
        fromTextError $
          "Failure parsing InputTimecodeSource from value: '" <> e
            <> "'. Accepted values: embedded, specifiedstart, zerobased"

instance ToText InputTimecodeSource where
  toText = \case
    ITSEmbedded -> "EMBEDDED"
    ITSSpecifiedstart -> "SPECIFIEDSTART"
    ITSZerobased -> "ZEROBASED"

instance Hashable InputTimecodeSource

instance NFData InputTimecodeSource

instance ToByteString InputTimecodeSource

instance ToQuery InputTimecodeSource

instance ToHeader InputTimecodeSource

instance ToJSON InputTimecodeSource where
  toJSON = toJSONText

instance FromJSON InputTimecodeSource where
  parseJSON = parseJSONText "InputTimecodeSource"
