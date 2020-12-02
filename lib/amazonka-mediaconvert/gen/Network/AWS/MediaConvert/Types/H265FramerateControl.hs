{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265FramerateControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265FramerateControl where

import Network.AWS.Prelude

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
data H265FramerateControl
  = HFCInitializeFromSource
  | HFCSpecified
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

instance FromText H265FramerateControl where
  parser =
    takeLowerText >>= \case
      "initialize_from_source" -> pure HFCInitializeFromSource
      "specified" -> pure HFCSpecified
      e ->
        fromTextError $
          "Failure parsing H265FramerateControl from value: '" <> e
            <> "'. Accepted values: initialize_from_source, specified"

instance ToText H265FramerateControl where
  toText = \case
    HFCInitializeFromSource -> "INITIALIZE_FROM_SOURCE"
    HFCSpecified -> "SPECIFIED"

instance Hashable H265FramerateControl

instance NFData H265FramerateControl

instance ToByteString H265FramerateControl

instance ToQuery H265FramerateControl

instance ToHeader H265FramerateControl

instance ToJSON H265FramerateControl where
  toJSON = toJSONText

instance FromJSON H265FramerateControl where
  parseJSON = parseJSONText "H265FramerateControl"
