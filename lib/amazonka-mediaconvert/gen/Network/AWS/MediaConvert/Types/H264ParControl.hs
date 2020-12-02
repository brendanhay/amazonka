{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264ParControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264ParControl where

import Network.AWS.Prelude

-- | Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
data H264ParControl
  = HInitializeFromSource
  | HSpecified
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

instance FromText H264ParControl where
  parser =
    takeLowerText >>= \case
      "initialize_from_source" -> pure HInitializeFromSource
      "specified" -> pure HSpecified
      e ->
        fromTextError $
          "Failure parsing H264ParControl from value: '" <> e
            <> "'. Accepted values: initialize_from_source, specified"

instance ToText H264ParControl where
  toText = \case
    HInitializeFromSource -> "INITIALIZE_FROM_SOURCE"
    HSpecified -> "SPECIFIED"

instance Hashable H264ParControl

instance NFData H264ParControl

instance ToByteString H264ParControl

instance ToQuery H264ParControl

instance ToHeader H264ParControl

instance ToJSON H264ParControl where
  toJSON = toJSONText

instance FromJSON H264ParControl where
  parseJSON = parseJSONText "H264ParControl"
