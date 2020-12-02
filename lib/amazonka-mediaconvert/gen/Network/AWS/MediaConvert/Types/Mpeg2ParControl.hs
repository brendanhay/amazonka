{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2ParControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2ParControl where

import Network.AWS.Prelude

-- | Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
data Mpeg2ParControl
  = MPCInitializeFromSource
  | MPCSpecified
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

instance FromText Mpeg2ParControl where
  parser =
    takeLowerText >>= \case
      "initialize_from_source" -> pure MPCInitializeFromSource
      "specified" -> pure MPCSpecified
      e ->
        fromTextError $
          "Failure parsing Mpeg2ParControl from value: '" <> e
            <> "'. Accepted values: initialize_from_source, specified"

instance ToText Mpeg2ParControl where
  toText = \case
    MPCInitializeFromSource -> "INITIALIZE_FROM_SOURCE"
    MPCSpecified -> "SPECIFIED"

instance Hashable Mpeg2ParControl

instance NFData Mpeg2ParControl

instance ToByteString Mpeg2ParControl

instance ToQuery Mpeg2ParControl

instance ToHeader Mpeg2ParControl

instance ToJSON Mpeg2ParControl where
  toJSON = toJSONText

instance FromJSON Mpeg2ParControl where
  parseJSON = parseJSONText "Mpeg2ParControl"
