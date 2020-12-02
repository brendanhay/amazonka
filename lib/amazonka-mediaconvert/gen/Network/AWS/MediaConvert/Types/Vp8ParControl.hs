{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Vp8ParControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Vp8ParControl where

import Network.AWS.Prelude

-- | Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
data Vp8ParControl
  = VInitializeFromSource
  | VSpecified
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

instance FromText Vp8ParControl where
  parser =
    takeLowerText >>= \case
      "initialize_from_source" -> pure VInitializeFromSource
      "specified" -> pure VSpecified
      e ->
        fromTextError $
          "Failure parsing Vp8ParControl from value: '" <> e
            <> "'. Accepted values: initialize_from_source, specified"

instance ToText Vp8ParControl where
  toText = \case
    VInitializeFromSource -> "INITIALIZE_FROM_SOURCE"
    VSpecified -> "SPECIFIED"

instance Hashable Vp8ParControl

instance NFData Vp8ParControl

instance ToByteString Vp8ParControl

instance ToQuery Vp8ParControl

instance ToHeader Vp8ParControl

instance ToJSON Vp8ParControl where
  toJSON = toJSONText

instance FromJSON Vp8ParControl where
  parseJSON = parseJSONText "Vp8ParControl"
