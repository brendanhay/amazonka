{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Vp9ParControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Vp9ParControl where

import Network.AWS.Prelude

-- | Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
data Vp9ParControl
  = VPCInitializeFromSource
  | VPCSpecified
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

instance FromText Vp9ParControl where
  parser =
    takeLowerText >>= \case
      "initialize_from_source" -> pure VPCInitializeFromSource
      "specified" -> pure VPCSpecified
      e ->
        fromTextError $
          "Failure parsing Vp9ParControl from value: '" <> e
            <> "'. Accepted values: initialize_from_source, specified"

instance ToText Vp9ParControl where
  toText = \case
    VPCInitializeFromSource -> "INITIALIZE_FROM_SOURCE"
    VPCSpecified -> "SPECIFIED"

instance Hashable Vp9ParControl

instance NFData Vp9ParControl

instance ToByteString Vp9ParControl

instance ToQuery Vp9ParControl

instance ToHeader Vp9ParControl

instance ToJSON Vp9ParControl where
  toJSON = toJSONText

instance FromJSON Vp9ParControl where
  parseJSON = parseJSONText "Vp9ParControl"
