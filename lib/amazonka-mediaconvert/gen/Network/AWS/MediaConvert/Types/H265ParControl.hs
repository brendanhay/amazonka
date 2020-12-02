{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265ParControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265ParControl where

import Network.AWS.Prelude

-- | Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
data H265ParControl
  = HPCInitializeFromSource
  | HPCSpecified
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

instance FromText H265ParControl where
  parser =
    takeLowerText >>= \case
      "initialize_from_source" -> pure HPCInitializeFromSource
      "specified" -> pure HPCSpecified
      e ->
        fromTextError $
          "Failure parsing H265ParControl from value: '" <> e
            <> "'. Accepted values: initialize_from_source, specified"

instance ToText H265ParControl where
  toText = \case
    HPCInitializeFromSource -> "INITIALIZE_FROM_SOURCE"
    HPCSpecified -> "SPECIFIED"

instance Hashable H265ParControl

instance NFData H265ParControl

instance ToByteString H265ParControl

instance ToQuery H265ParControl

instance ToHeader H265ParControl

instance ToJSON H265ParControl where
  toJSON = toJSONText

instance FromJSON H265ParControl where
  parseJSON = parseJSONText "H265ParControl"
