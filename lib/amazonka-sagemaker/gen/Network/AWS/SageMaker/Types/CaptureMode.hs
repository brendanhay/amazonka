{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CaptureMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CaptureMode where

import Network.AWS.Prelude

data CaptureMode
  = Input
  | Output
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

instance FromText CaptureMode where
  parser =
    takeLowerText >>= \case
      "input" -> pure Input
      "output" -> pure Output
      e ->
        fromTextError $
          "Failure parsing CaptureMode from value: '" <> e
            <> "'. Accepted values: input, output"

instance ToText CaptureMode where
  toText = \case
    Input -> "Input"
    Output -> "Output"

instance Hashable CaptureMode

instance NFData CaptureMode

instance ToByteString CaptureMode

instance ToQuery CaptureMode

instance ToHeader CaptureMode

instance ToJSON CaptureMode where
  toJSON = toJSONText

instance FromJSON CaptureMode where
  parseJSON = parseJSONText "CaptureMode"
