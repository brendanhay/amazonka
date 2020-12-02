{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrainingInputMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrainingInputMode where

import Network.AWS.Prelude

data TrainingInputMode
  = File
  | Pipe
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

instance FromText TrainingInputMode where
  parser =
    takeLowerText >>= \case
      "file" -> pure File
      "pipe" -> pure Pipe
      e ->
        fromTextError $
          "Failure parsing TrainingInputMode from value: '" <> e
            <> "'. Accepted values: file, pipe"

instance ToText TrainingInputMode where
  toText = \case
    File -> "File"
    Pipe -> "Pipe"

instance Hashable TrainingInputMode

instance NFData TrainingInputMode

instance ToByteString TrainingInputMode

instance ToQuery TrainingInputMode

instance ToHeader TrainingInputMode

instance ToJSON TrainingInputMode where
  toJSON = toJSONText

instance FromJSON TrainingInputMode where
  parseJSON = parseJSONText "TrainingInputMode"
