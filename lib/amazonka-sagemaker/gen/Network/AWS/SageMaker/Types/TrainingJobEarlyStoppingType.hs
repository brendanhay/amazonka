{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrainingJobEarlyStoppingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrainingJobEarlyStoppingType where

import Network.AWS.Prelude

data TrainingJobEarlyStoppingType
  = TJESTAuto
  | TJESTOff
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

instance FromText TrainingJobEarlyStoppingType where
  parser =
    takeLowerText >>= \case
      "auto" -> pure TJESTAuto
      "off" -> pure TJESTOff
      e ->
        fromTextError $
          "Failure parsing TrainingJobEarlyStoppingType from value: '" <> e
            <> "'. Accepted values: auto, off"

instance ToText TrainingJobEarlyStoppingType where
  toText = \case
    TJESTAuto -> "Auto"
    TJESTOff -> "Off"

instance Hashable TrainingJobEarlyStoppingType

instance NFData TrainingJobEarlyStoppingType

instance ToByteString TrainingJobEarlyStoppingType

instance ToQuery TrainingJobEarlyStoppingType

instance ToHeader TrainingJobEarlyStoppingType

instance ToJSON TrainingJobEarlyStoppingType where
  toJSON = toJSONText

instance FromJSON TrainingJobEarlyStoppingType where
  parseJSON = parseJSONText "TrainingJobEarlyStoppingType"
