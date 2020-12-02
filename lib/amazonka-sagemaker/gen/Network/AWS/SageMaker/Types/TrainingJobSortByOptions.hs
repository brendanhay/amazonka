{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrainingJobSortByOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrainingJobSortByOptions where

import Network.AWS.Prelude

data TrainingJobSortByOptions
  = TJSBOCreationTime
  | TJSBOFinalObjectiveMetricValue
  | TJSBOName
  | TJSBOStatus
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

instance FromText TrainingJobSortByOptions where
  parser =
    takeLowerText >>= \case
      "creationtime" -> pure TJSBOCreationTime
      "finalobjectivemetricvalue" -> pure TJSBOFinalObjectiveMetricValue
      "name" -> pure TJSBOName
      "status" -> pure TJSBOStatus
      e ->
        fromTextError $
          "Failure parsing TrainingJobSortByOptions from value: '" <> e
            <> "'. Accepted values: creationtime, finalobjectivemetricvalue, name, status"

instance ToText TrainingJobSortByOptions where
  toText = \case
    TJSBOCreationTime -> "CreationTime"
    TJSBOFinalObjectiveMetricValue -> "FinalObjectiveMetricValue"
    TJSBOName -> "Name"
    TJSBOStatus -> "Status"

instance Hashable TrainingJobSortByOptions

instance NFData TrainingJobSortByOptions

instance ToByteString TrainingJobSortByOptions

instance ToQuery TrainingJobSortByOptions

instance ToHeader TrainingJobSortByOptions

instance ToJSON TrainingJobSortByOptions where
  toJSON = toJSONText
