{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CandidateSortBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CandidateSortBy where

import Network.AWS.Prelude

data CandidateSortBy
  = CSBCreationTime
  | CSBFinalObjectiveMetricValue
  | CSBStatus
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

instance FromText CandidateSortBy where
  parser =
    takeLowerText >>= \case
      "creationtime" -> pure CSBCreationTime
      "finalobjectivemetricvalue" -> pure CSBFinalObjectiveMetricValue
      "status" -> pure CSBStatus
      e ->
        fromTextError $
          "Failure parsing CandidateSortBy from value: '" <> e
            <> "'. Accepted values: creationtime, finalobjectivemetricvalue, status"

instance ToText CandidateSortBy where
  toText = \case
    CSBCreationTime -> "CreationTime"
    CSBFinalObjectiveMetricValue -> "FinalObjectiveMetricValue"
    CSBStatus -> "Status"

instance Hashable CandidateSortBy

instance NFData CandidateSortBy

instance ToByteString CandidateSortBy

instance ToQuery CandidateSortBy

instance ToHeader CandidateSortBy

instance ToJSON CandidateSortBy where
  toJSON = toJSONText
