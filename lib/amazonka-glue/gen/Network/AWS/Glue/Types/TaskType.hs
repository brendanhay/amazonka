{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TaskType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TaskType where

import Network.AWS.Prelude

data TaskType
  = TTEvaluation
  | TTExportLabels
  | TTFindMatches
  | TTImportLabels
  | TTLabelingSetGeneration
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

instance FromText TaskType where
  parser =
    takeLowerText >>= \case
      "evaluation" -> pure TTEvaluation
      "export_labels" -> pure TTExportLabels
      "find_matches" -> pure TTFindMatches
      "import_labels" -> pure TTImportLabels
      "labeling_set_generation" -> pure TTLabelingSetGeneration
      e ->
        fromTextError $
          "Failure parsing TaskType from value: '" <> e
            <> "'. Accepted values: evaluation, export_labels, find_matches, import_labels, labeling_set_generation"

instance ToText TaskType where
  toText = \case
    TTEvaluation -> "EVALUATION"
    TTExportLabels -> "EXPORT_LABELS"
    TTFindMatches -> "FIND_MATCHES"
    TTImportLabels -> "IMPORT_LABELS"
    TTLabelingSetGeneration -> "LABELING_SET_GENERATION"

instance Hashable TaskType

instance NFData TaskType

instance ToByteString TaskType

instance ToQuery TaskType

instance ToHeader TaskType

instance ToJSON TaskType where
  toJSON = toJSONText

instance FromJSON TaskType where
  parseJSON = parseJSONText "TaskType"
