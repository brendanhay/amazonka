{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.AnomalyFeedbackType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.AnomalyFeedbackType where

import Network.AWS.Prelude

data AnomalyFeedbackType
  = NO
  | PlannedActivity
  | Yes
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

instance FromText AnomalyFeedbackType where
  parser =
    takeLowerText >>= \case
      "no" -> pure NO
      "planned_activity" -> pure PlannedActivity
      "yes" -> pure Yes
      e ->
        fromTextError $
          "Failure parsing AnomalyFeedbackType from value: '" <> e
            <> "'. Accepted values: no, planned_activity, yes"

instance ToText AnomalyFeedbackType where
  toText = \case
    NO -> "NO"
    PlannedActivity -> "PLANNED_ACTIVITY"
    Yes -> "YES"

instance Hashable AnomalyFeedbackType

instance NFData AnomalyFeedbackType

instance ToByteString AnomalyFeedbackType

instance ToQuery AnomalyFeedbackType

instance ToHeader AnomalyFeedbackType

instance ToJSON AnomalyFeedbackType where
  toJSON = toJSONText

instance FromJSON AnomalyFeedbackType where
  parseJSON = parseJSONText "AnomalyFeedbackType"
