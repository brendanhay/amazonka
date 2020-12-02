{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.AggregatedSourceStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.AggregatedSourceStatusType where

import Network.AWS.Prelude

data AggregatedSourceStatusType
  = ASSTFailed
  | ASSTOutdated
  | ASSTSucceeded
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

instance FromText AggregatedSourceStatusType where
  parser =
    takeLowerText >>= \case
      "failed" -> pure ASSTFailed
      "outdated" -> pure ASSTOutdated
      "succeeded" -> pure ASSTSucceeded
      e ->
        fromTextError $
          "Failure parsing AggregatedSourceStatusType from value: '" <> e
            <> "'. Accepted values: failed, outdated, succeeded"

instance ToText AggregatedSourceStatusType where
  toText = \case
    ASSTFailed -> "FAILED"
    ASSTOutdated -> "OUTDATED"
    ASSTSucceeded -> "SUCCEEDED"

instance Hashable AggregatedSourceStatusType

instance NFData AggregatedSourceStatusType

instance ToByteString AggregatedSourceStatusType

instance ToQuery AggregatedSourceStatusType

instance ToHeader AggregatedSourceStatusType

instance ToJSON AggregatedSourceStatusType where
  toJSON = toJSONText

instance FromJSON AggregatedSourceStatusType where
  parseJSON = parseJSONText "AggregatedSourceStatusType"
