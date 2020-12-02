{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ContributorInsightsAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ContributorInsightsAction where

import Network.AWS.Prelude

data ContributorInsightsAction
  = Disable
  | Enable
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

instance FromText ContributorInsightsAction where
  parser =
    takeLowerText >>= \case
      "disable" -> pure Disable
      "enable" -> pure Enable
      e ->
        fromTextError $
          "Failure parsing ContributorInsightsAction from value: '" <> e
            <> "'. Accepted values: disable, enable"

instance ToText ContributorInsightsAction where
  toText = \case
    Disable -> "DISABLE"
    Enable -> "ENABLE"

instance Hashable ContributorInsightsAction

instance NFData ContributorInsightsAction

instance ToByteString ContributorInsightsAction

instance ToQuery ContributorInsightsAction

instance ToHeader ContributorInsightsAction

instance ToJSON ContributorInsightsAction where
  toJSON = toJSONText
