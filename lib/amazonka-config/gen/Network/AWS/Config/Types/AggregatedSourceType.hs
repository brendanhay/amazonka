{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.AggregatedSourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.AggregatedSourceType where

import Network.AWS.Prelude

data AggregatedSourceType
  = Account
  | Organization
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

instance FromText AggregatedSourceType where
  parser =
    takeLowerText >>= \case
      "account" -> pure Account
      "organization" -> pure Organization
      e ->
        fromTextError $
          "Failure parsing AggregatedSourceType from value: '" <> e
            <> "'. Accepted values: account, organization"

instance ToText AggregatedSourceType where
  toText = \case
    Account -> "ACCOUNT"
    Organization -> "ORGANIZATION"

instance Hashable AggregatedSourceType

instance NFData AggregatedSourceType

instance ToByteString AggregatedSourceType

instance ToQuery AggregatedSourceType

instance ToHeader AggregatedSourceType

instance FromJSON AggregatedSourceType where
  parseJSON = parseJSONText "AggregatedSourceType"
