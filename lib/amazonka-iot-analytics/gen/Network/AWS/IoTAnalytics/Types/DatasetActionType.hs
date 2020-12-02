{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetActionType where

import Network.AWS.Prelude

data DatasetActionType
  = Container
  | Query
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

instance FromText DatasetActionType where
  parser =
    takeLowerText >>= \case
      "container" -> pure Container
      "query" -> pure Query
      e ->
        fromTextError $
          "Failure parsing DatasetActionType from value: '" <> e
            <> "'. Accepted values: container, query"

instance ToText DatasetActionType where
  toText = \case
    Container -> "CONTAINER"
    Query -> "QUERY"

instance Hashable DatasetActionType

instance NFData DatasetActionType

instance ToByteString DatasetActionType

instance ToQuery DatasetActionType

instance ToHeader DatasetActionType

instance FromJSON DatasetActionType where
  parseJSON = parseJSONText "DatasetActionType"
