{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.UsageStatisticType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.UsageStatisticType where

import Network.AWS.Prelude

data UsageStatisticType
  = SumByAccount
  | SumByDataSource
  | SumByResource
  | TopResources
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

instance FromText UsageStatisticType where
  parser =
    takeLowerText >>= \case
      "sum_by_account" -> pure SumByAccount
      "sum_by_data_source" -> pure SumByDataSource
      "sum_by_resource" -> pure SumByResource
      "top_resources" -> pure TopResources
      e ->
        fromTextError $
          "Failure parsing UsageStatisticType from value: '" <> e
            <> "'. Accepted values: sum_by_account, sum_by_data_source, sum_by_resource, top_resources"

instance ToText UsageStatisticType where
  toText = \case
    SumByAccount -> "SUM_BY_ACCOUNT"
    SumByDataSource -> "SUM_BY_DATA_SOURCE"
    SumByResource -> "SUM_BY_RESOURCE"
    TopResources -> "TOP_RESOURCES"

instance Hashable UsageStatisticType

instance NFData UsageStatisticType

instance ToByteString UsageStatisticType

instance ToQuery UsageStatisticType

instance ToHeader UsageStatisticType

instance ToJSON UsageStatisticType where
  toJSON = toJSONText
