{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.CacheClusterStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.CacheClusterStatus where

import Network.AWS.Prelude

-- | Returns the status of the __CacheCluster__ .
data CacheClusterStatus
  = Available
  | CreateInProgress
  | DeleteInProgress
  | FlushInProgress
  | NotAvailable
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

instance FromText CacheClusterStatus where
  parser =
    takeLowerText >>= \case
      "available" -> pure Available
      "create_in_progress" -> pure CreateInProgress
      "delete_in_progress" -> pure DeleteInProgress
      "flush_in_progress" -> pure FlushInProgress
      "not_available" -> pure NotAvailable
      e ->
        fromTextError $
          "Failure parsing CacheClusterStatus from value: '" <> e
            <> "'. Accepted values: available, create_in_progress, delete_in_progress, flush_in_progress, not_available"

instance ToText CacheClusterStatus where
  toText = \case
    Available -> "AVAILABLE"
    CreateInProgress -> "CREATE_IN_PROGRESS"
    DeleteInProgress -> "DELETE_IN_PROGRESS"
    FlushInProgress -> "FLUSH_IN_PROGRESS"
    NotAvailable -> "NOT_AVAILABLE"

instance Hashable CacheClusterStatus

instance NFData CacheClusterStatus

instance ToByteString CacheClusterStatus

instance ToQuery CacheClusterStatus

instance ToHeader CacheClusterStatus

instance FromJSON CacheClusterStatus where
  parseJSON = parseJSONText "CacheClusterStatus"
