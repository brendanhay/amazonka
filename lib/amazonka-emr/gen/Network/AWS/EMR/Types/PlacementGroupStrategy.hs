{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.PlacementGroupStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.PlacementGroupStrategy where

import Network.AWS.Prelude

data PlacementGroupStrategy
  = PGSCluster
  | PGSNone
  | PGSPartition
  | PGSSpread
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

instance FromText PlacementGroupStrategy where
  parser =
    takeLowerText >>= \case
      "cluster" -> pure PGSCluster
      "none" -> pure PGSNone
      "partition" -> pure PGSPartition
      "spread" -> pure PGSSpread
      e ->
        fromTextError $
          "Failure parsing PlacementGroupStrategy from value: '" <> e
            <> "'. Accepted values: cluster, none, partition, spread"

instance ToText PlacementGroupStrategy where
  toText = \case
    PGSCluster -> "CLUSTER"
    PGSNone -> "NONE"
    PGSPartition -> "PARTITION"
    PGSSpread -> "SPREAD"

instance Hashable PlacementGroupStrategy

instance NFData PlacementGroupStrategy

instance ToByteString PlacementGroupStrategy

instance ToQuery PlacementGroupStrategy

instance ToHeader PlacementGroupStrategy

instance ToJSON PlacementGroupStrategy where
  toJSON = toJSONText

instance FromJSON PlacementGroupStrategy where
  parseJSON = parseJSONText "PlacementGroupStrategy"
