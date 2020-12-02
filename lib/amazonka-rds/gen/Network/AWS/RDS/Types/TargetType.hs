{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.TargetType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.TargetType where

import Network.AWS.Prelude

data TargetType
  = RDSInstance
  | RDSServerlessEndpoint
  | TrackedCluster
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

instance FromText TargetType where
  parser =
    takeLowerText >>= \case
      "rds_instance" -> pure RDSInstance
      "rds_serverless_endpoint" -> pure RDSServerlessEndpoint
      "tracked_cluster" -> pure TrackedCluster
      e ->
        fromTextError $
          "Failure parsing TargetType from value: '" <> e
            <> "'. Accepted values: rds_instance, rds_serverless_endpoint, tracked_cluster"

instance ToText TargetType where
  toText = \case
    RDSInstance -> "RDS_INSTANCE"
    RDSServerlessEndpoint -> "RDS_SERVERLESS_ENDPOINT"
    TrackedCluster -> "TRACKED_CLUSTER"

instance Hashable TargetType

instance NFData TargetType

instance ToByteString TargetType

instance ToQuery TargetType

instance ToHeader TargetType

instance FromXML TargetType where
  parseXML = parseXMLText "TargetType"
