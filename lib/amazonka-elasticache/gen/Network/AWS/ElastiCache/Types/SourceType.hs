{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.SourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.SourceType where

import Network.AWS.Prelude

data SourceType
  = CacheCluster
  | CacheParameterGroup
  | CacheSecurityGroup
  | CacheSubnetGroup
  | ReplicationGroup
  | User
  | UserGroup
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

instance FromText SourceType where
  parser =
    takeLowerText >>= \case
      "cache-cluster" -> pure CacheCluster
      "cache-parameter-group" -> pure CacheParameterGroup
      "cache-security-group" -> pure CacheSecurityGroup
      "cache-subnet-group" -> pure CacheSubnetGroup
      "replication-group" -> pure ReplicationGroup
      "user" -> pure User
      "user-group" -> pure UserGroup
      e ->
        fromTextError $
          "Failure parsing SourceType from value: '" <> e
            <> "'. Accepted values: cache-cluster, cache-parameter-group, cache-security-group, cache-subnet-group, replication-group, user, user-group"

instance ToText SourceType where
  toText = \case
    CacheCluster -> "cache-cluster"
    CacheParameterGroup -> "cache-parameter-group"
    CacheSecurityGroup -> "cache-security-group"
    CacheSubnetGroup -> "cache-subnet-group"
    ReplicationGroup -> "replication-group"
    User -> "user"
    UserGroup -> "user-group"

instance Hashable SourceType

instance NFData SourceType

instance ToByteString SourceType

instance ToQuery SourceType

instance ToHeader SourceType

instance FromXML SourceType where
  parseXML = parseXMLText "SourceType"
