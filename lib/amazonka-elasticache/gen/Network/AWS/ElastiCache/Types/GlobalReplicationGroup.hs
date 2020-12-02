{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.GlobalReplicationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.GlobalReplicationGroup where

import Network.AWS.ElastiCache.Types.GlobalNodeGroup
import Network.AWS.ElastiCache.Types.GlobalReplicationGroupMember
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Consists of a primary cluster that accepts writes and an associated secondary cluster that resides in a different AWS region. The secondary cluster accepts only reads. The primary cluster automatically replicates updates to the secondary cluster.
--
--
--     * The __GlobalReplicationGroupIdSuffix__ represents the name of the Global Datastore, which is what you use to associate a secondary cluster.
--
--
--
--
-- /See:/ 'globalReplicationGroup' smart constructor.
data GlobalReplicationGroup = GlobalReplicationGroup'
  { _grgEngineVersion ::
      !(Maybe Text),
    _grgStatus :: !(Maybe Text),
    _grgCacheNodeType :: !(Maybe Text),
    _grgClusterEnabled :: !(Maybe Bool),
    _grgAtRestEncryptionEnabled :: !(Maybe Bool),
    _grgARN :: !(Maybe Text),
    _grgTransitEncryptionEnabled :: !(Maybe Bool),
    _grgMembers ::
      !(Maybe [GlobalReplicationGroupMember]),
    _grgEngine :: !(Maybe Text),
    _grgAuthTokenEnabled :: !(Maybe Bool),
    _grgGlobalNodeGroups ::
      !(Maybe [GlobalNodeGroup]),
    _grgGlobalReplicationGroupId :: !(Maybe Text),
    _grgGlobalReplicationGroupDescription ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GlobalReplicationGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grgEngineVersion' - The Elasticache Redis engine version.
--
-- * 'grgStatus' - The status of the Global Datastore
--
-- * 'grgCacheNodeType' - The cache node type of the Global Datastore
--
-- * 'grgClusterEnabled' - A flag that indicates whether the Global Datastore is cluster enabled.
--
-- * 'grgAtRestEncryptionEnabled' - A flag that enables encryption at rest when set to @true@ . You cannot modify the value of @AtRestEncryptionEnabled@ after the replication group is created. To enable encryption at rest on a replication group you must set @AtRestEncryptionEnabled@ to @true@ when you create the replication group.  __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
--
-- * 'grgARN' - The ARN (Amazon Resource Name) of the global replication group.
--
-- * 'grgTransitEncryptionEnabled' - A flag that enables in-transit encryption when set to true. You cannot modify the value of @TransitEncryptionEnabled@ after the cluster is created. To enable in-transit encryption on a cluster you must set @TransitEncryptionEnabled@ to true when you create a cluster.
--
-- * 'grgMembers' - The replication groups that comprise the Global Datastore.
--
-- * 'grgEngine' - The Elasticache engine. For Redis only.
--
-- * 'grgAuthTokenEnabled' - A flag that enables using an @AuthToken@ (password) when issuing Redis commands. Default: @false@
--
-- * 'grgGlobalNodeGroups' - Indicates the slot configuration and global identifier for each slice group.
--
-- * 'grgGlobalReplicationGroupId' - The name of the Global Datastore
--
-- * 'grgGlobalReplicationGroupDescription' - The optional description of the Global Datastore
globalReplicationGroup ::
  GlobalReplicationGroup
globalReplicationGroup =
  GlobalReplicationGroup'
    { _grgEngineVersion = Nothing,
      _grgStatus = Nothing,
      _grgCacheNodeType = Nothing,
      _grgClusterEnabled = Nothing,
      _grgAtRestEncryptionEnabled = Nothing,
      _grgARN = Nothing,
      _grgTransitEncryptionEnabled = Nothing,
      _grgMembers = Nothing,
      _grgEngine = Nothing,
      _grgAuthTokenEnabled = Nothing,
      _grgGlobalNodeGroups = Nothing,
      _grgGlobalReplicationGroupId = Nothing,
      _grgGlobalReplicationGroupDescription = Nothing
    }

-- | The Elasticache Redis engine version.
grgEngineVersion :: Lens' GlobalReplicationGroup (Maybe Text)
grgEngineVersion = lens _grgEngineVersion (\s a -> s {_grgEngineVersion = a})

-- | The status of the Global Datastore
grgStatus :: Lens' GlobalReplicationGroup (Maybe Text)
grgStatus = lens _grgStatus (\s a -> s {_grgStatus = a})

-- | The cache node type of the Global Datastore
grgCacheNodeType :: Lens' GlobalReplicationGroup (Maybe Text)
grgCacheNodeType = lens _grgCacheNodeType (\s a -> s {_grgCacheNodeType = a})

-- | A flag that indicates whether the Global Datastore is cluster enabled.
grgClusterEnabled :: Lens' GlobalReplicationGroup (Maybe Bool)
grgClusterEnabled = lens _grgClusterEnabled (\s a -> s {_grgClusterEnabled = a})

-- | A flag that enables encryption at rest when set to @true@ . You cannot modify the value of @AtRestEncryptionEnabled@ after the replication group is created. To enable encryption at rest on a replication group you must set @AtRestEncryptionEnabled@ to @true@ when you create the replication group.  __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
grgAtRestEncryptionEnabled :: Lens' GlobalReplicationGroup (Maybe Bool)
grgAtRestEncryptionEnabled = lens _grgAtRestEncryptionEnabled (\s a -> s {_grgAtRestEncryptionEnabled = a})

-- | The ARN (Amazon Resource Name) of the global replication group.
grgARN :: Lens' GlobalReplicationGroup (Maybe Text)
grgARN = lens _grgARN (\s a -> s {_grgARN = a})

-- | A flag that enables in-transit encryption when set to true. You cannot modify the value of @TransitEncryptionEnabled@ after the cluster is created. To enable in-transit encryption on a cluster you must set @TransitEncryptionEnabled@ to true when you create a cluster.
grgTransitEncryptionEnabled :: Lens' GlobalReplicationGroup (Maybe Bool)
grgTransitEncryptionEnabled = lens _grgTransitEncryptionEnabled (\s a -> s {_grgTransitEncryptionEnabled = a})

-- | The replication groups that comprise the Global Datastore.
grgMembers :: Lens' GlobalReplicationGroup [GlobalReplicationGroupMember]
grgMembers = lens _grgMembers (\s a -> s {_grgMembers = a}) . _Default . _Coerce

-- | The Elasticache engine. For Redis only.
grgEngine :: Lens' GlobalReplicationGroup (Maybe Text)
grgEngine = lens _grgEngine (\s a -> s {_grgEngine = a})

-- | A flag that enables using an @AuthToken@ (password) when issuing Redis commands. Default: @false@
grgAuthTokenEnabled :: Lens' GlobalReplicationGroup (Maybe Bool)
grgAuthTokenEnabled = lens _grgAuthTokenEnabled (\s a -> s {_grgAuthTokenEnabled = a})

-- | Indicates the slot configuration and global identifier for each slice group.
grgGlobalNodeGroups :: Lens' GlobalReplicationGroup [GlobalNodeGroup]
grgGlobalNodeGroups = lens _grgGlobalNodeGroups (\s a -> s {_grgGlobalNodeGroups = a}) . _Default . _Coerce

-- | The name of the Global Datastore
grgGlobalReplicationGroupId :: Lens' GlobalReplicationGroup (Maybe Text)
grgGlobalReplicationGroupId = lens _grgGlobalReplicationGroupId (\s a -> s {_grgGlobalReplicationGroupId = a})

-- | The optional description of the Global Datastore
grgGlobalReplicationGroupDescription :: Lens' GlobalReplicationGroup (Maybe Text)
grgGlobalReplicationGroupDescription = lens _grgGlobalReplicationGroupDescription (\s a -> s {_grgGlobalReplicationGroupDescription = a})

instance FromXML GlobalReplicationGroup where
  parseXML x =
    GlobalReplicationGroup'
      <$> (x .@? "EngineVersion")
      <*> (x .@? "Status")
      <*> (x .@? "CacheNodeType")
      <*> (x .@? "ClusterEnabled")
      <*> (x .@? "AtRestEncryptionEnabled")
      <*> (x .@? "ARN")
      <*> (x .@? "TransitEncryptionEnabled")
      <*> ( x .@? "Members" .!@ mempty
              >>= may (parseXMLList "GlobalReplicationGroupMember")
          )
      <*> (x .@? "Engine")
      <*> (x .@? "AuthTokenEnabled")
      <*> ( x .@? "GlobalNodeGroups" .!@ mempty
              >>= may (parseXMLList "GlobalNodeGroup")
          )
      <*> (x .@? "GlobalReplicationGroupId")
      <*> (x .@? "GlobalReplicationGroupDescription")

instance Hashable GlobalReplicationGroup

instance NFData GlobalReplicationGroup
