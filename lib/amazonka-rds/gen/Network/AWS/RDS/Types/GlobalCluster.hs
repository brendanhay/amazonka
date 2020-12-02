{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.GlobalCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.GlobalCluster where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.GlobalClusterMember

-- | A data type representing an Aurora global database.
--
--
--
-- /See:/ 'globalCluster' smart constructor.
data GlobalCluster = GlobalCluster'
  { _gcEngineVersion ::
      !(Maybe Text),
    _gcStatus :: !(Maybe Text),
    _gcDeletionProtection :: !(Maybe Bool),
    _gcStorageEncrypted :: !(Maybe Bool),
    _gcGlobalClusterIdentifier :: !(Maybe Text),
    _gcEngine :: !(Maybe Text),
    _gcGlobalClusterARN :: !(Maybe Text),
    _gcDatabaseName :: !(Maybe Text),
    _gcGlobalClusterMembers :: !(Maybe [GlobalClusterMember]),
    _gcGlobalClusterResourceId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GlobalCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcEngineVersion' - Indicates the database engine version.
--
-- * 'gcStatus' - Specifies the current state of this global database cluster.
--
-- * 'gcDeletionProtection' - The deletion protection setting for the new global database cluster.
--
-- * 'gcStorageEncrypted' - The storage encryption setting for the global database cluster.
--
-- * 'gcGlobalClusterIdentifier' - Contains a user-supplied global database cluster identifier. This identifier is the unique key that identifies a global database cluster.
--
-- * 'gcEngine' - The Aurora database engine used by the global database cluster.
--
-- * 'gcGlobalClusterARN' - The Amazon Resource Name (ARN) for the global database cluster.
--
-- * 'gcDatabaseName' - The default database name within the new global database cluster.
--
-- * 'gcGlobalClusterMembers' - The list of cluster IDs for secondary clusters within the global database cluster. Currently limited to 1 item.
--
-- * 'gcGlobalClusterResourceId' - The AWS Region-unique, immutable identifier for the global database cluster. This identifier is found in AWS CloudTrail log entries whenever the AWS KMS key for the DB cluster is accessed.
globalCluster ::
  GlobalCluster
globalCluster =
  GlobalCluster'
    { _gcEngineVersion = Nothing,
      _gcStatus = Nothing,
      _gcDeletionProtection = Nothing,
      _gcStorageEncrypted = Nothing,
      _gcGlobalClusterIdentifier = Nothing,
      _gcEngine = Nothing,
      _gcGlobalClusterARN = Nothing,
      _gcDatabaseName = Nothing,
      _gcGlobalClusterMembers = Nothing,
      _gcGlobalClusterResourceId = Nothing
    }

-- | Indicates the database engine version.
gcEngineVersion :: Lens' GlobalCluster (Maybe Text)
gcEngineVersion = lens _gcEngineVersion (\s a -> s {_gcEngineVersion = a})

-- | Specifies the current state of this global database cluster.
gcStatus :: Lens' GlobalCluster (Maybe Text)
gcStatus = lens _gcStatus (\s a -> s {_gcStatus = a})

-- | The deletion protection setting for the new global database cluster.
gcDeletionProtection :: Lens' GlobalCluster (Maybe Bool)
gcDeletionProtection = lens _gcDeletionProtection (\s a -> s {_gcDeletionProtection = a})

-- | The storage encryption setting for the global database cluster.
gcStorageEncrypted :: Lens' GlobalCluster (Maybe Bool)
gcStorageEncrypted = lens _gcStorageEncrypted (\s a -> s {_gcStorageEncrypted = a})

-- | Contains a user-supplied global database cluster identifier. This identifier is the unique key that identifies a global database cluster.
gcGlobalClusterIdentifier :: Lens' GlobalCluster (Maybe Text)
gcGlobalClusterIdentifier = lens _gcGlobalClusterIdentifier (\s a -> s {_gcGlobalClusterIdentifier = a})

-- | The Aurora database engine used by the global database cluster.
gcEngine :: Lens' GlobalCluster (Maybe Text)
gcEngine = lens _gcEngine (\s a -> s {_gcEngine = a})

-- | The Amazon Resource Name (ARN) for the global database cluster.
gcGlobalClusterARN :: Lens' GlobalCluster (Maybe Text)
gcGlobalClusterARN = lens _gcGlobalClusterARN (\s a -> s {_gcGlobalClusterARN = a})

-- | The default database name within the new global database cluster.
gcDatabaseName :: Lens' GlobalCluster (Maybe Text)
gcDatabaseName = lens _gcDatabaseName (\s a -> s {_gcDatabaseName = a})

-- | The list of cluster IDs for secondary clusters within the global database cluster. Currently limited to 1 item.
gcGlobalClusterMembers :: Lens' GlobalCluster [GlobalClusterMember]
gcGlobalClusterMembers = lens _gcGlobalClusterMembers (\s a -> s {_gcGlobalClusterMembers = a}) . _Default . _Coerce

-- | The AWS Region-unique, immutable identifier for the global database cluster. This identifier is found in AWS CloudTrail log entries whenever the AWS KMS key for the DB cluster is accessed.
gcGlobalClusterResourceId :: Lens' GlobalCluster (Maybe Text)
gcGlobalClusterResourceId = lens _gcGlobalClusterResourceId (\s a -> s {_gcGlobalClusterResourceId = a})

instance FromXML GlobalCluster where
  parseXML x =
    GlobalCluster'
      <$> (x .@? "EngineVersion")
      <*> (x .@? "Status")
      <*> (x .@? "DeletionProtection")
      <*> (x .@? "StorageEncrypted")
      <*> (x .@? "GlobalClusterIdentifier")
      <*> (x .@? "Engine")
      <*> (x .@? "GlobalClusterArn")
      <*> (x .@? "DatabaseName")
      <*> ( x .@? "GlobalClusterMembers" .!@ mempty
              >>= may (parseXMLList "GlobalClusterMember")
          )
      <*> (x .@? "GlobalClusterResourceId")

instance Hashable GlobalCluster

instance NFData GlobalCluster
