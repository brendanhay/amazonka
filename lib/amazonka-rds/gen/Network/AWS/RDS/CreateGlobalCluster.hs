{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateGlobalCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Aurora global database spread across multiple AWS Regions. The global database contains a single primary cluster with read-write capability, and a read-only secondary cluster that receives data from the primary cluster through high-speed replication performed by the Aurora storage subsystem.
--
--
-- You can create a global database that is initially empty, and then add a primary cluster and a secondary cluster to it. Or you can specify an existing Aurora cluster during the create operation, and this cluster becomes the primary cluster of the global database.
module Network.AWS.RDS.CreateGlobalCluster
  ( -- * Creating a Request
    createGlobalCluster,
    CreateGlobalCluster,

    -- * Request Lenses
    cgcEngineVersion,
    cgcDeletionProtection,
    cgcStorageEncrypted,
    cgcSourceDBClusterIdentifier,
    cgcGlobalClusterIdentifier,
    cgcEngine,
    cgcDatabaseName,

    -- * Destructuring the Response
    createGlobalClusterResponse,
    CreateGlobalClusterResponse,

    -- * Response Lenses
    cgcrsGlobalCluster,
    cgcrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createGlobalCluster' smart constructor.
data CreateGlobalCluster = CreateGlobalCluster'
  { _cgcEngineVersion ::
      !(Maybe Text),
    _cgcDeletionProtection :: !(Maybe Bool),
    _cgcStorageEncrypted :: !(Maybe Bool),
    _cgcSourceDBClusterIdentifier :: !(Maybe Text),
    _cgcGlobalClusterIdentifier :: !(Maybe Text),
    _cgcEngine :: !(Maybe Text),
    _cgcDatabaseName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateGlobalCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgcEngineVersion' - The engine version of the Aurora global database.
--
-- * 'cgcDeletionProtection' - The deletion protection setting for the new global database. The global database can't be deleted when deletion protection is enabled.
--
-- * 'cgcStorageEncrypted' - The storage encryption setting for the new global database cluster.
--
-- * 'cgcSourceDBClusterIdentifier' - The Amazon Resource Name (ARN) to use as the primary cluster of the global database. This parameter is optional.
--
-- * 'cgcGlobalClusterIdentifier' - The cluster identifier of the new global database cluster.
--
-- * 'cgcEngine' - The name of the database engine to be used for this DB cluster.
--
-- * 'cgcDatabaseName' - The name for your database of up to 64 alpha-numeric characters. If you do not provide a name, Amazon Aurora will not create a database in the global database cluster you are creating.
createGlobalCluster ::
  CreateGlobalCluster
createGlobalCluster =
  CreateGlobalCluster'
    { _cgcEngineVersion = Nothing,
      _cgcDeletionProtection = Nothing,
      _cgcStorageEncrypted = Nothing,
      _cgcSourceDBClusterIdentifier = Nothing,
      _cgcGlobalClusterIdentifier = Nothing,
      _cgcEngine = Nothing,
      _cgcDatabaseName = Nothing
    }

-- | The engine version of the Aurora global database.
cgcEngineVersion :: Lens' CreateGlobalCluster (Maybe Text)
cgcEngineVersion = lens _cgcEngineVersion (\s a -> s {_cgcEngineVersion = a})

-- | The deletion protection setting for the new global database. The global database can't be deleted when deletion protection is enabled.
cgcDeletionProtection :: Lens' CreateGlobalCluster (Maybe Bool)
cgcDeletionProtection = lens _cgcDeletionProtection (\s a -> s {_cgcDeletionProtection = a})

-- | The storage encryption setting for the new global database cluster.
cgcStorageEncrypted :: Lens' CreateGlobalCluster (Maybe Bool)
cgcStorageEncrypted = lens _cgcStorageEncrypted (\s a -> s {_cgcStorageEncrypted = a})

-- | The Amazon Resource Name (ARN) to use as the primary cluster of the global database. This parameter is optional.
cgcSourceDBClusterIdentifier :: Lens' CreateGlobalCluster (Maybe Text)
cgcSourceDBClusterIdentifier = lens _cgcSourceDBClusterIdentifier (\s a -> s {_cgcSourceDBClusterIdentifier = a})

-- | The cluster identifier of the new global database cluster.
cgcGlobalClusterIdentifier :: Lens' CreateGlobalCluster (Maybe Text)
cgcGlobalClusterIdentifier = lens _cgcGlobalClusterIdentifier (\s a -> s {_cgcGlobalClusterIdentifier = a})

-- | The name of the database engine to be used for this DB cluster.
cgcEngine :: Lens' CreateGlobalCluster (Maybe Text)
cgcEngine = lens _cgcEngine (\s a -> s {_cgcEngine = a})

-- | The name for your database of up to 64 alpha-numeric characters. If you do not provide a name, Amazon Aurora will not create a database in the global database cluster you are creating.
cgcDatabaseName :: Lens' CreateGlobalCluster (Maybe Text)
cgcDatabaseName = lens _cgcDatabaseName (\s a -> s {_cgcDatabaseName = a})

instance AWSRequest CreateGlobalCluster where
  type Rs CreateGlobalCluster = CreateGlobalClusterResponse
  request = postQuery rds
  response =
    receiveXMLWrapper
      "CreateGlobalClusterResult"
      ( \s h x ->
          CreateGlobalClusterResponse'
            <$> (x .@? "GlobalCluster") <*> (pure (fromEnum s))
      )

instance Hashable CreateGlobalCluster

instance NFData CreateGlobalCluster

instance ToHeaders CreateGlobalCluster where
  toHeaders = const mempty

instance ToPath CreateGlobalCluster where
  toPath = const "/"

instance ToQuery CreateGlobalCluster where
  toQuery CreateGlobalCluster' {..} =
    mconcat
      [ "Action" =: ("CreateGlobalCluster" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "EngineVersion" =: _cgcEngineVersion,
        "DeletionProtection" =: _cgcDeletionProtection,
        "StorageEncrypted" =: _cgcStorageEncrypted,
        "SourceDBClusterIdentifier" =: _cgcSourceDBClusterIdentifier,
        "GlobalClusterIdentifier" =: _cgcGlobalClusterIdentifier,
        "Engine" =: _cgcEngine,
        "DatabaseName" =: _cgcDatabaseName
      ]

-- | /See:/ 'createGlobalClusterResponse' smart constructor.
data CreateGlobalClusterResponse = CreateGlobalClusterResponse'
  { _cgcrsGlobalCluster ::
      !(Maybe GlobalCluster),
    _cgcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateGlobalClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgcrsGlobalCluster' - Undocumented member.
--
-- * 'cgcrsResponseStatus' - -- | The response status code.
createGlobalClusterResponse ::
  -- | 'cgcrsResponseStatus'
  Int ->
  CreateGlobalClusterResponse
createGlobalClusterResponse pResponseStatus_ =
  CreateGlobalClusterResponse'
    { _cgcrsGlobalCluster = Nothing,
      _cgcrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
cgcrsGlobalCluster :: Lens' CreateGlobalClusterResponse (Maybe GlobalCluster)
cgcrsGlobalCluster = lens _cgcrsGlobalCluster (\s a -> s {_cgcrsGlobalCluster = a})

-- | -- | The response status code.
cgcrsResponseStatus :: Lens' CreateGlobalClusterResponse Int
cgcrsResponseStatus = lens _cgcrsResponseStatus (\s a -> s {_cgcrsResponseStatus = a})

instance NFData CreateGlobalClusterResponse
