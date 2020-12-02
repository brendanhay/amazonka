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
-- Module      : Network.AWS.CloudHSMv2.CreateCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new AWS CloudHSM cluster.
module Network.AWS.CloudHSMv2.CreateCluster
  ( -- * Creating a Request
    createCluster,
    CreateCluster,

    -- * Request Lenses
    ccBackupRetentionPolicy,
    ccTagList,
    ccSourceBackupId,
    ccHSMType,
    ccSubnetIds,

    -- * Destructuring the Response
    createClusterResponse,
    CreateClusterResponse,

    -- * Response Lenses
    ccrsCluster,
    ccrsResponseStatus,
  )
where

import Network.AWS.CloudHSMv2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createCluster' smart constructor.
data CreateCluster = CreateCluster'
  { _ccBackupRetentionPolicy ::
      !(Maybe BackupRetentionPolicy),
    _ccTagList :: !(Maybe [Tag]),
    _ccSourceBackupId :: !(Maybe Text),
    _ccHSMType :: !Text,
    _ccSubnetIds :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccBackupRetentionPolicy' - A policy that defines how the service retains backups.
--
-- * 'ccTagList' - Tags to apply to the CloudHSM cluster during creation.
--
-- * 'ccSourceBackupId' - The identifier (ID) of the cluster backup to restore. Use this value to restore the cluster from a backup instead of creating a new cluster. To find the backup ID, use 'DescribeBackups' .
--
-- * 'ccHSMType' - The type of HSM to use in the cluster. Currently the only allowed value is @hsm1.medium@ .
--
-- * 'ccSubnetIds' - The identifiers (IDs) of the subnets where you are creating the cluster. You must specify at least one subnet. If you specify multiple subnets, they must meet the following criteria:     * All subnets must be in the same virtual private cloud (VPC).     * You can specify only one subnet per Availability Zone.
createCluster ::
  -- | 'ccHSMType'
  Text ->
  -- | 'ccSubnetIds'
  NonEmpty Text ->
  CreateCluster
createCluster pHSMType_ pSubnetIds_ =
  CreateCluster'
    { _ccBackupRetentionPolicy = Nothing,
      _ccTagList = Nothing,
      _ccSourceBackupId = Nothing,
      _ccHSMType = pHSMType_,
      _ccSubnetIds = _List1 # pSubnetIds_
    }

-- | A policy that defines how the service retains backups.
ccBackupRetentionPolicy :: Lens' CreateCluster (Maybe BackupRetentionPolicy)
ccBackupRetentionPolicy = lens _ccBackupRetentionPolicy (\s a -> s {_ccBackupRetentionPolicy = a})

-- | Tags to apply to the CloudHSM cluster during creation.
ccTagList :: Lens' CreateCluster [Tag]
ccTagList = lens _ccTagList (\s a -> s {_ccTagList = a}) . _Default . _Coerce

-- | The identifier (ID) of the cluster backup to restore. Use this value to restore the cluster from a backup instead of creating a new cluster. To find the backup ID, use 'DescribeBackups' .
ccSourceBackupId :: Lens' CreateCluster (Maybe Text)
ccSourceBackupId = lens _ccSourceBackupId (\s a -> s {_ccSourceBackupId = a})

-- | The type of HSM to use in the cluster. Currently the only allowed value is @hsm1.medium@ .
ccHSMType :: Lens' CreateCluster Text
ccHSMType = lens _ccHSMType (\s a -> s {_ccHSMType = a})

-- | The identifiers (IDs) of the subnets where you are creating the cluster. You must specify at least one subnet. If you specify multiple subnets, they must meet the following criteria:     * All subnets must be in the same virtual private cloud (VPC).     * You can specify only one subnet per Availability Zone.
ccSubnetIds :: Lens' CreateCluster (NonEmpty Text)
ccSubnetIds = lens _ccSubnetIds (\s a -> s {_ccSubnetIds = a}) . _List1

instance AWSRequest CreateCluster where
  type Rs CreateCluster = CreateClusterResponse
  request = postJSON cloudHSMv2
  response =
    receiveJSON
      ( \s h x ->
          CreateClusterResponse'
            <$> (x .?> "Cluster") <*> (pure (fromEnum s))
      )

instance Hashable CreateCluster

instance NFData CreateCluster

instance ToHeaders CreateCluster where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("BaldrApiService.CreateCluster" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateCluster where
  toJSON CreateCluster' {..} =
    object
      ( catMaybes
          [ ("BackupRetentionPolicy" .=) <$> _ccBackupRetentionPolicy,
            ("TagList" .=) <$> _ccTagList,
            ("SourceBackupId" .=) <$> _ccSourceBackupId,
            Just ("HsmType" .= _ccHSMType),
            Just ("SubnetIds" .= _ccSubnetIds)
          ]
      )

instance ToPath CreateCluster where
  toPath = const "/"

instance ToQuery CreateCluster where
  toQuery = const mempty

-- | /See:/ 'createClusterResponse' smart constructor.
data CreateClusterResponse = CreateClusterResponse'
  { _ccrsCluster ::
      !(Maybe Cluster),
    _ccrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrsCluster' - Information about the cluster that was created.
--
-- * 'ccrsResponseStatus' - -- | The response status code.
createClusterResponse ::
  -- | 'ccrsResponseStatus'
  Int ->
  CreateClusterResponse
createClusterResponse pResponseStatus_ =
  CreateClusterResponse'
    { _ccrsCluster = Nothing,
      _ccrsResponseStatus = pResponseStatus_
    }

-- | Information about the cluster that was created.
ccrsCluster :: Lens' CreateClusterResponse (Maybe Cluster)
ccrsCluster = lens _ccrsCluster (\s a -> s {_ccrsCluster = a})

-- | -- | The response status code.
ccrsResponseStatus :: Lens' CreateClusterResponse Int
ccrsResponseStatus = lens _ccrsResponseStatus (\s a -> s {_ccrsResponseStatus = a})

instance NFData CreateClusterResponse
