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
-- Module      : Network.AWS.CloudHSMv2.ModifyCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies AWS CloudHSM cluster.
module Network.AWS.CloudHSMv2.ModifyCluster
  ( -- * Creating a Request
    modifyCluster,
    ModifyCluster,

    -- * Request Lenses
    mcBackupRetentionPolicy,
    mcClusterId,

    -- * Destructuring the Response
    modifyClusterResponse,
    ModifyClusterResponse,

    -- * Response Lenses
    mcrsCluster,
    mcrsResponseStatus,
  )
where

import Network.AWS.CloudHSMv2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyCluster' smart constructor.
data ModifyCluster = ModifyCluster'
  { _mcBackupRetentionPolicy ::
      !BackupRetentionPolicy,
    _mcClusterId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcBackupRetentionPolicy' - A policy that defines how the service retains backups.
--
-- * 'mcClusterId' - The identifier (ID) of the cluster that you want to modify. To find the cluster ID, use 'DescribeClusters' .
modifyCluster ::
  -- | 'mcBackupRetentionPolicy'
  BackupRetentionPolicy ->
  -- | 'mcClusterId'
  Text ->
  ModifyCluster
modifyCluster pBackupRetentionPolicy_ pClusterId_ =
  ModifyCluster'
    { _mcBackupRetentionPolicy =
        pBackupRetentionPolicy_,
      _mcClusterId = pClusterId_
    }

-- | A policy that defines how the service retains backups.
mcBackupRetentionPolicy :: Lens' ModifyCluster BackupRetentionPolicy
mcBackupRetentionPolicy = lens _mcBackupRetentionPolicy (\s a -> s {_mcBackupRetentionPolicy = a})

-- | The identifier (ID) of the cluster that you want to modify. To find the cluster ID, use 'DescribeClusters' .
mcClusterId :: Lens' ModifyCluster Text
mcClusterId = lens _mcClusterId (\s a -> s {_mcClusterId = a})

instance AWSRequest ModifyCluster where
  type Rs ModifyCluster = ModifyClusterResponse
  request = postJSON cloudHSMv2
  response =
    receiveJSON
      ( \s h x ->
          ModifyClusterResponse'
            <$> (x .?> "Cluster") <*> (pure (fromEnum s))
      )

instance Hashable ModifyCluster

instance NFData ModifyCluster

instance ToHeaders ModifyCluster where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("BaldrApiService.ModifyCluster" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ModifyCluster where
  toJSON ModifyCluster' {..} =
    object
      ( catMaybes
          [ Just ("BackupRetentionPolicy" .= _mcBackupRetentionPolicy),
            Just ("ClusterId" .= _mcClusterId)
          ]
      )

instance ToPath ModifyCluster where
  toPath = const "/"

instance ToQuery ModifyCluster where
  toQuery = const mempty

-- | /See:/ 'modifyClusterResponse' smart constructor.
data ModifyClusterResponse = ModifyClusterResponse'
  { _mcrsCluster ::
      !(Maybe Cluster),
    _mcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcrsCluster' - Undocumented member.
--
-- * 'mcrsResponseStatus' - -- | The response status code.
modifyClusterResponse ::
  -- | 'mcrsResponseStatus'
  Int ->
  ModifyClusterResponse
modifyClusterResponse pResponseStatus_ =
  ModifyClusterResponse'
    { _mcrsCluster = Nothing,
      _mcrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
mcrsCluster :: Lens' ModifyClusterResponse (Maybe Cluster)
mcrsCluster = lens _mcrsCluster (\s a -> s {_mcrsCluster = a})

-- | -- | The response status code.
mcrsResponseStatus :: Lens' ModifyClusterResponse Int
mcrsResponseStatus = lens _mcrsResponseStatus (\s a -> s {_mcrsResponseStatus = a})

instance NFData ModifyClusterResponse
