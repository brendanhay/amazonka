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
-- Module      : Network.AWS.RDS.RemoveFromGlobalCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches an Aurora secondary cluster from an Aurora global database cluster. The cluster becomes a standalone cluster with read-write capability instead of being read-only and receiving data from a primary cluster in a different region.
module Network.AWS.RDS.RemoveFromGlobalCluster
  ( -- * Creating a Request
    removeFromGlobalCluster,
    RemoveFromGlobalCluster,

    -- * Request Lenses
    rfgcDBClusterIdentifier,
    rfgcGlobalClusterIdentifier,

    -- * Destructuring the Response
    removeFromGlobalClusterResponse,
    RemoveFromGlobalClusterResponse,

    -- * Response Lenses
    rfgcrsGlobalCluster,
    rfgcrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'removeFromGlobalCluster' smart constructor.
data RemoveFromGlobalCluster = RemoveFromGlobalCluster'
  { _rfgcDBClusterIdentifier ::
      !(Maybe Text),
    _rfgcGlobalClusterIdentifier ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RemoveFromGlobalCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rfgcDBClusterIdentifier' - The Amazon Resource Name (ARN) identifying the cluster that was detached from the Aurora global database cluster.
--
-- * 'rfgcGlobalClusterIdentifier' - The cluster identifier to detach from the Aurora global database cluster.
removeFromGlobalCluster ::
  RemoveFromGlobalCluster
removeFromGlobalCluster =
  RemoveFromGlobalCluster'
    { _rfgcDBClusterIdentifier = Nothing,
      _rfgcGlobalClusterIdentifier = Nothing
    }

-- | The Amazon Resource Name (ARN) identifying the cluster that was detached from the Aurora global database cluster.
rfgcDBClusterIdentifier :: Lens' RemoveFromGlobalCluster (Maybe Text)
rfgcDBClusterIdentifier = lens _rfgcDBClusterIdentifier (\s a -> s {_rfgcDBClusterIdentifier = a})

-- | The cluster identifier to detach from the Aurora global database cluster.
rfgcGlobalClusterIdentifier :: Lens' RemoveFromGlobalCluster (Maybe Text)
rfgcGlobalClusterIdentifier = lens _rfgcGlobalClusterIdentifier (\s a -> s {_rfgcGlobalClusterIdentifier = a})

instance AWSRequest RemoveFromGlobalCluster where
  type Rs RemoveFromGlobalCluster = RemoveFromGlobalClusterResponse
  request = postQuery rds
  response =
    receiveXMLWrapper
      "RemoveFromGlobalClusterResult"
      ( \s h x ->
          RemoveFromGlobalClusterResponse'
            <$> (x .@? "GlobalCluster") <*> (pure (fromEnum s))
      )

instance Hashable RemoveFromGlobalCluster

instance NFData RemoveFromGlobalCluster

instance ToHeaders RemoveFromGlobalCluster where
  toHeaders = const mempty

instance ToPath RemoveFromGlobalCluster where
  toPath = const "/"

instance ToQuery RemoveFromGlobalCluster where
  toQuery RemoveFromGlobalCluster' {..} =
    mconcat
      [ "Action" =: ("RemoveFromGlobalCluster" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "DbClusterIdentifier" =: _rfgcDBClusterIdentifier,
        "GlobalClusterIdentifier" =: _rfgcGlobalClusterIdentifier
      ]

-- | /See:/ 'removeFromGlobalClusterResponse' smart constructor.
data RemoveFromGlobalClusterResponse = RemoveFromGlobalClusterResponse'
  { _rfgcrsGlobalCluster ::
      !(Maybe GlobalCluster),
    _rfgcrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RemoveFromGlobalClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rfgcrsGlobalCluster' - Undocumented member.
--
-- * 'rfgcrsResponseStatus' - -- | The response status code.
removeFromGlobalClusterResponse ::
  -- | 'rfgcrsResponseStatus'
  Int ->
  RemoveFromGlobalClusterResponse
removeFromGlobalClusterResponse pResponseStatus_ =
  RemoveFromGlobalClusterResponse'
    { _rfgcrsGlobalCluster = Nothing,
      _rfgcrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
rfgcrsGlobalCluster :: Lens' RemoveFromGlobalClusterResponse (Maybe GlobalCluster)
rfgcrsGlobalCluster = lens _rfgcrsGlobalCluster (\s a -> s {_rfgcrsGlobalCluster = a})

-- | -- | The response status code.
rfgcrsResponseStatus :: Lens' RemoveFromGlobalClusterResponse Int
rfgcrsResponseStatus = lens _rfgcrsResponseStatus (\s a -> s {_rfgcrsResponseStatus = a})

instance NFData RemoveFromGlobalClusterResponse
