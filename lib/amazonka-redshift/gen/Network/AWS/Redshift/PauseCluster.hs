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
-- Module      : Network.AWS.Redshift.PauseCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Pauses a cluster.
module Network.AWS.Redshift.PauseCluster
  ( -- * Creating a Request
    pauseCluster,
    PauseCluster,

    -- * Request Lenses
    pcClusterIdentifier,

    -- * Destructuring the Response
    pauseClusterResponse,
    PauseClusterResponse,

    -- * Response Lenses
    pcrsCluster,
    pcrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Request
import Network.AWS.Response

-- | Describes a pause cluster operation. For example, a scheduled action to run the @PauseCluster@ API operation.
--
--
--
-- /See:/ 'pauseCluster' smart constructor.
newtype PauseCluster = PauseCluster' {_pcClusterIdentifier :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PauseCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcClusterIdentifier' - The identifier of the cluster to be paused.
pauseCluster ::
  -- | 'pcClusterIdentifier'
  Text ->
  PauseCluster
pauseCluster pClusterIdentifier_ =
  PauseCluster' {_pcClusterIdentifier = pClusterIdentifier_}

-- | The identifier of the cluster to be paused.
pcClusterIdentifier :: Lens' PauseCluster Text
pcClusterIdentifier = lens _pcClusterIdentifier (\s a -> s {_pcClusterIdentifier = a})

instance AWSRequest PauseCluster where
  type Rs PauseCluster = PauseClusterResponse
  request = postQuery redshift
  response =
    receiveXMLWrapper
      "PauseClusterResult"
      ( \s h x ->
          PauseClusterResponse'
            <$> (x .@? "Cluster") <*> (pure (fromEnum s))
      )

instance Hashable PauseCluster

instance NFData PauseCluster

instance ToHeaders PauseCluster where
  toHeaders = const mempty

instance ToPath PauseCluster where
  toPath = const "/"

instance ToQuery PauseCluster where
  toQuery PauseCluster' {..} =
    mconcat
      [ "Action" =: ("PauseCluster" :: ByteString),
        "Version" =: ("2012-12-01" :: ByteString),
        "ClusterIdentifier" =: _pcClusterIdentifier
      ]

-- | /See:/ 'pauseClusterResponse' smart constructor.
data PauseClusterResponse = PauseClusterResponse'
  { _pcrsCluster ::
      !(Maybe Cluster),
    _pcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PauseClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcrsCluster' - Undocumented member.
--
-- * 'pcrsResponseStatus' - -- | The response status code.
pauseClusterResponse ::
  -- | 'pcrsResponseStatus'
  Int ->
  PauseClusterResponse
pauseClusterResponse pResponseStatus_ =
  PauseClusterResponse'
    { _pcrsCluster = Nothing,
      _pcrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
pcrsCluster :: Lens' PauseClusterResponse (Maybe Cluster)
pcrsCluster = lens _pcrsCluster (\s a -> s {_pcrsCluster = a})

-- | -- | The response status code.
pcrsResponseStatus :: Lens' PauseClusterResponse Int
pcrsResponseStatus = lens _pcrsResponseStatus (\s a -> s {_pcrsResponseStatus = a})

instance NFData PauseClusterResponse
