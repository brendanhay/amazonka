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
-- Module      : Network.AWS.Redshift.RebootCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reboots a cluster. This action is taken as soon as possible. It results in a momentary outage to the cluster, during which the cluster status is set to @rebooting@ . A cluster event is created when the reboot is completed. Any pending cluster modifications (see 'ModifyCluster' ) are applied at this reboot. For more information about managing clusters, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters> in the /Amazon Redshift Cluster Management Guide/ .
module Network.AWS.Redshift.RebootCluster
  ( -- * Creating a Request
    rebootCluster,
    RebootCluster,

    -- * Request Lenses
    rebClusterIdentifier,

    -- * Destructuring the Response
    rebootClusterResponse,
    RebootClusterResponse,

    -- * Response Lenses
    rrsCluster,
    rrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'rebootCluster' smart constructor.
newtype RebootCluster = RebootCluster'
  { _rebClusterIdentifier ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RebootCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rebClusterIdentifier' - The cluster identifier.
rebootCluster ::
  -- | 'rebClusterIdentifier'
  Text ->
  RebootCluster
rebootCluster pClusterIdentifier_ =
  RebootCluster' {_rebClusterIdentifier = pClusterIdentifier_}

-- | The cluster identifier.
rebClusterIdentifier :: Lens' RebootCluster Text
rebClusterIdentifier = lens _rebClusterIdentifier (\s a -> s {_rebClusterIdentifier = a})

instance AWSRequest RebootCluster where
  type Rs RebootCluster = RebootClusterResponse
  request = postQuery redshift
  response =
    receiveXMLWrapper
      "RebootClusterResult"
      ( \s h x ->
          RebootClusterResponse'
            <$> (x .@? "Cluster") <*> (pure (fromEnum s))
      )

instance Hashable RebootCluster

instance NFData RebootCluster

instance ToHeaders RebootCluster where
  toHeaders = const mempty

instance ToPath RebootCluster where
  toPath = const "/"

instance ToQuery RebootCluster where
  toQuery RebootCluster' {..} =
    mconcat
      [ "Action" =: ("RebootCluster" :: ByteString),
        "Version" =: ("2012-12-01" :: ByteString),
        "ClusterIdentifier" =: _rebClusterIdentifier
      ]

-- | /See:/ 'rebootClusterResponse' smart constructor.
data RebootClusterResponse = RebootClusterResponse'
  { _rrsCluster ::
      !(Maybe Cluster),
    _rrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RebootClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrsCluster' - Undocumented member.
--
-- * 'rrsResponseStatus' - -- | The response status code.
rebootClusterResponse ::
  -- | 'rrsResponseStatus'
  Int ->
  RebootClusterResponse
rebootClusterResponse pResponseStatus_ =
  RebootClusterResponse'
    { _rrsCluster = Nothing,
      _rrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
rrsCluster :: Lens' RebootClusterResponse (Maybe Cluster)
rrsCluster = lens _rrsCluster (\s a -> s {_rrsCluster = a})

-- | -- | The response status code.
rrsResponseStatus :: Lens' RebootClusterResponse Int
rrsResponseStatus = lens _rrsResponseStatus (\s a -> s {_rrsResponseStatus = a})

instance NFData RebootClusterResponse
