{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.RebootCluster
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reboots a cluster. This action is taken as soon as possible. It results in a momentary outage to the cluster, during which the cluster status is set to @rebooting@ . A cluster event is created when the reboot is completed. Any pending cluster modifications (see 'ModifyCluster' ) are applied at this reboot. For more information about managing clusters, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters> in the /Amazon Redshift Cluster Management Guide/ .
--
--
module Network.AWS.Redshift.RebootCluster
    (
    -- * Creating a Request
      rebootCluster
    , RebootCluster
    -- * Request Lenses
    , rcClusterIdentifier

    -- * Destructuring the Response
    , rebootClusterResponse
    , RebootClusterResponse
    -- * Response Lenses
    , rcrsCluster
    , rcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'rebootCluster' smart constructor.
newtype RebootCluster = RebootCluster'
  { _rcClusterIdentifier :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RebootCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcClusterIdentifier' - The cluster identifier.
rebootCluster
    :: Text -- ^ 'rcClusterIdentifier'
    -> RebootCluster
rebootCluster pClusterIdentifier_ =
  RebootCluster' {_rcClusterIdentifier = pClusterIdentifier_}


-- | The cluster identifier.
rcClusterIdentifier :: Lens' RebootCluster Text
rcClusterIdentifier = lens _rcClusterIdentifier (\ s a -> s{_rcClusterIdentifier = a})

instance AWSRequest RebootCluster where
        type Rs RebootCluster = RebootClusterResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper "RebootClusterResult"
              (\ s h x ->
                 RebootClusterResponse' <$>
                   (x .@? "Cluster") <*> (pure (fromEnum s)))

instance Hashable RebootCluster where

instance NFData RebootCluster where

instance ToHeaders RebootCluster where
        toHeaders = const mempty

instance ToPath RebootCluster where
        toPath = const "/"

instance ToQuery RebootCluster where
        toQuery RebootCluster'{..}
          = mconcat
              ["Action" =: ("RebootCluster" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "ClusterIdentifier" =: _rcClusterIdentifier]

-- | /See:/ 'rebootClusterResponse' smart constructor.
data RebootClusterResponse = RebootClusterResponse'
  { _rcrsCluster        :: !(Maybe Cluster)
  , _rcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RebootClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcrsCluster' - Undocumented member.
--
-- * 'rcrsResponseStatus' - -- | The response status code.
rebootClusterResponse
    :: Int -- ^ 'rcrsResponseStatus'
    -> RebootClusterResponse
rebootClusterResponse pResponseStatus_ =
  RebootClusterResponse'
    {_rcrsCluster = Nothing, _rcrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
rcrsCluster :: Lens' RebootClusterResponse (Maybe Cluster)
rcrsCluster = lens _rcrsCluster (\ s a -> s{_rcrsCluster = a})

-- | -- | The response status code.
rcrsResponseStatus :: Lens' RebootClusterResponse Int
rcrsResponseStatus = lens _rcrsResponseStatus (\ s a -> s{_rcrsResponseStatus = a})

instance NFData RebootClusterResponse where
