{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Redshift.RebootCluster
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Reboots a cluster. This action is taken as soon as possible. It results
-- in a momentary outage to the cluster, during which the cluster status is
-- set to @rebooting@. A cluster event is created when the reboot is
-- completed. Any pending cluster modifications (see ModifyCluster) are
-- applied at this reboot. For more information about managing clusters, go
-- to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters>
-- in the /Amazon Redshift Cluster Management Guide/
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_RebootCluster.html>
module Network.AWS.Redshift.RebootCluster
    (
    -- * Request
      RebootCluster
    -- ** Request constructor
    , rebootCluster
    -- ** Request lenses
    , rcClusterIdentifier

    -- * Response
    , RebootClusterResponse
    -- ** Response constructor
    , rebootClusterResponse
    -- ** Response lenses
    , rcrCluster
    , rcrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'rebootCluster' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcClusterIdentifier'
newtype RebootCluster = RebootCluster'
    { _rcClusterIdentifier :: Text
    } deriving (Eq,Read,Show)

-- | 'RebootCluster' smart constructor.
rebootCluster :: Text -> RebootCluster
rebootCluster pClusterIdentifier =
    RebootCluster'
    { _rcClusterIdentifier = pClusterIdentifier
    }

-- | The cluster identifier.
rcClusterIdentifier :: Lens' RebootCluster Text
rcClusterIdentifier = lens _rcClusterIdentifier (\ s a -> s{_rcClusterIdentifier = a});

instance AWSRequest RebootCluster where
        type Sv RebootCluster = Redshift
        type Rs RebootCluster = RebootClusterResponse
        request = post
        response
          = receiveXMLWrapper "RebootClusterResult"
              (\ s h x ->
                 RebootClusterResponse' <$>
                   (x .@? "Cluster") <*> (pure (fromEnum s)))

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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcrCluster'
--
-- * 'rcrStatus'
data RebootClusterResponse = RebootClusterResponse'
    { _rcrCluster :: Maybe Cluster
    , _rcrStatus  :: !Int
    } deriving (Eq,Read,Show)

-- | 'RebootClusterResponse' smart constructor.
rebootClusterResponse :: Int -> RebootClusterResponse
rebootClusterResponse pStatus =
    RebootClusterResponse'
    { _rcrCluster = Nothing
    , _rcrStatus = pStatus
    }

-- | FIXME: Undocumented member.
rcrCluster :: Lens' RebootClusterResponse (Maybe Cluster)
rcrCluster = lens _rcrCluster (\ s a -> s{_rcrCluster = a});

-- | FIXME: Undocumented member.
rcrStatus :: Lens' RebootClusterResponse Int
rcrStatus = lens _rcrStatus (\ s a -> s{_rcrStatus = a});
