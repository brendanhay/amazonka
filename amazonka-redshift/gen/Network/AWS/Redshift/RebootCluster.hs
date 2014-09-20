{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.RebootCluster
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Reboots a cluster. This action is taken as soon as possible. It results in
-- a momentary outage to the cluster, during which the cluster status is set
-- to rebooting. A cluster event is created when the reboot is completed. Any
-- pending cluster modifications (see ModifyCluster) are applied at this
-- reboot. For more information about managing clusters, go to Amazon Redshift
-- Clusters in the Amazon Redshift Management Guide
-- https://redshift.us-east-1.amazonaws.com/ ?Action=RebootCluster
-- &ClusterIdentifier=examplecluster &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T021951Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 1.0 5439
-- examplecluster.cobaosmlqshn.us-east-1.redshift.amazonaws.com rebooting 2 1
-- true false dev sun:06:30-sun:07:00 in-sync default.redshift-1.0
-- 2013-01-22T19:23:59.368Z active default us-east-1c dw1.xlarge
-- examplecluster true adminuser 5edee79e-6503-11e2-9e70-918437dd236d.
module Network.AWS.Redshift.RebootCluster
    (
    -- * Request
      RebootCluster
    -- ** Request constructor
    , rebootCluster
    -- ** Request lenses
    , rc1ClusterIdentifier

    -- * Response
    , RebootClusterResponse
    -- ** Response constructor
    , rebootClusterResponse
    -- ** Response lenses
    , rcrCluster
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import Network.AWS.Prelude

-- | 
newtype RebootCluster = RebootCluster
    { _rc1ClusterIdentifier :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RebootCluster' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ClusterIdentifier ::@ @Text@
--
rebootCluster :: Text -- ^ 'rc1ClusterIdentifier'
              -> RebootCluster
rebootCluster p1 = RebootCluster
    { _rc1ClusterIdentifier = p1
    }

-- | The cluster identifier.
rc1ClusterIdentifier :: Lens' RebootCluster Text
rc1ClusterIdentifier =
    lens _rc1ClusterIdentifier (\s a -> s { _rc1ClusterIdentifier = a })

instance ToQuery RebootCluster where
    toQuery = genericQuery def

newtype RebootClusterResponse = RebootClusterResponse
    { _rcrCluster :: Maybe Cluster
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RebootClusterResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Cluster ::@ @Maybe Cluster@
--
rebootClusterResponse :: RebootClusterResponse
rebootClusterResponse = RebootClusterResponse
    { _rcrCluster = Nothing
    }

-- | Describes a cluster.
rcrCluster :: Lens' RebootClusterResponse (Maybe Cluster)
rcrCluster = lens _rcrCluster (\s a -> s { _rcrCluster = a })

instance FromXML RebootClusterResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RebootCluster where
    type Sv RebootCluster = Redshift
    type Rs RebootCluster = RebootClusterResponse

    request = post "RebootCluster"
    response _ = xmlResponse
