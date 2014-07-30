{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.RebootCluster
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
module Network.AWS.Redshift.V2012_12_01.RebootCluster where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.Redshift.V2012_12_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data RebootCluster = RebootCluster
    { _rcoClusterIdentifier :: Text
      -- ^ The cluster identifier.
    } deriving (Generic)

instance ToQuery RebootCluster where
    toQuery = genericToQuery def

instance AWSRequest RebootCluster where
    type Sv RebootCluster = Redshift
    type Rs RebootCluster = RebootClusterResponse

    request = post "RebootCluster"
    response _ = xmlResponse

data RebootClusterResponse = RebootClusterResponse
    { _cyCluster :: Maybe Cluster
      -- ^ Describes a cluster.
    } deriving (Generic)

instance FromXML RebootClusterResponse where
    fromXMLOptions = xmlOptions
