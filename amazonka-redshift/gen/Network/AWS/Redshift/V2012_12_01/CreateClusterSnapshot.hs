{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.CreateClusterSnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a manual snapshot of the specified cluster. The cluster must be in
-- the available state. For more information about working with snapshots, go
-- to Amazon Redshift Snapshots in the Amazon Redshift Management Guide.
-- https://redshift.us-east-1.amazonaws.com/ ?Action=CreateClusterSnapshot
-- &ClusterIdentifier=examplecluster &SnapshotIdentifier=snapshot-1234
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T010824Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 5439 my-snapshot-123
-- creating manual 1.0 2013-01-23T01:08:29.142Z 2 dev 2013-01-22T19:23:59.368Z
-- us-east-1c dw1.xlarge examplecluster adminuser
-- 65baef14-64f9-11e2-bea9-49e0ce183f07.
module Network.AWS.Redshift.V2012_12_01.CreateClusterSnapshot
    (
    -- * Request
      CreateClusterSnapshot
    -- ** Request constructor
    , mkCreateClusterSnapshot
    -- ** Request lenses
    , ccs1SnapshotIdentifier
    , ccs1ClusterIdentifier

    -- * Response
    , CreateClusterSnapshotResponse
    -- ** Response constructor
    , mkCreateClusterSnapshotResponse
    -- ** Response lenses
    , ccsrrSnapshot
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | 
data CreateClusterSnapshot = CreateClusterSnapshot
    { _ccs1SnapshotIdentifier :: Text
    , _ccs1ClusterIdentifier :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateClusterSnapshot' request.
mkCreateClusterSnapshot :: Text -- ^ 'ccs1SnapshotIdentifier'
                        -> Text -- ^ 'ccs1ClusterIdentifier'
                        -> CreateClusterSnapshot
mkCreateClusterSnapshot p1 p2 = CreateClusterSnapshot
    { _ccs1SnapshotIdentifier = p1
    , _ccs1ClusterIdentifier = p2
    }

-- | A unique identifier for the snapshot that you are requesting. This
-- identifier must be unique for all snapshots within the AWS account.
-- Constraints: Cannot be null, empty, or blank Must contain from 1 to 255
-- alphanumeric characters or hyphens First character must be a letter Cannot
-- end with a hyphen or contain two consecutive hyphens Example:
-- my-snapshot-id.
ccs1SnapshotIdentifier :: Lens' CreateClusterSnapshot Text
ccs1SnapshotIdentifier =
    lens _ccs1SnapshotIdentifier (\s a -> s { _ccs1SnapshotIdentifier = a })

-- | The cluster identifier for which you want a snapshot.
ccs1ClusterIdentifier :: Lens' CreateClusterSnapshot Text
ccs1ClusterIdentifier =
    lens _ccs1ClusterIdentifier (\s a -> s { _ccs1ClusterIdentifier = a })

instance ToQuery CreateClusterSnapshot where
    toQuery = genericQuery def

newtype CreateClusterSnapshotResponse = CreateClusterSnapshotResponse
    { _ccsrrSnapshot :: Maybe Snapshot
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateClusterSnapshotResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkCreateClusterSnapshotResponse :: CreateClusterSnapshotResponse
mkCreateClusterSnapshotResponse = CreateClusterSnapshotResponse
    { _ccsrrSnapshot = Nothing
    }

-- | Describes a snapshot.
ccsrrSnapshot :: Lens' CreateClusterSnapshotResponse (Maybe Snapshot)
ccsrrSnapshot = lens _ccsrrSnapshot (\s a -> s { _ccsrrSnapshot = a })

instance FromXML CreateClusterSnapshotResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateClusterSnapshot where
    type Sv CreateClusterSnapshot = Redshift
    type Rs CreateClusterSnapshot = CreateClusterSnapshotResponse

    request = post "CreateClusterSnapshot"
    response _ = xmlResponse
