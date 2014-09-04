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
    , createClusterSnapshot
    -- ** Request lenses
    , ccsnSnapshotIdentifier
    , ccsnClusterIdentifier

    -- * Response
    , CreateClusterSnapshotResponse
    -- ** Response lenses
    , ssssssrSnapshot
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateClusterSnapshot' request.
createClusterSnapshot :: Text -- ^ 'ccsnSnapshotIdentifier'
                      -> Text -- ^ 'ccsnClusterIdentifier'
                      -> CreateClusterSnapshot
createClusterSnapshot p1 p2 = CreateClusterSnapshot
    { _ccsnSnapshotIdentifier = p1
    , _ccsnClusterIdentifier = p2
    }
{-# INLINE createClusterSnapshot #-}

data CreateClusterSnapshot = CreateClusterSnapshot
    { _ccsnSnapshotIdentifier :: Text
      -- ^ A unique identifier for the snapshot that you are requesting.
      -- This identifier must be unique for all snapshots within the AWS
      -- account. Constraints: Cannot be null, empty, or blank Must
      -- contain from 1 to 255 alphanumeric characters or hyphens First
      -- character must be a letter Cannot end with a hyphen or contain
      -- two consecutive hyphens Example: my-snapshot-id.
    , _ccsnClusterIdentifier :: Text
      -- ^ The cluster identifier for which you want a snapshot.
    } deriving (Show, Generic)

-- | A unique identifier for the snapshot that you are requesting. This
-- identifier must be unique for all snapshots within the AWS account.
-- Constraints: Cannot be null, empty, or blank Must contain from 1 to 255
-- alphanumeric characters or hyphens First character must be a letter Cannot
-- end with a hyphen or contain two consecutive hyphens Example:
-- my-snapshot-id.
ccsnSnapshotIdentifier :: Lens' CreateClusterSnapshot (Text)
ccsnSnapshotIdentifier f x =
    f (_ccsnSnapshotIdentifier x)
        <&> \y -> x { _ccsnSnapshotIdentifier = y }
{-# INLINE ccsnSnapshotIdentifier #-}

-- | The cluster identifier for which you want a snapshot.
ccsnClusterIdentifier :: Lens' CreateClusterSnapshot (Text)
ccsnClusterIdentifier f x =
    f (_ccsnClusterIdentifier x)
        <&> \y -> x { _ccsnClusterIdentifier = y }
{-# INLINE ccsnClusterIdentifier #-}

instance ToQuery CreateClusterSnapshot where
    toQuery = genericQuery def

data CreateClusterSnapshotResponse = CreateClusterSnapshotResponse
    { _ssssssrSnapshot :: Maybe Snapshot
      -- ^ Describes a snapshot.
    } deriving (Show, Generic)

-- | Describes a snapshot.
ssssssrSnapshot :: Lens' CreateClusterSnapshotResponse (Maybe Snapshot)
ssssssrSnapshot f x =
    f (_ssssssrSnapshot x)
        <&> \y -> x { _ssssssrSnapshot = y }
{-# INLINE ssssssrSnapshot #-}

instance FromXML CreateClusterSnapshotResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateClusterSnapshot where
    type Sv CreateClusterSnapshot = Redshift
    type Rs CreateClusterSnapshot = CreateClusterSnapshotResponse

    request = post "CreateClusterSnapshot"
    response _ = xmlResponse
