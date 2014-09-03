{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.CopyClusterSnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Copies the specified automated cluster snapshot to a new manual cluster
-- snapshot. The source must be an automated snapshot and it must be in the
-- available state. When you delete a cluster, Amazon Redshift deletes any
-- automated snapshots of the cluster. Also, when the retention period of the
-- snapshot expires, Amazon Redshift automatically deletes it. If you want to
-- keep an automated snapshot for a longer period, you can make a manual copy
-- of the snapshot. Manual snapshots are retained until you delete them. For
-- more information about working with snapshots, go to Amazon Redshift
-- Snapshots in the Amazon Redshift Management Guide.
-- https://redshift.us-east-1.amazonaws.com/ ?Action=CopyClusterSnapshot
-- &SourceSnapshotIdentifier=cm:examplecluster-2013-01-22-19-27-58
-- &TargetSnapshotIdentifier=my-snapshot-456 &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T014618Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 5439 my-snapshot-456
-- available manual 1.0 2013-01-22T19:27:58.931Z 2 dev
-- 2013-01-22T19:23:59.368Z us-east-1c dw1.xlarge examplecluster adminuser
-- aebb56f5-64fe-11e2-88c5-53eb05787dfb.
module Network.AWS.Redshift.V2012_12_01.CopyClusterSnapshot
    (
    -- * Request
      CopyClusterSnapshot
    -- ** Request constructor
    , copyClusterSnapshot
    -- ** Request lenses
    , ccsmSourceSnapshotIdentifier
    , ccsmTargetSnapshotIdentifier
    , ccsmSourceSnapshotClusterIdentifier

    -- * Response
    , CopyClusterSnapshotResponse
    -- ** Response lenses
    , ssrSnapshot
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CopyClusterSnapshot' request.
copyClusterSnapshot :: Text -- ^ 'ccsmSourceSnapshotIdentifier'
                    -> Text -- ^ 'ccsmTargetSnapshotIdentifier'
                    -> CopyClusterSnapshot
copyClusterSnapshot p1 p2 = CopyClusterSnapshot
    { _ccsmSourceSnapshotIdentifier = p1
    , _ccsmTargetSnapshotIdentifier = p2
    , _ccsmSourceSnapshotClusterIdentifier = Nothing
    }

data CopyClusterSnapshot = CopyClusterSnapshot
    { _ccsmSourceSnapshotIdentifier :: Text
      -- ^ The identifier for the source snapshot. Constraints: Must be the
      -- identifier for a valid automated snapshot whose state is
      -- available.
    , _ccsmTargetSnapshotIdentifier :: Text
      -- ^ The identifier given to the new manual snapshot. Constraints:
      -- Cannot be null, empty, or blank. Must contain from 1 to 255
      -- alphanumeric characters or hyphens. First character must be a
      -- letter. Cannot end with a hyphen or contain two consecutive
      -- hyphens. Must be unique for the AWS account that is making the
      -- request.
    , _ccsmSourceSnapshotClusterIdentifier :: Maybe Text
      -- ^ The identifier of the cluster the source snapshot was created
      -- from. This parameter is required if your IAM user has a policy
      -- containing a snapshot resource element that specifies anything
      -- other than * for the cluster name. Constraints: Must be the
      -- identifier for a valid cluster.
    } deriving (Show, Generic)

-- | The identifier for the source snapshot. Constraints: Must be the identifier
-- for a valid automated snapshot whose state is available.
ccsmSourceSnapshotIdentifier
    :: Functor f
    => (Text
    -> f (Text))
    -> CopyClusterSnapshot
    -> f CopyClusterSnapshot
ccsmSourceSnapshotIdentifier f x =
    (\y -> x { _ccsmSourceSnapshotIdentifier = y })
       <$> f (_ccsmSourceSnapshotIdentifier x)
{-# INLINE ccsmSourceSnapshotIdentifier #-}

-- | The identifier given to the new manual snapshot. Constraints: Cannot be
-- null, empty, or blank. Must contain from 1 to 255 alphanumeric characters
-- or hyphens. First character must be a letter. Cannot end with a hyphen or
-- contain two consecutive hyphens. Must be unique for the AWS account that is
-- making the request.
ccsmTargetSnapshotIdentifier
    :: Functor f
    => (Text
    -> f (Text))
    -> CopyClusterSnapshot
    -> f CopyClusterSnapshot
ccsmTargetSnapshotIdentifier f x =
    (\y -> x { _ccsmTargetSnapshotIdentifier = y })
       <$> f (_ccsmTargetSnapshotIdentifier x)
{-# INLINE ccsmTargetSnapshotIdentifier #-}

-- | The identifier of the cluster the source snapshot was created from. This
-- parameter is required if your IAM user has a policy containing a snapshot
-- resource element that specifies anything other than * for the cluster name.
-- Constraints: Must be the identifier for a valid cluster.
ccsmSourceSnapshotClusterIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CopyClusterSnapshot
    -> f CopyClusterSnapshot
ccsmSourceSnapshotClusterIdentifier f x =
    (\y -> x { _ccsmSourceSnapshotClusterIdentifier = y })
       <$> f (_ccsmSourceSnapshotClusterIdentifier x)
{-# INLINE ccsmSourceSnapshotClusterIdentifier #-}

instance ToQuery CopyClusterSnapshot where
    toQuery = genericQuery def

data CopyClusterSnapshotResponse = CopyClusterSnapshotResponse
    { _ssrSnapshot :: Maybe Snapshot
      -- ^ Describes a snapshot.
    } deriving (Show, Generic)

-- | Describes a snapshot.
ssrSnapshot
    :: Functor f
    => (Maybe Snapshot
    -> f (Maybe Snapshot))
    -> CopyClusterSnapshotResponse
    -> f CopyClusterSnapshotResponse
ssrSnapshot f x =
    (\y -> x { _ssrSnapshot = y })
       <$> f (_ssrSnapshot x)
{-# INLINE ssrSnapshot #-}

instance FromXML CopyClusterSnapshotResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CopyClusterSnapshot where
    type Sv CopyClusterSnapshot = Redshift
    type Rs CopyClusterSnapshot = CopyClusterSnapshotResponse

    request = post "CopyClusterSnapshot"
    response _ = xmlResponse
