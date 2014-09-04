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
    , mkCopyClusterSnapshotMessage
    -- ** Request lenses
    , ccsmSourceSnapshotIdentifier
    , ccsmSourceSnapshotClusterIdentifier
    , ccsmTargetSnapshotIdentifier

    -- * Response
    , CopyClusterSnapshotResponse
    -- ** Response lenses
    , ssrSnapshot
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CopyClusterSnapshot' request.
mkCopyClusterSnapshotMessage :: Text -- ^ 'ccsmSourceSnapshotIdentifier'
                             -> Text -- ^ 'ccsmTargetSnapshotIdentifier'
                             -> CopyClusterSnapshot
mkCopyClusterSnapshotMessage p1 p2 = CopyClusterSnapshot
    { _ccsmSourceSnapshotIdentifier = p1
    , _ccsmSourceSnapshotClusterIdentifier = Nothing
    , _ccsmTargetSnapshotIdentifier = p3
    }
{-# INLINE mkCopyClusterSnapshotMessage #-}

data CopyClusterSnapshot = CopyClusterSnapshot
    { _ccsmSourceSnapshotIdentifier :: Text
      -- ^ The identifier for the source snapshot. Constraints: Must be the
      -- identifier for a valid automated snapshot whose state is
      -- available.
    , _ccsmSourceSnapshotClusterIdentifier :: Maybe Text
      -- ^ The identifier of the cluster the source snapshot was created
      -- from. This parameter is required if your IAM user has a policy
      -- containing a snapshot resource element that specifies anything
      -- other than * for the cluster name. Constraints: Must be the
      -- identifier for a valid cluster.
    , _ccsmTargetSnapshotIdentifier :: Text
      -- ^ The identifier given to the new manual snapshot. Constraints:
      -- Cannot be null, empty, or blank. Must contain from 1 to 255
      -- alphanumeric characters or hyphens. First character must be a
      -- letter. Cannot end with a hyphen or contain two consecutive
      -- hyphens. Must be unique for the AWS account that is making the
      -- request.
    } deriving (Show, Generic)

-- | The identifier for the source snapshot. Constraints: Must be the identifier
-- for a valid automated snapshot whose state is available.
ccsmSourceSnapshotIdentifier :: Lens' CopyClusterSnapshot (Text)
ccsmSourceSnapshotIdentifier = lens _ccsmSourceSnapshotIdentifier (\s a -> s { _ccsmSourceSnapshotIdentifier = a })
{-# INLINE ccsmSourceSnapshotIdentifier #-}

-- | The identifier of the cluster the source snapshot was created from. This
-- parameter is required if your IAM user has a policy containing a snapshot
-- resource element that specifies anything other than * for the cluster name.
-- Constraints: Must be the identifier for a valid cluster.
ccsmSourceSnapshotClusterIdentifier :: Lens' CopyClusterSnapshot (Maybe Text)
ccsmSourceSnapshotClusterIdentifier = lens _ccsmSourceSnapshotClusterIdentifier (\s a -> s { _ccsmSourceSnapshotClusterIdentifier = a })
{-# INLINE ccsmSourceSnapshotClusterIdentifier #-}

-- | The identifier given to the new manual snapshot. Constraints: Cannot be
-- null, empty, or blank. Must contain from 1 to 255 alphanumeric characters
-- or hyphens. First character must be a letter. Cannot end with a hyphen or
-- contain two consecutive hyphens. Must be unique for the AWS account that is
-- making the request.
ccsmTargetSnapshotIdentifier :: Lens' CopyClusterSnapshot (Text)
ccsmTargetSnapshotIdentifier = lens _ccsmTargetSnapshotIdentifier (\s a -> s { _ccsmTargetSnapshotIdentifier = a })
{-# INLINE ccsmTargetSnapshotIdentifier #-}

instance ToQuery CopyClusterSnapshot where
    toQuery = genericQuery def

newtype CopyClusterSnapshotResponse = CopyClusterSnapshotResponse
    { _ssrSnapshot :: Maybe Snapshot
      -- ^ Describes a snapshot.
    } deriving (Show, Generic)

-- | Describes a snapshot.
ssrSnapshot :: Lens' CopyClusterSnapshotResponse (Maybe Snapshot)
ssrSnapshot = lens _ssrSnapshot (\s a -> s { _ssrSnapshot = a })
{-# INLINE ssrSnapshot #-}

instance FromXML CopyClusterSnapshotResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CopyClusterSnapshot where
    type Sv CopyClusterSnapshot = Redshift
    type Rs CopyClusterSnapshot = CopyClusterSnapshotResponse

    request = post "CopyClusterSnapshot"
    response _ = xmlResponse
