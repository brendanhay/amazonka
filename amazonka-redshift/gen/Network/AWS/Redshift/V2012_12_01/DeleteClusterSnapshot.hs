{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.DeleteClusterSnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified manual snapshot. The snapshot must be in the
-- available state, with no other users authorized to access the snapshot.
-- Unlike automated snapshots, manual snapshots are retained even after you
-- delete your cluster. Amazon Redshift does not delete your manual snapshots.
-- You must delete manual snapshot explicitly to avoid getting charged. If
-- other accounts are authorized to access the snapshot, you must revoke all
-- of the authorizations before you can delete the snapshot.
-- https://redshift.us-east-1.amazonaws.com/ ?Action=DeleteClusterSnapshot
-- &SnapshotIdentifier=snapshot-1234 &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121208/us-east-1/redshift/aws4_request
-- &x-amz-date=20121208T005225Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 2012-12-07T23:31:02.372Z
-- 5439 snapshot-1234 deleted 2012-12-06T23:09:01.475Z manual 1.0 us-east-1a
-- examplecluster masteruser dw1.xlarge mydb 3
-- 88a31de4-40d1-11e2-8a25-eb010998df4e.
module Network.AWS.Redshift.V2012_12_01.DeleteClusterSnapshot
    (
    -- * Request
      DeleteClusterSnapshot
    -- ** Request constructor
    , mkDeleteClusterSnapshot
    -- ** Request lenses
    , dcsSnapshotIdentifier
    , dcsSnapshotClusterIdentifier

    -- * Response
    , DeleteClusterSnapshotResponse
    -- ** Response constructor
    , mkDeleteClusterSnapshotResponse
    -- ** Response lenses
    , dcsrSnapshot
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | 
data DeleteClusterSnapshot = DeleteClusterSnapshot
    { _dcsSnapshotIdentifier :: Text
    , _dcsSnapshotClusterIdentifier :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteClusterSnapshot' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SnapshotIdentifier ::@ @Text@
--
-- * @SnapshotClusterIdentifier ::@ @Maybe Text@
--
mkDeleteClusterSnapshot :: Text -- ^ 'dcsSnapshotIdentifier'
                        -> DeleteClusterSnapshot
mkDeleteClusterSnapshot p1 = DeleteClusterSnapshot
    { _dcsSnapshotIdentifier = p1
    , _dcsSnapshotClusterIdentifier = Nothing
    }

-- | The unique identifier of the manual snapshot to be deleted. Constraints:
-- Must be the name of an existing snapshot that is in the available state.
dcsSnapshotIdentifier :: Lens' DeleteClusterSnapshot Text
dcsSnapshotIdentifier =
    lens _dcsSnapshotIdentifier (\s a -> s { _dcsSnapshotIdentifier = a })

-- | The unique identifier of the cluster the snapshot was created from. This
-- parameter is required if your IAM user has a policy containing a snapshot
-- resource element that specifies anything other than * for the cluster name.
-- Constraints: Must be the name of valid cluster.
dcsSnapshotClusterIdentifier :: Lens' DeleteClusterSnapshot (Maybe Text)
dcsSnapshotClusterIdentifier =
    lens _dcsSnapshotClusterIdentifier
         (\s a -> s { _dcsSnapshotClusterIdentifier = a })

instance ToQuery DeleteClusterSnapshot where
    toQuery = genericQuery def

newtype DeleteClusterSnapshotResponse = DeleteClusterSnapshotResponse
    { _dcsrSnapshot :: Maybe Snapshot
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteClusterSnapshotResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Snapshot ::@ @Maybe Snapshot@
--
mkDeleteClusterSnapshotResponse :: DeleteClusterSnapshotResponse
mkDeleteClusterSnapshotResponse = DeleteClusterSnapshotResponse
    { _dcsrSnapshot = Nothing
    }

-- | Describes a snapshot.
dcsrSnapshot :: Lens' DeleteClusterSnapshotResponse (Maybe Snapshot)
dcsrSnapshot = lens _dcsrSnapshot (\s a -> s { _dcsrSnapshot = a })

instance FromXML DeleteClusterSnapshotResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DeleteClusterSnapshot where
    type Sv DeleteClusterSnapshot = Redshift
    type Rs DeleteClusterSnapshot = DeleteClusterSnapshotResponse

    request = post "DeleteClusterSnapshot"
    response _ = xmlResponse
