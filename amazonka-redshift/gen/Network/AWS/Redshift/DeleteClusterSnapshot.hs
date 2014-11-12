{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.Redshift.DeleteClusterSnapshot
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
module Network.AWS.Redshift.DeleteClusterSnapshot
    (
    -- * Request
      DeleteClusterSnapshotMessage
    -- ** Request constructor
    , deleteClusterSnapshot
    -- ** Request lenses
    , dcsmSnapshotClusterIdentifier
    , dcsmSnapshotIdentifier

    -- * Response
    , DeleteClusterSnapshotResult
    -- ** Response constructor
    , deleteClusterSnapshotResponse
    -- ** Response lenses
    , dcsrSnapshot
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data DeleteClusterSnapshotMessage = DeleteClusterSnapshotMessage
    { _dcsmSnapshotClusterIdentifier :: Maybe Text
    , _dcsmSnapshotIdentifier        :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteClusterSnapshotMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsmSnapshotClusterIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'dcsmSnapshotIdentifier' @::@ 'Text'
--
deleteClusterSnapshot :: Text -- ^ 'dcsmSnapshotIdentifier'
                      -> DeleteClusterSnapshotMessage
deleteClusterSnapshot p1 = DeleteClusterSnapshotMessage
    { _dcsmSnapshotIdentifier        = p1
    , _dcsmSnapshotClusterIdentifier = Nothing
    }

-- | The unique identifier of the cluster the snapshot was created from. This
-- parameter is required if your IAM user has a policy containing a snapshot
-- resource element that specifies anything other than * for the cluster
-- name. Constraints: Must be the name of valid cluster.
dcsmSnapshotClusterIdentifier :: Lens' DeleteClusterSnapshotMessage (Maybe Text)
dcsmSnapshotClusterIdentifier =
    lens _dcsmSnapshotClusterIdentifier
        (\s a -> s { _dcsmSnapshotClusterIdentifier = a })

-- | The unique identifier of the manual snapshot to be deleted. Constraints:
-- Must be the name of an existing snapshot that is in the available state.
dcsmSnapshotIdentifier :: Lens' DeleteClusterSnapshotMessage Text
dcsmSnapshotIdentifier =
    lens _dcsmSnapshotIdentifier (\s a -> s { _dcsmSnapshotIdentifier = a })

instance ToQuery DeleteClusterSnapshotMessage

instance ToPath DeleteClusterSnapshotMessage where
    toPath = const "/"

newtype DeleteClusterSnapshotResult = DeleteClusterSnapshotResult
    { _dcsrSnapshot :: Maybe Snapshot
    } deriving (Eq, Show, Generic)

-- | 'DeleteClusterSnapshotResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsrSnapshot' @::@ 'Maybe' 'Snapshot'
--
deleteClusterSnapshotResponse :: DeleteClusterSnapshotResult
deleteClusterSnapshotResponse = DeleteClusterSnapshotResult
    { _dcsrSnapshot = Nothing
    }

dcsrSnapshot :: Lens' DeleteClusterSnapshotResult (Maybe Snapshot)
dcsrSnapshot = lens _dcsrSnapshot (\s a -> s { _dcsrSnapshot = a })

instance FromXML DeleteClusterSnapshotResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteClusterSnapshotResult"

instance AWSRequest DeleteClusterSnapshotMessage where
    type Sv DeleteClusterSnapshotMessage = Redshift
    type Rs DeleteClusterSnapshotMessage = DeleteClusterSnapshotResult

    request  = post "DeleteClusterSnapshot"
    response = xmlResponse $ \h x -> DeleteClusterSnapshotResult
        <$> x %| "Snapshot"
