{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.CreateClusterSnapshot
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
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_CreateClusterSnapshot.html>
module Network.AWS.Redshift.CreateClusterSnapshot
    (
    -- * Request
      CreateClusterSnapshot
    -- ** Request constructor
    , createClusterSnapshot
    -- ** Request lenses
    , ccsClusterIdentifier
    , ccsSnapshotIdentifier

    -- * Response
    , CreateClusterSnapshotResponse
    -- ** Response constructor
    , createClusterSnapshotResponse
    -- ** Response lenses
    , ccsr1Snapshot
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data CreateClusterSnapshot = CreateClusterSnapshot
    { _ccsClusterIdentifier  :: Text
    , _ccsSnapshotIdentifier :: Text
    } deriving (Eq, Ord, Show)

-- | 'CreateClusterSnapshot' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccsClusterIdentifier' @::@ 'Text'
--
-- * 'ccsSnapshotIdentifier' @::@ 'Text'
--
createClusterSnapshot :: Text -- ^ 'ccsSnapshotIdentifier'
                      -> Text -- ^ 'ccsClusterIdentifier'
                      -> CreateClusterSnapshot
createClusterSnapshot p1 p2 = CreateClusterSnapshot
    { _ccsSnapshotIdentifier = p1
    , _ccsClusterIdentifier  = p2
    }

-- | The cluster identifier for which you want a snapshot.
ccsClusterIdentifier :: Lens' CreateClusterSnapshot Text
ccsClusterIdentifier =
    lens _ccsClusterIdentifier (\s a -> s { _ccsClusterIdentifier = a })

-- | A unique identifier for the snapshot that you are requesting. This
-- identifier must be unique for all snapshots within the AWS account.
-- Constraints: Cannot be null, empty, or blank Must contain from 1 to 255
-- alphanumeric characters or hyphens First character must be a letter
-- Cannot end with a hyphen or contain two consecutive hyphens Example:
-- my-snapshot-id.
ccsSnapshotIdentifier :: Lens' CreateClusterSnapshot Text
ccsSnapshotIdentifier =
    lens _ccsSnapshotIdentifier (\s a -> s { _ccsSnapshotIdentifier = a })

newtype CreateClusterSnapshotResponse = CreateClusterSnapshotResponse
    { _ccsr1Snapshot :: Maybe Snapshot
    } deriving (Eq, Show)

-- | 'CreateClusterSnapshotResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccsr1Snapshot' @::@ 'Maybe' 'Snapshot'
--
createClusterSnapshotResponse :: CreateClusterSnapshotResponse
createClusterSnapshotResponse = CreateClusterSnapshotResponse
    { _ccsr1Snapshot = Nothing
    }

ccsr1Snapshot :: Lens' CreateClusterSnapshotResponse (Maybe Snapshot)
ccsr1Snapshot = lens _ccsr1Snapshot (\s a -> s { _ccsr1Snapshot = a })

instance ToPath CreateClusterSnapshot where
    toPath = const "/"

instance ToQuery CreateClusterSnapshot where
    toQuery CreateClusterSnapshot{..} = mconcat
        [ "ClusterIdentifier"  =? _ccsClusterIdentifier
        , "SnapshotIdentifier" =? _ccsSnapshotIdentifier
        ]

instance ToHeaders CreateClusterSnapshot

instance AWSRequest CreateClusterSnapshot where
    type Sv CreateClusterSnapshot = Redshift
    type Rs CreateClusterSnapshot = CreateClusterSnapshotResponse

    request  = post "CreateClusterSnapshot"
    response = xmlResponse

instance FromXML CreateClusterSnapshotResponse where
    parseXML = withElement "CreateClusterSnapshotResult" $ \x -> CreateClusterSnapshotResponse
        <$> x .@? "Snapshot"


Some kind of operator / class to check the types whether to continue?
