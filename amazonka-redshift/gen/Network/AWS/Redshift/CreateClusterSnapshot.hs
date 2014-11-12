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
module Network.AWS.Redshift.CreateClusterSnapshot
    (
    -- * Request
      CreateClusterSnapshotMessage
    -- ** Request constructor
    , createClusterSnapshotMessage
    -- ** Request lenses
    , ccsmClusterIdentifier
    , ccsmSnapshotIdentifier

    -- * Response
    , CreateClusterSnapshotResult
    -- ** Response constructor
    , createClusterSnapshotResult
    -- ** Response lenses
    , ccsr1Snapshot
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data CreateClusterSnapshotMessage = CreateClusterSnapshotMessage
    { _ccsmClusterIdentifier  :: Text
    , _ccsmSnapshotIdentifier :: Text
    } (Eq, Ord, Show, Generic)

-- | 'CreateClusterSnapshotMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccsmClusterIdentifier' @::@ 'Text'
--
-- * 'ccsmSnapshotIdentifier' @::@ 'Text'
--
createClusterSnapshotMessage :: Text -- ^ 'ccsmSnapshotIdentifier'
                             -> Text -- ^ 'ccsmClusterIdentifier'
                             -> CreateClusterSnapshotMessage
createClusterSnapshotMessage p1 p2 = CreateClusterSnapshotMessage
    { _ccsmSnapshotIdentifier = p1
    , _ccsmClusterIdentifier  = p2
    }

-- | The cluster identifier for which you want a snapshot.
ccsmClusterIdentifier :: Lens' CreateClusterSnapshotMessage Text
ccsmClusterIdentifier =
    lens _ccsmClusterIdentifier (\s a -> s { _ccsmClusterIdentifier = a })

-- | A unique identifier for the snapshot that you are requesting. This
-- identifier must be unique for all snapshots within the AWS account.
-- Constraints: Cannot be null, empty, or blank Must contain from 1 to 255
-- alphanumeric characters or hyphens First character must be a letter
-- Cannot end with a hyphen or contain two consecutive hyphens Example:
-- my-snapshot-id.
ccsmSnapshotIdentifier :: Lens' CreateClusterSnapshotMessage Text
ccsmSnapshotIdentifier =
    lens _ccsmSnapshotIdentifier (\s a -> s { _ccsmSnapshotIdentifier = a })
instance ToQuery CreateClusterSnapshotMessage

instance ToPath CreateClusterSnapshotMessage where
    toPath = const "/"

newtype CreateClusterSnapshotResult = CreateClusterSnapshotResult
    { _ccsr1Snapshot :: Maybe Snapshot
    } (Eq, Show, Generic)

-- | 'CreateClusterSnapshotResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccsr1Snapshot' @::@ 'Maybe' 'Snapshot'
--
createClusterSnapshotResult :: CreateClusterSnapshotResult
createClusterSnapshotResult = CreateClusterSnapshotResult
    { _ccsr1Snapshot = Nothing
    }

ccsr1Snapshot :: Lens' CreateClusterSnapshotResult (Maybe Snapshot)
ccsr1Snapshot = lens _ccsr1Snapshot (\s a -> s { _ccsr1Snapshot = a })

instance FromXML CreateClusterSnapshotResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateClusterSnapshotResult"

instance AWSRequest CreateClusterSnapshotMessage where
    type Sv CreateClusterSnapshotMessage = Redshift
    type Rs CreateClusterSnapshotMessage = CreateClusterSnapshotResult

    request  = post "CreateClusterSnapshot"
    response = xmlResponse $ \h x -> CreateClusterSnapshotResult
        <$> x %| "Snapshot"
