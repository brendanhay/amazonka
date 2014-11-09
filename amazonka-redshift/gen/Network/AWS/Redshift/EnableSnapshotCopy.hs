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

-- Module      : Network.AWS.Redshift.EnableSnapshotCopy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Enables the automatic copy of snapshots from one region to another region
-- for a specified cluster.
module Network.AWS.Redshift.EnableSnapshotCopy
    (
    -- * Request
      EnableSnapshotCopyMessage
    -- ** Request constructor
    , enableSnapshotCopyMessage
    -- ** Request lenses
    , escmClusterIdentifier
    , escmDestinationRegion
    , escmRetentionPeriod

    -- * Response
    , EnableSnapshotCopyResult
    -- ** Response constructor
    , enableSnapshotCopyResult
    -- ** Response lenses
    , escrCluster
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data EnableSnapshotCopyMessage = EnableSnapshotCopyMessage
    { _escmClusterIdentifier :: Text
    , _escmDestinationRegion :: Text
    , _escmRetentionPeriod   :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'EnableSnapshotCopyMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'escmClusterIdentifier' @::@ 'Text'
--
-- * 'escmDestinationRegion' @::@ 'Text'
--
-- * 'escmRetentionPeriod' @::@ 'Maybe' 'Int'
--
enableSnapshotCopyMessage :: Text -- ^ 'escmClusterIdentifier'
                          -> Text -- ^ 'escmDestinationRegion'
                          -> EnableSnapshotCopyMessage
enableSnapshotCopyMessage p1 p2 = EnableSnapshotCopyMessage
    { _escmClusterIdentifier = p1
    , _escmDestinationRegion = p2
    , _escmRetentionPeriod   = Nothing
    }

-- | The unique identifier of the source cluster to copy snapshots from.
-- Constraints: Must be the valid name of an existing cluster that does not
-- already have cross-region snapshot copy enabled.
escmClusterIdentifier :: Lens' EnableSnapshotCopyMessage Text
escmClusterIdentifier =
    lens _escmClusterIdentifier (\s a -> s { _escmClusterIdentifier = a })

-- | The destination region that you want to copy snapshots to. Constraints:
-- Must be the name of a valid region. For more information, see Regions and
-- Endpoints in the Amazon Web Services General Reference.
escmDestinationRegion :: Lens' EnableSnapshotCopyMessage Text
escmDestinationRegion =
    lens _escmDestinationRegion (\s a -> s { _escmDestinationRegion = a })

-- | The number of days to retain automated snapshots in the destination
-- region after they are copied from the source region. Default: 7.
-- Constraints: Must be at least 1 and no more than 35.
escmRetentionPeriod :: Lens' EnableSnapshotCopyMessage (Maybe Int)
escmRetentionPeriod =
    lens _escmRetentionPeriod (\s a -> s { _escmRetentionPeriod = a })

instance ToPath EnableSnapshotCopyMessage where
    toPath = const "/"

instance ToQuery EnableSnapshotCopyMessage

newtype EnableSnapshotCopyResult = EnableSnapshotCopyResult
    { _escrCluster :: Maybe Cluster
    } deriving (Eq, Show, Generic)

-- | 'EnableSnapshotCopyResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'escrCluster' @::@ 'Maybe' 'Cluster'
--
enableSnapshotCopyResult :: EnableSnapshotCopyResult
enableSnapshotCopyResult = EnableSnapshotCopyResult
    { _escrCluster = Nothing
    }

escrCluster :: Lens' EnableSnapshotCopyResult (Maybe Cluster)
escrCluster = lens _escrCluster (\s a -> s { _escrCluster = a })

instance AWSRequest EnableSnapshotCopyMessage where
    type Sv EnableSnapshotCopyMessage = Redshift
    type Rs EnableSnapshotCopyMessage = EnableSnapshotCopyResult

    request  = post "EnableSnapshotCopy"
    response = const . xmlResponse $ \h x -> EnableSnapshotCopyResult
newtype
