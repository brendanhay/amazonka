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

-- Module      : Network.AWS.Redshift.ModifySnapshotCopyRetentionPeriod
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies the number of days to retain automated snapshots in the
-- destination region after they are copied from the source region.
module Network.AWS.Redshift.ModifySnapshotCopyRetentionPeriod
    (
    -- * Request
      ModifySnapshotCopyRetentionPeriodMessage
    -- ** Request constructor
    , modifySnapshotCopyRetentionPeriod
    -- ** Request lenses
    , mscrpmClusterIdentifier
    , mscrpmRetentionPeriod

    -- * Response
    , ModifySnapshotCopyRetentionPeriodResult
    -- ** Response constructor
    , modifySnapshotCopyRetentionPeriodResponse
    -- ** Response lenses
    , mscrprCluster
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data ModifySnapshotCopyRetentionPeriodMessage = ModifySnapshotCopyRetentionPeriodMessage
    { _mscrpmClusterIdentifier :: Text
    , _mscrpmRetentionPeriod   :: Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'ModifySnapshotCopyRetentionPeriodMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mscrpmClusterIdentifier' @::@ 'Text'
--
-- * 'mscrpmRetentionPeriod' @::@ 'Int'
--
modifySnapshotCopyRetentionPeriod :: Text -- ^ 'mscrpmClusterIdentifier'
                                  -> Int -- ^ 'mscrpmRetentionPeriod'
                                  -> ModifySnapshotCopyRetentionPeriodMessage
modifySnapshotCopyRetentionPeriod p1 p2 = ModifySnapshotCopyRetentionPeriodMessage
    { _mscrpmClusterIdentifier = p1
    , _mscrpmRetentionPeriod   = p2
    }

-- | The unique identifier of the cluster for which you want to change the
-- retention period for automated snapshots that are copied to a destination
-- region. Constraints: Must be the valid name of an existing cluster that
-- has cross-region snapshot copy enabled.
mscrpmClusterIdentifier :: Lens' ModifySnapshotCopyRetentionPeriodMessage Text
mscrpmClusterIdentifier =
    lens _mscrpmClusterIdentifier (\s a -> s { _mscrpmClusterIdentifier = a })

-- | The number of days to retain automated snapshots in the destination
-- region after they are copied from the source region. If you decrease the
-- retention period for automated snapshots that are copied to a destination
-- region, Amazon Redshift will delete any existing automated snapshots that
-- were copied to the destination region and that fall outside of the new
-- retention period. Constraints: Must be at least 1 and no more than 35.
mscrpmRetentionPeriod :: Lens' ModifySnapshotCopyRetentionPeriodMessage Int
mscrpmRetentionPeriod =
    lens _mscrpmRetentionPeriod (\s a -> s { _mscrpmRetentionPeriod = a })

instance ToPath ModifySnapshotCopyRetentionPeriodMessage where
    toPath = const "/"

instance ToQuery ModifySnapshotCopyRetentionPeriodMessage

newtype ModifySnapshotCopyRetentionPeriodResult = ModifySnapshotCopyRetentionPeriodResult
    { _mscrprCluster :: Maybe Cluster
    } deriving (Eq, Show, Generic)

-- | 'ModifySnapshotCopyRetentionPeriodResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mscrprCluster' @::@ 'Maybe' 'Cluster'
--
modifySnapshotCopyRetentionPeriodResponse :: ModifySnapshotCopyRetentionPeriodResult
modifySnapshotCopyRetentionPeriodResponse = ModifySnapshotCopyRetentionPeriodResult
    { _mscrprCluster = Nothing
    }

mscrprCluster :: Lens' ModifySnapshotCopyRetentionPeriodResult (Maybe Cluster)
mscrprCluster = lens _mscrprCluster (\s a -> s { _mscrprCluster = a })

instance AWSRequest ModifySnapshotCopyRetentionPeriodMessage where
    type Sv ModifySnapshotCopyRetentionPeriodMessage = Redshift
    type Rs ModifySnapshotCopyRetentionPeriodMessage = ModifySnapshotCopyRetentionPeriodResult

    request  = post "ModifySnapshotCopyRetentionPeriod"
    response = xmlResponse $ \h x -> ModifySnapshotCopyRetentionPeriodResult
        <$> x %| "Cluster"
