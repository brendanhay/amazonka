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
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_ModifySnapshotCopyRetentionPeriod.html>
module Network.AWS.Redshift.ModifySnapshotCopyRetentionPeriod
    (
    -- * Request
      ModifySnapshotCopyRetentionPeriod
    -- ** Request constructor
    , modifySnapshotCopyRetentionPeriod
    -- ** Request lenses
    , mscrpClusterIdentifier
    , mscrpRetentionPeriod

    -- * Response
    , ModifySnapshotCopyRetentionPeriodResponse
    -- ** Response constructor
    , modifySnapshotCopyRetentionPeriodResponse
    -- ** Response lenses
    , mscrprCluster
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data ModifySnapshotCopyRetentionPeriod = ModifySnapshotCopyRetentionPeriod
    { _mscrpClusterIdentifier :: Text
    , _mscrpRetentionPeriod   :: Int
    } deriving (Eq, Ord, Show)

-- | 'ModifySnapshotCopyRetentionPeriod' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mscrpClusterIdentifier' @::@ 'Text'
--
-- * 'mscrpRetentionPeriod' @::@ 'Int'
--
modifySnapshotCopyRetentionPeriod :: Text -- ^ 'mscrpClusterIdentifier'
                                  -> Int -- ^ 'mscrpRetentionPeriod'
                                  -> ModifySnapshotCopyRetentionPeriod
modifySnapshotCopyRetentionPeriod p1 p2 = ModifySnapshotCopyRetentionPeriod
    { _mscrpClusterIdentifier = p1
    , _mscrpRetentionPeriod   = p2
    }

-- | The unique identifier of the cluster for which you want to change the
-- retention period for automated snapshots that are copied to a destination
-- region. Constraints: Must be the valid name of an existing cluster that
-- has cross-region snapshot copy enabled.
mscrpClusterIdentifier :: Lens' ModifySnapshotCopyRetentionPeriod Text
mscrpClusterIdentifier =
    lens _mscrpClusterIdentifier (\s a -> s { _mscrpClusterIdentifier = a })

-- | The number of days to retain automated snapshots in the destination
-- region after they are copied from the source region. If you decrease the
-- retention period for automated snapshots that are copied to a destination
-- region, Amazon Redshift will delete any existing automated snapshots that
-- were copied to the destination region and that fall outside of the new
-- retention period. Constraints: Must be at least 1 and no more than 35.
mscrpRetentionPeriod :: Lens' ModifySnapshotCopyRetentionPeriod Int
mscrpRetentionPeriod =
    lens _mscrpRetentionPeriod (\s a -> s { _mscrpRetentionPeriod = a })

newtype ModifySnapshotCopyRetentionPeriodResponse = ModifySnapshotCopyRetentionPeriodResponse
    { _mscrprCluster :: Maybe Cluster
    } deriving (Eq, Show)

-- | 'ModifySnapshotCopyRetentionPeriodResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mscrprCluster' @::@ 'Maybe' 'Cluster'
--
modifySnapshotCopyRetentionPeriodResponse :: ModifySnapshotCopyRetentionPeriodResponse
modifySnapshotCopyRetentionPeriodResponse = ModifySnapshotCopyRetentionPeriodResponse
    { _mscrprCluster = Nothing
    }

mscrprCluster :: Lens' ModifySnapshotCopyRetentionPeriodResponse (Maybe Cluster)
mscrprCluster = lens _mscrprCluster (\s a -> s { _mscrprCluster = a })

instance ToPath ModifySnapshotCopyRetentionPeriod where
    toPath = const "/"

instance ToQuery ModifySnapshotCopyRetentionPeriod where
    toQuery ModifySnapshotCopyRetentionPeriod{..} = mconcat
        [ "ClusterIdentifier" =? _mscrpClusterIdentifier
        , "RetentionPeriod"   =? _mscrpRetentionPeriod
        ]

instance ToHeaders ModifySnapshotCopyRetentionPeriod

instance AWSRequest ModifySnapshotCopyRetentionPeriod where
    type Sv ModifySnapshotCopyRetentionPeriod = Redshift
    type Rs ModifySnapshotCopyRetentionPeriod = ModifySnapshotCopyRetentionPeriodResponse

    request  = post "ModifySnapshotCopyRetentionPeriod"
    response = xmlResponse

instance FromXML ModifySnapshotCopyRetentionPeriodResponse where
    parseXML = withElement "ModifySnapshotCopyRetentionPeriodResult" $ \x -> ModifySnapshotCopyRetentionPeriodResponse
        <$> x .@? "Cluster"
