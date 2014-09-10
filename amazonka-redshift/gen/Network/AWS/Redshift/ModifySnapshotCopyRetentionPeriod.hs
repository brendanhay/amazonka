{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift
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
module Network.AWS.Redshift
    (
    -- * Request
      ModifySnapshotCopyRetentionPeriod
    -- ** Request constructor
    , mkModifySnapshotCopyRetentionPeriod
    -- ** Request lenses
    , mscrpClusterIdentifier
    , mscrpRetentionPeriod

    -- * Response
    , ModifySnapshotCopyRetentionPeriodResponse
    -- ** Response constructor
    , mkModifySnapshotCopyRetentionPeriodResponse
    -- ** Response lenses
    , mscrprCluster
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import Network.AWS.Prelude

-- | 
data ModifySnapshotCopyRetentionPeriod = ModifySnapshotCopyRetentionPeriod
    { _mscrpClusterIdentifier :: Text
    , _mscrpRetentionPeriod :: Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifySnapshotCopyRetentionPeriod' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ClusterIdentifier ::@ @Text@
--
-- * @RetentionPeriod ::@ @Integer@
--
mkModifySnapshotCopyRetentionPeriod :: Text -- ^ 'mscrpClusterIdentifier'
                                    -> Integer -- ^ 'mscrpRetentionPeriod'
                                    -> ModifySnapshotCopyRetentionPeriod
mkModifySnapshotCopyRetentionPeriod p1 p2 = ModifySnapshotCopyRetentionPeriod
    { _mscrpClusterIdentifier = p1
    , _mscrpRetentionPeriod = p2
    }

-- | The unique identifier of the cluster for which you want to change the
-- retention period for automated snapshots that are copied to a destination
-- region. Constraints: Must be the valid name of an existing cluster that has
-- cross-region snapshot copy enabled.
mscrpClusterIdentifier :: Lens' ModifySnapshotCopyRetentionPeriod Text
mscrpClusterIdentifier =
    lens _mscrpClusterIdentifier (\s a -> s { _mscrpClusterIdentifier = a })

-- | The number of days to retain automated snapshots in the destination region
-- after they are copied from the source region. If you decrease the retention
-- period for automated snapshots that are copied to a destination region,
-- Amazon Redshift will delete any existing automated snapshots that were
-- copied to the destination region and that fall outside of the new retention
-- period. Constraints: Must be at least 1 and no more than 35.
mscrpRetentionPeriod :: Lens' ModifySnapshotCopyRetentionPeriod Integer
mscrpRetentionPeriod =
    lens _mscrpRetentionPeriod (\s a -> s { _mscrpRetentionPeriod = a })

instance ToQuery ModifySnapshotCopyRetentionPeriod where
    toQuery = genericQuery def

newtype ModifySnapshotCopyRetentionPeriodResponse = ModifySnapshotCopyRetentionPeriodResponse
    { _mscrprCluster :: Maybe Cluster
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifySnapshotCopyRetentionPeriodResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Cluster ::@ @Maybe Cluster@
--
mkModifySnapshotCopyRetentionPeriodResponse :: ModifySnapshotCopyRetentionPeriodResponse
mkModifySnapshotCopyRetentionPeriodResponse = ModifySnapshotCopyRetentionPeriodResponse
    { _mscrprCluster = Nothing
    }

-- | Describes a cluster.
mscrprCluster :: Lens' ModifySnapshotCopyRetentionPeriodResponse (Maybe Cluster)
mscrprCluster = lens _mscrprCluster (\s a -> s { _mscrprCluster = a })

instance FromXML ModifySnapshotCopyRetentionPeriodResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ModifySnapshotCopyRetentionPeriod where
    type Sv ModifySnapshotCopyRetentionPeriod = Redshift
    type Rs ModifySnapshotCopyRetentionPeriod = ModifySnapshotCopyRetentionPeriodResponse

    request = post "ModifySnapshotCopyRetentionPeriod"
    response _ = xmlResponse
