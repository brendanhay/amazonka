{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.ModifySnapshotCopyRetentionPeriod
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
module Network.AWS.Redshift.V2012_12_01.ModifySnapshotCopyRetentionPeriod
    (
    -- * Request
      ModifySnapshotCopyRetentionPeriod
    -- ** Request constructor
    , mkModifySnapshotCopyRetentionPeriodMessage
    -- ** Request lenses
    , mscrpmClusterIdentifier
    , mscrpmRetentionPeriod

    -- * Response
    , ModifySnapshotCopyRetentionPeriodResponse
    -- ** Response lenses
    , ccuCluster
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifySnapshotCopyRetentionPeriod' request.
mkModifySnapshotCopyRetentionPeriodMessage :: Text -- ^ 'mscrpmClusterIdentifier'
                                           -> Integer -- ^ 'mscrpmRetentionPeriod'
                                           -> ModifySnapshotCopyRetentionPeriod
mkModifySnapshotCopyRetentionPeriodMessage p1 p2 = ModifySnapshotCopyRetentionPeriod
    { _mscrpmClusterIdentifier = p1
    , _mscrpmRetentionPeriod = p2
    }
{-# INLINE mkModifySnapshotCopyRetentionPeriodMessage #-}

data ModifySnapshotCopyRetentionPeriod = ModifySnapshotCopyRetentionPeriod
    { _mscrpmClusterIdentifier :: Text
      -- ^ The unique identifier of the cluster for which you want to change
      -- the retention period for automated snapshots that are copied to a
      -- destination region. Constraints: Must be the valid name of an
      -- existing cluster that has cross-region snapshot copy enabled.
    , _mscrpmRetentionPeriod :: Integer
      -- ^ The number of days to retain automated snapshots in the
      -- destination region after they are copied from the source region.
      -- If you decrease the retention period for automated snapshots that
      -- are copied to a destination region, Amazon Redshift will delete
      -- any existing automated snapshots that were copied to the
      -- destination region and that fall outside of the new retention
      -- period. Constraints: Must be at least 1 and no more than 35.
    } deriving (Show, Generic)

-- | The unique identifier of the cluster for which you want to change the
-- retention period for automated snapshots that are copied to a destination
-- region. Constraints: Must be the valid name of an existing cluster that has
-- cross-region snapshot copy enabled.
mscrpmClusterIdentifier :: Lens' ModifySnapshotCopyRetentionPeriod (Text)
mscrpmClusterIdentifier = lens _mscrpmClusterIdentifier (\s a -> s { _mscrpmClusterIdentifier = a })
{-# INLINE mscrpmClusterIdentifier #-}

-- | The number of days to retain automated snapshots in the destination region
-- after they are copied from the source region. If you decrease the retention
-- period for automated snapshots that are copied to a destination region,
-- Amazon Redshift will delete any existing automated snapshots that were
-- copied to the destination region and that fall outside of the new retention
-- period. Constraints: Must be at least 1 and no more than 35.
mscrpmRetentionPeriod :: Lens' ModifySnapshotCopyRetentionPeriod (Integer)
mscrpmRetentionPeriod = lens _mscrpmRetentionPeriod (\s a -> s { _mscrpmRetentionPeriod = a })
{-# INLINE mscrpmRetentionPeriod #-}

instance ToQuery ModifySnapshotCopyRetentionPeriod where
    toQuery = genericQuery def

newtype ModifySnapshotCopyRetentionPeriodResponse = ModifySnapshotCopyRetentionPeriodResponse
    { _ccuCluster :: Maybe Cluster
      -- ^ Describes a cluster.
    } deriving (Show, Generic)

-- | Describes a cluster.
ccuCluster :: Lens' ModifySnapshotCopyRetentionPeriodResponse (Maybe Cluster)
ccuCluster = lens _ccuCluster (\s a -> s { _ccuCluster = a })
{-# INLINE ccuCluster #-}

instance FromXML ModifySnapshotCopyRetentionPeriodResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ModifySnapshotCopyRetentionPeriod where
    type Sv ModifySnapshotCopyRetentionPeriod = Redshift
    type Rs ModifySnapshotCopyRetentionPeriod = ModifySnapshotCopyRetentionPeriodResponse

    request = post "ModifySnapshotCopyRetentionPeriod"
    response _ = xmlResponse
