{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.EnableSnapshotCopy
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
module Network.AWS.Redshift.V2012_12_01.EnableSnapshotCopy
    (
    -- * Request
      EnableSnapshotCopy
    -- ** Request constructor
    , enableSnapshotCopy
    -- ** Request lenses
    , escmClusterIdentifier
    , escmDestinationRegion
    , escmRetentionPeriod

    -- * Response
    , EnableSnapshotCopyResponse
    -- ** Response lenses
    , czCluster
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'EnableSnapshotCopy' request.
enableSnapshotCopy :: Text -- ^ 'escmClusterIdentifier'
                   -> Text -- ^ 'escmDestinationRegion'
                   -> EnableSnapshotCopy
enableSnapshotCopy p1 p2 = EnableSnapshotCopy
    { _escmClusterIdentifier = p1
    , _escmDestinationRegion = p2
    , _escmRetentionPeriod = Nothing
    }
{-# INLINE enableSnapshotCopy #-}

data EnableSnapshotCopy = EnableSnapshotCopy
    { _escmClusterIdentifier :: Text
      -- ^ The unique identifier of the source cluster to copy snapshots
      -- from. Constraints: Must be the valid name of an existing cluster
      -- that does not already have cross-region snapshot copy enabled.
    , _escmDestinationRegion :: Text
      -- ^ The destination region that you want to copy snapshots to.
      -- Constraints: Must be the name of a valid region. For more
      -- information, see Regions and Endpoints in the Amazon Web Services
      -- General Reference.
    , _escmRetentionPeriod :: Maybe Integer
      -- ^ The number of days to retain automated snapshots in the
      -- destination region after they are copied from the source region.
      -- Default: 7. Constraints: Must be at least 1 and no more than 35.
    } deriving (Show, Generic)

-- | The unique identifier of the source cluster to copy snapshots from.
-- Constraints: Must be the valid name of an existing cluster that does not
-- already have cross-region snapshot copy enabled.
escmClusterIdentifier :: Lens' EnableSnapshotCopy (Text)
escmClusterIdentifier f x =
    f (_escmClusterIdentifier x)
        <&> \y -> x { _escmClusterIdentifier = y }
{-# INLINE escmClusterIdentifier #-}

-- | The destination region that you want to copy snapshots to. Constraints:
-- Must be the name of a valid region. For more information, see Regions and
-- Endpoints in the Amazon Web Services General Reference.
escmDestinationRegion :: Lens' EnableSnapshotCopy (Text)
escmDestinationRegion f x =
    f (_escmDestinationRegion x)
        <&> \y -> x { _escmDestinationRegion = y }
{-# INLINE escmDestinationRegion #-}

-- | The number of days to retain automated snapshots in the destination region
-- after they are copied from the source region. Default: 7. Constraints: Must
-- be at least 1 and no more than 35.
escmRetentionPeriod :: Lens' EnableSnapshotCopy (Maybe Integer)
escmRetentionPeriod f x =
    f (_escmRetentionPeriod x)
        <&> \y -> x { _escmRetentionPeriod = y }
{-# INLINE escmRetentionPeriod #-}

instance ToQuery EnableSnapshotCopy where
    toQuery = genericQuery def

data EnableSnapshotCopyResponse = EnableSnapshotCopyResponse
    { _czCluster :: Maybe Cluster
      -- ^ Describes a cluster.
    } deriving (Show, Generic)

-- | Describes a cluster.
czCluster :: Lens' EnableSnapshotCopyResponse (Maybe Cluster)
czCluster f x =
    f (_czCluster x)
        <&> \y -> x { _czCluster = y }
{-# INLINE czCluster #-}

instance FromXML EnableSnapshotCopyResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest EnableSnapshotCopy where
    type Sv EnableSnapshotCopy = Redshift
    type Rs EnableSnapshotCopy = EnableSnapshotCopyResponse

    request = post "EnableSnapshotCopy"
    response _ = xmlResponse
