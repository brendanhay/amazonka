{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.DisableSnapshotCopy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Disables the automatic copying of snapshots from one region to another
-- region for a specified cluster.
module Network.AWS.Redshift.V2012_12_01.DisableSnapshotCopy
    (
    -- * Request
      DisableSnapshotCopy
    -- ** Request constructor
    , disableSnapshotCopy
    -- ** Request lenses
    , dscmClusterIdentifier

    -- * Response
    , DisableSnapshotCopyResponse
    -- ** Response lenses
    , cyCluster
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DisableSnapshotCopy' request.
disableSnapshotCopy :: Text -- ^ 'dscmClusterIdentifier'
                    -> DisableSnapshotCopy
disableSnapshotCopy p1 = DisableSnapshotCopy
    { _dscmClusterIdentifier = p1
    }

data DisableSnapshotCopy = DisableSnapshotCopy
    { _dscmClusterIdentifier :: Text
      -- ^ The unique identifier of the source cluster that you want to
      -- disable copying of snapshots to a destination region.
      -- Constraints: Must be the valid name of an existing cluster that
      -- has cross-region snapshot copy enabled.
    } deriving (Show, Generic)

-- | The unique identifier of the source cluster that you want to disable
-- copying of snapshots to a destination region. Constraints: Must be the
-- valid name of an existing cluster that has cross-region snapshot copy
-- enabled.
dscmClusterIdentifier
    :: Functor f
    => (Text
    -> f (Text))
    -> DisableSnapshotCopy
    -> f DisableSnapshotCopy
dscmClusterIdentifier f x =
    (\y -> x { _dscmClusterIdentifier = y })
       <$> f (_dscmClusterIdentifier x)
{-# INLINE dscmClusterIdentifier #-}

instance ToQuery DisableSnapshotCopy where
    toQuery = genericQuery def

data DisableSnapshotCopyResponse = DisableSnapshotCopyResponse
    { _cyCluster :: Maybe Cluster
      -- ^ Describes a cluster.
    } deriving (Show, Generic)

-- | Describes a cluster.
cyCluster
    :: Functor f
    => (Maybe Cluster
    -> f (Maybe Cluster))
    -> DisableSnapshotCopyResponse
    -> f DisableSnapshotCopyResponse
cyCluster f x =
    (\y -> x { _cyCluster = y })
       <$> f (_cyCluster x)
{-# INLINE cyCluster #-}

instance FromXML DisableSnapshotCopyResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DisableSnapshotCopy where
    type Sv DisableSnapshotCopy = Redshift
    type Rs DisableSnapshotCopy = DisableSnapshotCopyResponse

    request = post "DisableSnapshotCopy"
    response _ = xmlResponse
