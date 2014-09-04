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
    , mkDisableSnapshotCopyMessage
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

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DisableSnapshotCopy' request.
mkDisableSnapshotCopyMessage :: Text -- ^ 'dscmClusterIdentifier'
                             -> DisableSnapshotCopy
mkDisableSnapshotCopyMessage p1 = DisableSnapshotCopy
    { _dscmClusterIdentifier = p1
    }
{-# INLINE mkDisableSnapshotCopyMessage #-}

newtype DisableSnapshotCopy = DisableSnapshotCopy
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
dscmClusterIdentifier :: Lens' DisableSnapshotCopy (Text)
dscmClusterIdentifier = lens _dscmClusterIdentifier (\s a -> s { _dscmClusterIdentifier = a })
{-# INLINE dscmClusterIdentifier #-}

instance ToQuery DisableSnapshotCopy where
    toQuery = genericQuery def

newtype DisableSnapshotCopyResponse = DisableSnapshotCopyResponse
    { _cyCluster :: Maybe Cluster
      -- ^ Describes a cluster.
    } deriving (Show, Generic)

-- | Describes a cluster.
cyCluster :: Lens' DisableSnapshotCopyResponse (Maybe Cluster)
cyCluster = lens _cyCluster (\s a -> s { _cyCluster = a })
{-# INLINE cyCluster #-}

instance FromXML DisableSnapshotCopyResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DisableSnapshotCopy where
    type Sv DisableSnapshotCopy = Redshift
    type Rs DisableSnapshotCopy = DisableSnapshotCopyResponse

    request = post "DisableSnapshotCopy"
    response _ = xmlResponse
