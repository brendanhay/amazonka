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
    , mkDisableSnapshotCopy
    -- ** Request lenses
    , dscClusterIdentifier

    -- * Response
    , DisableSnapshotCopyResponse
    -- ** Response constructor
    , mkDisableSnapshotCopyResponse
    -- ** Response lenses
    , dscrCluster
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | 
newtype DisableSnapshotCopy = DisableSnapshotCopy
    { _dscClusterIdentifier :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DisableSnapshotCopy' request.
mkDisableSnapshotCopy :: Text -- ^ 'dscClusterIdentifier'
                      -> DisableSnapshotCopy
mkDisableSnapshotCopy p1 = DisableSnapshotCopy
    { _dscClusterIdentifier = p1
    }

-- | The unique identifier of the source cluster that you want to disable
-- copying of snapshots to a destination region. Constraints: Must be the
-- valid name of an existing cluster that has cross-region snapshot copy
-- enabled.
dscClusterIdentifier :: Lens' DisableSnapshotCopy Text
dscClusterIdentifier =
    lens _dscClusterIdentifier (\s a -> s { _dscClusterIdentifier = a })

instance ToQuery DisableSnapshotCopy where
    toQuery = genericQuery def

newtype DisableSnapshotCopyResponse = DisableSnapshotCopyResponse
    { _dscrCluster :: Maybe Cluster
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DisableSnapshotCopyResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDisableSnapshotCopyResponse :: DisableSnapshotCopyResponse
mkDisableSnapshotCopyResponse = DisableSnapshotCopyResponse
    { _dscrCluster = Nothing
    }

-- | Describes a cluster.
dscrCluster :: Lens' DisableSnapshotCopyResponse (Maybe Cluster)
dscrCluster = lens _dscrCluster (\s a -> s { _dscrCluster = a })

instance FromXML DisableSnapshotCopyResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DisableSnapshotCopy where
    type Sv DisableSnapshotCopy = Redshift
    type Rs DisableSnapshotCopy = DisableSnapshotCopyResponse

    request = post "DisableSnapshotCopy"
    response _ = xmlResponse
