{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.CopySnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CopySnapshot operation makes a copy of an existing snapshot.
module Network.AWS.ElastiCache.CopySnapshot
    (
    -- * Request
      CopySnapshot
    -- ** Request constructor
    , copySnapshot
    -- ** Request lenses
    , csSourceSnapshotName
    , csTargetSnapshotName

    -- * Response
    , CopySnapshotResponse
    -- ** Response constructor
    , copySnapshotResponse
    -- ** Response lenses
    , csrSnapshot
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import Network.AWS.Prelude

-- | Represents the input of a CopySnapshotMessage operation.
data CopySnapshot = CopySnapshot
    { _csSourceSnapshotName :: Text
    , _csTargetSnapshotName :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CopySnapshot' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SourceSnapshotName ::@ @Text@
--
-- * @TargetSnapshotName ::@ @Text@
--
copySnapshot :: Text -- ^ 'csSourceSnapshotName'
             -> Text -- ^ 'csTargetSnapshotName'
             -> CopySnapshot
copySnapshot p1 p2 = CopySnapshot
    { _csSourceSnapshotName = p1
    , _csTargetSnapshotName = p2
    }

-- | The name of an existing snapshot from which to copy.
csSourceSnapshotName :: Lens' CopySnapshot Text
csSourceSnapshotName =
    lens _csSourceSnapshotName (\s a -> s { _csSourceSnapshotName = a })

-- | A name for the copied snapshot.
csTargetSnapshotName :: Lens' CopySnapshot Text
csTargetSnapshotName =
    lens _csTargetSnapshotName (\s a -> s { _csTargetSnapshotName = a })

instance ToQuery CopySnapshot where
    toQuery = genericQuery def

newtype CopySnapshotResponse = CopySnapshotResponse
    { _csrSnapshot :: Maybe Snapshot
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CopySnapshotResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Snapshot ::@ @Maybe Snapshot@
--
copySnapshotResponse :: CopySnapshotResponse
copySnapshotResponse = CopySnapshotResponse
    { _csrSnapshot = Nothing
    }

-- | Represents a copy of an entire cache cluster as of the time when the
-- snapshot was taken.
csrSnapshot :: Lens' CopySnapshotResponse (Maybe Snapshot)
csrSnapshot = lens _csrSnapshot (\s a -> s { _csrSnapshot = a })

instance FromXML CopySnapshotResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CopySnapshot where
    type Sv CopySnapshot = ElastiCache
    type Rs CopySnapshot = CopySnapshotResponse

    request = post "CopySnapshot"
    response _ = xmlResponse
