{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.CopySnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CopySnapshot operation makes a copy of an existing snapshot.
module Network.AWS.ElastiCache.V2014_07_15.CopySnapshot
    (
    -- * Request
      CopySnapshot
    -- ** Request constructor
    , mkCopySnapshotMessage
    -- ** Request lenses
    , csmSourceSnapshotName
    , csmTargetSnapshotName

    -- * Response
    , CopySnapshotResponse
    -- ** Response lenses
    , swSnapshot
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CopySnapshot' request.
mkCopySnapshotMessage :: Text -- ^ 'csmSourceSnapshotName'
                      -> Text -- ^ 'csmTargetSnapshotName'
                      -> CopySnapshot
mkCopySnapshotMessage p1 p2 = CopySnapshot
    { _csmSourceSnapshotName = p1
    , _csmTargetSnapshotName = p2
    }
{-# INLINE mkCopySnapshotMessage #-}

data CopySnapshot = CopySnapshot
    { _csmSourceSnapshotName :: Text
      -- ^ The name of an existing snapshot from which to copy.
    , _csmTargetSnapshotName :: Text
      -- ^ A name for the copied snapshot.
    } deriving (Show, Generic)

-- | The name of an existing snapshot from which to copy.
csmSourceSnapshotName :: Lens' CopySnapshot (Text)
csmSourceSnapshotName = lens _csmSourceSnapshotName (\s a -> s { _csmSourceSnapshotName = a })
{-# INLINE csmSourceSnapshotName #-}

-- | A name for the copied snapshot.
csmTargetSnapshotName :: Lens' CopySnapshot (Text)
csmTargetSnapshotName = lens _csmTargetSnapshotName (\s a -> s { _csmTargetSnapshotName = a })
{-# INLINE csmTargetSnapshotName #-}

instance ToQuery CopySnapshot where
    toQuery = genericQuery def

newtype CopySnapshotResponse = CopySnapshotResponse
    { _swSnapshot :: Maybe Snapshot
      -- ^ Represents a copy of an entire cache cluster as of the time when
      -- the snapshot was taken.
    } deriving (Show, Generic)

-- | Represents a copy of an entire cache cluster as of the time when the
-- snapshot was taken.
swSnapshot :: Lens' CopySnapshotResponse (Maybe Snapshot)
swSnapshot = lens _swSnapshot (\s a -> s { _swSnapshot = a })
{-# INLINE swSnapshot #-}

instance FromXML CopySnapshotResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CopySnapshot where
    type Sv CopySnapshot = ElastiCache
    type Rs CopySnapshot = CopySnapshotResponse

    request = post "CopySnapshot"
    response _ = xmlResponse
