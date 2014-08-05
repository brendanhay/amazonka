{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElastiCache.V2014_03_24.CreateSnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreateSnapshot operation creates a copy of an entire cache cluster at a
-- specific moment in time.
module Network.AWS.ElastiCache.V2014_03_24.CreateSnapshot where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_03_24.Types
import Network.AWS.Prelude

data CreateSnapshot = CreateSnapshot
    { _csoCacheClusterId :: Text
      -- ^ The identifier of an existing cache cluster. The snapshot will be
      -- created from this cache cluster.
    , _csoSnapshotName :: Text
      -- ^ A name for the snapshot being created.
    } deriving (Show, Generic)

makeLenses ''CreateSnapshot

instance ToQuery CreateSnapshot where
    toQuery = genericToQuery def

data CreateSnapshotResponse = CreateSnapshotResponse
    { _sssssssssssssssssssssrSnapshot :: Maybe Snapshot
      -- ^ Represents a copy of an entire cache cluster as of the time when
      -- the snapshot was taken.
    } deriving (Show, Generic)

makeLenses ''CreateSnapshotResponse

instance AWSRequest CreateSnapshot where
    type Sv CreateSnapshot = ElastiCache
    type Rs CreateSnapshot = CreateSnapshotResponse

    request = post "CreateSnapshot"
    response _ = cursorResponse $ \hs xml ->
        pure CreateSnapshotResponse
            <*> xml %|? "Snapshot"
