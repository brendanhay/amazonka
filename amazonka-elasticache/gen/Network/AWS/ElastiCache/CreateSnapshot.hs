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

-- Module      : Network.AWS.ElastiCache.CreateSnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | The /CreateSnapshot/ action creates a copy of an entire cache cluster at a
-- specific moment in time.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_CreateSnapshot.html>
module Network.AWS.ElastiCache.CreateSnapshot
    (
    -- * Request
      CreateSnapshot
    -- ** Request constructor
    , createSnapshot
    -- ** Request lenses
    , csCacheClusterId
    , csSnapshotName

    -- * Response
    , CreateSnapshotResponse
    -- ** Response constructor
    , createSnapshotResponse
    -- ** Response lenses
    , csr1Snapshot
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

data CreateSnapshot = CreateSnapshot
    { _csCacheClusterId :: Text
    , _csSnapshotName   :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'CreateSnapshot' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csCacheClusterId' @::@ 'Text'
--
-- * 'csSnapshotName' @::@ 'Text'
--
createSnapshot :: Text -- ^ 'csCacheClusterId'
               -> Text -- ^ 'csSnapshotName'
               -> CreateSnapshot
createSnapshot p1 p2 = CreateSnapshot
    { _csCacheClusterId = p1
    , _csSnapshotName   = p2
    }

-- | The identifier of an existing cache cluster. The snapshot will be created
-- from this cache cluster.
csCacheClusterId :: Lens' CreateSnapshot Text
csCacheClusterId = lens _csCacheClusterId (\s a -> s { _csCacheClusterId = a })

-- | A name for the snapshot being created.
csSnapshotName :: Lens' CreateSnapshot Text
csSnapshotName = lens _csSnapshotName (\s a -> s { _csSnapshotName = a })

newtype CreateSnapshotResponse = CreateSnapshotResponse
    { _csr1Snapshot :: Maybe Snapshot
    } deriving (Eq, Read, Show)

-- | 'CreateSnapshotResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csr1Snapshot' @::@ 'Maybe' 'Snapshot'
--
createSnapshotResponse :: CreateSnapshotResponse
createSnapshotResponse = CreateSnapshotResponse
    { _csr1Snapshot = Nothing
    }

csr1Snapshot :: Lens' CreateSnapshotResponse (Maybe Snapshot)
csr1Snapshot = lens _csr1Snapshot (\s a -> s { _csr1Snapshot = a })

instance ToPath CreateSnapshot where
    toPath = const "/"

instance ToQuery CreateSnapshot where
    toQuery CreateSnapshot{..} = mconcat
        [ "CacheClusterId" =? _csCacheClusterId
        , "SnapshotName"   =? _csSnapshotName
        ]

instance ToHeaders CreateSnapshot

instance AWSRequest CreateSnapshot where
    type Sv CreateSnapshot = ElastiCache
    type Rs CreateSnapshot = CreateSnapshotResponse

    request  = post "CreateSnapshot"
    response = xmlResponse

instance FromXML CreateSnapshotResponse where
    parseXML = withElement "CreateSnapshotResult" $ \x -> CreateSnapshotResponse
        <$> x .@? "Snapshot"
