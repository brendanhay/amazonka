{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.ElastiCache.DeleteSnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DeleteSnapshot operation deletes an existing snapshot. When you receive
-- a successful response from this operation, ElastiCache immediately begins
-- deleting the snapshot; you cannot cancel or revert this operation.
module Network.AWS.ElastiCache.DeleteSnapshot
    (
    -- * Request
      DeleteSnapshot
    -- ** Request constructor
    , deleteSnapshot
    -- ** Request lenses
    , ds1SnapshotName

    -- * Response
    , DeleteSnapshotResponse
    -- ** Response constructor
    , deleteSnapshotResponse
    -- ** Response lenses
    , dsrSnapshot
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

newtype DeleteSnapshot = DeleteSnapshot
    { _ds1SnapshotName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteSnapshot' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ds1SnapshotName' @::@ 'Text'
--
deleteSnapshot :: Text -- ^ 'ds1SnapshotName'
               -> DeleteSnapshot
deleteSnapshot p1 = DeleteSnapshot
    { _ds1SnapshotName = p1
    }

-- | The name of the snapshot to be deleted.
ds1SnapshotName :: Lens' DeleteSnapshot Text
ds1SnapshotName = lens _ds1SnapshotName (\s a -> s { _ds1SnapshotName = a })

instance ToQuery DeleteSnapshot

instance ToPath DeleteSnapshot where
    toPath = const "/"

newtype DeleteSnapshotResponse = DeleteSnapshotResponse
    { _dsrSnapshot :: Maybe Snapshot
    } deriving (Eq, Show, Generic)

-- | 'DeleteSnapshotResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrSnapshot' @::@ 'Maybe' 'Snapshot'
--
deleteSnapshotResponse :: DeleteSnapshotResponse
deleteSnapshotResponse = DeleteSnapshotResponse
    { _dsrSnapshot = Nothing
    }

dsrSnapshot :: Lens' DeleteSnapshotResponse (Maybe Snapshot)
dsrSnapshot = lens _dsrSnapshot (\s a -> s { _dsrSnapshot = a })

instance FromXML DeleteSnapshotResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteSnapshotResponse"

instance AWSRequest DeleteSnapshot where
    type Sv DeleteSnapshot = ElastiCache
    type Rs DeleteSnapshot = DeleteSnapshotResponse

    request  = post "DeleteSnapshot"
    response = xmlResponse $ \h x -> DeleteSnapshotResponse
        <$> x %| "Snapshot"
