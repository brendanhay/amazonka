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
      DeleteSnapshotMessage
    -- ** Request constructor
    , deleteSnapshot
    -- ** Request lenses
    , dsmSnapshotName

    -- * Response
    , DeleteSnapshotResult
    -- ** Response constructor
    , deleteSnapshotResponse
    -- ** Response lenses
    , dsrSnapshot
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

newtype DeleteSnapshotMessage = DeleteSnapshotMessage
    { _dsmSnapshotName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteSnapshotMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsmSnapshotName' @::@ 'Text'
--
deleteSnapshot :: Text -- ^ 'dsmSnapshotName'
               -> DeleteSnapshotMessage
deleteSnapshot p1 = DeleteSnapshotMessage
    { _dsmSnapshotName = p1
    }

-- | The name of the snapshot to be deleted.
dsmSnapshotName :: Lens' DeleteSnapshotMessage Text
dsmSnapshotName = lens _dsmSnapshotName (\s a -> s { _dsmSnapshotName = a })

instance ToQuery DeleteSnapshotMessage

instance ToPath DeleteSnapshotMessage where
    toPath = const "/"

newtype DeleteSnapshotResult = DeleteSnapshotResult
    { _dsrSnapshot :: Maybe Snapshot
    } deriving (Eq, Show, Generic)

-- | 'DeleteSnapshotResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrSnapshot' @::@ 'Maybe' 'Snapshot'
--
deleteSnapshotResponse :: DeleteSnapshotResult
deleteSnapshotResponse = DeleteSnapshotResult
    { _dsrSnapshot = Nothing
    }

dsrSnapshot :: Lens' DeleteSnapshotResult (Maybe Snapshot)
dsrSnapshot = lens _dsrSnapshot (\s a -> s { _dsrSnapshot = a })

instance FromXML DeleteSnapshotResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteSnapshotResult"

instance AWSRequest DeleteSnapshotMessage where
    type Sv DeleteSnapshotMessage = ElastiCache
    type Rs DeleteSnapshotMessage = DeleteSnapshotResult

    request  = post "DeleteSnapshot"
    response = xmlResponse $ \h x -> DeleteSnapshotResult
        <$> x %| "Snapshot"
