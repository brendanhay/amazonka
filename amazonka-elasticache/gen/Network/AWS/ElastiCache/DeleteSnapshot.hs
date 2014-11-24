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

-- Module      : Network.AWS.ElastiCache.DeleteSnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The /DeleteSnapshot/ operation deletes an existing snapshot. When you
-- receive a successful response from this operation, ElastiCache immediately
-- begins deleting the snapshot; you cannot cancel or revert this operation.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DeleteSnapshot.html>
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
import qualified GHC.Exts

newtype DeleteSnapshot = DeleteSnapshot
    { _ds1SnapshotName :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

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

newtype DeleteSnapshotResponse = DeleteSnapshotResponse
    { _dsrSnapshot :: Maybe Snapshot
    } deriving (Eq, Show)

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

instance ToPath DeleteSnapshot where
    toPath = const "/"

instance ToQuery DeleteSnapshot where
    toQuery DeleteSnapshot{..} = mconcat
        [ "SnapshotName" =? _ds1SnapshotName
        ]

instance ToHeaders DeleteSnapshot

instance AWSRequest DeleteSnapshot where
    type Sv DeleteSnapshot = ElastiCache
    type Rs DeleteSnapshot = DeleteSnapshotResponse

    request  = post "DeleteSnapshot"
    response = xmlResponse

instance FromXML DeleteSnapshotResponse where
    parseXML = withElement "DeleteSnapshotResult" $ \x -> DeleteSnapshotResponse
        <$> x .@? "Snapshot"
