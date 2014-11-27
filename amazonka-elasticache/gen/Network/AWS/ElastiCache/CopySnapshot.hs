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

-- Module      : Network.AWS.ElastiCache.CopySnapshot
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

-- | The /CopySnapshot/ operation makes a copy of an existing snapshot.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_CopySnapshot.html>
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

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

data CopySnapshot = CopySnapshot
    { _csSourceSnapshotName :: Text
    , _csTargetSnapshotName :: Text
    } deriving (Eq, Ord, Show)

-- | 'CopySnapshot' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csSourceSnapshotName' @::@ 'Text'
--
-- * 'csTargetSnapshotName' @::@ 'Text'
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

newtype CopySnapshotResponse = CopySnapshotResponse
    { _csrSnapshot :: Maybe Snapshot
    } deriving (Eq, Show)

-- | 'CopySnapshotResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csrSnapshot' @::@ 'Maybe' 'Snapshot'
--
copySnapshotResponse :: CopySnapshotResponse
copySnapshotResponse = CopySnapshotResponse
    { _csrSnapshot = Nothing
    }

csrSnapshot :: Lens' CopySnapshotResponse (Maybe Snapshot)
csrSnapshot = lens _csrSnapshot (\s a -> s { _csrSnapshot = a })

instance ToPath CopySnapshot where
    toPath = const "/"

instance ToQuery CopySnapshot where
    toQuery CopySnapshot{..} = mconcat
        [ "SourceSnapshotName" =? _csSourceSnapshotName
        , "TargetSnapshotName" =? _csTargetSnapshotName
        ]

instance ToHeaders CopySnapshot

instance AWSRequest CopySnapshot where
    type Sv CopySnapshot = ElastiCache
    type Rs CopySnapshot = CopySnapshotResponse

    request  = post "CopySnapshot"
    response = xmlResponse

instance FromXML CopySnapshotResponse where
    parseXML = withElement "CopySnapshotResult" $ \x -> CopySnapshotResponse
        <$> x .@? "Snapshot"
