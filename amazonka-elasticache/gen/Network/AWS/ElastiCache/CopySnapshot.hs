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
      CopySnapshotMessage
    -- ** Request constructor
    , copySnapshotMessage
    -- ** Request lenses
    , csmSourceSnapshotName
    , csmTargetSnapshotName

    -- * Response
    , CopySnapshotResult
    -- ** Response constructor
    , copySnapshotResult
    -- ** Response lenses
    , csrSnapshot
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

data CopySnapshotMessage = CopySnapshotMessage
    { _csmSourceSnapshotName :: Text
    , _csmTargetSnapshotName :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CopySnapshotMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csmSourceSnapshotName' @::@ 'Text'
--
-- * 'csmTargetSnapshotName' @::@ 'Text'
--
copySnapshotMessage :: Text -- ^ 'csmSourceSnapshotName'
                    -> Text -- ^ 'csmTargetSnapshotName'
                    -> CopySnapshotMessage
copySnapshotMessage p1 p2 = CopySnapshotMessage
    { _csmSourceSnapshotName = p1
    , _csmTargetSnapshotName = p2
    }

-- | The name of an existing snapshot from which to copy.
csmSourceSnapshotName :: Lens' CopySnapshotMessage Text
csmSourceSnapshotName =
    lens _csmSourceSnapshotName (\s a -> s { _csmSourceSnapshotName = a })

-- | A name for the copied snapshot.
csmTargetSnapshotName :: Lens' CopySnapshotMessage Text
csmTargetSnapshotName =
    lens _csmTargetSnapshotName (\s a -> s { _csmTargetSnapshotName = a })

instance ToQuery CopySnapshotMessage

instance ToPath CopySnapshotMessage where
    toPath = const "/"

newtype CopySnapshotResult = CopySnapshotResult
    { _csrSnapshot :: Maybe Snapshot
    } deriving (Eq, Show, Generic)

-- | 'CopySnapshotResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csrSnapshot' @::@ 'Maybe' 'Snapshot'
--
copySnapshotResult :: CopySnapshotResult
copySnapshotResult = CopySnapshotResult
    { _csrSnapshot = Nothing
    }

csrSnapshot :: Lens' CopySnapshotResult (Maybe Snapshot)
csrSnapshot = lens _csrSnapshot (\s a -> s { _csrSnapshot = a })

instance FromXML CopySnapshotResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CopySnapshotResult"

instance AWSRequest CopySnapshotMessage where
    type Sv CopySnapshotMessage = ElastiCache
    type Rs CopySnapshotMessage = CopySnapshotResult

    request  = post "CopySnapshot"
    response = xmlResponse $ \h x -> CopySnapshotResult
        <$> x %| "Snapshot"
