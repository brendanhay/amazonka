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

-- Module      : Network.AWS.Redshift.DisableSnapshotCopy
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
module Network.AWS.Redshift.DisableSnapshotCopy
    (
    -- * Request
      DisableSnapshotCopyMessage
    -- ** Request constructor
    , disableSnapshotCopyMessage
    -- ** Request lenses
    , dscmClusterIdentifier

    -- * Response
    , DisableSnapshotCopyResult
    -- ** Response constructor
    , disableSnapshotCopyResult
    -- ** Response lenses
    , dscrCluster
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

newtype DisableSnapshotCopyMessage = DisableSnapshotCopyMessage
    { _dscmClusterIdentifier :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DisableSnapshotCopyMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dscmClusterIdentifier' @::@ 'Text'
--
disableSnapshotCopyMessage :: Text -- ^ 'dscmClusterIdentifier'
                           -> DisableSnapshotCopyMessage
disableSnapshotCopyMessage p1 = DisableSnapshotCopyMessage
    { _dscmClusterIdentifier = p1
    }

-- | The unique identifier of the source cluster that you want to disable
-- copying of snapshots to a destination region. Constraints: Must be the
-- valid name of an existing cluster that has cross-region snapshot copy
-- enabled.
dscmClusterIdentifier :: Lens' DisableSnapshotCopyMessage Text
dscmClusterIdentifier =
    lens _dscmClusterIdentifier (\s a -> s { _dscmClusterIdentifier = a })

instance ToPath DisableSnapshotCopyMessage where
    toPath = const "/"

instance ToQuery DisableSnapshotCopyMessage

newtype DisableSnapshotCopyResult = DisableSnapshotCopyResult
    { _dscrCluster :: Maybe Cluster
    } deriving (Eq, Show, Generic)

-- | 'DisableSnapshotCopyResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dscrCluster' @::@ 'Maybe' 'Cluster'
--
disableSnapshotCopyResult :: DisableSnapshotCopyResult
disableSnapshotCopyResult = DisableSnapshotCopyResult
    { _dscrCluster = Nothing
    }

dscrCluster :: Lens' DisableSnapshotCopyResult (Maybe Cluster)
dscrCluster = lens _dscrCluster (\s a -> s { _dscrCluster = a })

instance AWSRequest DisableSnapshotCopyMessage where
    type Sv DisableSnapshotCopyMessage = Redshift
    type Rs DisableSnapshotCopyMessage = DisableSnapshotCopyResult

    request  = post "DisableSnapshotCopy"
    response = const . xmlResponse $ \h x -> DisableSnapshotCopyResult
newtype
