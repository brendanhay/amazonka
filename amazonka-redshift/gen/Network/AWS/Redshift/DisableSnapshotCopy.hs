{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DisableSnapshotCopy.html>
module Network.AWS.Redshift.DisableSnapshotCopy
    (
    -- * Request
      DisableSnapshotCopy
    -- ** Request constructor
    , disableSnapshotCopy
    -- ** Request lenses
    , dscClusterIdentifier

    -- * Response
    , DisableSnapshotCopyResponse
    -- ** Response constructor
    , disableSnapshotCopyResponse
    -- ** Response lenses
    , dscrCluster
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

newtype DisableSnapshotCopy = DisableSnapshotCopy
    { _dscClusterIdentifier :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DisableSnapshotCopy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dscClusterIdentifier' @::@ 'Text'
--
disableSnapshotCopy :: Text -- ^ 'dscClusterIdentifier'
                    -> DisableSnapshotCopy
disableSnapshotCopy p1 = DisableSnapshotCopy
    { _dscClusterIdentifier = p1
    }

-- | The unique identifier of the source cluster that you want to disable
-- copying of snapshots to a destination region. Constraints: Must be the
-- valid name of an existing cluster that has cross-region snapshot copy
-- enabled.
dscClusterIdentifier :: Lens' DisableSnapshotCopy Text
dscClusterIdentifier =
    lens _dscClusterIdentifier (\s a -> s { _dscClusterIdentifier = a })

newtype DisableSnapshotCopyResponse = DisableSnapshotCopyResponse
    { _dscrCluster :: Maybe Cluster
    } deriving (Eq, Show, Generic)

-- | 'DisableSnapshotCopyResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dscrCluster' @::@ 'Maybe' 'Cluster'
--
disableSnapshotCopyResponse :: DisableSnapshotCopyResponse
disableSnapshotCopyResponse = DisableSnapshotCopyResponse
    { _dscrCluster = Nothing
    }

dscrCluster :: Lens' DisableSnapshotCopyResponse (Maybe Cluster)
dscrCluster = lens _dscrCluster (\s a -> s { _dscrCluster = a })

instance ToPath DisableSnapshotCopy where
    toPath = const "/"

instance ToQuery DisableSnapshotCopy

instance ToHeaders DisableSnapshotCopy

instance AWSRequest DisableSnapshotCopy where
    type Sv DisableSnapshotCopy = Redshift
    type Rs DisableSnapshotCopy = DisableSnapshotCopyResponse

    request  = post "DisableSnapshotCopy"
    response = xmlResponse

instance FromXML DisableSnapshotCopyResponse where
    parseXML = withElement "DisableSnapshotCopyResult" $ \x ->
            <$> x .@? "Cluster"
