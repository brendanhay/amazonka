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

-- Module      : Network.AWS.Redshift.RevokeSnapshotAccess
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes the ability of the specified AWS customer account to restore the
-- specified snapshot. If the account is currently restoring the snapshot, the
-- restore will run to completion. For more information about working with
-- snapshots, go to Amazon Redshift Snapshots in the Amazon Redshift
-- Management Guide.
module Network.AWS.Redshift.RevokeSnapshotAccess
    (
    -- * Request
      RevokeSnapshotAccessMessage
    -- ** Request constructor
    , revokeSnapshotAccessMessage
    -- ** Request lenses
    , rsamAccountWithRestoreAccess
    , rsamSnapshotClusterIdentifier
    , rsamSnapshotIdentifier

    -- * Response
    , RevokeSnapshotAccessResult
    -- ** Response constructor
    , revokeSnapshotAccessResult
    -- ** Response lenses
    , rsarSnapshot
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data RevokeSnapshotAccessMessage = RevokeSnapshotAccessMessage
    { _rsamAccountWithRestoreAccess  :: Text
    , _rsamSnapshotClusterIdentifier :: Maybe Text
    , _rsamSnapshotIdentifier        :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RevokeSnapshotAccessMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsamAccountWithRestoreAccess' @::@ 'Text'
--
-- * 'rsamSnapshotClusterIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'rsamSnapshotIdentifier' @::@ 'Text'
--
revokeSnapshotAccessMessage :: Text -- ^ 'rsamSnapshotIdentifier'
                            -> Text -- ^ 'rsamAccountWithRestoreAccess'
                            -> RevokeSnapshotAccessMessage
revokeSnapshotAccessMessage p1 p2 = RevokeSnapshotAccessMessage
    { _rsamSnapshotIdentifier        = p1
    , _rsamAccountWithRestoreAccess  = p2
    , _rsamSnapshotClusterIdentifier = Nothing
    }

-- | The identifier of the AWS customer account that can no longer restore the
-- specified snapshot.
rsamAccountWithRestoreAccess :: Lens' RevokeSnapshotAccessMessage Text
rsamAccountWithRestoreAccess =
    lens _rsamAccountWithRestoreAccess
        (\s a -> s { _rsamAccountWithRestoreAccess = a })

-- | The identifier of the cluster the snapshot was created from. This
-- parameter is required if your IAM user has a policy containing a snapshot
-- resource element that specifies anything other than * for the cluster
-- name.
rsamSnapshotClusterIdentifier :: Lens' RevokeSnapshotAccessMessage (Maybe Text)
rsamSnapshotClusterIdentifier =
    lens _rsamSnapshotClusterIdentifier
        (\s a -> s { _rsamSnapshotClusterIdentifier = a })

-- | The identifier of the snapshot that the account can no longer access.
rsamSnapshotIdentifier :: Lens' RevokeSnapshotAccessMessage Text
rsamSnapshotIdentifier =
    lens _rsamSnapshotIdentifier (\s a -> s { _rsamSnapshotIdentifier = a })

instance ToQuery RevokeSnapshotAccessMessage

instance ToPath RevokeSnapshotAccessMessage where
    toPath = const "/"

newtype RevokeSnapshotAccessResult = RevokeSnapshotAccessResult
    { _rsarSnapshot :: Maybe Snapshot
    } deriving (Eq, Show, Generic)

-- | 'RevokeSnapshotAccessResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsarSnapshot' @::@ 'Maybe' 'Snapshot'
--
revokeSnapshotAccessResult :: RevokeSnapshotAccessResult
revokeSnapshotAccessResult = RevokeSnapshotAccessResult
    { _rsarSnapshot = Nothing
    }

rsarSnapshot :: Lens' RevokeSnapshotAccessResult (Maybe Snapshot)
rsarSnapshot = lens _rsarSnapshot (\s a -> s { _rsarSnapshot = a })

instance FromXML RevokeSnapshotAccessResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RevokeSnapshotAccessResult"

instance AWSRequest RevokeSnapshotAccessMessage where
    type Sv RevokeSnapshotAccessMessage = Redshift
    type Rs RevokeSnapshotAccessMessage = RevokeSnapshotAccessResult

    request  = post "RevokeSnapshotAccess"
    response = xmlResponse $ \h x -> RevokeSnapshotAccessResult
        <$> x %| "Snapshot"
