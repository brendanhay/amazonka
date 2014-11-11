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

-- Module      : Network.AWS.Redshift.AuthorizeSnapshotAccess
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Authorizes the specified AWS customer account to restore the specified
-- snapshot. For more information about working with snapshots, go to Amazon
-- Redshift Snapshots in the Amazon Redshift Management Guide.
module Network.AWS.Redshift.AuthorizeSnapshotAccess
    (
    -- * Request
      AuthorizeSnapshotAccessMessage
    -- ** Request constructor
    , authorizeSnapshotAccessMessage
    -- ** Request lenses
    , asamAccountWithRestoreAccess
    , asamSnapshotClusterIdentifier
    , asamSnapshotIdentifier

    -- * Response
    , AuthorizeSnapshotAccessResult
    -- ** Response constructor
    , authorizeSnapshotAccessResult
    -- ** Response lenses
    , asarSnapshot
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data AuthorizeSnapshotAccessMessage = AuthorizeSnapshotAccessMessage
    { _asamAccountWithRestoreAccess  :: Text
    , _asamSnapshotClusterIdentifier :: Maybe Text
    , _asamSnapshotIdentifier        :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AuthorizeSnapshotAccessMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asamAccountWithRestoreAccess' @::@ 'Text'
--
-- * 'asamSnapshotClusterIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'asamSnapshotIdentifier' @::@ 'Text'
--
authorizeSnapshotAccessMessage :: Text -- ^ 'asamSnapshotIdentifier'
                               -> Text -- ^ 'asamAccountWithRestoreAccess'
                               -> AuthorizeSnapshotAccessMessage
authorizeSnapshotAccessMessage p1 p2 = AuthorizeSnapshotAccessMessage
    { _asamSnapshotIdentifier        = p1
    , _asamAccountWithRestoreAccess  = p2
    , _asamSnapshotClusterIdentifier = Nothing
    }

-- | The identifier of the AWS customer account authorized to restore the
-- specified snapshot.
asamAccountWithRestoreAccess :: Lens' AuthorizeSnapshotAccessMessage Text
asamAccountWithRestoreAccess =
    lens _asamAccountWithRestoreAccess
        (\s a -> s { _asamAccountWithRestoreAccess = a })

-- | The identifier of the cluster the snapshot was created from. This
-- parameter is required if your IAM user has a policy containing a snapshot
-- resource element that specifies anything other than * for the cluster
-- name.
asamSnapshotClusterIdentifier :: Lens' AuthorizeSnapshotAccessMessage (Maybe Text)
asamSnapshotClusterIdentifier =
    lens _asamSnapshotClusterIdentifier
        (\s a -> s { _asamSnapshotClusterIdentifier = a })

-- | The identifier of the snapshot the account is authorized to restore.
asamSnapshotIdentifier :: Lens' AuthorizeSnapshotAccessMessage Text
asamSnapshotIdentifier =
    lens _asamSnapshotIdentifier (\s a -> s { _asamSnapshotIdentifier = a })
instance ToQuery AuthorizeSnapshotAccessMessage

instance ToPath AuthorizeSnapshotAccessMessage where
    toPath = const "/"

newtype AuthorizeSnapshotAccessResult = AuthorizeSnapshotAccessResult
    { _asarSnapshot :: Maybe Snapshot
    } deriving (Eq, Show, Generic)

-- | 'AuthorizeSnapshotAccessResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asarSnapshot' @::@ 'Maybe' 'Snapshot'
--
authorizeSnapshotAccessResult :: AuthorizeSnapshotAccessResult
authorizeSnapshotAccessResult = AuthorizeSnapshotAccessResult
    { _asarSnapshot = Nothing
    }

asarSnapshot :: Lens' AuthorizeSnapshotAccessResult (Maybe Snapshot)
asarSnapshot = lens _asarSnapshot (\s a -> s { _asarSnapshot = a })
instance FromXML AuthorizeSnapshotAccessResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AuthorizeSnapshotAccessResult"

instance AWSRequest AuthorizeSnapshotAccessMessage where
    type Sv AuthorizeSnapshotAccessMessage = Redshift
    type Rs AuthorizeSnapshotAccessMessage = AuthorizeSnapshotAccessResult

    request  = post "AuthorizeSnapshotAccess"
    response = xmlResponse $ \h x -> AuthorizeSnapshotAccessResult
        <$> x %| "Snapshot"
