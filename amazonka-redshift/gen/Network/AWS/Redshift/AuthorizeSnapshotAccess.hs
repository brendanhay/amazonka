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
      AuthorizeSnapshotAccess
    -- ** Request constructor
    , authorizeSnapshotAccess
    -- ** Request lenses
    , asaAccountWithRestoreAccess
    , asaSnapshotClusterIdentifier
    , asaSnapshotIdentifier

    -- * Response
    , AuthorizeSnapshotAccessResponse
    -- ** Response constructor
    , authorizeSnapshotAccessResponse
    -- ** Response lenses
    , asarSnapshot
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data AuthorizeSnapshotAccess = AuthorizeSnapshotAccess
    { _asaAccountWithRestoreAccess  :: Text
    , _asaSnapshotClusterIdentifier :: Maybe Text
    , _asaSnapshotIdentifier        :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AuthorizeSnapshotAccess' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asaAccountWithRestoreAccess' @::@ 'Text'
--
-- * 'asaSnapshotClusterIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'asaSnapshotIdentifier' @::@ 'Text'
--
authorizeSnapshotAccess :: Text -- ^ 'asaSnapshotIdentifier'
                        -> Text -- ^ 'asaAccountWithRestoreAccess'
                        -> AuthorizeSnapshotAccess
authorizeSnapshotAccess p1 p2 = AuthorizeSnapshotAccess
    { _asaSnapshotIdentifier        = p1
    , _asaAccountWithRestoreAccess  = p2
    , _asaSnapshotClusterIdentifier = Nothing
    }

-- | The identifier of the AWS customer account authorized to restore the
-- specified snapshot.
asaAccountWithRestoreAccess :: Lens' AuthorizeSnapshotAccess Text
asaAccountWithRestoreAccess =
    lens _asaAccountWithRestoreAccess
        (\s a -> s { _asaAccountWithRestoreAccess = a })

-- | The identifier of the cluster the snapshot was created from. This
-- parameter is required if your IAM user has a policy containing a snapshot
-- resource element that specifies anything other than * for the cluster
-- name.
asaSnapshotClusterIdentifier :: Lens' AuthorizeSnapshotAccess (Maybe Text)
asaSnapshotClusterIdentifier =
    lens _asaSnapshotClusterIdentifier
        (\s a -> s { _asaSnapshotClusterIdentifier = a })

-- | The identifier of the snapshot the account is authorized to restore.
asaSnapshotIdentifier :: Lens' AuthorizeSnapshotAccess Text
asaSnapshotIdentifier =
    lens _asaSnapshotIdentifier (\s a -> s { _asaSnapshotIdentifier = a })

instance ToQuery AuthorizeSnapshotAccess

instance ToPath AuthorizeSnapshotAccess where
    toPath = const "/"

newtype AuthorizeSnapshotAccessResponse = AuthorizeSnapshotAccessResponse
    { _asarSnapshot :: Maybe Snapshot
    } deriving (Eq, Show, Generic)

-- | 'AuthorizeSnapshotAccessResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asarSnapshot' @::@ 'Maybe' 'Snapshot'
--
authorizeSnapshotAccessResponse :: AuthorizeSnapshotAccessResponse
authorizeSnapshotAccessResponse = AuthorizeSnapshotAccessResponse
    { _asarSnapshot = Nothing
    }

asarSnapshot :: Lens' AuthorizeSnapshotAccessResponse (Maybe Snapshot)
asarSnapshot = lens _asarSnapshot (\s a -> s { _asarSnapshot = a })

instance AWSRequest AuthorizeSnapshotAccess where
    type Sv AuthorizeSnapshotAccess = Redshift
    type Rs AuthorizeSnapshotAccess = AuthorizeSnapshotAccessResponse

    request  = post "AuthorizeSnapshotAccess"
    response = xmlResponse $ \h x -> AuthorizeSnapshotAccessResponse
        <$> x %| "Snapshot"
