{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.AuthorizeSnapshotAccess
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
module Network.AWS.Redshift.V2012_12_01.AuthorizeSnapshotAccess
    (
    -- * Request
      AuthorizeSnapshotAccess
    -- ** Request constructor
    , authorizeSnapshotAccess
    -- ** Request lenses
    , asamSnapshotIdentifier
    , asamAccountWithRestoreAccess
    , asamSnapshotClusterIdentifier

    -- * Response
    , AuthorizeSnapshotAccessResponse
    -- ** Response lenses
    , swSnapshot
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'AuthorizeSnapshotAccess' request.
authorizeSnapshotAccess :: Text -- ^ 'asamSnapshotIdentifier'
                        -> Text -- ^ 'asamAccountWithRestoreAccess'
                        -> AuthorizeSnapshotAccess
authorizeSnapshotAccess p1 p2 = AuthorizeSnapshotAccess
    { _asamSnapshotIdentifier = p1
    , _asamAccountWithRestoreAccess = p2
    , _asamSnapshotClusterIdentifier = Nothing
    }
{-# INLINE authorizeSnapshotAccess #-}

data AuthorizeSnapshotAccess = AuthorizeSnapshotAccess
    { _asamSnapshotIdentifier :: Text
      -- ^ The identifier of the snapshot the account is authorized to
      -- restore.
    , _asamAccountWithRestoreAccess :: Text
      -- ^ The identifier of the AWS customer account authorized to restore
      -- the specified snapshot.
    , _asamSnapshotClusterIdentifier :: Maybe Text
      -- ^ The identifier of the cluster the snapshot was created from. This
      -- parameter is required if your IAM user has a policy containing a
      -- snapshot resource element that specifies anything other than *
      -- for the cluster name.
    } deriving (Show, Generic)

-- | The identifier of the snapshot the account is authorized to restore.
asamSnapshotIdentifier :: Lens' AuthorizeSnapshotAccess (Text)
asamSnapshotIdentifier f x =
    f (_asamSnapshotIdentifier x)
        <&> \y -> x { _asamSnapshotIdentifier = y }
{-# INLINE asamSnapshotIdentifier #-}

-- | The identifier of the AWS customer account authorized to restore the
-- specified snapshot.
asamAccountWithRestoreAccess :: Lens' AuthorizeSnapshotAccess (Text)
asamAccountWithRestoreAccess f x =
    f (_asamAccountWithRestoreAccess x)
        <&> \y -> x { _asamAccountWithRestoreAccess = y }
{-# INLINE asamAccountWithRestoreAccess #-}

-- | The identifier of the cluster the snapshot was created from. This parameter
-- is required if your IAM user has a policy containing a snapshot resource
-- element that specifies anything other than * for the cluster name.
asamSnapshotClusterIdentifier :: Lens' AuthorizeSnapshotAccess (Maybe Text)
asamSnapshotClusterIdentifier f x =
    f (_asamSnapshotClusterIdentifier x)
        <&> \y -> x { _asamSnapshotClusterIdentifier = y }
{-# INLINE asamSnapshotClusterIdentifier #-}

instance ToQuery AuthorizeSnapshotAccess where
    toQuery = genericQuery def

data AuthorizeSnapshotAccessResponse = AuthorizeSnapshotAccessResponse
    { _swSnapshot :: Maybe Snapshot
      -- ^ Describes a snapshot.
    } deriving (Show, Generic)

-- | Describes a snapshot.
swSnapshot :: Lens' AuthorizeSnapshotAccessResponse (Maybe Snapshot)
swSnapshot f x =
    f (_swSnapshot x)
        <&> \y -> x { _swSnapshot = y }
{-# INLINE swSnapshot #-}

instance FromXML AuthorizeSnapshotAccessResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest AuthorizeSnapshotAccess where
    type Sv AuthorizeSnapshotAccess = Redshift
    type Rs AuthorizeSnapshotAccess = AuthorizeSnapshotAccessResponse

    request = post "AuthorizeSnapshotAccess"
    response _ = xmlResponse
