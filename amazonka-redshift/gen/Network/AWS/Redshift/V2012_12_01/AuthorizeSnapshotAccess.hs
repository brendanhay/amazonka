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
asamSnapshotIdentifier
    :: Functor f
    => (Text
    -> f (Text))
    -> AuthorizeSnapshotAccess
    -> f AuthorizeSnapshotAccess
asamSnapshotIdentifier f x =
    (\y -> x { _asamSnapshotIdentifier = y })
       <$> f (_asamSnapshotIdentifier x)
{-# INLINE asamSnapshotIdentifier #-}

-- | The identifier of the AWS customer account authorized to restore the
-- specified snapshot.
asamAccountWithRestoreAccess
    :: Functor f
    => (Text
    -> f (Text))
    -> AuthorizeSnapshotAccess
    -> f AuthorizeSnapshotAccess
asamAccountWithRestoreAccess f x =
    (\y -> x { _asamAccountWithRestoreAccess = y })
       <$> f (_asamAccountWithRestoreAccess x)
{-# INLINE asamAccountWithRestoreAccess #-}

-- | The identifier of the cluster the snapshot was created from. This parameter
-- is required if your IAM user has a policy containing a snapshot resource
-- element that specifies anything other than * for the cluster name.
asamSnapshotClusterIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AuthorizeSnapshotAccess
    -> f AuthorizeSnapshotAccess
asamSnapshotClusterIdentifier f x =
    (\y -> x { _asamSnapshotClusterIdentifier = y })
       <$> f (_asamSnapshotClusterIdentifier x)
{-# INLINE asamSnapshotClusterIdentifier #-}

instance ToQuery AuthorizeSnapshotAccess where
    toQuery = genericQuery def

data AuthorizeSnapshotAccessResponse = AuthorizeSnapshotAccessResponse
    { _swSnapshot :: Maybe Snapshot
      -- ^ Describes a snapshot.
    } deriving (Show, Generic)

-- | Describes a snapshot.
swSnapshot
    :: Functor f
    => (Maybe Snapshot
    -> f (Maybe Snapshot))
    -> AuthorizeSnapshotAccessResponse
    -> f AuthorizeSnapshotAccessResponse
swSnapshot f x =
    (\y -> x { _swSnapshot = y })
       <$> f (_swSnapshot x)
{-# INLINE swSnapshot #-}

instance FromXML AuthorizeSnapshotAccessResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest AuthorizeSnapshotAccess where
    type Sv AuthorizeSnapshotAccess = Redshift
    type Rs AuthorizeSnapshotAccess = AuthorizeSnapshotAccessResponse

    request = post "AuthorizeSnapshotAccess"
    response _ = xmlResponse
