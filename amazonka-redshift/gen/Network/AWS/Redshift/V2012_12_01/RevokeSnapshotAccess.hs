{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.RevokeSnapshotAccess
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
module Network.AWS.Redshift.V2012_12_01.RevokeSnapshotAccess
    (
    -- * Request
      RevokeSnapshotAccess
    -- ** Request constructor
    , mkRevokeSnapshotAccessMessage
    -- ** Request lenses
    , rsamSnapshotIdentifier
    , rsamSnapshotClusterIdentifier
    , rsamAccountWithRestoreAccess

    -- * Response
    , RevokeSnapshotAccessResponse
    -- ** Response lenses
    , sssssssssssssssssssssssssssssssssssssrSnapshot
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RevokeSnapshotAccess' request.
mkRevokeSnapshotAccessMessage :: Text -- ^ 'rsamSnapshotIdentifier'
                              -> Text -- ^ 'rsamAccountWithRestoreAccess'
                              -> RevokeSnapshotAccess
mkRevokeSnapshotAccessMessage p1 p2 = RevokeSnapshotAccess
    { _rsamSnapshotIdentifier = p1
    , _rsamSnapshotClusterIdentifier = Nothing
    , _rsamAccountWithRestoreAccess = p3
    }
{-# INLINE mkRevokeSnapshotAccessMessage #-}

data RevokeSnapshotAccess = RevokeSnapshotAccess
    { _rsamSnapshotIdentifier :: Text
      -- ^ The identifier of the snapshot that the account can no longer
      -- access.
    , _rsamSnapshotClusterIdentifier :: Maybe Text
      -- ^ The identifier of the cluster the snapshot was created from. This
      -- parameter is required if your IAM user has a policy containing a
      -- snapshot resource element that specifies anything other than *
      -- for the cluster name.
    , _rsamAccountWithRestoreAccess :: Text
      -- ^ The identifier of the AWS customer account that can no longer
      -- restore the specified snapshot.
    } deriving (Show, Generic)

-- | The identifier of the snapshot that the account can no longer access.
rsamSnapshotIdentifier :: Lens' RevokeSnapshotAccess (Text)
rsamSnapshotIdentifier = lens _rsamSnapshotIdentifier (\s a -> s { _rsamSnapshotIdentifier = a })
{-# INLINE rsamSnapshotIdentifier #-}

-- | The identifier of the cluster the snapshot was created from. This parameter
-- is required if your IAM user has a policy containing a snapshot resource
-- element that specifies anything other than * for the cluster name.
rsamSnapshotClusterIdentifier :: Lens' RevokeSnapshotAccess (Maybe Text)
rsamSnapshotClusterIdentifier = lens _rsamSnapshotClusterIdentifier (\s a -> s { _rsamSnapshotClusterIdentifier = a })
{-# INLINE rsamSnapshotClusterIdentifier #-}

-- | The identifier of the AWS customer account that can no longer restore the
-- specified snapshot.
rsamAccountWithRestoreAccess :: Lens' RevokeSnapshotAccess (Text)
rsamAccountWithRestoreAccess = lens _rsamAccountWithRestoreAccess (\s a -> s { _rsamAccountWithRestoreAccess = a })
{-# INLINE rsamAccountWithRestoreAccess #-}

instance ToQuery RevokeSnapshotAccess where
    toQuery = genericQuery def

newtype RevokeSnapshotAccessResponse = RevokeSnapshotAccessResponse
    { _sssssssssssssssssssssssssssssssssssssrSnapshot :: Maybe Snapshot
      -- ^ Describes a snapshot.
    } deriving (Show, Generic)

-- | Describes a snapshot.
sssssssssssssssssssssssssssssssssssssrSnapshot :: Lens' RevokeSnapshotAccessResponse (Maybe Snapshot)
sssssssssssssssssssssssssssssssssssssrSnapshot = lens _sssssssssssssssssssssssssssssssssssssrSnapshot (\s a -> s { _sssssssssssssssssssssssssssssssssssssrSnapshot = a })
{-# INLINE sssssssssssssssssssssssssssssssssssssrSnapshot #-}

instance FromXML RevokeSnapshotAccessResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RevokeSnapshotAccess where
    type Sv RevokeSnapshotAccess = Redshift
    type Rs RevokeSnapshotAccess = RevokeSnapshotAccessResponse

    request = post "RevokeSnapshotAccess"
    response _ = xmlResponse
