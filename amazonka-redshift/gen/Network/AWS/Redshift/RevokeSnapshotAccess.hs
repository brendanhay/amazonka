{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift
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
module Network.AWS.Redshift
    (
    -- * Request
      RevokeSnapshotAccess
    -- ** Request constructor
    , mkRevokeSnapshotAccess
    -- ** Request lenses
    , rsaSnapshotIdentifier
    , rsaSnapshotClusterIdentifier
    , rsaAccountWithRestoreAccess

    -- * Response
    , RevokeSnapshotAccessResponse
    -- ** Response constructor
    , mkRevokeSnapshotAccessResponse
    -- ** Response lenses
    , rsarSnapshot
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import Network.AWS.Prelude

-- | 
data RevokeSnapshotAccess = RevokeSnapshotAccess
    { _rsaSnapshotIdentifier :: Text
    , _rsaSnapshotClusterIdentifier :: Maybe Text
    , _rsaAccountWithRestoreAccess :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RevokeSnapshotAccess' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SnapshotIdentifier ::@ @Text@
--
-- * @SnapshotClusterIdentifier ::@ @Maybe Text@
--
-- * @AccountWithRestoreAccess ::@ @Text@
--
mkRevokeSnapshotAccess :: Text -- ^ 'rsaSnapshotIdentifier'
                       -> Text -- ^ 'rsaAccountWithRestoreAccess'
                       -> RevokeSnapshotAccess
mkRevokeSnapshotAccess p1 p3 = RevokeSnapshotAccess
    { _rsaSnapshotIdentifier = p1
    , _rsaSnapshotClusterIdentifier = Nothing
    , _rsaAccountWithRestoreAccess = p3
    }

-- | The identifier of the snapshot that the account can no longer access.
rsaSnapshotIdentifier :: Lens' RevokeSnapshotAccess Text
rsaSnapshotIdentifier =
    lens _rsaSnapshotIdentifier (\s a -> s { _rsaSnapshotIdentifier = a })

-- | The identifier of the cluster the snapshot was created from. This parameter
-- is required if your IAM user has a policy containing a snapshot resource
-- element that specifies anything other than * for the cluster name.
rsaSnapshotClusterIdentifier :: Lens' RevokeSnapshotAccess (Maybe Text)
rsaSnapshotClusterIdentifier =
    lens _rsaSnapshotClusterIdentifier
         (\s a -> s { _rsaSnapshotClusterIdentifier = a })

-- | The identifier of the AWS customer account that can no longer restore the
-- specified snapshot.
rsaAccountWithRestoreAccess :: Lens' RevokeSnapshotAccess Text
rsaAccountWithRestoreAccess =
    lens _rsaAccountWithRestoreAccess
         (\s a -> s { _rsaAccountWithRestoreAccess = a })

instance ToQuery RevokeSnapshotAccess where
    toQuery = genericQuery def

newtype RevokeSnapshotAccessResponse = RevokeSnapshotAccessResponse
    { _rsarSnapshot :: Maybe Snapshot
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RevokeSnapshotAccessResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Snapshot ::@ @Maybe Snapshot@
--
mkRevokeSnapshotAccessResponse :: RevokeSnapshotAccessResponse
mkRevokeSnapshotAccessResponse = RevokeSnapshotAccessResponse
    { _rsarSnapshot = Nothing
    }

-- | Describes a snapshot.
rsarSnapshot :: Lens' RevokeSnapshotAccessResponse (Maybe Snapshot)
rsarSnapshot = lens _rsarSnapshot (\s a -> s { _rsarSnapshot = a })

instance FromXML RevokeSnapshotAccessResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RevokeSnapshotAccess where
    type Sv RevokeSnapshotAccess = Redshift
    type Rs RevokeSnapshotAccess = RevokeSnapshotAccessResponse

    request = post "RevokeSnapshotAccess"
    response _ = xmlResponse
