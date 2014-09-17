{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
    , mkAuthorizeSnapshotAccess
    -- ** Request lenses
    , asaSnapshotIdentifier
    , asaSnapshotClusterIdentifier
    , asaAccountWithRestoreAccess

    -- * Response
    , AuthorizeSnapshotAccessResponse
    -- ** Response constructor
    , mkAuthorizeSnapshotAccessResponse
    -- ** Response lenses
    , asarSnapshot
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import Network.AWS.Prelude

-- | 
data AuthorizeSnapshotAccess = AuthorizeSnapshotAccess
    { _asaSnapshotIdentifier :: Text
    , _asaSnapshotClusterIdentifier :: Maybe Text
    , _asaAccountWithRestoreAccess :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AuthorizeSnapshotAccess' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SnapshotIdentifier ::@ @Text@
--
-- * @SnapshotClusterIdentifier ::@ @Maybe Text@
--
-- * @AccountWithRestoreAccess ::@ @Text@
--
mkAuthorizeSnapshotAccess :: Text -- ^ 'asaSnapshotIdentifier'
                          -> Text -- ^ 'asaAccountWithRestoreAccess'
                          -> AuthorizeSnapshotAccess
mkAuthorizeSnapshotAccess p1 p3 = AuthorizeSnapshotAccess
    { _asaSnapshotIdentifier = p1
    , _asaSnapshotClusterIdentifier = Nothing
    , _asaAccountWithRestoreAccess = p3
    }

-- | The identifier of the snapshot the account is authorized to restore.
asaSnapshotIdentifier :: Lens' AuthorizeSnapshotAccess Text
asaSnapshotIdentifier =
    lens _asaSnapshotIdentifier (\s a -> s { _asaSnapshotIdentifier = a })

-- | The identifier of the cluster the snapshot was created from. This parameter
-- is required if your IAM user has a policy containing a snapshot resource
-- element that specifies anything other than * for the cluster name.
asaSnapshotClusterIdentifier :: Lens' AuthorizeSnapshotAccess (Maybe Text)
asaSnapshotClusterIdentifier =
    lens _asaSnapshotClusterIdentifier
         (\s a -> s { _asaSnapshotClusterIdentifier = a })

-- | The identifier of the AWS customer account authorized to restore the
-- specified snapshot.
asaAccountWithRestoreAccess :: Lens' AuthorizeSnapshotAccess Text
asaAccountWithRestoreAccess =
    lens _asaAccountWithRestoreAccess
         (\s a -> s { _asaAccountWithRestoreAccess = a })

instance ToQuery AuthorizeSnapshotAccess where
    toQuery = genericQuery def

newtype AuthorizeSnapshotAccessResponse = AuthorizeSnapshotAccessResponse
    { _asarSnapshot :: Maybe Snapshot
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AuthorizeSnapshotAccessResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Snapshot ::@ @Maybe Snapshot@
--
mkAuthorizeSnapshotAccessResponse :: AuthorizeSnapshotAccessResponse
mkAuthorizeSnapshotAccessResponse = AuthorizeSnapshotAccessResponse
    { _asarSnapshot = Nothing
    }

-- | Describes a snapshot.
asarSnapshot :: Lens' AuthorizeSnapshotAccessResponse (Maybe Snapshot)
asarSnapshot = lens _asarSnapshot (\s a -> s { _asarSnapshot = a })

instance FromXML AuthorizeSnapshotAccessResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest AuthorizeSnapshotAccess where
    type Sv AuthorizeSnapshotAccess = Redshift
    type Rs AuthorizeSnapshotAccess = AuthorizeSnapshotAccessResponse

    request = post "AuthorizeSnapshotAccess"
    response _ = xmlResponse
