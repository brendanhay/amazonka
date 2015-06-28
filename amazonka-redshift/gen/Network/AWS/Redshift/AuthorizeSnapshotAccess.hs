{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Redshift.AuthorizeSnapshotAccess
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Authorizes the specified AWS customer account to restore the specified
-- snapshot.
--
-- For more information about working with snapshots, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html Amazon Redshift Snapshots>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_AuthorizeSnapshotAccess.html>
module Network.AWS.Redshift.AuthorizeSnapshotAccess
    (
    -- * Request
      AuthorizeSnapshotAccess
    -- ** Request constructor
    , authorizeSnapshotAccess
    -- ** Request lenses
    , asaSnapshotClusterIdentifier
    , asaSnapshotIdentifier
    , asaAccountWithRestoreAccess

    -- * Response
    , AuthorizeSnapshotAccessResponse
    -- ** Response constructor
    , authorizeSnapshotAccessResponse
    -- ** Response lenses
    , asarSnapshot
    , asarStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'authorizeSnapshotAccess' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asaSnapshotClusterIdentifier'
--
-- * 'asaSnapshotIdentifier'
--
-- * 'asaAccountWithRestoreAccess'
data AuthorizeSnapshotAccess = AuthorizeSnapshotAccess'
    { _asaSnapshotClusterIdentifier :: !(Maybe Text)
    , _asaSnapshotIdentifier        :: !Text
    , _asaAccountWithRestoreAccess  :: !Text
    } deriving (Eq,Read,Show)

-- | 'AuthorizeSnapshotAccess' smart constructor.
authorizeSnapshotAccess :: Text -> Text -> AuthorizeSnapshotAccess
authorizeSnapshotAccess pSnapshotIdentifier pAccountWithRestoreAccess =
    AuthorizeSnapshotAccess'
    { _asaSnapshotClusterIdentifier = Nothing
    , _asaSnapshotIdentifier = pSnapshotIdentifier
    , _asaAccountWithRestoreAccess = pAccountWithRestoreAccess
    }

-- | The identifier of the cluster the snapshot was created from. This
-- parameter is required if your IAM user has a policy containing a
-- snapshot resource element that specifies anything other than * for the
-- cluster name.
asaSnapshotClusterIdentifier :: Lens' AuthorizeSnapshotAccess (Maybe Text)
asaSnapshotClusterIdentifier = lens _asaSnapshotClusterIdentifier (\ s a -> s{_asaSnapshotClusterIdentifier = a});

-- | The identifier of the snapshot the account is authorized to restore.
asaSnapshotIdentifier :: Lens' AuthorizeSnapshotAccess Text
asaSnapshotIdentifier = lens _asaSnapshotIdentifier (\ s a -> s{_asaSnapshotIdentifier = a});

-- | The identifier of the AWS customer account authorized to restore the
-- specified snapshot.
asaAccountWithRestoreAccess :: Lens' AuthorizeSnapshotAccess Text
asaAccountWithRestoreAccess = lens _asaAccountWithRestoreAccess (\ s a -> s{_asaAccountWithRestoreAccess = a});

instance AWSRequest AuthorizeSnapshotAccess where
        type Sv AuthorizeSnapshotAccess = Redshift
        type Rs AuthorizeSnapshotAccess =
             AuthorizeSnapshotAccessResponse
        request = post
        response
          = receiveXMLWrapper "AuthorizeSnapshotAccessResult"
              (\ s h x ->
                 AuthorizeSnapshotAccessResponse' <$>
                   (x .@? "Snapshot") <*> (pure s))

instance ToHeaders AuthorizeSnapshotAccess where
        toHeaders = const mempty

instance ToPath AuthorizeSnapshotAccess where
        toPath = const "/"

instance ToQuery AuthorizeSnapshotAccess where
        toQuery AuthorizeSnapshotAccess'{..}
          = mconcat
              ["Action" =:
                 ("AuthorizeSnapshotAccess" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "SnapshotClusterIdentifier" =:
                 _asaSnapshotClusterIdentifier,
               "SnapshotIdentifier" =: _asaSnapshotIdentifier,
               "AccountWithRestoreAccess" =:
                 _asaAccountWithRestoreAccess]

-- | /See:/ 'authorizeSnapshotAccessResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asarSnapshot'
--
-- * 'asarStatus'
data AuthorizeSnapshotAccessResponse = AuthorizeSnapshotAccessResponse'
    { _asarSnapshot :: !(Maybe Snapshot)
    , _asarStatus   :: !Status
    } deriving (Eq,Show)

-- | 'AuthorizeSnapshotAccessResponse' smart constructor.
authorizeSnapshotAccessResponse :: Status -> AuthorizeSnapshotAccessResponse
authorizeSnapshotAccessResponse pStatus =
    AuthorizeSnapshotAccessResponse'
    { _asarSnapshot = Nothing
    , _asarStatus = pStatus
    }

-- | FIXME: Undocumented member.
asarSnapshot :: Lens' AuthorizeSnapshotAccessResponse (Maybe Snapshot)
asarSnapshot = lens _asarSnapshot (\ s a -> s{_asarSnapshot = a});

-- | FIXME: Undocumented member.
asarStatus :: Lens' AuthorizeSnapshotAccessResponse Status
asarStatus = lens _asarStatus (\ s a -> s{_asarStatus = a});
