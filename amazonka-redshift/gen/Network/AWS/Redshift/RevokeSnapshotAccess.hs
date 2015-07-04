{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.Redshift.RevokeSnapshotAccess
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Removes the ability of the specified AWS customer account to restore the
-- specified snapshot. If the account is currently restoring the snapshot,
-- the restore will run to completion.
--
-- For more information about working with snapshots, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html Amazon Redshift Snapshots>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_RevokeSnapshotAccess.html>
module Network.AWS.Redshift.RevokeSnapshotAccess
    (
    -- * Request
      RevokeSnapshotAccess
    -- ** Request constructor
    , revokeSnapshotAccess
    -- ** Request lenses
    , rsaSnapshotClusterIdentifier
    , rsaSnapshotIdentifier
    , rsaAccountWithRestoreAccess

    -- * Response
    , RevokeSnapshotAccessResponse
    -- ** Response constructor
    , revokeSnapshotAccessResponse
    -- ** Response lenses
    , rsarSnapshot
    , rsarStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'revokeSnapshotAccess' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsaSnapshotClusterIdentifier'
--
-- * 'rsaSnapshotIdentifier'
--
-- * 'rsaAccountWithRestoreAccess'
data RevokeSnapshotAccess = RevokeSnapshotAccess'
    { _rsaSnapshotClusterIdentifier :: !(Maybe Text)
    , _rsaSnapshotIdentifier        :: !Text
    , _rsaAccountWithRestoreAccess  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RevokeSnapshotAccess' smart constructor.
revokeSnapshotAccess :: Text -> Text -> RevokeSnapshotAccess
revokeSnapshotAccess pSnapshotIdentifier pAccountWithRestoreAccess =
    RevokeSnapshotAccess'
    { _rsaSnapshotClusterIdentifier = Nothing
    , _rsaSnapshotIdentifier = pSnapshotIdentifier
    , _rsaAccountWithRestoreAccess = pAccountWithRestoreAccess
    }

-- | The identifier of the cluster the snapshot was created from. This
-- parameter is required if your IAM user has a policy containing a
-- snapshot resource element that specifies anything other than * for the
-- cluster name.
rsaSnapshotClusterIdentifier :: Lens' RevokeSnapshotAccess (Maybe Text)
rsaSnapshotClusterIdentifier = lens _rsaSnapshotClusterIdentifier (\ s a -> s{_rsaSnapshotClusterIdentifier = a});

-- | The identifier of the snapshot that the account can no longer access.
rsaSnapshotIdentifier :: Lens' RevokeSnapshotAccess Text
rsaSnapshotIdentifier = lens _rsaSnapshotIdentifier (\ s a -> s{_rsaSnapshotIdentifier = a});

-- | The identifier of the AWS customer account that can no longer restore
-- the specified snapshot.
rsaAccountWithRestoreAccess :: Lens' RevokeSnapshotAccess Text
rsaAccountWithRestoreAccess = lens _rsaAccountWithRestoreAccess (\ s a -> s{_rsaAccountWithRestoreAccess = a});

instance AWSRequest RevokeSnapshotAccess where
        type Sv RevokeSnapshotAccess = Redshift
        type Rs RevokeSnapshotAccess =
             RevokeSnapshotAccessResponse
        request = post
        response
          = receiveXMLWrapper "RevokeSnapshotAccessResult"
              (\ s h x ->
                 RevokeSnapshotAccessResponse' <$>
                   (x .@? "Snapshot") <*> (pure (fromEnum s)))

instance ToHeaders RevokeSnapshotAccess where
        toHeaders = const mempty

instance ToPath RevokeSnapshotAccess where
        toPath = const "/"

instance ToQuery RevokeSnapshotAccess where
        toQuery RevokeSnapshotAccess'{..}
          = mconcat
              ["Action" =: ("RevokeSnapshotAccess" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "SnapshotClusterIdentifier" =:
                 _rsaSnapshotClusterIdentifier,
               "SnapshotIdentifier" =: _rsaSnapshotIdentifier,
               "AccountWithRestoreAccess" =:
                 _rsaAccountWithRestoreAccess]

-- | /See:/ 'revokeSnapshotAccessResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsarSnapshot'
--
-- * 'rsarStatus'
data RevokeSnapshotAccessResponse = RevokeSnapshotAccessResponse'
    { _rsarSnapshot :: !(Maybe Snapshot)
    , _rsarStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RevokeSnapshotAccessResponse' smart constructor.
revokeSnapshotAccessResponse :: Int -> RevokeSnapshotAccessResponse
revokeSnapshotAccessResponse pStatus =
    RevokeSnapshotAccessResponse'
    { _rsarSnapshot = Nothing
    , _rsarStatus = pStatus
    }

-- | FIXME: Undocumented member.
rsarSnapshot :: Lens' RevokeSnapshotAccessResponse (Maybe Snapshot)
rsarSnapshot = lens _rsarSnapshot (\ s a -> s{_rsarSnapshot = a});

-- | FIXME: Undocumented member.
rsarStatus :: Lens' RevokeSnapshotAccessResponse Int
rsarStatus = lens _rsarStatus (\ s a -> s{_rsarStatus = a});
