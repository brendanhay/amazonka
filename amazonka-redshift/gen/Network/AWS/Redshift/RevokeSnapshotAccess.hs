{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.RevokeSnapshotAccess
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Removes the ability of the specified AWS customer account to restore the
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
    , rsarqSnapshotClusterIdentifier
    , rsarqSnapshotIdentifier
    , rsarqAccountWithRestoreAccess

    -- * Response
    , RevokeSnapshotAccessResponse
    -- ** Response constructor
    , revokeSnapshotAccessResponse
    -- ** Response lenses
    , rsarsSnapshot
    , rsarsStatus
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
-- * 'rsarqSnapshotClusterIdentifier'
--
-- * 'rsarqSnapshotIdentifier'
--
-- * 'rsarqAccountWithRestoreAccess'
data RevokeSnapshotAccess = RevokeSnapshotAccess'
    { _rsarqSnapshotClusterIdentifier :: !(Maybe Text)
    , _rsarqSnapshotIdentifier        :: !Text
    , _rsarqAccountWithRestoreAccess  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RevokeSnapshotAccess' smart constructor.
revokeSnapshotAccess :: Text -> Text -> RevokeSnapshotAccess
revokeSnapshotAccess pSnapshotIdentifier pAccountWithRestoreAccess =
    RevokeSnapshotAccess'
    { _rsarqSnapshotClusterIdentifier = Nothing
    , _rsarqSnapshotIdentifier = pSnapshotIdentifier
    , _rsarqAccountWithRestoreAccess = pAccountWithRestoreAccess
    }

-- | The identifier of the cluster the snapshot was created from. This
-- parameter is required if your IAM user has a policy containing a
-- snapshot resource element that specifies anything other than * for the
-- cluster name.
rsarqSnapshotClusterIdentifier :: Lens' RevokeSnapshotAccess (Maybe Text)
rsarqSnapshotClusterIdentifier = lens _rsarqSnapshotClusterIdentifier (\ s a -> s{_rsarqSnapshotClusterIdentifier = a});

-- | The identifier of the snapshot that the account can no longer access.
rsarqSnapshotIdentifier :: Lens' RevokeSnapshotAccess Text
rsarqSnapshotIdentifier = lens _rsarqSnapshotIdentifier (\ s a -> s{_rsarqSnapshotIdentifier = a});

-- | The identifier of the AWS customer account that can no longer restore
-- the specified snapshot.
rsarqAccountWithRestoreAccess :: Lens' RevokeSnapshotAccess Text
rsarqAccountWithRestoreAccess = lens _rsarqAccountWithRestoreAccess (\ s a -> s{_rsarqAccountWithRestoreAccess = a});

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
                 _rsarqSnapshotClusterIdentifier,
               "SnapshotIdentifier" =: _rsarqSnapshotIdentifier,
               "AccountWithRestoreAccess" =:
                 _rsarqAccountWithRestoreAccess]

-- | /See:/ 'revokeSnapshotAccessResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsarsSnapshot'
--
-- * 'rsarsStatus'
data RevokeSnapshotAccessResponse = RevokeSnapshotAccessResponse'
    { _rsarsSnapshot :: !(Maybe Snapshot)
    , _rsarsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RevokeSnapshotAccessResponse' smart constructor.
revokeSnapshotAccessResponse :: Int -> RevokeSnapshotAccessResponse
revokeSnapshotAccessResponse pStatus =
    RevokeSnapshotAccessResponse'
    { _rsarsSnapshot = Nothing
    , _rsarsStatus = pStatus
    }

-- | FIXME: Undocumented member.
rsarsSnapshot :: Lens' RevokeSnapshotAccessResponse (Maybe Snapshot)
rsarsSnapshot = lens _rsarsSnapshot (\ s a -> s{_rsarsSnapshot = a});

-- | FIXME: Undocumented member.
rsarsStatus :: Lens' RevokeSnapshotAccessResponse Int
rsarsStatus = lens _rsarsStatus (\ s a -> s{_rsarsStatus = a});
