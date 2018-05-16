{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.RevokeSnapshotAccess
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the ability of the specified AWS customer account to restore the specified snapshot. If the account is currently restoring the snapshot, the restore will run to completion.
--
--
-- For more information about working with snapshots, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html Amazon Redshift Snapshots> in the /Amazon Redshift Cluster Management Guide/ .
--
module Network.AWS.Redshift.RevokeSnapshotAccess
    (
    -- * Creating a Request
      revokeSnapshotAccess
    , RevokeSnapshotAccess
    -- * Request Lenses
    , rsaSnapshotClusterIdentifier
    , rsaSnapshotIdentifier
    , rsaAccountWithRestoreAccess

    -- * Destructuring the Response
    , revokeSnapshotAccessResponse
    , RevokeSnapshotAccessResponse
    -- * Response Lenses
    , rsarsSnapshot
    , rsarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'revokeSnapshotAccess' smart constructor.
data RevokeSnapshotAccess = RevokeSnapshotAccess'
  { _rsaSnapshotClusterIdentifier :: !(Maybe Text)
  , _rsaSnapshotIdentifier        :: !Text
  , _rsaAccountWithRestoreAccess  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RevokeSnapshotAccess' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsaSnapshotClusterIdentifier' - The identifier of the cluster the snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
--
-- * 'rsaSnapshotIdentifier' - The identifier of the snapshot that the account can no longer access.
--
-- * 'rsaAccountWithRestoreAccess' - The identifier of the AWS customer account that can no longer restore the specified snapshot.
revokeSnapshotAccess
    :: Text -- ^ 'rsaSnapshotIdentifier'
    -> Text -- ^ 'rsaAccountWithRestoreAccess'
    -> RevokeSnapshotAccess
revokeSnapshotAccess pSnapshotIdentifier_ pAccountWithRestoreAccess_ =
  RevokeSnapshotAccess'
    { _rsaSnapshotClusterIdentifier = Nothing
    , _rsaSnapshotIdentifier = pSnapshotIdentifier_
    , _rsaAccountWithRestoreAccess = pAccountWithRestoreAccess_
    }


-- | The identifier of the cluster the snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
rsaSnapshotClusterIdentifier :: Lens' RevokeSnapshotAccess (Maybe Text)
rsaSnapshotClusterIdentifier = lens _rsaSnapshotClusterIdentifier (\ s a -> s{_rsaSnapshotClusterIdentifier = a})

-- | The identifier of the snapshot that the account can no longer access.
rsaSnapshotIdentifier :: Lens' RevokeSnapshotAccess Text
rsaSnapshotIdentifier = lens _rsaSnapshotIdentifier (\ s a -> s{_rsaSnapshotIdentifier = a})

-- | The identifier of the AWS customer account that can no longer restore the specified snapshot.
rsaAccountWithRestoreAccess :: Lens' RevokeSnapshotAccess Text
rsaAccountWithRestoreAccess = lens _rsaAccountWithRestoreAccess (\ s a -> s{_rsaAccountWithRestoreAccess = a})

instance AWSRequest RevokeSnapshotAccess where
        type Rs RevokeSnapshotAccess =
             RevokeSnapshotAccessResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper "RevokeSnapshotAccessResult"
              (\ s h x ->
                 RevokeSnapshotAccessResponse' <$>
                   (x .@? "Snapshot") <*> (pure (fromEnum s)))

instance Hashable RevokeSnapshotAccess where

instance NFData RevokeSnapshotAccess where

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
data RevokeSnapshotAccessResponse = RevokeSnapshotAccessResponse'
  { _rsarsSnapshot       :: !(Maybe Snapshot)
  , _rsarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RevokeSnapshotAccessResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsarsSnapshot' - Undocumented member.
--
-- * 'rsarsResponseStatus' - -- | The response status code.
revokeSnapshotAccessResponse
    :: Int -- ^ 'rsarsResponseStatus'
    -> RevokeSnapshotAccessResponse
revokeSnapshotAccessResponse pResponseStatus_ =
  RevokeSnapshotAccessResponse'
    {_rsarsSnapshot = Nothing, _rsarsResponseStatus = pResponseStatus_}


-- | Undocumented member.
rsarsSnapshot :: Lens' RevokeSnapshotAccessResponse (Maybe Snapshot)
rsarsSnapshot = lens _rsarsSnapshot (\ s a -> s{_rsarsSnapshot = a})

-- | -- | The response status code.
rsarsResponseStatus :: Lens' RevokeSnapshotAccessResponse Int
rsarsResponseStatus = lens _rsarsResponseStatus (\ s a -> s{_rsarsResponseStatus = a})

instance NFData RevokeSnapshotAccessResponse where
