{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.DeleteVault
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | This operation deletes a vault. Amazon Glacier will delete a vault only
-- if there are no archives in the vault as of the last inventory and there
-- have been no writes to the vault since the last inventory. If either of
-- these conditions is not satisfied, the vault deletion fails (that is,
-- the vault is not removed) and Amazon Glacier returns an error. You can
-- use DescribeVault to return the number of archives in a vault, and you
-- can use
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-initiate-job-post.html Initiate a Job (POST jobs)>
-- to initiate a new inventory retrieval for a vault. The inventory
-- contains the archive IDs you use to delete archives using
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-archive-delete.html Delete Archive (DELETE archive)>.
--
-- This operation is idempotent.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don\'t have any
-- permissions by default. You must grant them explicit permission to
-- perform specific actions. For more information, see
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)>.
--
-- For conceptual information and underlying REST API, go to
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/deleting-vaults.html Deleting a Vault in Amazon Glacier>
-- and
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-delete.html Delete Vault>
-- in the /Amazon Glacier Developer Guide/.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-DeleteVault.html>
module Network.AWS.Glacier.DeleteVault
    (
    -- * Request
      DeleteVault
    -- ** Request constructor
    , deleteVault
    -- ** Request lenses
    , delAccountId
    , delVaultName

    -- * Response
    , DeleteVaultResponse
    -- ** Response constructor
    , deleteVaultResponse
    ) where

import           Network.AWS.Glacier.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Provides options for deleting a vault from Amazon Glacier.
--
-- /See:/ 'deleteVault' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delAccountId'
--
-- * 'delVaultName'
data DeleteVault = DeleteVault'
    { _delAccountId :: !Text
    , _delVaultName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteVault' smart constructor.
deleteVault :: Text -> Text -> DeleteVault
deleteVault pAccountId pVaultName =
    DeleteVault'
    { _delAccountId = pAccountId
    , _delVaultName = pVaultName
    }

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (apos-apos) in the ID.
delAccountId :: Lens' DeleteVault Text
delAccountId = lens _delAccountId (\ s a -> s{_delAccountId = a});

-- | The name of the vault.
delVaultName :: Lens' DeleteVault Text
delVaultName = lens _delVaultName (\ s a -> s{_delVaultName = a});

instance AWSRequest DeleteVault where
        type Sv DeleteVault = Glacier
        type Rs DeleteVault = DeleteVaultResponse
        request = delete
        response = receiveNull DeleteVaultResponse'

instance ToHeaders DeleteVault where
        toHeaders = const mempty

instance ToPath DeleteVault where
        toPath DeleteVault'{..}
          = mconcat
              ["/", toText _delAccountId, "/vaults/",
               toText _delVaultName]

instance ToQuery DeleteVault where
        toQuery = const mempty

-- | /See:/ 'deleteVaultResponse' smart constructor.
data DeleteVaultResponse =
    DeleteVaultResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteVaultResponse' smart constructor.
deleteVaultResponse :: DeleteVaultResponse
deleteVaultResponse = DeleteVaultResponse'
