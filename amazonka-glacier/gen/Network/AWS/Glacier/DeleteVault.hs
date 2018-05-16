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
-- Module      : Network.AWS.Glacier.DeleteVault
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation deletes a vault. Amazon Glacier will delete a vault only if there are no archives in the vault as of the last inventory and there have been no writes to the vault since the last inventory. If either of these conditions is not satisfied, the vault deletion fails (that is, the vault is not removed) and Amazon Glacier returns an error. You can use 'DescribeVault' to return the number of archives in a vault, and you can use <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-initiate-job-post.html Initiate a Job (POST jobs)> to initiate a new inventory retrieval for a vault. The inventory contains the archive IDs you use to delete archives using <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-archive-delete.html Delete Archive (DELETE archive)> .
--
--
-- This operation is idempotent.
--
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
--
-- For conceptual information and underlying REST API, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/deleting-vaults.html Deleting a Vault in Amazon Glacier> and <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-delete.html Delete Vault > in the /Amazon Glacier Developer Guide/ .
--
module Network.AWS.Glacier.DeleteVault
    (
    -- * Creating a Request
      deleteVault
    , DeleteVault
    -- * Request Lenses
    , dAccountId
    , dVaultName

    -- * Destructuring the Response
    , deleteVaultResponse
    , DeleteVaultResponse
    ) where

import Network.AWS.Glacier.Types
import Network.AWS.Glacier.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Provides options for deleting a vault from Amazon Glacier.
--
--
--
-- /See:/ 'deleteVault' smart constructor.
data DeleteVault = DeleteVault'
  { _dAccountId :: !Text
  , _dVaultName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVault' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dAccountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- * 'dVaultName' - The name of the vault.
deleteVault
    :: Text -- ^ 'dAccountId'
    -> Text -- ^ 'dVaultName'
    -> DeleteVault
deleteVault pAccountId_ pVaultName_ =
  DeleteVault' {_dAccountId = pAccountId_, _dVaultName = pVaultName_}


-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
dAccountId :: Lens' DeleteVault Text
dAccountId = lens _dAccountId (\ s a -> s{_dAccountId = a})

-- | The name of the vault.
dVaultName :: Lens' DeleteVault Text
dVaultName = lens _dVaultName (\ s a -> s{_dVaultName = a})

instance AWSRequest DeleteVault where
        type Rs DeleteVault = DeleteVaultResponse
        request = delete glacier
        response = receiveNull DeleteVaultResponse'

instance Hashable DeleteVault where

instance NFData DeleteVault where

instance ToHeaders DeleteVault where
        toHeaders = const mempty

instance ToPath DeleteVault where
        toPath DeleteVault'{..}
          = mconcat
              ["/", toBS _dAccountId, "/vaults/", toBS _dVaultName]

instance ToQuery DeleteVault where
        toQuery = const mempty

-- | /See:/ 'deleteVaultResponse' smart constructor.
data DeleteVaultResponse =
  DeleteVaultResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVaultResponse' with the minimum fields required to make a request.
--
deleteVaultResponse
    :: DeleteVaultResponse
deleteVaultResponse = DeleteVaultResponse'


instance NFData DeleteVaultResponse where
