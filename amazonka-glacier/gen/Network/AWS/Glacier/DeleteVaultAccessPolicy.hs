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
-- Module      : Network.AWS.Glacier.DeleteVaultAccessPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation deletes the access policy associated with the specified vault. The operation is eventually consistent; that is, it might take some time for Amazon Glacier to completely remove the access policy, and you might still see the effect of the policy for a short time after you send the delete request.
--
--
-- This operation is idempotent. You can invoke delete multiple times, even if there is no policy associated with the vault. For more information about vault access policies, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-access-policy.html Amazon Glacier Access Control with Vault Access Policies> .
--
module Network.AWS.Glacier.DeleteVaultAccessPolicy
    (
    -- * Creating a Request
      deleteVaultAccessPolicy
    , DeleteVaultAccessPolicy
    -- * Request Lenses
    , dvapAccountId
    , dvapVaultName

    -- * Destructuring the Response
    , deleteVaultAccessPolicyResponse
    , DeleteVaultAccessPolicyResponse
    ) where

import Network.AWS.Glacier.Types
import Network.AWS.Glacier.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | DeleteVaultAccessPolicy input.
--
--
--
-- /See:/ 'deleteVaultAccessPolicy' smart constructor.
data DeleteVaultAccessPolicy = DeleteVaultAccessPolicy'
  { _dvapAccountId :: !Text
  , _dvapVaultName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVaultAccessPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvapAccountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- * 'dvapVaultName' - The name of the vault.
deleteVaultAccessPolicy
    :: Text -- ^ 'dvapAccountId'
    -> Text -- ^ 'dvapVaultName'
    -> DeleteVaultAccessPolicy
deleteVaultAccessPolicy pAccountId_ pVaultName_ =
  DeleteVaultAccessPolicy'
    {_dvapAccountId = pAccountId_, _dvapVaultName = pVaultName_}


-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
dvapAccountId :: Lens' DeleteVaultAccessPolicy Text
dvapAccountId = lens _dvapAccountId (\ s a -> s{_dvapAccountId = a})

-- | The name of the vault.
dvapVaultName :: Lens' DeleteVaultAccessPolicy Text
dvapVaultName = lens _dvapVaultName (\ s a -> s{_dvapVaultName = a})

instance AWSRequest DeleteVaultAccessPolicy where
        type Rs DeleteVaultAccessPolicy =
             DeleteVaultAccessPolicyResponse
        request = delete glacier
        response
          = receiveNull DeleteVaultAccessPolicyResponse'

instance Hashable DeleteVaultAccessPolicy where

instance NFData DeleteVaultAccessPolicy where

instance ToHeaders DeleteVaultAccessPolicy where
        toHeaders = const mempty

instance ToPath DeleteVaultAccessPolicy where
        toPath DeleteVaultAccessPolicy'{..}
          = mconcat
              ["/", toBS _dvapAccountId, "/vaults/",
               toBS _dvapVaultName, "/access-policy"]

instance ToQuery DeleteVaultAccessPolicy where
        toQuery = const mempty

-- | /See:/ 'deleteVaultAccessPolicyResponse' smart constructor.
data DeleteVaultAccessPolicyResponse =
  DeleteVaultAccessPolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVaultAccessPolicyResponse' with the minimum fields required to make a request.
--
deleteVaultAccessPolicyResponse
    :: DeleteVaultAccessPolicyResponse
deleteVaultAccessPolicyResponse = DeleteVaultAccessPolicyResponse'


instance NFData DeleteVaultAccessPolicyResponse where
