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
-- Module      : Network.AWS.Glacier.SetVaultAccessPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation configures an access policy for a vault and will overwrite an existing policy. To configure a vault access policy, send a PUT request to the @access-policy@ subresource of the vault. An access policy is specific to a vault and is also called a vault subresource. You can set one access policy per vault and the policy can be up to 20 KB in size. For more information about vault access policies, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-access-policy.html Amazon Glacier Access Control with Vault Access Policies> .
--
--
module Network.AWS.Glacier.SetVaultAccessPolicy
    (
    -- * Creating a Request
      setVaultAccessPolicy
    , SetVaultAccessPolicy
    -- * Request Lenses
    , svapPolicy
    , svapAccountId
    , svapVaultName

    -- * Destructuring the Response
    , setVaultAccessPolicyResponse
    , SetVaultAccessPolicyResponse
    ) where

import Network.AWS.Glacier.Types
import Network.AWS.Glacier.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | SetVaultAccessPolicy input.
--
--
--
-- /See:/ 'setVaultAccessPolicy' smart constructor.
data SetVaultAccessPolicy = SetVaultAccessPolicy'
  { _svapPolicy    :: !(Maybe VaultAccessPolicy)
  , _svapAccountId :: !Text
  , _svapVaultName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetVaultAccessPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'svapPolicy' - The vault access policy as a JSON string.
--
-- * 'svapAccountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- * 'svapVaultName' - The name of the vault.
setVaultAccessPolicy
    :: Text -- ^ 'svapAccountId'
    -> Text -- ^ 'svapVaultName'
    -> SetVaultAccessPolicy
setVaultAccessPolicy pAccountId_ pVaultName_ =
  SetVaultAccessPolicy'
    { _svapPolicy = Nothing
    , _svapAccountId = pAccountId_
    , _svapVaultName = pVaultName_
    }


-- | The vault access policy as a JSON string.
svapPolicy :: Lens' SetVaultAccessPolicy (Maybe VaultAccessPolicy)
svapPolicy = lens _svapPolicy (\ s a -> s{_svapPolicy = a})

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
svapAccountId :: Lens' SetVaultAccessPolicy Text
svapAccountId = lens _svapAccountId (\ s a -> s{_svapAccountId = a})

-- | The name of the vault.
svapVaultName :: Lens' SetVaultAccessPolicy Text
svapVaultName = lens _svapVaultName (\ s a -> s{_svapVaultName = a})

instance AWSRequest SetVaultAccessPolicy where
        type Rs SetVaultAccessPolicy =
             SetVaultAccessPolicyResponse
        request = putJSON glacier
        response = receiveNull SetVaultAccessPolicyResponse'

instance Hashable SetVaultAccessPolicy where

instance NFData SetVaultAccessPolicy where

instance ToHeaders SetVaultAccessPolicy where
        toHeaders = const mempty

instance ToJSON SetVaultAccessPolicy where
        toJSON SetVaultAccessPolicy'{..}
          = object (catMaybes [("policy" .=) <$> _svapPolicy])

instance ToPath SetVaultAccessPolicy where
        toPath SetVaultAccessPolicy'{..}
          = mconcat
              ["/", toBS _svapAccountId, "/vaults/",
               toBS _svapVaultName, "/access-policy"]

instance ToQuery SetVaultAccessPolicy where
        toQuery = const mempty

-- | /See:/ 'setVaultAccessPolicyResponse' smart constructor.
data SetVaultAccessPolicyResponse =
  SetVaultAccessPolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetVaultAccessPolicyResponse' with the minimum fields required to make a request.
--
setVaultAccessPolicyResponse
    :: SetVaultAccessPolicyResponse
setVaultAccessPolicyResponse = SetVaultAccessPolicyResponse'


instance NFData SetVaultAccessPolicyResponse where
