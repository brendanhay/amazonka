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
-- Module      : Network.AWS.Glacier.GetVaultAccessPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation retrieves the 'access-policy' subresource set on the
-- vault; for more information on setting this subresource, see
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-SetVaultAccessPolicy.html Set Vault Access Policy (PUT access-policy)>.
-- If there is no access policy set on the vault, the operation returns a
-- '404 Not found' error. For more information about vault access policies,
-- see
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-access-policy.html Amazon Glacier Access Control with Vault Access Policies>.
--
-- /See:/ <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-GetVaultAccessPolicy.html AWS API Reference> for GetVaultAccessPolicy.
module Network.AWS.Glacier.GetVaultAccessPolicy
    (
    -- * Creating a Request
      getVaultAccessPolicy
    , GetVaultAccessPolicy
    -- * Request Lenses
    , gvapAccountId
    , gvapVaultName

    -- * Destructuring the Response
    , getVaultAccessPolicyResponse
    , GetVaultAccessPolicyResponse
    -- * Response Lenses
    , gvaprsPolicy
    , gvaprsStatus
    ) where

import           Network.AWS.Glacier.Types
import           Network.AWS.Glacier.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input for GetVaultAccessPolicy.
--
-- /See:/ 'getVaultAccessPolicy' smart constructor.
data GetVaultAccessPolicy = GetVaultAccessPolicy'
    { _gvapAccountId :: !Text
    , _gvapVaultName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetVaultAccessPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gvapAccountId'
--
-- * 'gvapVaultName'
getVaultAccessPolicy
    :: Text -- ^ 'gvapAccountId'
    -> Text -- ^ 'gvapVaultName'
    -> GetVaultAccessPolicy
getVaultAccessPolicy pAccountId_ pVaultName_ =
    GetVaultAccessPolicy'
    { _gvapAccountId = pAccountId_
    , _gvapVaultName = pVaultName_
    }

-- | The 'AccountId' value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- apos'-'apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (apos-apos) in the ID.
gvapAccountId :: Lens' GetVaultAccessPolicy Text
gvapAccountId = lens _gvapAccountId (\ s a -> s{_gvapAccountId = a});

-- | The name of the vault.
gvapVaultName :: Lens' GetVaultAccessPolicy Text
gvapVaultName = lens _gvapVaultName (\ s a -> s{_gvapVaultName = a});

instance AWSRequest GetVaultAccessPolicy where
        type Rs GetVaultAccessPolicy =
             GetVaultAccessPolicyResponse
        request = get glacier
        response
          = receiveJSON
              (\ s h x ->
                 GetVaultAccessPolicyResponse' <$>
                   (x .?> "policy") <*> (pure (fromEnum s)))

instance ToHeaders GetVaultAccessPolicy where
        toHeaders = const mempty

instance ToPath GetVaultAccessPolicy where
        toPath GetVaultAccessPolicy'{..}
          = mconcat
              ["/", toBS _gvapAccountId, "/vaults/",
               toBS _gvapVaultName, "/access-policy"]

instance ToQuery GetVaultAccessPolicy where
        toQuery = const mempty

-- | Output for GetVaultAccessPolicy.
--
-- /See:/ 'getVaultAccessPolicyResponse' smart constructor.
data GetVaultAccessPolicyResponse = GetVaultAccessPolicyResponse'
    { _gvaprsPolicy :: !(Maybe VaultAccessPolicy)
    , _gvaprsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetVaultAccessPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gvaprsPolicy'
--
-- * 'gvaprsStatus'
getVaultAccessPolicyResponse
    :: Int -- ^ 'gvaprsStatus'
    -> GetVaultAccessPolicyResponse
getVaultAccessPolicyResponse pStatus_ =
    GetVaultAccessPolicyResponse'
    { _gvaprsPolicy = Nothing
    , _gvaprsStatus = pStatus_
    }

-- | Contains the returned vault access policy as a JSON string.
gvaprsPolicy :: Lens' GetVaultAccessPolicyResponse (Maybe VaultAccessPolicy)
gvaprsPolicy = lens _gvaprsPolicy (\ s a -> s{_gvaprsPolicy = a});

-- | The response status code.
gvaprsStatus :: Lens' GetVaultAccessPolicyResponse Int
gvaprsStatus = lens _gvaprsStatus (\ s a -> s{_gvaprsStatus = a});
