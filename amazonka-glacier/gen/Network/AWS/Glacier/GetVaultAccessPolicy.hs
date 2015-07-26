{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.GetVaultAccessPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation retrieves the @access-policy@ subresource set on the
-- vault; for more information on setting this subresource, see
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-SetVaultAccessPolicy.html Set Vault Access Policy (PUT access-policy)>.
-- If there is no access policy set on the vault, the operation returns a
-- @404 Not found@ error. For more information about vault access policies,
-- see
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-access-policy.html Amazon Glacier Access Control with Vault Access Policies>.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-GetVaultAccessPolicy.html>
module Network.AWS.Glacier.GetVaultAccessPolicy
    (
    -- * Request
      GetVaultAccessPolicy
    -- ** Request constructor
    , getVaultAccessPolicy
    -- ** Request lenses
    , gvapAccountId
    , gvapVaultName

    -- * Response
    , GetVaultAccessPolicyResponse
    -- ** Response constructor
    , getVaultAccessPolicyResponse
    -- ** Response lenses
    , gvaprsPolicy
    , gvaprsStatus
    ) where

import           Network.AWS.Glacier.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input for GetVaultAccessPolicy.
--
-- /See:/ 'getVaultAccessPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gvapAccountId'
--
-- * 'gvapVaultName'
data GetVaultAccessPolicy = GetVaultAccessPolicy'
    { _gvapAccountId :: !Text
    , _gvapVaultName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetVaultAccessPolicy' smart constructor.
getVaultAccessPolicy :: Text -> Text -> GetVaultAccessPolicy
getVaultAccessPolicy pAccountId_ pVaultName_ =
    GetVaultAccessPolicy'
    { _gvapAccountId = pAccountId_
    , _gvapVaultName = pVaultName_
    }

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (apos-apos) in the ID.
gvapAccountId :: Lens' GetVaultAccessPolicy Text
gvapAccountId = lens _gvapAccountId (\ s a -> s{_gvapAccountId = a});

-- | The name of the vault.
gvapVaultName :: Lens' GetVaultAccessPolicy Text
gvapVaultName = lens _gvapVaultName (\ s a -> s{_gvapVaultName = a});

instance AWSRequest GetVaultAccessPolicy where
        type Sv GetVaultAccessPolicy = Glacier
        type Rs GetVaultAccessPolicy =
             GetVaultAccessPolicyResponse
        request = get
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
              ["/", toPath _gvapAccountId, "/vaults/",
               toPath _gvapVaultName, "/access-policy"]

instance ToQuery GetVaultAccessPolicy where
        toQuery = const mempty

-- | Output for GetVaultAccessPolicy.
--
-- /See:/ 'getVaultAccessPolicyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gvaprsPolicy'
--
-- * 'gvaprsStatus'
data GetVaultAccessPolicyResponse = GetVaultAccessPolicyResponse'
    { _gvaprsPolicy :: !(Maybe VaultAccessPolicy)
    , _gvaprsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetVaultAccessPolicyResponse' smart constructor.
getVaultAccessPolicyResponse :: Int -> GetVaultAccessPolicyResponse
getVaultAccessPolicyResponse pStatus_ =
    GetVaultAccessPolicyResponse'
    { _gvaprsPolicy = Nothing
    , _gvaprsStatus = pStatus_
    }

-- | Contains the returned vault access policy as a JSON string.
gvaprsPolicy :: Lens' GetVaultAccessPolicyResponse (Maybe VaultAccessPolicy)
gvaprsPolicy = lens _gvaprsPolicy (\ s a -> s{_gvaprsPolicy = a});

-- | FIXME: Undocumented member.
gvaprsStatus :: Lens' GetVaultAccessPolicyResponse Int
gvaprsStatus = lens _gvaprsStatus (\ s a -> s{_gvaprsStatus = a});
