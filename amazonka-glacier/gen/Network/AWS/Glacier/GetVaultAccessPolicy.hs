{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Glacier.GetVaultAccessPolicy
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

-- | This operation retrieves the @access-policy@ subresource set on the
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
    , gvaprPolicy
    , gvaprStatus
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
    } deriving (Eq,Read,Show)

-- | 'GetVaultAccessPolicy' smart constructor.
getVaultAccessPolicy :: Text -> Text -> GetVaultAccessPolicy
getVaultAccessPolicy pAccountId pVaultName =
    GetVaultAccessPolicy'
    { _gvapAccountId = pAccountId
    , _gvapVaultName = pVaultName
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
                   (x .?> "policy") <*> (pure s))

instance ToHeaders GetVaultAccessPolicy where
        toHeaders = const mempty

instance ToPath GetVaultAccessPolicy where
        toPath GetVaultAccessPolicy'{..}
          = mconcat
              ["/", toText _gvapAccountId, "/vaults/",
               toText _gvapVaultName, "/access-policy"]

instance ToQuery GetVaultAccessPolicy where
        toQuery = const mempty

-- | Output for GetVaultAccessPolicy.
--
-- /See:/ 'getVaultAccessPolicyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gvaprPolicy'
--
-- * 'gvaprStatus'
data GetVaultAccessPolicyResponse = GetVaultAccessPolicyResponse'
    { _gvaprPolicy :: !(Maybe VaultAccessPolicy)
    , _gvaprStatus :: !Status
    } deriving (Eq,Read,Show)

-- | 'GetVaultAccessPolicyResponse' smart constructor.
getVaultAccessPolicyResponse :: Status -> GetVaultAccessPolicyResponse
getVaultAccessPolicyResponse pStatus =
    GetVaultAccessPolicyResponse'
    { _gvaprPolicy = Nothing
    , _gvaprStatus = pStatus
    }

-- | Contains the returned vault access policy as a JSON string.
gvaprPolicy :: Lens' GetVaultAccessPolicyResponse (Maybe VaultAccessPolicy)
gvaprPolicy = lens _gvaprPolicy (\ s a -> s{_gvaprPolicy = a});

-- | FIXME: Undocumented member.
gvaprStatus :: Lens' GetVaultAccessPolicyResponse Status
gvaprStatus = lens _gvaprStatus (\ s a -> s{_gvaprStatus = a});
