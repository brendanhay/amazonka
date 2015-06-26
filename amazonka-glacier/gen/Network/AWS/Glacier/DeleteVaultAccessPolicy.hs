{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Glacier.DeleteVaultAccessPolicy
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

-- | This operation deletes the access policy associated with the specified
-- vault. The operation is eventually consistent—that is, it might take
-- some time for Amazon Glacier to completely remove the access policy, and
-- you might still see the effect of the policy for a short time after you
-- send the delete request.
--
-- This operation is idempotent. You can invoke delete multiple times, even
-- if there is no policy associated with the vault. For more information
-- about vault access policies, see
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-access-policy.html Amazon Glacier Access Control with Vault Access Policies>.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-DeleteVaultAccessPolicy.html>
module Network.AWS.Glacier.DeleteVaultAccessPolicy
    (
    -- * Request
      DeleteVaultAccessPolicy
    -- ** Request constructor
    , deleteVaultAccessPolicy
    -- ** Request lenses
    , dvapAccountId
    , dvapVaultName

    -- * Response
    , DeleteVaultAccessPolicyResponse
    -- ** Response constructor
    , deleteVaultAccessPolicyResponse
    ) where

import Network.AWS.Glacier.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | DeleteVaultAccessPolicy input.
--
-- /See:/ 'deleteVaultAccessPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvapAccountId'
--
-- * 'dvapVaultName'
data DeleteVaultAccessPolicy = DeleteVaultAccessPolicy'{_dvapAccountId :: Text, _dvapVaultName :: Text} deriving (Eq, Read, Show)

-- | 'DeleteVaultAccessPolicy' smart constructor.
deleteVaultAccessPolicy :: Text -> Text -> DeleteVaultAccessPolicy
deleteVaultAccessPolicy pAccountId pVaultName = DeleteVaultAccessPolicy'{_dvapAccountId = pAccountId, _dvapVaultName = pVaultName};

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (apos-apos) in the ID.
dvapAccountId :: Lens' DeleteVaultAccessPolicy Text
dvapAccountId = lens _dvapAccountId (\ s a -> s{_dvapAccountId = a});

-- | The name of the vault.
dvapVaultName :: Lens' DeleteVaultAccessPolicy Text
dvapVaultName = lens _dvapVaultName (\ s a -> s{_dvapVaultName = a});

instance AWSRequest DeleteVaultAccessPolicy where
        type Sv DeleteVaultAccessPolicy = Glacier
        type Rs DeleteVaultAccessPolicy =
             DeleteVaultAccessPolicyResponse
        request = delete
        response
          = receiveNull DeleteVaultAccessPolicyResponse'

instance ToHeaders DeleteVaultAccessPolicy where
        toHeaders = const mempty

instance ToPath DeleteVaultAccessPolicy where
        toPath DeleteVaultAccessPolicy'{..}
          = mconcat
              ["/", toText _dvapAccountId, "/vaults/",
               toText _dvapVaultName, "/access-policy"]

instance ToQuery DeleteVaultAccessPolicy where
        toQuery = const mempty

-- | /See:/ 'deleteVaultAccessPolicyResponse' smart constructor.
data DeleteVaultAccessPolicyResponse = DeleteVaultAccessPolicyResponse' deriving (Eq, Read, Show)

-- | 'DeleteVaultAccessPolicyResponse' smart constructor.
deleteVaultAccessPolicyResponse :: DeleteVaultAccessPolicyResponse
deleteVaultAccessPolicyResponse = DeleteVaultAccessPolicyResponse';
