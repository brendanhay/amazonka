{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Glacier.DeleteVaultAccessPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation deletes the access policy associated with the specified vault.
-- The operation is eventually consistent—that is, it might take some time for
-- Amazon Glacier to completely remove the access policy, and you might still
-- see the effect of the policy for a short time after you send the delete
-- request.
--
-- This operation is idempotent. You can invoke delete multiple times, even if
-- there is no policy associated with the vault. For more information about
-- vault access policies, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-access-policy.html Amazon Glacier Access Control with Vault AccessPolicies>.
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

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Glacier.Types
import qualified GHC.Exts

data DeleteVaultAccessPolicy = DeleteVaultAccessPolicy
    { _dvapAccountId :: Text
    , _dvapVaultName :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DeleteVaultAccessPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvapAccountId' @::@ 'Text'
--
-- * 'dvapVaultName' @::@ 'Text'
--
deleteVaultAccessPolicy :: Text -- ^ 'dvapAccountId'
                        -> Text -- ^ 'dvapVaultName'
                        -> DeleteVaultAccessPolicy
deleteVaultAccessPolicy p1 p2 = DeleteVaultAccessPolicy
    { _dvapAccountId = p1
    , _dvapVaultName = p2
    }

-- | The 'AccountId' value is the AWS account ID of the account that owns the vault.
-- You can either specify an AWS account ID or optionally a single apos'-'apos
-- (hyphen), in which case Amazon Glacier uses the AWS account ID associated
-- with the credentials used to sign the request. If you use an account ID, do
-- not include any hyphens (apos-apos) in the ID.
dvapAccountId :: Lens' DeleteVaultAccessPolicy Text
dvapAccountId = lens _dvapAccountId (\s a -> s { _dvapAccountId = a })

-- | The name of the vault.
dvapVaultName :: Lens' DeleteVaultAccessPolicy Text
dvapVaultName = lens _dvapVaultName (\s a -> s { _dvapVaultName = a })

data DeleteVaultAccessPolicyResponse = DeleteVaultAccessPolicyResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeleteVaultAccessPolicyResponse' constructor.
deleteVaultAccessPolicyResponse :: DeleteVaultAccessPolicyResponse
deleteVaultAccessPolicyResponse = DeleteVaultAccessPolicyResponse

instance ToPath DeleteVaultAccessPolicy where
    toPath DeleteVaultAccessPolicy{..} = mconcat
        [ "/"
        , toText _dvapAccountId
        , "/vaults/"
        , toText _dvapVaultName
        , "/access-policy"
        ]

instance ToQuery DeleteVaultAccessPolicy where
    toQuery = const mempty

instance ToHeaders DeleteVaultAccessPolicy

instance ToJSON DeleteVaultAccessPolicy where
    toJSON = const (toJSON Empty)

instance AWSRequest DeleteVaultAccessPolicy where
    type Sv DeleteVaultAccessPolicy = Glacier
    type Rs DeleteVaultAccessPolicy = DeleteVaultAccessPolicyResponse

    request  = delete
    response = nullResponse DeleteVaultAccessPolicyResponse
