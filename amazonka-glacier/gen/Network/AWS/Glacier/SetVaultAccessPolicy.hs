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

-- Module      : Network.AWS.Glacier.SetVaultAccessPolicy
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

-- | This operation configures an access policy for a vault and will overwrite an
-- existing policy. To configure a vault access policy, send a PUT request to
-- the 'access-policy' subresource of the vault. An access policy is specific to a
-- vault and is also called a vault subresource. You can set one access policy
-- per vault and the policy can be up to 20 KB in size. For more information
-- about vault access policies, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-access-policy.html Amazon Glacier Access Control with VaultAccess Policies>.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-SetVaultAccessPolicy.html>
module Network.AWS.Glacier.SetVaultAccessPolicy
    (
    -- * Request
      SetVaultAccessPolicy
    -- ** Request constructor
    , setVaultAccessPolicy
    -- ** Request lenses
    , svapAccountId
    , svapPolicy
    , svapVaultName

    -- * Response
    , SetVaultAccessPolicyResponse
    -- ** Response constructor
    , setVaultAccessPolicyResponse
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Glacier.Types
import qualified GHC.Exts

data SetVaultAccessPolicy = SetVaultAccessPolicy
    { _svapAccountId :: Text
    , _svapPolicy    :: Maybe VaultAccessPolicy
    , _svapVaultName :: Text
    } deriving (Eq, Read, Show)

-- | 'SetVaultAccessPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'svapAccountId' @::@ 'Text'
--
-- * 'svapPolicy' @::@ 'Maybe' 'VaultAccessPolicy'
--
-- * 'svapVaultName' @::@ 'Text'
--
setVaultAccessPolicy :: Text -- ^ 'svapAccountId'
                     -> Text -- ^ 'svapVaultName'
                     -> SetVaultAccessPolicy
setVaultAccessPolicy p1 p2 = SetVaultAccessPolicy
    { _svapAccountId = p1
    , _svapVaultName = p2
    , _svapPolicy    = Nothing
    }

-- | The 'AccountId' value is the AWS account ID of the account that owns the vault.
-- You can either specify an AWS account ID or optionally a single apos'-'apos
-- (hyphen), in which case Amazon Glacier uses the AWS account ID associated
-- with the credentials used to sign the request. If you use an account ID, do
-- not include any hyphens (apos-apos) in the ID.
svapAccountId :: Lens' SetVaultAccessPolicy Text
svapAccountId = lens _svapAccountId (\s a -> s { _svapAccountId = a })

-- | The vault access policy as a JSON string.
svapPolicy :: Lens' SetVaultAccessPolicy (Maybe VaultAccessPolicy)
svapPolicy = lens _svapPolicy (\s a -> s { _svapPolicy = a })

-- | The name of the vault.
svapVaultName :: Lens' SetVaultAccessPolicy Text
svapVaultName = lens _svapVaultName (\s a -> s { _svapVaultName = a })

data SetVaultAccessPolicyResponse = SetVaultAccessPolicyResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'SetVaultAccessPolicyResponse' constructor.
setVaultAccessPolicyResponse :: SetVaultAccessPolicyResponse
setVaultAccessPolicyResponse = SetVaultAccessPolicyResponse

instance ToPath SetVaultAccessPolicy where
    toPath SetVaultAccessPolicy{..} = mconcat
        [ "/"
        , toText _svapAccountId
        , "/vaults/"
        , toText _svapVaultName
        , "/access-policy"
        ]

instance ToQuery SetVaultAccessPolicy where
    toQuery = const mempty

instance ToHeaders SetVaultAccessPolicy

instance ToJSON SetVaultAccessPolicy where
    toJSON SetVaultAccessPolicy{..} = object
        [ "policy" .= _svapPolicy
        ]

instance AWSRequest SetVaultAccessPolicy where
    type Sv SetVaultAccessPolicy = Glacier
    type Rs SetVaultAccessPolicy = SetVaultAccessPolicyResponse

    request  = put
    response = nullResponse SetVaultAccessPolicyResponse
