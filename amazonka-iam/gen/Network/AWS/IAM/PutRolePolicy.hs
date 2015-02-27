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

-- Module      : Network.AWS.IAM.PutRolePolicy
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

-- | Adds (or updates) an inline policy document that is embedded in the specified
-- role.
--
-- When you embed an inline policy in a role, the inline policy is used as the
-- role's access (permissions) policy. The role's trust policy is created at the
-- same time as the role, using 'CreateRole'. You can update a role's trust policy
-- using 'UpdateAssumeRolePolicy'. For more information about roles, go to <http://docs.aws.amazon.com/IAM/latest/UserGuide/roles-toplevel.html UsingRoles to Delegate Permissions and Federate Identities>.
--
-- A role can also have a managed policy attached to it. To attach a managed
-- policy to a role, use 'AttachRolePolicy'. To create a new managed policy, use 'CreatePolicy'. For information about policies, refer to <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and InlinePolicies> in the /Using IAM/ guide.
--
-- For information about limits on the number of inline policies that you can
-- embed with a role, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAM Entities> in the /Using IAM/ guide.
--
-- Because policy documents can be large, you should use POST rather than GET
-- when calling 'PutRolePolicy'. For general information about using the Query API
-- with IAM, go to <http://docs.aws.amazon.com/IAM/latest/UserGuide/IAM_UsingQueryAPI.html Making Query Requests> in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_PutRolePolicy.html>
module Network.AWS.IAM.PutRolePolicy
    (
    -- * Request
      PutRolePolicy
    -- ** Request constructor
    , putRolePolicy
    -- ** Request lenses
    , prpPolicyDocument
    , prpPolicyName
    , prpRoleName

    -- * Response
    , PutRolePolicyResponse
    -- ** Response constructor
    , putRolePolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data PutRolePolicy = PutRolePolicy
    { _prpPolicyDocument :: Text
    , _prpPolicyName     :: Text
    , _prpRoleName       :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'PutRolePolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prpPolicyDocument' @::@ 'Text'
--
-- * 'prpPolicyName' @::@ 'Text'
--
-- * 'prpRoleName' @::@ 'Text'
--
putRolePolicy :: Text -- ^ 'prpRoleName'
              -> Text -- ^ 'prpPolicyName'
              -> Text -- ^ 'prpPolicyDocument'
              -> PutRolePolicy
putRolePolicy p1 p2 p3 = PutRolePolicy
    { _prpRoleName       = p1
    , _prpPolicyName     = p2
    , _prpPolicyDocument = p3
    }

-- | The policy document.
prpPolicyDocument :: Lens' PutRolePolicy Text
prpPolicyDocument =
    lens _prpPolicyDocument (\s a -> s { _prpPolicyDocument = a })

-- | The name of the policy document.
prpPolicyName :: Lens' PutRolePolicy Text
prpPolicyName = lens _prpPolicyName (\s a -> s { _prpPolicyName = a })

-- | The name of the role to associate the policy with.
prpRoleName :: Lens' PutRolePolicy Text
prpRoleName = lens _prpRoleName (\s a -> s { _prpRoleName = a })

data PutRolePolicyResponse = PutRolePolicyResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'PutRolePolicyResponse' constructor.
putRolePolicyResponse :: PutRolePolicyResponse
putRolePolicyResponse = PutRolePolicyResponse

instance ToPath PutRolePolicy where
    toPath = const "/"

instance ToQuery PutRolePolicy where
    toQuery PutRolePolicy{..} = mconcat
        [ "PolicyDocument" =? _prpPolicyDocument
        , "PolicyName"     =? _prpPolicyName
        , "RoleName"       =? _prpRoleName
        ]

instance ToHeaders PutRolePolicy

instance AWSRequest PutRolePolicy where
    type Sv PutRolePolicy = IAM
    type Rs PutRolePolicy = PutRolePolicyResponse

    request  = post "PutRolePolicy"
    response = nullResponse PutRolePolicyResponse
