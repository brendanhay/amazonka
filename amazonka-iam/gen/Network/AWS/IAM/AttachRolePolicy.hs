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

-- Module      : Network.AWS.IAM.AttachRolePolicy
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

-- | Attaches the specified managed policy to the specified role.
--
-- When you attach a managed policy to a role, the managed policy is used as
-- the role's access (permissions) policy. You cannot use a managed policy as
-- the role's trust policy. The role's trust policy is created at the same time
-- as the role, using 'CreateRole'. You can update a role's trust policy using 'UpdateAssumeRolePolicy'.
--
-- Use this API to attach a managed policy to a role. To embed an inline policy
-- in a role, use 'PutRolePolicy'. For more information about policies, refer to <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_AttachRolePolicy.html>
module Network.AWS.IAM.AttachRolePolicy
    (
    -- * Request
      AttachRolePolicy
    -- ** Request constructor
    , attachRolePolicy
    -- ** Request lenses
    , arpPolicyArn
    , arpRoleName

    -- * Response
    , AttachRolePolicyResponse
    -- ** Response constructor
    , attachRolePolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data AttachRolePolicy = AttachRolePolicy
    { _arpPolicyArn :: Text
    , _arpRoleName  :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'AttachRolePolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'arpPolicyArn' @::@ 'Text'
--
-- * 'arpRoleName' @::@ 'Text'
--
attachRolePolicy :: Text -- ^ 'arpRoleName'
                 -> Text -- ^ 'arpPolicyArn'
                 -> AttachRolePolicy
attachRolePolicy p1 p2 = AttachRolePolicy
    { _arpRoleName  = p1
    , _arpPolicyArn = p2
    }

arpPolicyArn :: Lens' AttachRolePolicy Text
arpPolicyArn = lens _arpPolicyArn (\s a -> s { _arpPolicyArn = a })

-- | The name (friendly name, not ARN) of the role to attach the policy to.
arpRoleName :: Lens' AttachRolePolicy Text
arpRoleName = lens _arpRoleName (\s a -> s { _arpRoleName = a })

data AttachRolePolicyResponse = AttachRolePolicyResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'AttachRolePolicyResponse' constructor.
attachRolePolicyResponse :: AttachRolePolicyResponse
attachRolePolicyResponse = AttachRolePolicyResponse

instance ToPath AttachRolePolicy where
    toPath = const "/"

instance ToQuery AttachRolePolicy where
    toQuery AttachRolePolicy{..} = mconcat
        [ "PolicyArn" =? _arpPolicyArn
        , "RoleName"  =? _arpRoleName
        ]

instance ToHeaders AttachRolePolicy

instance AWSRequest AttachRolePolicy where
    type Sv AttachRolePolicy = IAM
    type Rs AttachRolePolicy = AttachRolePolicyResponse

    request  = post "AttachRolePolicy"
    response = nullResponse AttachRolePolicyResponse
