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

-- Module      : Network.AWS.IAM.DetachRolePolicy
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

-- | Removes the specified managed policy from the specified role.
--
-- A role can also have inline policies embedded with it. To delete an inline
-- policy, use the 'DeleteRolePolicy' API. For information about policies, refer
-- to <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DetachRolePolicy.html>
module Network.AWS.IAM.DetachRolePolicy
    (
    -- * Request
      DetachRolePolicy
    -- ** Request constructor
    , detachRolePolicy
    -- ** Request lenses
    , drpPolicyArn
    , drpRoleName

    -- * Response
    , DetachRolePolicyResponse
    -- ** Response constructor
    , detachRolePolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data DetachRolePolicy = DetachRolePolicy
    { _drpPolicyArn :: Text
    , _drpRoleName  :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DetachRolePolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drpPolicyArn' @::@ 'Text'
--
-- * 'drpRoleName' @::@ 'Text'
--
detachRolePolicy :: Text -- ^ 'drpRoleName'
                 -> Text -- ^ 'drpPolicyArn'
                 -> DetachRolePolicy
detachRolePolicy p1 p2 = DetachRolePolicy
    { _drpRoleName  = p1
    , _drpPolicyArn = p2
    }

drpPolicyArn :: Lens' DetachRolePolicy Text
drpPolicyArn = lens _drpPolicyArn (\s a -> s { _drpPolicyArn = a })

-- | The name (friendly name, not ARN) of the role to detach the policy from.
drpRoleName :: Lens' DetachRolePolicy Text
drpRoleName = lens _drpRoleName (\s a -> s { _drpRoleName = a })

data DetachRolePolicyResponse = DetachRolePolicyResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DetachRolePolicyResponse' constructor.
detachRolePolicyResponse :: DetachRolePolicyResponse
detachRolePolicyResponse = DetachRolePolicyResponse

instance ToPath DetachRolePolicy where
    toPath = const "/"

instance ToQuery DetachRolePolicy where
    toQuery DetachRolePolicy{..} = mconcat
        [ "PolicyArn" =? _drpPolicyArn
        , "RoleName"  =? _drpRoleName
        ]

instance ToHeaders DetachRolePolicy

instance AWSRequest DetachRolePolicy where
    type Sv DetachRolePolicy = IAM
    type Rs DetachRolePolicy = DetachRolePolicyResponse

    request  = post "DetachRolePolicy"
    response = nullResponse DetachRolePolicyResponse
