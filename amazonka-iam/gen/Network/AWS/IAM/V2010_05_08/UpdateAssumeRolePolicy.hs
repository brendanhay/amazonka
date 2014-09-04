{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.UpdateAssumeRolePolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the policy that grants an entity permission to assume a role. For
-- more information about roles, go to Working with Roles.
-- https://iam.amazonaws.com/ ?Action=UpdateAssumeRolePolicy
-- &PolicyDocument={"Version":"2012-10-17","Statement":[{"Effect":"Allow","Principal":{"Service":["ec2.amazonaws.com"]},"Action":["sts:AssumeRole"]}]}
-- &RoleName=S3Access &Version=2010-05-08 &AUTHPARAMS
-- 309c1671-99ed-11e1-a4c3-270EXAMPLE04.
module Network.AWS.IAM.V2010_05_08.UpdateAssumeRolePolicy
    (
    -- * Request
      UpdateAssumeRolePolicy
    -- ** Request constructor
    , updateAssumeRolePolicy
    -- ** Request lenses
    , uarprPolicyDocument
    , uarprRoleName

    -- * Response
    , UpdateAssumeRolePolicyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'UpdateAssumeRolePolicy' request.
updateAssumeRolePolicy :: Text -- ^ 'uarprPolicyDocument'
                       -> Text -- ^ 'uarprRoleName'
                       -> UpdateAssumeRolePolicy
updateAssumeRolePolicy p1 p2 = UpdateAssumeRolePolicy
    { _uarprPolicyDocument = p1
    , _uarprRoleName = p2
    }
{-# INLINE updateAssumeRolePolicy #-}

data UpdateAssumeRolePolicy = UpdateAssumeRolePolicy
    { _uarprPolicyDocument :: Text
      -- ^ The policy that grants an entity permission to assume the role.
    , _uarprRoleName :: Text
      -- ^ Name of the role to update.
    } deriving (Show, Generic)

-- | The policy that grants an entity permission to assume the role.
uarprPolicyDocument :: Lens' UpdateAssumeRolePolicy (Text)
uarprPolicyDocument f x =
    f (_uarprPolicyDocument x)
        <&> \y -> x { _uarprPolicyDocument = y }
{-# INLINE uarprPolicyDocument #-}

-- | Name of the role to update.
uarprRoleName :: Lens' UpdateAssumeRolePolicy (Text)
uarprRoleName f x =
    f (_uarprRoleName x)
        <&> \y -> x { _uarprRoleName = y }
{-# INLINE uarprRoleName #-}

instance ToQuery UpdateAssumeRolePolicy where
    toQuery = genericQuery def

data UpdateAssumeRolePolicyResponse = UpdateAssumeRolePolicyResponse
    deriving (Eq, Show, Generic)

instance AWSRequest UpdateAssumeRolePolicy where
    type Sv UpdateAssumeRolePolicy = IAM
    type Rs UpdateAssumeRolePolicy = UpdateAssumeRolePolicyResponse

    request = post "UpdateAssumeRolePolicy"
    response _ = nullaryResponse UpdateAssumeRolePolicyResponse
