{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.UpdateAssumeRolePolicy
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
module Network.AWS.IAM
    (
    -- * Request
      UpdateAssumeRolePolicy
    -- ** Request constructor
    , mkUpdateAssumeRolePolicy
    -- ** Request lenses
    , uarpRoleName
    , uarpPolicyDocument

    -- * Response
    , UpdateAssumeRolePolicyResponse
    -- ** Response constructor
    , mkUpdateAssumeRolePolicyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data UpdateAssumeRolePolicy = UpdateAssumeRolePolicy
    { _uarpRoleName :: !Text
    , _uarpPolicyDocument :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateAssumeRolePolicy' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @RoleName ::@ @Text@
--
-- * @PolicyDocument ::@ @Text@
--
mkUpdateAssumeRolePolicy :: Text -- ^ 'uarpRoleName'
                         -> Text -- ^ 'uarpPolicyDocument'
                         -> UpdateAssumeRolePolicy
mkUpdateAssumeRolePolicy p1 p2 = UpdateAssumeRolePolicy
    { _uarpRoleName = p1
    , _uarpPolicyDocument = p2
    }

-- | Name of the role to update.
uarpRoleName :: Lens' UpdateAssumeRolePolicy Text
uarpRoleName = lens _uarpRoleName (\s a -> s { _uarpRoleName = a })

-- | The policy that grants an entity permission to assume the role.
uarpPolicyDocument :: Lens' UpdateAssumeRolePolicy Text
uarpPolicyDocument =
    lens _uarpPolicyDocument (\s a -> s { _uarpPolicyDocument = a })

instance ToQuery UpdateAssumeRolePolicy where
    toQuery = genericQuery def

data UpdateAssumeRolePolicyResponse = UpdateAssumeRolePolicyResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateAssumeRolePolicyResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkUpdateAssumeRolePolicyResponse :: UpdateAssumeRolePolicyResponse
mkUpdateAssumeRolePolicyResponse = UpdateAssumeRolePolicyResponse

instance AWSRequest UpdateAssumeRolePolicy where
    type Sv UpdateAssumeRolePolicy = IAM
    type Rs UpdateAssumeRolePolicy = UpdateAssumeRolePolicyResponse

    request = post "UpdateAssumeRolePolicy"
    response _ = nullaryResponse UpdateAssumeRolePolicyResponse
