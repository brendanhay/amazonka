{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.PutRolePolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds (or updates) a policy document associated with the specified role. For
-- information about policies, go to Overview of Policies in the Using IAM
-- guide. For information about limits on the policies you can associate with
-- a role, see Limitations on IAM Entities in the Using IAM guide. Because
-- policy documents can be large, you should use POST rather than GET when
-- calling PutRolePolicy. For information about setting up signatures and
-- authorization through the API, go to Signing AWS API Requests in the AWS
-- General Reference. For general information about using the Query API with
-- IAM, go to Making Query Requests in the Using IAM guide.
-- https://iam.amazonaws.com/ ?Action=PutRolePolicy &RoleName=S3Access
-- &PolicyName=S3AccessPolicy
-- &PolicyDocument={"Version":"2012-10-17","Statement":[{"Effect":"Allow","Action":"s3:*","Resource":"*"}]}
-- &Version=2010-05-08 &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.PutRolePolicy
    (
    -- * Request
      PutRolePolicy
    -- ** Request constructor
    , putRolePolicy
    -- ** Request lenses
    , prprPolicyDocument
    , prprPolicyName
    , prprRoleName

    -- * Response
    , PutRolePolicyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'PutRolePolicy' request.
putRolePolicy :: Text -- ^ 'prprPolicyDocument'
              -> Text -- ^ 'prprPolicyName'
              -> Text -- ^ 'prprRoleName'
              -> PutRolePolicy
putRolePolicy p1 p2 p3 = PutRolePolicy
    { _prprPolicyDocument = p1
    , _prprPolicyName = p2
    , _prprRoleName = p3
    }
{-# INLINE putRolePolicy #-}

data PutRolePolicy = PutRolePolicy
    { _prprPolicyDocument :: Text
      -- ^ The policy document.
    , _prprPolicyName :: Text
      -- ^ Name of the policy document.
    , _prprRoleName :: Text
      -- ^ Name of the role to associate the policy with.
    } deriving (Show, Generic)

-- | The policy document.
prprPolicyDocument :: Lens' PutRolePolicy (Text)
prprPolicyDocument f x =
    f (_prprPolicyDocument x)
        <&> \y -> x { _prprPolicyDocument = y }
{-# INLINE prprPolicyDocument #-}

-- | Name of the policy document.
prprPolicyName :: Lens' PutRolePolicy (Text)
prprPolicyName f x =
    f (_prprPolicyName x)
        <&> \y -> x { _prprPolicyName = y }
{-# INLINE prprPolicyName #-}

-- | Name of the role to associate the policy with.
prprRoleName :: Lens' PutRolePolicy (Text)
prprRoleName f x =
    f (_prprRoleName x)
        <&> \y -> x { _prprRoleName = y }
{-# INLINE prprRoleName #-}

instance ToQuery PutRolePolicy where
    toQuery = genericQuery def

data PutRolePolicyResponse = PutRolePolicyResponse
    deriving (Eq, Show, Generic)

instance AWSRequest PutRolePolicy where
    type Sv PutRolePolicy = IAM
    type Rs PutRolePolicy = PutRolePolicyResponse

    request = post "PutRolePolicy"
    response _ = nullaryResponse PutRolePolicyResponse
