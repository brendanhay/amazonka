{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.GetRolePolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves the specified policy document for the specified role. For more
-- information about roles, go to Working with Roles. The returned policy is
-- URL-encoded according to RFC 3986. For more information about RFC 3986, go
-- to http://www.faqs.org/rfcs/rfc3986.html. https://iam.amazonaws.com/
-- ?Action=GetRolePolicy &PolicyName=S3AccessPolicy &RoleName=S3Access
-- &Version=2010-05-08 &AUTHPARAMS S3AccessPolicy S3Access
-- {"Version":"2012-10-17","Statement":[{"Effect":"Allow","Action":["s3:*"],"Resource":["*"]}]}
-- 7e7cd8bc-99ef-11e1-a4c3-27EXAMPLE804.
module Network.AWS.IAM.V2010_05_08.GetRolePolicy
    (
    -- * Request
      GetRolePolicy
    -- ** Request constructor
    , getRolePolicy
    -- ** Request lenses
    , grprPolicyName
    , grprRoleName

    -- * Response
    , GetRolePolicyResponse
    -- ** Response lenses
    , grpsPolicyDocument
    , grpsPolicyName
    , grpsRoleName
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetRolePolicy' request.
getRolePolicy :: Text -- ^ 'grprPolicyName'
              -> Text -- ^ 'grprRoleName'
              -> GetRolePolicy
getRolePolicy p1 p2 = GetRolePolicy
    { _grprPolicyName = p1
    , _grprRoleName = p2
    }
{-# INLINE getRolePolicy #-}

data GetRolePolicy = GetRolePolicy
    { _grprPolicyName :: Text
      -- ^ Name of the policy document to get.
    , _grprRoleName :: Text
      -- ^ Name of the role associated with the policy.
    } deriving (Show, Generic)

-- | Name of the policy document to get.
grprPolicyName :: Lens' GetRolePolicy (Text)
grprPolicyName f x =
    f (_grprPolicyName x)
        <&> \y -> x { _grprPolicyName = y }
{-# INLINE grprPolicyName #-}

-- | Name of the role associated with the policy.
grprRoleName :: Lens' GetRolePolicy (Text)
grprRoleName f x =
    f (_grprRoleName x)
        <&> \y -> x { _grprRoleName = y }
{-# INLINE grprRoleName #-}

instance ToQuery GetRolePolicy where
    toQuery = genericQuery def

data GetRolePolicyResponse = GetRolePolicyResponse
    { _grpsPolicyDocument :: Text
      -- ^ The policy document.
    , _grpsPolicyName :: Text
      -- ^ The name of the policy.
    , _grpsRoleName :: Text
      -- ^ The role the policy is associated with.
    } deriving (Show, Generic)

-- | The policy document.
grpsPolicyDocument :: Lens' GetRolePolicyResponse (Text)
grpsPolicyDocument f x =
    f (_grpsPolicyDocument x)
        <&> \y -> x { _grpsPolicyDocument = y }
{-# INLINE grpsPolicyDocument #-}

-- | The name of the policy.
grpsPolicyName :: Lens' GetRolePolicyResponse (Text)
grpsPolicyName f x =
    f (_grpsPolicyName x)
        <&> \y -> x { _grpsPolicyName = y }
{-# INLINE grpsPolicyName #-}

-- | The role the policy is associated with.
grpsRoleName :: Lens' GetRolePolicyResponse (Text)
grpsRoleName f x =
    f (_grpsRoleName x)
        <&> \y -> x { _grpsRoleName = y }
{-# INLINE grpsRoleName #-}

instance FromXML GetRolePolicyResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetRolePolicy where
    type Sv GetRolePolicy = IAM
    type Rs GetRolePolicy = GetRolePolicyResponse

    request = post "GetRolePolicy"
    response _ = xmlResponse
