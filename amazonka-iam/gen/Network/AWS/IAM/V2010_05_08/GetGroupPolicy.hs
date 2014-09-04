{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.GetGroupPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves the specified policy document for the specified group. The
-- returned policy is URL-encoded according to RFC 3986. For more information
-- about RFC 3986, go to http://www.faqs.org/rfcs/rfc3986.html.
-- https://iam.amazonaws.com/ ?Action=GetGroupPolicy &GroupName=Admins
-- &PolicyName=AdminRoot &AUTHPARAMS Admins AdminRoot
-- {"Version":"2012-10-17","Statement":[{"Effect":"Allow","Action":"*","Resource":"*"}]}
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.GetGroupPolicy
    (
    -- * Request
      GetGroupPolicy
    -- ** Request constructor
    , getGroupPolicy
    -- ** Request lenses
    , ggprGroupName
    , ggprPolicyName

    -- * Response
    , GetGroupPolicyResponse
    -- ** Response lenses
    , ggpsGroupName
    , ggpsPolicyDocument
    , ggpsPolicyName
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetGroupPolicy' request.
getGroupPolicy :: Text -- ^ 'ggprGroupName'
               -> Text -- ^ 'ggprPolicyName'
               -> GetGroupPolicy
getGroupPolicy p1 p2 = GetGroupPolicy
    { _ggprGroupName = p1
    , _ggprPolicyName = p2
    }
{-# INLINE getGroupPolicy #-}

data GetGroupPolicy = GetGroupPolicy
    { _ggprGroupName :: Text
      -- ^ Name of the group the policy is associated with.
    , _ggprPolicyName :: Text
      -- ^ Name of the policy document to get.
    } deriving (Show, Generic)

-- | Name of the group the policy is associated with.
ggprGroupName :: Lens' GetGroupPolicy (Text)
ggprGroupName f x =
    f (_ggprGroupName x)
        <&> \y -> x { _ggprGroupName = y }
{-# INLINE ggprGroupName #-}

-- | Name of the policy document to get.
ggprPolicyName :: Lens' GetGroupPolicy (Text)
ggprPolicyName f x =
    f (_ggprPolicyName x)
        <&> \y -> x { _ggprPolicyName = y }
{-# INLINE ggprPolicyName #-}

instance ToQuery GetGroupPolicy where
    toQuery = genericQuery def

data GetGroupPolicyResponse = GetGroupPolicyResponse
    { _ggpsGroupName :: Text
      -- ^ The group the policy is associated with.
    , _ggpsPolicyDocument :: Text
      -- ^ The policy document.
    , _ggpsPolicyName :: Text
      -- ^ The name of the policy.
    } deriving (Show, Generic)

-- | The group the policy is associated with.
ggpsGroupName :: Lens' GetGroupPolicyResponse (Text)
ggpsGroupName f x =
    f (_ggpsGroupName x)
        <&> \y -> x { _ggpsGroupName = y }
{-# INLINE ggpsGroupName #-}

-- | The policy document.
ggpsPolicyDocument :: Lens' GetGroupPolicyResponse (Text)
ggpsPolicyDocument f x =
    f (_ggpsPolicyDocument x)
        <&> \y -> x { _ggpsPolicyDocument = y }
{-# INLINE ggpsPolicyDocument #-}

-- | The name of the policy.
ggpsPolicyName :: Lens' GetGroupPolicyResponse (Text)
ggpsPolicyName f x =
    f (_ggpsPolicyName x)
        <&> \y -> x { _ggpsPolicyName = y }
{-# INLINE ggpsPolicyName #-}

instance FromXML GetGroupPolicyResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetGroupPolicy where
    type Sv GetGroupPolicy = IAM
    type Rs GetGroupPolicy = GetGroupPolicyResponse

    request = post "GetGroupPolicy"
    response _ = xmlResponse
