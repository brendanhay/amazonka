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
    , mkGetGroupPolicy
    -- ** Request lenses
    , ggpGroupName
    , ggpPolicyName

    -- * Response
    , GetGroupPolicyResponse
    -- ** Response lenses
    , ggprsGroupName
    , ggprsPolicyName
    , ggprsPolicyDocument
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | 
data GetGroupPolicy = GetGroupPolicy
    { _ggpGroupName :: Text
    , _ggpPolicyName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetGroupPolicy' request.
mkGetGroupPolicy :: Text -- ^ 'ggpGroupName'
                 -> Text -- ^ 'ggpPolicyName'
                 -> GetGroupPolicy
mkGetGroupPolicy p1 p2 = GetGroupPolicy
    { _ggpGroupName = p1
    , _ggpPolicyName = p2
    }

-- | Name of the group the policy is associated with.
ggpGroupName :: Lens' GetGroupPolicy Text
ggpGroupName = lens _ggpGroupName (\s a -> s { _ggpGroupName = a })

-- | Name of the policy document to get.
ggpPolicyName :: Lens' GetGroupPolicy Text
ggpPolicyName = lens _ggpPolicyName (\s a -> s { _ggpPolicyName = a })

instance ToQuery GetGroupPolicy where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the GetGroupPolicy
-- action.
data GetGroupPolicyResponse = GetGroupPolicyResponse
    { _ggprsGroupName :: Text
    , _ggprsPolicyName :: Text
    , _ggprsPolicyDocument :: Text
    } deriving (Show, Generic)

-- | The group the policy is associated with.
ggprsGroupName :: Lens' GetGroupPolicyResponse Text
ggprsGroupName = lens _ggprsGroupName (\s a -> s { _ggprsGroupName = a })

-- | The name of the policy.
ggprsPolicyName :: Lens' GetGroupPolicyResponse Text
ggprsPolicyName = lens _ggprsPolicyName (\s a -> s { _ggprsPolicyName = a })

-- | The policy document.
ggprsPolicyDocument :: Lens' GetGroupPolicyResponse Text
ggprsPolicyDocument =
    lens _ggprsPolicyDocument (\s a -> s { _ggprsPolicyDocument = a })

instance FromXML GetGroupPolicyResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetGroupPolicy where
    type Sv GetGroupPolicy = IAM
    type Rs GetGroupPolicy = GetGroupPolicyResponse

    request = post "GetGroupPolicy"
    response _ = xmlResponse
