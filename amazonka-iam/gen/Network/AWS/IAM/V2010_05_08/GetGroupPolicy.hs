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
    , mkGetGroupPolicyRequest
    -- ** Request lenses
    , ggprGroupName
    , ggprPolicyName

    -- * Response
    , GetGroupPolicyResponse
    -- ** Response lenses
    , ggpsGroupName
    , ggpsPolicyName
    , ggpsPolicyDocument
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetGroupPolicy' request.
mkGetGroupPolicyRequest :: Text -- ^ 'ggprGroupName'
                        -> Text -- ^ 'ggprPolicyName'
                        -> GetGroupPolicy
mkGetGroupPolicyRequest p1 p2 = GetGroupPolicy
    { _ggprGroupName = p1
    , _ggprPolicyName = p2
    }
{-# INLINE mkGetGroupPolicyRequest #-}

data GetGroupPolicy = GetGroupPolicy
    { _ggprGroupName :: Text
      -- ^ Name of the group the policy is associated with.
    , _ggprPolicyName :: Text
      -- ^ Name of the policy document to get.
    } deriving (Show, Generic)

-- | Name of the group the policy is associated with.
ggprGroupName :: Lens' GetGroupPolicy (Text)
ggprGroupName = lens _ggprGroupName (\s a -> s { _ggprGroupName = a })
{-# INLINE ggprGroupName #-}

-- | Name of the policy document to get.
ggprPolicyName :: Lens' GetGroupPolicy (Text)
ggprPolicyName = lens _ggprPolicyName (\s a -> s { _ggprPolicyName = a })
{-# INLINE ggprPolicyName #-}

instance ToQuery GetGroupPolicy where
    toQuery = genericQuery def

data GetGroupPolicyResponse = GetGroupPolicyResponse
    { _ggpsGroupName :: Text
      -- ^ The group the policy is associated with.
    , _ggpsPolicyName :: Text
      -- ^ The name of the policy.
    , _ggpsPolicyDocument :: Text
      -- ^ The policy document.
    } deriving (Show, Generic)

-- | The group the policy is associated with.
ggpsGroupName :: Lens' GetGroupPolicyResponse (Text)
ggpsGroupName = lens _ggpsGroupName (\s a -> s { _ggpsGroupName = a })
{-# INLINE ggpsGroupName #-}

-- | The name of the policy.
ggpsPolicyName :: Lens' GetGroupPolicyResponse (Text)
ggpsPolicyName = lens _ggpsPolicyName (\s a -> s { _ggpsPolicyName = a })
{-# INLINE ggpsPolicyName #-}

-- | The policy document.
ggpsPolicyDocument :: Lens' GetGroupPolicyResponse (Text)
ggpsPolicyDocument = lens _ggpsPolicyDocument (\s a -> s { _ggpsPolicyDocument = a })
{-# INLINE ggpsPolicyDocument #-}

instance FromXML GetGroupPolicyResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetGroupPolicy where
    type Sv GetGroupPolicy = IAM
    type Rs GetGroupPolicy = GetGroupPolicyResponse

    request = post "GetGroupPolicy"
    response _ = xmlResponse
