{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.GetGroupPolicy
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
module Network.AWS.IAM.GetGroupPolicy
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
    -- ** Response constructor
    , mkGetGroupPolicyResponse
    -- ** Response lenses
    , ggprGroupName
    , ggprPolicyName
    , ggprPolicyDocument
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data GetGroupPolicy = GetGroupPolicy
    { _ggpGroupName :: !Text
    , _ggpPolicyName :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetGroupPolicy' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GroupName ::@ @Text@
--
-- * @PolicyName ::@ @Text@
--
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
    { _ggprGroupName :: !Text
    , _ggprPolicyName :: !Text
    , _ggprPolicyDocument :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetGroupPolicyResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GroupName ::@ @Text@
--
-- * @PolicyName ::@ @Text@
--
-- * @PolicyDocument ::@ @Text@
--
mkGetGroupPolicyResponse :: Text -- ^ 'ggprGroupName'
                         -> Text -- ^ 'ggprPolicyName'
                         -> Text -- ^ 'ggprPolicyDocument'
                         -> GetGroupPolicyResponse
mkGetGroupPolicyResponse p1 p2 p3 = GetGroupPolicyResponse
    { _ggprGroupName = p1
    , _ggprPolicyName = p2
    , _ggprPolicyDocument = p3
    }

-- | The group the policy is associated with.
ggprGroupName :: Lens' GetGroupPolicyResponse Text
ggprGroupName = lens _ggprGroupName (\s a -> s { _ggprGroupName = a })

-- | The name of the policy.
ggprPolicyName :: Lens' GetGroupPolicyResponse Text
ggprPolicyName = lens _ggprPolicyName (\s a -> s { _ggprPolicyName = a })

-- | The policy document.
ggprPolicyDocument :: Lens' GetGroupPolicyResponse Text
ggprPolicyDocument =
    lens _ggprPolicyDocument (\s a -> s { _ggprPolicyDocument = a })

instance FromXML GetGroupPolicyResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetGroupPolicy where
    type Sv GetGroupPolicy = IAM
    type Rs GetGroupPolicy = GetGroupPolicyResponse

    request = post "GetGroupPolicy"
    response _ = xmlResponse
