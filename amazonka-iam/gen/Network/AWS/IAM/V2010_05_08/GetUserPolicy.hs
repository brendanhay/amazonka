{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.GetUserPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves the specified policy document for the specified user. The
-- returned policy is URL-encoded according to RFC 3986. For more information
-- about RFC 3986, go to http://www.faqs.org/rfcs/rfc3986.html.
-- https://iam.amazonaws.com/ ?Action=GetUserPolicy &UserName=Bob
-- &PolicyName=AllAccessPolicy &AUTHPARAMS Bob AllAccessPolicy
-- {"Version":"2012-10-17","Statement":[{"Effect":"Allow","Action":"*","Resource":"*"}]}
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.GetUserPolicy
    (
    -- * Request
      GetUserPolicy
    -- ** Request constructor
    , mkGetUserPolicy
    -- ** Request lenses
    , gupUserName
    , gupPolicyName

    -- * Response
    , GetUserPolicyResponse
    -- ** Response lenses
    , guprsUserName
    , guprsPolicyName
    , guprsPolicyDocument
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | 
data GetUserPolicy = GetUserPolicy
    { _gupUserName :: Text
    , _gupPolicyName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetUserPolicy' request.
mkGetUserPolicy :: Text -- ^ 'gupUserName'
                -> Text -- ^ 'gupPolicyName'
                -> GetUserPolicy
mkGetUserPolicy p1 p2 = GetUserPolicy
    { _gupUserName = p1
    , _gupPolicyName = p2
    }

-- | Name of the user who the policy is associated with.
gupUserName :: Lens' GetUserPolicy Text
gupUserName = lens _gupUserName (\s a -> s { _gupUserName = a })

-- | Name of the policy document to get.
gupPolicyName :: Lens' GetUserPolicy Text
gupPolicyName = lens _gupPolicyName (\s a -> s { _gupPolicyName = a })

instance ToQuery GetUserPolicy where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the GetUserPolicy action.
data GetUserPolicyResponse = GetUserPolicyResponse
    { _guprsUserName :: Text
    , _guprsPolicyName :: Text
    , _guprsPolicyDocument :: Text
    } deriving (Show, Generic)

-- | The user the policy is associated with.
guprsUserName :: Lens' GetUserPolicyResponse Text
guprsUserName = lens _guprsUserName (\s a -> s { _guprsUserName = a })

-- | The name of the policy.
guprsPolicyName :: Lens' GetUserPolicyResponse Text
guprsPolicyName = lens _guprsPolicyName (\s a -> s { _guprsPolicyName = a })

-- | The policy document.
guprsPolicyDocument :: Lens' GetUserPolicyResponse Text
guprsPolicyDocument =
    lens _guprsPolicyDocument (\s a -> s { _guprsPolicyDocument = a })

instance FromXML GetUserPolicyResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetUserPolicy where
    type Sv GetUserPolicy = IAM
    type Rs GetUserPolicy = GetUserPolicyResponse

    request = post "GetUserPolicy"
    response _ = xmlResponse
