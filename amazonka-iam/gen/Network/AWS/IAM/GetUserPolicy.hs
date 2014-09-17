{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.GetUserPolicy
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
module Network.AWS.IAM.GetUserPolicy
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
    -- ** Response constructor
    , mkGetUserPolicyResponse
    -- ** Response lenses
    , guprUserName
    , guprPolicyName
    , guprPolicyDocument
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data GetUserPolicy = GetUserPolicy
    { _gupUserName :: Text
    , _gupPolicyName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetUserPolicy' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @UserName ::@ @Text@
--
-- * @PolicyName ::@ @Text@
--
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
    { _guprUserName :: Text
    , _guprPolicyName :: Text
    , _guprPolicyDocument :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetUserPolicyResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @UserName ::@ @Text@
--
-- * @PolicyName ::@ @Text@
--
-- * @PolicyDocument ::@ @Text@
--
mkGetUserPolicyResponse :: Text -- ^ 'guprUserName'
                        -> Text -- ^ 'guprPolicyName'
                        -> Text -- ^ 'guprPolicyDocument'
                        -> GetUserPolicyResponse
mkGetUserPolicyResponse p1 p2 p3 = GetUserPolicyResponse
    { _guprUserName = p1
    , _guprPolicyName = p2
    , _guprPolicyDocument = p3
    }

-- | The user the policy is associated with.
guprUserName :: Lens' GetUserPolicyResponse Text
guprUserName = lens _guprUserName (\s a -> s { _guprUserName = a })

-- | The name of the policy.
guprPolicyName :: Lens' GetUserPolicyResponse Text
guprPolicyName = lens _guprPolicyName (\s a -> s { _guprPolicyName = a })

-- | The policy document.
guprPolicyDocument :: Lens' GetUserPolicyResponse Text
guprPolicyDocument =
    lens _guprPolicyDocument (\s a -> s { _guprPolicyDocument = a })

instance FromXML GetUserPolicyResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetUserPolicy where
    type Sv GetUserPolicy = IAM
    type Rs GetUserPolicy = GetUserPolicyResponse

    request = post "GetUserPolicy"
    response _ = xmlResponse
