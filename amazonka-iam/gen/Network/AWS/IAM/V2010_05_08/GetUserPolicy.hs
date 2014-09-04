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
    , getUserPolicy
    -- ** Request lenses
    , guprUserName
    , guprPolicyName

    -- * Response
    , GetUserPolicyResponse
    -- ** Response lenses
    , gupsUserName
    , gupsPolicyDocument
    , gupsPolicyName
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetUserPolicy' request.
getUserPolicy :: Text -- ^ 'guprUserName'
              -> Text -- ^ 'guprPolicyName'
              -> GetUserPolicy
getUserPolicy p1 p2 = GetUserPolicy
    { _guprUserName = p1
    , _guprPolicyName = p2
    }
{-# INLINE getUserPolicy #-}

data GetUserPolicy = GetUserPolicy
    { _guprUserName :: Text
      -- ^ Name of the user who the policy is associated with.
    , _guprPolicyName :: Text
      -- ^ Name of the policy document to get.
    } deriving (Show, Generic)

-- | Name of the user who the policy is associated with.
guprUserName :: Lens' GetUserPolicy (Text)
guprUserName f x =
    f (_guprUserName x)
        <&> \y -> x { _guprUserName = y }
{-# INLINE guprUserName #-}

-- | Name of the policy document to get.
guprPolicyName :: Lens' GetUserPolicy (Text)
guprPolicyName f x =
    f (_guprPolicyName x)
        <&> \y -> x { _guprPolicyName = y }
{-# INLINE guprPolicyName #-}

instance ToQuery GetUserPolicy where
    toQuery = genericQuery def

data GetUserPolicyResponse = GetUserPolicyResponse
    { _gupsUserName :: Text
      -- ^ The user the policy is associated with.
    , _gupsPolicyDocument :: Text
      -- ^ The policy document.
    , _gupsPolicyName :: Text
      -- ^ The name of the policy.
    } deriving (Show, Generic)

-- | The user the policy is associated with.
gupsUserName :: Lens' GetUserPolicyResponse (Text)
gupsUserName f x =
    f (_gupsUserName x)
        <&> \y -> x { _gupsUserName = y }
{-# INLINE gupsUserName #-}

-- | The policy document.
gupsPolicyDocument :: Lens' GetUserPolicyResponse (Text)
gupsPolicyDocument f x =
    f (_gupsPolicyDocument x)
        <&> \y -> x { _gupsPolicyDocument = y }
{-# INLINE gupsPolicyDocument #-}

-- | The name of the policy.
gupsPolicyName :: Lens' GetUserPolicyResponse (Text)
gupsPolicyName f x =
    f (_gupsPolicyName x)
        <&> \y -> x { _gupsPolicyName = y }
{-# INLINE gupsPolicyName #-}

instance FromXML GetUserPolicyResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetUserPolicy where
    type Sv GetUserPolicy = IAM
    type Rs GetUserPolicy = GetUserPolicyResponse

    request = post "GetUserPolicy"
    response _ = xmlResponse
