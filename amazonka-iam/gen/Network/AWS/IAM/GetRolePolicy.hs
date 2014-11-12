{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.IAM.GetRolePolicy
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
-- to http://www.faqs.org/rfcs/rfc3986.html.
module Network.AWS.IAM.GetRolePolicy
    (
    -- * Request
      GetRolePolicy
    -- ** Request constructor
    , getRolePolicy
    -- ** Request lenses
    , grpPolicyName
    , grpRoleName

    -- * Response
    , GetRolePolicyResponse
    -- ** Response constructor
    , getRolePolicyResponse
    -- ** Response lenses
    , grprPolicyDocument
    , grprPolicyName
    , grprRoleName
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types

data GetRolePolicy = GetRolePolicy
    { _grpPolicyName :: Text
    , _grpRoleName   :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetRolePolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grpPolicyName' @::@ 'Text'
--
-- * 'grpRoleName' @::@ 'Text'
--
getRolePolicy :: Text -- ^ 'grpRoleName'
              -> Text -- ^ 'grpPolicyName'
              -> GetRolePolicy
getRolePolicy p1 p2 = GetRolePolicy
    { _grpRoleName   = p1
    , _grpPolicyName = p2
    }

-- | The name of the policy document to get.
grpPolicyName :: Lens' GetRolePolicy Text
grpPolicyName = lens _grpPolicyName (\s a -> s { _grpPolicyName = a })

-- | The name of the role associated with the policy.
grpRoleName :: Lens' GetRolePolicy Text
grpRoleName = lens _grpRoleName (\s a -> s { _grpRoleName = a })

instance ToQuery GetRolePolicy

instance ToPath GetRolePolicy where
    toPath = const "/"

data GetRolePolicyResponse = GetRolePolicyResponse
    { _grprPolicyDocument :: Text
    , _grprPolicyName     :: Text
    , _grprRoleName       :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetRolePolicyResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grprPolicyDocument' @::@ 'Text'
--
-- * 'grprPolicyName' @::@ 'Text'
--
-- * 'grprRoleName' @::@ 'Text'
--
getRolePolicyResponse :: Text -- ^ 'grprRoleName'
                      -> Text -- ^ 'grprPolicyName'
                      -> Text -- ^ 'grprPolicyDocument'
                      -> GetRolePolicyResponse
getRolePolicyResponse p1 p2 p3 = GetRolePolicyResponse
    { _grprRoleName       = p1
    , _grprPolicyName     = p2
    , _grprPolicyDocument = p3
    }

-- | The policy document.
grprPolicyDocument :: Lens' GetRolePolicyResponse Text
grprPolicyDocument =
    lens _grprPolicyDocument (\s a -> s { _grprPolicyDocument = a })

-- | The name of the policy.
grprPolicyName :: Lens' GetRolePolicyResponse Text
grprPolicyName = lens _grprPolicyName (\s a -> s { _grprPolicyName = a })

-- | The role the policy is associated with.
grprRoleName :: Lens' GetRolePolicyResponse Text
grprRoleName = lens _grprRoleName (\s a -> s { _grprRoleName = a })

instance FromXML GetRolePolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GetRolePolicyResponse"

instance AWSRequest GetRolePolicy where
    type Sv GetRolePolicy = IAM
    type Rs GetRolePolicy = GetRolePolicyResponse

    request  = post "GetRolePolicy"
    response = xmlResponse $ \h x -> GetRolePolicyResponse
        <$> x %| "PolicyDocument"
        <*> x %| "PolicyName"
        <*> x %| "RoleName"
