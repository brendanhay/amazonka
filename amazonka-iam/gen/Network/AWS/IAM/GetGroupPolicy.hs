{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
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
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Retrieves the specified policy document for the specified group. The
-- returned policy is URL-encoded according to RFC 3986. For more information
-- about RFC 3986, go to <http://www.faqs.org/rfcs/rfc3986.html http://www.faqs.org/rfcs/rfc3986.html>.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetGroupPolicy.html>
module Network.AWS.IAM.GetGroupPolicy
    (
    -- * Request
      GetGroupPolicy
    -- ** Request constructor
    , getGroupPolicy
    -- ** Request lenses
    , ggpGroupName
    , ggpPolicyName

    -- * Response
    , GetGroupPolicyResponse
    -- ** Response constructor
    , getGroupPolicyResponse
    -- ** Response lenses
    , ggprGroupName
    , ggprPolicyDocument
    , ggprPolicyName
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data GetGroupPolicy = GetGroupPolicy
    { _ggpGroupName  :: Text
    , _ggpPolicyName :: Text
    } deriving (Eq, Ord, Show)

-- | 'GetGroupPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ggpGroupName' @::@ 'Text'
--
-- * 'ggpPolicyName' @::@ 'Text'
--
getGroupPolicy :: Text -- ^ 'ggpGroupName'
               -> Text -- ^ 'ggpPolicyName'
               -> GetGroupPolicy
getGroupPolicy p1 p2 = GetGroupPolicy
    { _ggpGroupName  = p1
    , _ggpPolicyName = p2
    }

-- | The name of the group the policy is associated with.
ggpGroupName :: Lens' GetGroupPolicy Text
ggpGroupName = lens _ggpGroupName (\s a -> s { _ggpGroupName = a })

-- | The name of the policy document to get.
ggpPolicyName :: Lens' GetGroupPolicy Text
ggpPolicyName = lens _ggpPolicyName (\s a -> s { _ggpPolicyName = a })

data GetGroupPolicyResponse = GetGroupPolicyResponse
    { _ggprGroupName      :: Text
    , _ggprPolicyDocument :: Text
    , _ggprPolicyName     :: Text
    } deriving (Eq, Ord, Show)

-- | 'GetGroupPolicyResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ggprGroupName' @::@ 'Text'
--
-- * 'ggprPolicyDocument' @::@ 'Text'
--
-- * 'ggprPolicyName' @::@ 'Text'
--
getGroupPolicyResponse :: Text -- ^ 'ggprGroupName'
                       -> Text -- ^ 'ggprPolicyName'
                       -> Text -- ^ 'ggprPolicyDocument'
                       -> GetGroupPolicyResponse
getGroupPolicyResponse p1 p2 p3 = GetGroupPolicyResponse
    { _ggprGroupName      = p1
    , _ggprPolicyName     = p2
    , _ggprPolicyDocument = p3
    }

-- | The group the policy is associated with.
ggprGroupName :: Lens' GetGroupPolicyResponse Text
ggprGroupName = lens _ggprGroupName (\s a -> s { _ggprGroupName = a })

-- | The policy document.
ggprPolicyDocument :: Lens' GetGroupPolicyResponse Text
ggprPolicyDocument =
    lens _ggprPolicyDocument (\s a -> s { _ggprPolicyDocument = a })

-- | The name of the policy.
ggprPolicyName :: Lens' GetGroupPolicyResponse Text
ggprPolicyName = lens _ggprPolicyName (\s a -> s { _ggprPolicyName = a })

instance ToPath GetGroupPolicy where
    toPath = const "/"

instance ToQuery GetGroupPolicy where
    toQuery GetGroupPolicy{..} = mconcat
        [ "GroupName"  =? _ggpGroupName
        , "PolicyName" =? _ggpPolicyName
        ]

instance ToHeaders GetGroupPolicy

instance AWSRequest GetGroupPolicy where
    type Sv GetGroupPolicy = IAM
    type Rs GetGroupPolicy = GetGroupPolicyResponse

    request  = post "GetGroupPolicy"
    response = xmlResponse

instance FromXML GetGroupPolicyResponse where
    parseXML = withElement "GetGroupPolicyResult" $ \x -> GetGroupPolicyResponse
        <$> x .@  "GroupName"
        <*> x .@  "PolicyDocument"
        <*> x .@  "PolicyName"
