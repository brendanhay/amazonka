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

-- Module      : Network.AWS.IAM.GetPolicy
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

-- | Retrieves information about the specified managed policy, including the
-- policy's default version and the total number of users, groups, and roles
-- that the policy is attached to. For a list of the specific users, groups, and
-- roles that the policy is attached to, use the 'ListEntitiesForPolicy' API. This
-- API returns metadata about the policy. To retrieve the policy document for a
-- specific version of the policy, use 'GetPolicyVersion'.
--
-- This API retrieves information about managed policies. To retrieve
-- information about an inline policy that is embedded with a user, group, or
-- role, use the 'GetUserPolicy', 'GetGroupPolicy', or 'GetRolePolicy' API.
--
-- For more information about policies, refer to <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and InlinePolicies> in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetPolicy.html>
module Network.AWS.IAM.GetPolicy
    (
    -- * Request
      GetPolicy
    -- ** Request constructor
    , getPolicy
    -- ** Request lenses
    , gpPolicyArn

    -- * Response
    , GetPolicyResponse
    -- ** Response constructor
    , getPolicyResponse
    -- ** Response lenses
    , gprPolicy
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

newtype GetPolicy = GetPolicy
    { _gpPolicyArn :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'GetPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gpPolicyArn' @::@ 'Text'
--
getPolicy :: Text -- ^ 'gpPolicyArn'
          -> GetPolicy
getPolicy p1 = GetPolicy
    { _gpPolicyArn = p1
    }

gpPolicyArn :: Lens' GetPolicy Text
gpPolicyArn = lens _gpPolicyArn (\s a -> s { _gpPolicyArn = a })

newtype GetPolicyResponse = GetPolicyResponse
    { _gprPolicy :: Maybe Policy
    } deriving (Eq, Read, Show)

-- | 'GetPolicyResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gprPolicy' @::@ 'Maybe' 'Policy'
--
getPolicyResponse :: GetPolicyResponse
getPolicyResponse = GetPolicyResponse
    { _gprPolicy = Nothing
    }

-- | Information about the policy.
gprPolicy :: Lens' GetPolicyResponse (Maybe Policy)
gprPolicy = lens _gprPolicy (\s a -> s { _gprPolicy = a })

instance ToPath GetPolicy where
    toPath = const "/"

instance ToQuery GetPolicy where
    toQuery GetPolicy{..} = mconcat
        [ "PolicyArn" =? _gpPolicyArn
        ]

instance ToHeaders GetPolicy

instance AWSRequest GetPolicy where
    type Sv GetPolicy = IAM
    type Rs GetPolicy = GetPolicyResponse

    request  = post "GetPolicy"
    response = xmlResponse

instance FromXML GetPolicyResponse where
    parseXML = withElement "GetPolicyResult" $ \x -> GetPolicyResponse
        <$> x .@? "Policy"
