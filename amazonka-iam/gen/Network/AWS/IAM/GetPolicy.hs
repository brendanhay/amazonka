{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.GetPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
-- policy\'s default version and the total number of users, groups, and
-- roles that the policy is attached to. For a list of the specific users,
-- groups, and roles that the policy is attached to, use the
-- ListEntitiesForPolicy API. This API returns metadata about the policy.
-- To retrieve the policy document for a specific version of the policy,
-- use GetPolicyVersion.
--
-- This API retrieves information about managed policies. To retrieve
-- information about an inline policy that is embedded with a user, group,
-- or role, use the GetUserPolicy, GetGroupPolicy, or GetRolePolicy API.
--
-- For more information about policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetPolicy.html>
module Network.AWS.IAM.GetPolicy
    (
    -- * Request
      GetPolicy
    -- ** Request constructor
    , getPolicy
    -- ** Request lenses
    , gpPolicyARN

    -- * Response
    , GetPolicyResponse
    -- ** Response constructor
    , getPolicyResponse
    -- ** Response lenses
    , gprPolicy
    , gprStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gpPolicyARN'
newtype GetPolicy = GetPolicy'
    { _gpPolicyARN :: Text
    } deriving (Eq,Read,Show)

-- | 'GetPolicy' smart constructor.
getPolicy :: Text -> GetPolicy
getPolicy pPolicyARN =
    GetPolicy'
    { _gpPolicyARN = pPolicyARN
    }

-- | FIXME: Undocumented member.
gpPolicyARN :: Lens' GetPolicy Text
gpPolicyARN = lens _gpPolicyARN (\ s a -> s{_gpPolicyARN = a});

instance AWSRequest GetPolicy where
        type Sv GetPolicy = IAM
        type Rs GetPolicy = GetPolicyResponse
        request = post
        response
          = receiveXMLWrapper "GetPolicyResult"
              (\ s h x ->
                 GetPolicyResponse' <$>
                   (x .@? "Policy") <*> (pure (fromEnum s)))

instance ToHeaders GetPolicy where
        toHeaders = const mempty

instance ToPath GetPolicy where
        toPath = const "/"

instance ToQuery GetPolicy where
        toQuery GetPolicy'{..}
          = mconcat
              ["Action" =: ("GetPolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PolicyArn" =: _gpPolicyARN]

-- | Contains the response to a successful GetPolicy request.
--
-- /See:/ 'getPolicyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gprPolicy'
--
-- * 'gprStatus'
data GetPolicyResponse = GetPolicyResponse'
    { _gprPolicy :: Maybe Policy
    , _gprStatus :: !Int
    } deriving (Eq,Read,Show)

-- | 'GetPolicyResponse' smart constructor.
getPolicyResponse :: Int -> GetPolicyResponse
getPolicyResponse pStatus =
    GetPolicyResponse'
    { _gprPolicy = Nothing
    , _gprStatus = pStatus
    }

-- | Information about the policy.
gprPolicy :: Lens' GetPolicyResponse (Maybe Policy)
gprPolicy = lens _gprPolicy (\ s a -> s{_gprPolicy = a});

-- | FIXME: Undocumented member.
gprStatus :: Lens' GetPolicyResponse Int
gprStatus = lens _gprStatus (\ s a -> s{_gprStatus = a});
