{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified managed policy, including the
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
    , gprqPolicyARN

    -- * Response
    , GetPolicyResponse
    -- ** Response constructor
    , getPolicyResponse
    -- ** Response lenses
    , gprsPolicy
    , gprsStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gprqPolicyARN'
newtype GetPolicy = GetPolicy'
    { _gprqPolicyARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetPolicy' smart constructor.
getPolicy :: Text -> GetPolicy
getPolicy pPolicyARN =
    GetPolicy'
    { _gprqPolicyARN = pPolicyARN
    }

-- | FIXME: Undocumented member.
gprqPolicyARN :: Lens' GetPolicy Text
gprqPolicyARN = lens _gprqPolicyARN (\ s a -> s{_gprqPolicyARN = a});

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
               "PolicyArn" =: _gprqPolicyARN]

-- | Contains the response to a successful GetPolicy request.
--
-- /See:/ 'getPolicyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gprsPolicy'
--
-- * 'gprsStatus'
data GetPolicyResponse = GetPolicyResponse'
    { _gprsPolicy :: !(Maybe Policy)
    , _gprsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetPolicyResponse' smart constructor.
getPolicyResponse :: Int -> GetPolicyResponse
getPolicyResponse pStatus =
    GetPolicyResponse'
    { _gprsPolicy = Nothing
    , _gprsStatus = pStatus
    }

-- | Information about the policy.
gprsPolicy :: Lens' GetPolicyResponse (Maybe Policy)
gprsPolicy = lens _gprsPolicy (\ s a -> s{_gprsPolicy = a});

-- | FIXME: Undocumented member.
gprsStatus :: Lens' GetPolicyResponse Int
gprsStatus = lens _gprsStatus (\ s a -> s{_gprsStatus = a});
