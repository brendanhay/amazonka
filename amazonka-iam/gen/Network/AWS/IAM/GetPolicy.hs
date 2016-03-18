{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified managed policy, including the
-- policy\'s default version and the total number of users, groups, and
-- roles that the policy is attached to. For a list of the specific users,
-- groups, and roles that the policy is attached to, use the
-- < ListEntitiesForPolicy> API. This API returns metadata about the
-- policy. To retrieve the policy document for a specific version of the
-- policy, use < GetPolicyVersion>.
--
-- This API retrieves information about managed policies. To retrieve
-- information about an inline policy that is embedded with a user, group,
-- or role, use the < GetUserPolicy>, < GetGroupPolicy>, or
-- < GetRolePolicy> API.
--
-- For more information about policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /IAM User Guide/.
module Network.AWS.IAM.GetPolicy
    (
    -- * Creating a Request
      getPolicy
    , GetPolicy
    -- * Request Lenses
    , gpPolicyARN

    -- * Destructuring the Response
    , getPolicyResponse
    , GetPolicyResponse
    -- * Response Lenses
    , gprsPolicy
    , gprsResponseStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getPolicy' smart constructor.
newtype GetPolicy = GetPolicy'
    { _gpPolicyARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpPolicyARN'
getPolicy
    :: Text -- ^ 'gpPolicyARN'
    -> GetPolicy
getPolicy pPolicyARN_ =
    GetPolicy'
    { _gpPolicyARN = pPolicyARN_
    }

-- | Undocumented member.
gpPolicyARN :: Lens' GetPolicy Text
gpPolicyARN = lens _gpPolicyARN (\ s a -> s{_gpPolicyARN = a});

instance AWSRequest GetPolicy where
        type Rs GetPolicy = GetPolicyResponse
        request = postQuery iAM
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

-- | Contains the response to a successful < GetPolicy> request.
--
-- /See:/ 'getPolicyResponse' smart constructor.
data GetPolicyResponse = GetPolicyResponse'
    { _gprsPolicy         :: !(Maybe Policy)
    , _gprsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gprsPolicy'
--
-- * 'gprsResponseStatus'
getPolicyResponse
    :: Int -- ^ 'gprsResponseStatus'
    -> GetPolicyResponse
getPolicyResponse pResponseStatus_ =
    GetPolicyResponse'
    { _gprsPolicy = Nothing
    , _gprsResponseStatus = pResponseStatus_
    }

-- | Information about the policy.
gprsPolicy :: Lens' GetPolicyResponse (Maybe Policy)
gprsPolicy = lens _gprsPolicy (\ s a -> s{_gprsPolicy = a});

-- | The response status code.
gprsResponseStatus :: Lens' GetPolicyResponse Int
gprsResponseStatus = lens _gprsResponseStatus (\ s a -> s{_gprsResponseStatus = a});
