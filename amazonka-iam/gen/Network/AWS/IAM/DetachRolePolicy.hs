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
-- Module      : Network.AWS.IAM.DetachRolePolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified managed policy from the specified role.
--
-- A role can also have inline policies embedded with it. To delete an
-- inline policy, use the DeleteRolePolicy API. For information about
-- policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DetachRolePolicy.html AWS API Reference> for DetachRolePolicy.
module Network.AWS.IAM.DetachRolePolicy
    (
    -- * Creating a Request
      detachRolePolicy
    , DetachRolePolicy
    -- * Request Lenses
    , drpRoleName
    , drpPolicyARN

    -- * Destructuring the Response
    , detachRolePolicyResponse
    , DetachRolePolicyResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'detachRolePolicy' smart constructor.
data DetachRolePolicy = DetachRolePolicy'
    { _drpRoleName  :: !Text
    , _drpPolicyARN :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DetachRolePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drpRoleName'
--
-- * 'drpPolicyARN'
detachRolePolicy
    :: Text -- ^ 'drpRoleName'
    -> Text -- ^ 'drpPolicyARN'
    -> DetachRolePolicy
detachRolePolicy pRoleName_ pPolicyARN_ =
    DetachRolePolicy'
    { _drpRoleName = pRoleName_
    , _drpPolicyARN = pPolicyARN_
    }

-- | The name (friendly name, not ARN) of the role to detach the policy from.
drpRoleName :: Lens' DetachRolePolicy Text
drpRoleName = lens _drpRoleName (\ s a -> s{_drpRoleName = a});

-- | Undocumented member.
drpPolicyARN :: Lens' DetachRolePolicy Text
drpPolicyARN = lens _drpPolicyARN (\ s a -> s{_drpPolicyARN = a});

instance AWSRequest DetachRolePolicy where
        type Rs DetachRolePolicy = DetachRolePolicyResponse
        request = postQuery iAM
        response = receiveNull DetachRolePolicyResponse'

instance ToHeaders DetachRolePolicy where
        toHeaders = const mempty

instance ToPath DetachRolePolicy where
        toPath = const "/"

instance ToQuery DetachRolePolicy where
        toQuery DetachRolePolicy'{..}
          = mconcat
              ["Action" =: ("DetachRolePolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "RoleName" =: _drpRoleName,
               "PolicyArn" =: _drpPolicyARN]

-- | /See:/ 'detachRolePolicyResponse' smart constructor.
data DetachRolePolicyResponse =
    DetachRolePolicyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DetachRolePolicyResponse' with the minimum fields required to make a request.
--
detachRolePolicyResponse
    :: DetachRolePolicyResponse
detachRolePolicyResponse = DetachRolePolicyResponse'
