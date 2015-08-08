{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.AttachRolePolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the specified managed policy to the specified role.
--
-- When you attach a managed policy to a role, the managed policy is used
-- as the role\'s access (permissions) policy. You cannot use a managed
-- policy as the role\'s trust policy. The role\'s trust policy is created
-- at the same time as the role, using CreateRole. You can update a role\'s
-- trust policy using UpdateAssumeRolePolicy.
--
-- Use this API to attach a managed policy to a role. To embed an inline
-- policy in a role, use PutRolePolicy. For more information about
-- policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_AttachRolePolicy.html AWS API Reference> for AttachRolePolicy.
module Network.AWS.IAM.AttachRolePolicy
    (
    -- * Creating a Request
      AttachRolePolicy
    , attachRolePolicy
    -- * Request Lenses
    , arpRoleName
    , arpPolicyARN

    -- * Destructuring the Response
    , AttachRolePolicyResponse
    , attachRolePolicyResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'attachRolePolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'arpRoleName'
--
-- * 'arpPolicyARN'
data AttachRolePolicy = AttachRolePolicy'
    { _arpRoleName  :: !Text
    , _arpPolicyARN :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AttachRolePolicy' smart constructor.
attachRolePolicy :: Text -> Text -> AttachRolePolicy
attachRolePolicy pRoleName_ pPolicyARN_ =
    AttachRolePolicy'
    { _arpRoleName = pRoleName_
    , _arpPolicyARN = pPolicyARN_
    }

-- | The name (friendly name, not ARN) of the role to attach the policy to.
arpRoleName :: Lens' AttachRolePolicy Text
arpRoleName = lens _arpRoleName (\ s a -> s{_arpRoleName = a});

-- | Undocumented member.
arpPolicyARN :: Lens' AttachRolePolicy Text
arpPolicyARN = lens _arpPolicyARN (\ s a -> s{_arpPolicyARN = a});

instance AWSRequest AttachRolePolicy where
        type Sv AttachRolePolicy = IAM
        type Rs AttachRolePolicy = AttachRolePolicyResponse
        request = postQuery
        response = receiveNull AttachRolePolicyResponse'

instance ToHeaders AttachRolePolicy where
        toHeaders = const mempty

instance ToPath AttachRolePolicy where
        toPath = const "/"

instance ToQuery AttachRolePolicy where
        toQuery AttachRolePolicy'{..}
          = mconcat
              ["Action" =: ("AttachRolePolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "RoleName" =: _arpRoleName,
               "PolicyArn" =: _arpPolicyARN]

-- | /See:/ 'attachRolePolicyResponse' smart constructor.
data AttachRolePolicyResponse =
    AttachRolePolicyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AttachRolePolicyResponse' smart constructor.
attachRolePolicyResponse :: AttachRolePolicyResponse
attachRolePolicyResponse = AttachRolePolicyResponse'
