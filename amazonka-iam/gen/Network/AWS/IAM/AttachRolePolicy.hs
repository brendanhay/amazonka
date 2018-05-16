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
-- Module      : Network.AWS.IAM.AttachRolePolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the specified managed policy to the specified IAM role. When you attach a managed policy to a role, the managed policy becomes part of the role's permission (access) policy.
--
--
-- Use this API to attach a /managed/ policy to a role. To embed an inline policy in a role, use 'PutRolePolicy' . For more information about policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
module Network.AWS.IAM.AttachRolePolicy
    (
    -- * Creating a Request
      attachRolePolicy
    , AttachRolePolicy
    -- * Request Lenses
    , arpRoleName
    , arpPolicyARN

    -- * Destructuring the Response
    , attachRolePolicyResponse
    , AttachRolePolicyResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'attachRolePolicy' smart constructor.
data AttachRolePolicy = AttachRolePolicy'
  { _arpRoleName  :: !Text
  , _arpPolicyARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachRolePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arpRoleName' - The name (friendly name, not ARN) of the role to attach the policy to. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'arpPolicyARN' - The Amazon Resource Name (ARN) of the IAM policy you want to attach. For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
attachRolePolicy
    :: Text -- ^ 'arpRoleName'
    -> Text -- ^ 'arpPolicyARN'
    -> AttachRolePolicy
attachRolePolicy pRoleName_ pPolicyARN_ =
  AttachRolePolicy' {_arpRoleName = pRoleName_, _arpPolicyARN = pPolicyARN_}


-- | The name (friendly name, not ARN) of the role to attach the policy to. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
arpRoleName :: Lens' AttachRolePolicy Text
arpRoleName = lens _arpRoleName (\ s a -> s{_arpRoleName = a})

-- | The Amazon Resource Name (ARN) of the IAM policy you want to attach. For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
arpPolicyARN :: Lens' AttachRolePolicy Text
arpPolicyARN = lens _arpPolicyARN (\ s a -> s{_arpPolicyARN = a})

instance AWSRequest AttachRolePolicy where
        type Rs AttachRolePolicy = AttachRolePolicyResponse
        request = postQuery iam
        response = receiveNull AttachRolePolicyResponse'

instance Hashable AttachRolePolicy where

instance NFData AttachRolePolicy where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachRolePolicyResponse' with the minimum fields required to make a request.
--
attachRolePolicyResponse
    :: AttachRolePolicyResponse
attachRolePolicyResponse = AttachRolePolicyResponse'


instance NFData AttachRolePolicyResponse where
