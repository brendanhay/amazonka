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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified managed policy from the specified role.
--
--
-- A role can also have inline policies embedded with it. To delete an inline policy, use the 'DeleteRolePolicy' API. For information about policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
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

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detachRolePolicy' smart constructor.
data DetachRolePolicy = DetachRolePolicy'
  { _drpRoleName  :: !Text
  , _drpPolicyARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachRolePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drpRoleName' - The name (friendly name, not ARN) of the IAM role to detach the policy from. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'drpPolicyARN' - The Amazon Resource Name (ARN) of the IAM policy you want to detach. For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
detachRolePolicy
    :: Text -- ^ 'drpRoleName'
    -> Text -- ^ 'drpPolicyARN'
    -> DetachRolePolicy
detachRolePolicy pRoleName_ pPolicyARN_ =
  DetachRolePolicy' {_drpRoleName = pRoleName_, _drpPolicyARN = pPolicyARN_}


-- | The name (friendly name, not ARN) of the IAM role to detach the policy from. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
drpRoleName :: Lens' DetachRolePolicy Text
drpRoleName = lens _drpRoleName (\ s a -> s{_drpRoleName = a})

-- | The Amazon Resource Name (ARN) of the IAM policy you want to detach. For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
drpPolicyARN :: Lens' DetachRolePolicy Text
drpPolicyARN = lens _drpPolicyARN (\ s a -> s{_drpPolicyARN = a})

instance AWSRequest DetachRolePolicy where
        type Rs DetachRolePolicy = DetachRolePolicyResponse
        request = postQuery iam
        response = receiveNull DetachRolePolicyResponse'

instance Hashable DetachRolePolicy where

instance NFData DetachRolePolicy where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachRolePolicyResponse' with the minimum fields required to make a request.
--
detachRolePolicyResponse
    :: DetachRolePolicyResponse
detachRolePolicyResponse = DetachRolePolicyResponse'


instance NFData DetachRolePolicyResponse where
