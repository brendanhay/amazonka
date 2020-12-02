{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.AssociateDRTRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Authorizes the DDoS Response Team (DRT), using the specified role, to access your AWS account to assist with DDoS attack mitigation during potential attacks. This enables the DRT to inspect your AWS WAF configuration and create or update AWS WAF rules and web ACLs.
--
--
-- You can associate only one @RoleArn@ with your subscription. If you submit an @AssociateDRTRole@ request for an account that already has an associated role, the new @RoleArn@ will replace the existing @RoleArn@ .
--
-- Prior to making the @AssociateDRTRole@ request, you must attach the <https://console.aws.amazon.com/iam/home?#/policies/arn:aws:iam::aws:policy/service-role/AWSShieldDRTAccessPolicy AWSShieldDRTAccessPolicy> managed policy to the role you will specify in the request. For more information see < https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_manage-attach-detach.html Attaching and Detaching IAM Policies> . The role must also trust the service principal @drt.shield.amazonaws.com@ . For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html IAM JSON Policy Elements: Principal> .
--
-- The DRT will have access only to your AWS WAF and Shield resources. By submitting this request, you authorize the DRT to inspect your AWS WAF and Shield configuration and create and update AWS WAF rules and web ACLs on your behalf. The DRT takes these actions only if explicitly authorized by you.
--
-- You must have the @iam:PassRole@ permission to make an @AssociateDRTRole@ request. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use_passrole.html Granting a User Permissions to Pass a Role to an AWS Service> .
--
-- To use the services of the DRT and make an @AssociateDRTRole@ request, you must be subscribed to the <https://aws.amazon.com/premiumsupport/business-support/ Business Support plan> or the <https://aws.amazon.com/premiumsupport/enterprise-support/ Enterprise Support plan> .
module Network.AWS.Shield.AssociateDRTRole
  ( -- * Creating a Request
    associateDRTRole,
    AssociateDRTRole,

    -- * Request Lenses
    adrtrRoleARN,

    -- * Destructuring the Response
    associateDRTRoleResponse,
    AssociateDRTRoleResponse,

    -- * Response Lenses
    adrtrrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Shield.Types

-- | /See:/ 'associateDRTRole' smart constructor.
newtype AssociateDRTRole = AssociateDRTRole' {_adrtrRoleARN :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociateDRTRole' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adrtrRoleARN' - The Amazon Resource Name (ARN) of the role the DRT will use to access your AWS account. Prior to making the @AssociateDRTRole@ request, you must attach the <https://console.aws.amazon.com/iam/home?#/policies/arn:aws:iam::aws:policy/service-role/AWSShieldDRTAccessPolicy AWSShieldDRTAccessPolicy> managed policy to this role. For more information see < https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_manage-attach-detach.html Attaching and Detaching IAM Policies> .
associateDRTRole ::
  -- | 'adrtrRoleARN'
  Text ->
  AssociateDRTRole
associateDRTRole pRoleARN_ =
  AssociateDRTRole' {_adrtrRoleARN = pRoleARN_}

-- | The Amazon Resource Name (ARN) of the role the DRT will use to access your AWS account. Prior to making the @AssociateDRTRole@ request, you must attach the <https://console.aws.amazon.com/iam/home?#/policies/arn:aws:iam::aws:policy/service-role/AWSShieldDRTAccessPolicy AWSShieldDRTAccessPolicy> managed policy to this role. For more information see < https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_manage-attach-detach.html Attaching and Detaching IAM Policies> .
adrtrRoleARN :: Lens' AssociateDRTRole Text
adrtrRoleARN = lens _adrtrRoleARN (\s a -> s {_adrtrRoleARN = a})

instance AWSRequest AssociateDRTRole where
  type Rs AssociateDRTRole = AssociateDRTRoleResponse
  request = postJSON shield
  response =
    receiveEmpty
      (\s h x -> AssociateDRTRoleResponse' <$> (pure (fromEnum s)))

instance Hashable AssociateDRTRole

instance NFData AssociateDRTRole

instance ToHeaders AssociateDRTRole where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSShield_20160616.AssociateDRTRole" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON AssociateDRTRole where
  toJSON AssociateDRTRole' {..} =
    object (catMaybes [Just ("RoleArn" .= _adrtrRoleARN)])

instance ToPath AssociateDRTRole where
  toPath = const "/"

instance ToQuery AssociateDRTRole where
  toQuery = const mempty

-- | /See:/ 'associateDRTRoleResponse' smart constructor.
newtype AssociateDRTRoleResponse = AssociateDRTRoleResponse'
  { _adrtrrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociateDRTRoleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adrtrrsResponseStatus' - -- | The response status code.
associateDRTRoleResponse ::
  -- | 'adrtrrsResponseStatus'
  Int ->
  AssociateDRTRoleResponse
associateDRTRoleResponse pResponseStatus_ =
  AssociateDRTRoleResponse'
    { _adrtrrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
adrtrrsResponseStatus :: Lens' AssociateDRTRoleResponse Int
adrtrrsResponseStatus = lens _adrtrrsResponseStatus (\s a -> s {_adrtrrsResponseStatus = a})

instance NFData AssociateDRTRoleResponse
