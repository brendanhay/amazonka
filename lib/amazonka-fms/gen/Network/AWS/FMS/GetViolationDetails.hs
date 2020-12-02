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
-- Module      : Network.AWS.FMS.GetViolationDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves violations for a resource based on the specified AWS Firewall Manager policy and AWS account.
module Network.AWS.FMS.GetViolationDetails
  ( -- * Creating a Request
    getViolationDetails,
    GetViolationDetails,

    -- * Request Lenses
    gvdPolicyId,
    gvdMemberAccount,
    gvdResourceId,
    gvdResourceType,

    -- * Destructuring the Response
    getViolationDetailsResponse,
    GetViolationDetailsResponse,

    -- * Response Lenses
    gvdrsViolationDetail,
    gvdrsResponseStatus,
  )
where

import Network.AWS.FMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getViolationDetails' smart constructor.
data GetViolationDetails = GetViolationDetails'
  { _gvdPolicyId ::
      !Text,
    _gvdMemberAccount :: !Text,
    _gvdResourceId :: !Text,
    _gvdResourceType :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetViolationDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gvdPolicyId' - The ID of the AWS Firewall Manager policy that you want the details for. This currently only supports security group content audit policies.
--
-- * 'gvdMemberAccount' - The AWS account ID that you want the details for.
--
-- * 'gvdResourceId' - The ID of the resource that has violations.
--
-- * 'gvdResourceType' - The resource type. This is in the format shown in the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . Supported resource types are: @AWS::EC2::Instance@ , @AWS::EC2::NetworkInterface@ , @AWS::EC2::SecurityGroup@ , @AWS::NetworkFirewall::FirewallPolicy@ , and @AWS::EC2::Subnet@ .
getViolationDetails ::
  -- | 'gvdPolicyId'
  Text ->
  -- | 'gvdMemberAccount'
  Text ->
  -- | 'gvdResourceId'
  Text ->
  -- | 'gvdResourceType'
  Text ->
  GetViolationDetails
getViolationDetails
  pPolicyId_
  pMemberAccount_
  pResourceId_
  pResourceType_ =
    GetViolationDetails'
      { _gvdPolicyId = pPolicyId_,
        _gvdMemberAccount = pMemberAccount_,
        _gvdResourceId = pResourceId_,
        _gvdResourceType = pResourceType_
      }

-- | The ID of the AWS Firewall Manager policy that you want the details for. This currently only supports security group content audit policies.
gvdPolicyId :: Lens' GetViolationDetails Text
gvdPolicyId = lens _gvdPolicyId (\s a -> s {_gvdPolicyId = a})

-- | The AWS account ID that you want the details for.
gvdMemberAccount :: Lens' GetViolationDetails Text
gvdMemberAccount = lens _gvdMemberAccount (\s a -> s {_gvdMemberAccount = a})

-- | The ID of the resource that has violations.
gvdResourceId :: Lens' GetViolationDetails Text
gvdResourceId = lens _gvdResourceId (\s a -> s {_gvdResourceId = a})

-- | The resource type. This is in the format shown in the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . Supported resource types are: @AWS::EC2::Instance@ , @AWS::EC2::NetworkInterface@ , @AWS::EC2::SecurityGroup@ , @AWS::NetworkFirewall::FirewallPolicy@ , and @AWS::EC2::Subnet@ .
gvdResourceType :: Lens' GetViolationDetails Text
gvdResourceType = lens _gvdResourceType (\s a -> s {_gvdResourceType = a})

instance AWSRequest GetViolationDetails where
  type Rs GetViolationDetails = GetViolationDetailsResponse
  request = postJSON fms
  response =
    receiveJSON
      ( \s h x ->
          GetViolationDetailsResponse'
            <$> (x .?> "ViolationDetail") <*> (pure (fromEnum s))
      )

instance Hashable GetViolationDetails

instance NFData GetViolationDetails

instance ToHeaders GetViolationDetails where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSFMS_20180101.GetViolationDetails" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetViolationDetails where
  toJSON GetViolationDetails' {..} =
    object
      ( catMaybes
          [ Just ("PolicyId" .= _gvdPolicyId),
            Just ("MemberAccount" .= _gvdMemberAccount),
            Just ("ResourceId" .= _gvdResourceId),
            Just ("ResourceType" .= _gvdResourceType)
          ]
      )

instance ToPath GetViolationDetails where
  toPath = const "/"

instance ToQuery GetViolationDetails where
  toQuery = const mempty

-- | /See:/ 'getViolationDetailsResponse' smart constructor.
data GetViolationDetailsResponse = GetViolationDetailsResponse'
  { _gvdrsViolationDetail ::
      !(Maybe ViolationDetail),
    _gvdrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetViolationDetailsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gvdrsViolationDetail' - Violation detail for a resource.
--
-- * 'gvdrsResponseStatus' - -- | The response status code.
getViolationDetailsResponse ::
  -- | 'gvdrsResponseStatus'
  Int ->
  GetViolationDetailsResponse
getViolationDetailsResponse pResponseStatus_ =
  GetViolationDetailsResponse'
    { _gvdrsViolationDetail = Nothing,
      _gvdrsResponseStatus = pResponseStatus_
    }

-- | Violation detail for a resource.
gvdrsViolationDetail :: Lens' GetViolationDetailsResponse (Maybe ViolationDetail)
gvdrsViolationDetail = lens _gvdrsViolationDetail (\s a -> s {_gvdrsViolationDetail = a})

-- | -- | The response status code.
gvdrsResponseStatus :: Lens' GetViolationDetailsResponse Int
gvdrsResponseStatus = lens _gvdrsResponseStatus (\s a -> s {_gvdrsResponseStatus = a})

instance NFData GetViolationDetailsResponse
