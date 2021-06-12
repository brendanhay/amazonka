{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.GetViolationDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves violations for a resource based on the specified AWS Firewall
-- Manager policy and AWS account.
module Network.AWS.FMS.GetViolationDetails
  ( -- * Creating a Request
    GetViolationDetails (..),
    newGetViolationDetails,

    -- * Request Lenses
    getViolationDetails_policyId,
    getViolationDetails_memberAccount,
    getViolationDetails_resourceId,
    getViolationDetails_resourceType,

    -- * Destructuring the Response
    GetViolationDetailsResponse (..),
    newGetViolationDetailsResponse,

    -- * Response Lenses
    getViolationDetailsResponse_violationDetail,
    getViolationDetailsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetViolationDetails' smart constructor.
data GetViolationDetails = GetViolationDetails'
  { -- | The ID of the AWS Firewall Manager policy that you want the details for.
    -- This currently only supports security group content audit policies.
    policyId :: Core.Text,
    -- | The AWS account ID that you want the details for.
    memberAccount :: Core.Text,
    -- | The ID of the resource that has violations.
    resourceId :: Core.Text,
    -- | The resource type. This is in the format shown in the
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference>.
    -- Supported resource types are: @AWS::EC2::Instance@,
    -- @AWS::EC2::NetworkInterface@, @AWS::EC2::SecurityGroup@,
    -- @AWS::NetworkFirewall::FirewallPolicy@, and @AWS::EC2::Subnet@.
    resourceType :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetViolationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyId', 'getViolationDetails_policyId' - The ID of the AWS Firewall Manager policy that you want the details for.
-- This currently only supports security group content audit policies.
--
-- 'memberAccount', 'getViolationDetails_memberAccount' - The AWS account ID that you want the details for.
--
-- 'resourceId', 'getViolationDetails_resourceId' - The ID of the resource that has violations.
--
-- 'resourceType', 'getViolationDetails_resourceType' - The resource type. This is in the format shown in the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference>.
-- Supported resource types are: @AWS::EC2::Instance@,
-- @AWS::EC2::NetworkInterface@, @AWS::EC2::SecurityGroup@,
-- @AWS::NetworkFirewall::FirewallPolicy@, and @AWS::EC2::Subnet@.
newGetViolationDetails ::
  -- | 'policyId'
  Core.Text ->
  -- | 'memberAccount'
  Core.Text ->
  -- | 'resourceId'
  Core.Text ->
  -- | 'resourceType'
  Core.Text ->
  GetViolationDetails
newGetViolationDetails
  pPolicyId_
  pMemberAccount_
  pResourceId_
  pResourceType_ =
    GetViolationDetails'
      { policyId = pPolicyId_,
        memberAccount = pMemberAccount_,
        resourceId = pResourceId_,
        resourceType = pResourceType_
      }

-- | The ID of the AWS Firewall Manager policy that you want the details for.
-- This currently only supports security group content audit policies.
getViolationDetails_policyId :: Lens.Lens' GetViolationDetails Core.Text
getViolationDetails_policyId = Lens.lens (\GetViolationDetails' {policyId} -> policyId) (\s@GetViolationDetails' {} a -> s {policyId = a} :: GetViolationDetails)

-- | The AWS account ID that you want the details for.
getViolationDetails_memberAccount :: Lens.Lens' GetViolationDetails Core.Text
getViolationDetails_memberAccount = Lens.lens (\GetViolationDetails' {memberAccount} -> memberAccount) (\s@GetViolationDetails' {} a -> s {memberAccount = a} :: GetViolationDetails)

-- | The ID of the resource that has violations.
getViolationDetails_resourceId :: Lens.Lens' GetViolationDetails Core.Text
getViolationDetails_resourceId = Lens.lens (\GetViolationDetails' {resourceId} -> resourceId) (\s@GetViolationDetails' {} a -> s {resourceId = a} :: GetViolationDetails)

-- | The resource type. This is in the format shown in the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference>.
-- Supported resource types are: @AWS::EC2::Instance@,
-- @AWS::EC2::NetworkInterface@, @AWS::EC2::SecurityGroup@,
-- @AWS::NetworkFirewall::FirewallPolicy@, and @AWS::EC2::Subnet@.
getViolationDetails_resourceType :: Lens.Lens' GetViolationDetails Core.Text
getViolationDetails_resourceType = Lens.lens (\GetViolationDetails' {resourceType} -> resourceType) (\s@GetViolationDetails' {} a -> s {resourceType = a} :: GetViolationDetails)

instance Core.AWSRequest GetViolationDetails where
  type
    AWSResponse GetViolationDetails =
      GetViolationDetailsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetViolationDetailsResponse'
            Core.<$> (x Core..?> "ViolationDetail")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetViolationDetails

instance Core.NFData GetViolationDetails

instance Core.ToHeaders GetViolationDetails where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSFMS_20180101.GetViolationDetails" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetViolationDetails where
  toJSON GetViolationDetails' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PolicyId" Core..= policyId),
            Core.Just ("MemberAccount" Core..= memberAccount),
            Core.Just ("ResourceId" Core..= resourceId),
            Core.Just ("ResourceType" Core..= resourceType)
          ]
      )

instance Core.ToPath GetViolationDetails where
  toPath = Core.const "/"

instance Core.ToQuery GetViolationDetails where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetViolationDetailsResponse' smart constructor.
data GetViolationDetailsResponse = GetViolationDetailsResponse'
  { -- | Violation detail for a resource.
    violationDetail :: Core.Maybe ViolationDetail,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetViolationDetailsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'violationDetail', 'getViolationDetailsResponse_violationDetail' - Violation detail for a resource.
--
-- 'httpStatus', 'getViolationDetailsResponse_httpStatus' - The response's http status code.
newGetViolationDetailsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetViolationDetailsResponse
newGetViolationDetailsResponse pHttpStatus_ =
  GetViolationDetailsResponse'
    { violationDetail =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Violation detail for a resource.
getViolationDetailsResponse_violationDetail :: Lens.Lens' GetViolationDetailsResponse (Core.Maybe ViolationDetail)
getViolationDetailsResponse_violationDetail = Lens.lens (\GetViolationDetailsResponse' {violationDetail} -> violationDetail) (\s@GetViolationDetailsResponse' {} a -> s {violationDetail = a} :: GetViolationDetailsResponse)

-- | The response's http status code.
getViolationDetailsResponse_httpStatus :: Lens.Lens' GetViolationDetailsResponse Core.Int
getViolationDetailsResponse_httpStatus = Lens.lens (\GetViolationDetailsResponse' {httpStatus} -> httpStatus) (\s@GetViolationDetailsResponse' {} a -> s {httpStatus = a} :: GetViolationDetailsResponse)

instance Core.NFData GetViolationDetailsResponse
