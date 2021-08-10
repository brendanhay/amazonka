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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetViolationDetails' smart constructor.
data GetViolationDetails = GetViolationDetails'
  { -- | The ID of the AWS Firewall Manager policy that you want the details for.
    -- This currently only supports security group content audit policies.
    policyId :: Prelude.Text,
    -- | The AWS account ID that you want the details for.
    memberAccount :: Prelude.Text,
    -- | The ID of the resource that has violations.
    resourceId :: Prelude.Text,
    -- | The resource type. This is in the format shown in the
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference>.
    -- Supported resource types are: @AWS::EC2::Instance@,
    -- @AWS::EC2::NetworkInterface@, @AWS::EC2::SecurityGroup@,
    -- @AWS::NetworkFirewall::FirewallPolicy@, and @AWS::EC2::Subnet@.
    resourceType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'memberAccount'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'resourceType'
  Prelude.Text ->
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
getViolationDetails_policyId :: Lens.Lens' GetViolationDetails Prelude.Text
getViolationDetails_policyId = Lens.lens (\GetViolationDetails' {policyId} -> policyId) (\s@GetViolationDetails' {} a -> s {policyId = a} :: GetViolationDetails)

-- | The AWS account ID that you want the details for.
getViolationDetails_memberAccount :: Lens.Lens' GetViolationDetails Prelude.Text
getViolationDetails_memberAccount = Lens.lens (\GetViolationDetails' {memberAccount} -> memberAccount) (\s@GetViolationDetails' {} a -> s {memberAccount = a} :: GetViolationDetails)

-- | The ID of the resource that has violations.
getViolationDetails_resourceId :: Lens.Lens' GetViolationDetails Prelude.Text
getViolationDetails_resourceId = Lens.lens (\GetViolationDetails' {resourceId} -> resourceId) (\s@GetViolationDetails' {} a -> s {resourceId = a} :: GetViolationDetails)

-- | The resource type. This is in the format shown in the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference>.
-- Supported resource types are: @AWS::EC2::Instance@,
-- @AWS::EC2::NetworkInterface@, @AWS::EC2::SecurityGroup@,
-- @AWS::NetworkFirewall::FirewallPolicy@, and @AWS::EC2::Subnet@.
getViolationDetails_resourceType :: Lens.Lens' GetViolationDetails Prelude.Text
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
            Prelude.<$> (x Core..?> "ViolationDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetViolationDetails

instance Prelude.NFData GetViolationDetails

instance Core.ToHeaders GetViolationDetails where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSFMS_20180101.GetViolationDetails" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetViolationDetails where
  toJSON GetViolationDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("PolicyId" Core..= policyId),
            Prelude.Just ("MemberAccount" Core..= memberAccount),
            Prelude.Just ("ResourceId" Core..= resourceId),
            Prelude.Just ("ResourceType" Core..= resourceType)
          ]
      )

instance Core.ToPath GetViolationDetails where
  toPath = Prelude.const "/"

instance Core.ToQuery GetViolationDetails where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetViolationDetailsResponse' smart constructor.
data GetViolationDetailsResponse = GetViolationDetailsResponse'
  { -- | Violation detail for a resource.
    violationDetail :: Prelude.Maybe ViolationDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetViolationDetailsResponse
newGetViolationDetailsResponse pHttpStatus_ =
  GetViolationDetailsResponse'
    { violationDetail =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Violation detail for a resource.
getViolationDetailsResponse_violationDetail :: Lens.Lens' GetViolationDetailsResponse (Prelude.Maybe ViolationDetail)
getViolationDetailsResponse_violationDetail = Lens.lens (\GetViolationDetailsResponse' {violationDetail} -> violationDetail) (\s@GetViolationDetailsResponse' {} a -> s {violationDetail = a} :: GetViolationDetailsResponse)

-- | The response's http status code.
getViolationDetailsResponse_httpStatus :: Lens.Lens' GetViolationDetailsResponse Prelude.Int
getViolationDetailsResponse_httpStatus = Lens.lens (\GetViolationDetailsResponse' {httpStatus} -> httpStatus) (\s@GetViolationDetailsResponse' {} a -> s {httpStatus = a} :: GetViolationDetailsResponse)

instance Prelude.NFData GetViolationDetailsResponse
