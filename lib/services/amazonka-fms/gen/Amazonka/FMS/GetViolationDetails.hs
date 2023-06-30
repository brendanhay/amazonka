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
-- Module      : Amazonka.FMS.GetViolationDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves violations for a resource based on the specified Firewall
-- Manager policy and Amazon Web Services account.
module Amazonka.FMS.GetViolationDetails
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetViolationDetails' smart constructor.
data GetViolationDetails = GetViolationDetails'
  { -- | The ID of the Firewall Manager policy that you want the details for.
    -- This currently only supports security group content audit policies.
    policyId :: Prelude.Text,
    -- | The Amazon Web Services account ID that you want the details for.
    memberAccount :: Prelude.Text,
    -- | The ID of the resource that has violations.
    resourceId :: Prelude.Text,
    -- | The resource type. This is in the format shown in the
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html Amazon Web Services Resource Types Reference>.
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
-- 'policyId', 'getViolationDetails_policyId' - The ID of the Firewall Manager policy that you want the details for.
-- This currently only supports security group content audit policies.
--
-- 'memberAccount', 'getViolationDetails_memberAccount' - The Amazon Web Services account ID that you want the details for.
--
-- 'resourceId', 'getViolationDetails_resourceId' - The ID of the resource that has violations.
--
-- 'resourceType', 'getViolationDetails_resourceType' - The resource type. This is in the format shown in the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html Amazon Web Services Resource Types Reference>.
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

-- | The ID of the Firewall Manager policy that you want the details for.
-- This currently only supports security group content audit policies.
getViolationDetails_policyId :: Lens.Lens' GetViolationDetails Prelude.Text
getViolationDetails_policyId = Lens.lens (\GetViolationDetails' {policyId} -> policyId) (\s@GetViolationDetails' {} a -> s {policyId = a} :: GetViolationDetails)

-- | The Amazon Web Services account ID that you want the details for.
getViolationDetails_memberAccount :: Lens.Lens' GetViolationDetails Prelude.Text
getViolationDetails_memberAccount = Lens.lens (\GetViolationDetails' {memberAccount} -> memberAccount) (\s@GetViolationDetails' {} a -> s {memberAccount = a} :: GetViolationDetails)

-- | The ID of the resource that has violations.
getViolationDetails_resourceId :: Lens.Lens' GetViolationDetails Prelude.Text
getViolationDetails_resourceId = Lens.lens (\GetViolationDetails' {resourceId} -> resourceId) (\s@GetViolationDetails' {} a -> s {resourceId = a} :: GetViolationDetails)

-- | The resource type. This is in the format shown in the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html Amazon Web Services Resource Types Reference>.
-- Supported resource types are: @AWS::EC2::Instance@,
-- @AWS::EC2::NetworkInterface@, @AWS::EC2::SecurityGroup@,
-- @AWS::NetworkFirewall::FirewallPolicy@, and @AWS::EC2::Subnet@.
getViolationDetails_resourceType :: Lens.Lens' GetViolationDetails Prelude.Text
getViolationDetails_resourceType = Lens.lens (\GetViolationDetails' {resourceType} -> resourceType) (\s@GetViolationDetails' {} a -> s {resourceType = a} :: GetViolationDetails)

instance Core.AWSRequest GetViolationDetails where
  type
    AWSResponse GetViolationDetails =
      GetViolationDetailsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetViolationDetailsResponse'
            Prelude.<$> (x Data..?> "ViolationDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetViolationDetails where
  hashWithSalt _salt GetViolationDetails' {..} =
    _salt
      `Prelude.hashWithSalt` policyId
      `Prelude.hashWithSalt` memberAccount
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData GetViolationDetails where
  rnf GetViolationDetails' {..} =
    Prelude.rnf policyId
      `Prelude.seq` Prelude.rnf memberAccount
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType

instance Data.ToHeaders GetViolationDetails where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSFMS_20180101.GetViolationDetails" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetViolationDetails where
  toJSON GetViolationDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("PolicyId" Data..= policyId),
            Prelude.Just ("MemberAccount" Data..= memberAccount),
            Prelude.Just ("ResourceId" Data..= resourceId),
            Prelude.Just ("ResourceType" Data..= resourceType)
          ]
      )

instance Data.ToPath GetViolationDetails where
  toPath = Prelude.const "/"

instance Data.ToQuery GetViolationDetails where
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

instance Prelude.NFData GetViolationDetailsResponse where
  rnf GetViolationDetailsResponse' {..} =
    Prelude.rnf violationDetail
      `Prelude.seq` Prelude.rnf httpStatus
