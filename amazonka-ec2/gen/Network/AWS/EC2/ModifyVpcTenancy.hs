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
-- Module      : Network.AWS.EC2.ModifyVpcTenancy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the instance tenancy attribute of the specified VPC. You can
-- change the instance tenancy attribute of a VPC to @default@ only. You
-- cannot change the instance tenancy attribute to @dedicated@.
--
-- After you modify the tenancy of the VPC, any new instances that you
-- launch into the VPC have a tenancy of @default@, unless you specify
-- otherwise during launch. The tenancy of any existing instances in the
-- VPC is not affected.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-instance.html Dedicated Instances>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Network.AWS.EC2.ModifyVpcTenancy
  ( -- * Creating a Request
    ModifyVpcTenancy (..),
    newModifyVpcTenancy,

    -- * Request Lenses
    modifyVpcTenancy_dryRun,
    modifyVpcTenancy_vpcId,
    modifyVpcTenancy_instanceTenancy,

    -- * Destructuring the Response
    ModifyVpcTenancyResponse (..),
    newModifyVpcTenancyResponse,

    -- * Response Lenses
    modifyVpcTenancyResponse_returnValue,
    modifyVpcTenancyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyVpcTenancy' smart constructor.
data ModifyVpcTenancy = ModifyVpcTenancy'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the VPC.
    vpcId :: Core.Text,
    -- | The instance tenancy attribute for the VPC.
    instanceTenancy :: VpcTenancy
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyVpcTenancy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'modifyVpcTenancy_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'vpcId', 'modifyVpcTenancy_vpcId' - The ID of the VPC.
--
-- 'instanceTenancy', 'modifyVpcTenancy_instanceTenancy' - The instance tenancy attribute for the VPC.
newModifyVpcTenancy ::
  -- | 'vpcId'
  Core.Text ->
  -- | 'instanceTenancy'
  VpcTenancy ->
  ModifyVpcTenancy
newModifyVpcTenancy pVpcId_ pInstanceTenancy_ =
  ModifyVpcTenancy'
    { dryRun = Core.Nothing,
      vpcId = pVpcId_,
      instanceTenancy = pInstanceTenancy_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyVpcTenancy_dryRun :: Lens.Lens' ModifyVpcTenancy (Core.Maybe Core.Bool)
modifyVpcTenancy_dryRun = Lens.lens (\ModifyVpcTenancy' {dryRun} -> dryRun) (\s@ModifyVpcTenancy' {} a -> s {dryRun = a} :: ModifyVpcTenancy)

-- | The ID of the VPC.
modifyVpcTenancy_vpcId :: Lens.Lens' ModifyVpcTenancy Core.Text
modifyVpcTenancy_vpcId = Lens.lens (\ModifyVpcTenancy' {vpcId} -> vpcId) (\s@ModifyVpcTenancy' {} a -> s {vpcId = a} :: ModifyVpcTenancy)

-- | The instance tenancy attribute for the VPC.
modifyVpcTenancy_instanceTenancy :: Lens.Lens' ModifyVpcTenancy VpcTenancy
modifyVpcTenancy_instanceTenancy = Lens.lens (\ModifyVpcTenancy' {instanceTenancy} -> instanceTenancy) (\s@ModifyVpcTenancy' {} a -> s {instanceTenancy = a} :: ModifyVpcTenancy)

instance Core.AWSRequest ModifyVpcTenancy where
  type
    AWSResponse ModifyVpcTenancy =
      ModifyVpcTenancyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyVpcTenancyResponse'
            Core.<$> (x Core..@? "return")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyVpcTenancy

instance Core.NFData ModifyVpcTenancy

instance Core.ToHeaders ModifyVpcTenancy where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyVpcTenancy where
  toPath = Core.const "/"

instance Core.ToQuery ModifyVpcTenancy where
  toQuery ModifyVpcTenancy' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifyVpcTenancy" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "VpcId" Core.=: vpcId,
        "InstanceTenancy" Core.=: instanceTenancy
      ]

-- | /See:/ 'newModifyVpcTenancyResponse' smart constructor.
data ModifyVpcTenancyResponse = ModifyVpcTenancyResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, returns an error.
    returnValue :: Core.Maybe Core.Bool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyVpcTenancyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'returnValue', 'modifyVpcTenancyResponse_returnValue' - Returns @true@ if the request succeeds; otherwise, returns an error.
--
-- 'httpStatus', 'modifyVpcTenancyResponse_httpStatus' - The response's http status code.
newModifyVpcTenancyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyVpcTenancyResponse
newModifyVpcTenancyResponse pHttpStatus_ =
  ModifyVpcTenancyResponse'
    { returnValue =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, returns an error.
modifyVpcTenancyResponse_returnValue :: Lens.Lens' ModifyVpcTenancyResponse (Core.Maybe Core.Bool)
modifyVpcTenancyResponse_returnValue = Lens.lens (\ModifyVpcTenancyResponse' {returnValue} -> returnValue) (\s@ModifyVpcTenancyResponse' {} a -> s {returnValue = a} :: ModifyVpcTenancyResponse)

-- | The response's http status code.
modifyVpcTenancyResponse_httpStatus :: Lens.Lens' ModifyVpcTenancyResponse Core.Int
modifyVpcTenancyResponse_httpStatus = Lens.lens (\ModifyVpcTenancyResponse' {httpStatus} -> httpStatus) (\s@ModifyVpcTenancyResponse' {} a -> s {httpStatus = a} :: ModifyVpcTenancyResponse)

instance Core.NFData ModifyVpcTenancyResponse
