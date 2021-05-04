{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyVpcTenancy' smart constructor.
data ModifyVpcTenancy = ModifyVpcTenancy'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the VPC.
    vpcId :: Prelude.Text,
    -- | The instance tenancy attribute for the VPC.
    instanceTenancy :: VpcTenancy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'instanceTenancy'
  VpcTenancy ->
  ModifyVpcTenancy
newModifyVpcTenancy pVpcId_ pInstanceTenancy_ =
  ModifyVpcTenancy'
    { dryRun = Prelude.Nothing,
      vpcId = pVpcId_,
      instanceTenancy = pInstanceTenancy_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyVpcTenancy_dryRun :: Lens.Lens' ModifyVpcTenancy (Prelude.Maybe Prelude.Bool)
modifyVpcTenancy_dryRun = Lens.lens (\ModifyVpcTenancy' {dryRun} -> dryRun) (\s@ModifyVpcTenancy' {} a -> s {dryRun = a} :: ModifyVpcTenancy)

-- | The ID of the VPC.
modifyVpcTenancy_vpcId :: Lens.Lens' ModifyVpcTenancy Prelude.Text
modifyVpcTenancy_vpcId = Lens.lens (\ModifyVpcTenancy' {vpcId} -> vpcId) (\s@ModifyVpcTenancy' {} a -> s {vpcId = a} :: ModifyVpcTenancy)

-- | The instance tenancy attribute for the VPC.
modifyVpcTenancy_instanceTenancy :: Lens.Lens' ModifyVpcTenancy VpcTenancy
modifyVpcTenancy_instanceTenancy = Lens.lens (\ModifyVpcTenancy' {instanceTenancy} -> instanceTenancy) (\s@ModifyVpcTenancy' {} a -> s {instanceTenancy = a} :: ModifyVpcTenancy)

instance Prelude.AWSRequest ModifyVpcTenancy where
  type Rs ModifyVpcTenancy = ModifyVpcTenancyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyVpcTenancyResponse'
            Prelude.<$> (x Prelude..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyVpcTenancy

instance Prelude.NFData ModifyVpcTenancy

instance Prelude.ToHeaders ModifyVpcTenancy where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ModifyVpcTenancy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ModifyVpcTenancy where
  toQuery ModifyVpcTenancy' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ModifyVpcTenancy" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "VpcId" Prelude.=: vpcId,
        "InstanceTenancy" Prelude.=: instanceTenancy
      ]

-- | /See:/ 'newModifyVpcTenancyResponse' smart constructor.
data ModifyVpcTenancyResponse = ModifyVpcTenancyResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, returns an error.
    returnValue :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ModifyVpcTenancyResponse
newModifyVpcTenancyResponse pHttpStatus_ =
  ModifyVpcTenancyResponse'
    { returnValue =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, returns an error.
modifyVpcTenancyResponse_returnValue :: Lens.Lens' ModifyVpcTenancyResponse (Prelude.Maybe Prelude.Bool)
modifyVpcTenancyResponse_returnValue = Lens.lens (\ModifyVpcTenancyResponse' {returnValue} -> returnValue) (\s@ModifyVpcTenancyResponse' {} a -> s {returnValue = a} :: ModifyVpcTenancyResponse)

-- | The response's http status code.
modifyVpcTenancyResponse_httpStatus :: Lens.Lens' ModifyVpcTenancyResponse Prelude.Int
modifyVpcTenancyResponse_httpStatus = Lens.lens (\ModifyVpcTenancyResponse' {httpStatus} -> httpStatus) (\s@ModifyVpcTenancyResponse' {} a -> s {httpStatus = a} :: ModifyVpcTenancyResponse)

instance Prelude.NFData ModifyVpcTenancyResponse
