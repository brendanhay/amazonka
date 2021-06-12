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
-- Module      : Network.AWS.EC2.GetLaunchTemplateData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the configuration data of the specified instance. You can use
-- this data to create a launch template.
--
-- This action calls on other describe actions to get instance information.
-- Depending on your instance configuration, you may need to allow the
-- following actions in your IAM policy: DescribeSpotInstanceRequests,
-- DescribeInstanceCreditSpecifications, DescribeVolumes,
-- DescribeInstanceAttribute, and DescribeElasticGpus. Or, you can allow
-- @describe*@ depending on your instance requirements.
module Network.AWS.EC2.GetLaunchTemplateData
  ( -- * Creating a Request
    GetLaunchTemplateData (..),
    newGetLaunchTemplateData,

    -- * Request Lenses
    getLaunchTemplateData_dryRun,
    getLaunchTemplateData_instanceId,

    -- * Destructuring the Response
    GetLaunchTemplateDataResponse (..),
    newGetLaunchTemplateDataResponse,

    -- * Response Lenses
    getLaunchTemplateDataResponse_launchTemplateData,
    getLaunchTemplateDataResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetLaunchTemplateData' smart constructor.
data GetLaunchTemplateData = GetLaunchTemplateData'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the instance.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetLaunchTemplateData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'getLaunchTemplateData_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'instanceId', 'getLaunchTemplateData_instanceId' - The ID of the instance.
newGetLaunchTemplateData ::
  -- | 'instanceId'
  Core.Text ->
  GetLaunchTemplateData
newGetLaunchTemplateData pInstanceId_ =
  GetLaunchTemplateData'
    { dryRun = Core.Nothing,
      instanceId = pInstanceId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getLaunchTemplateData_dryRun :: Lens.Lens' GetLaunchTemplateData (Core.Maybe Core.Bool)
getLaunchTemplateData_dryRun = Lens.lens (\GetLaunchTemplateData' {dryRun} -> dryRun) (\s@GetLaunchTemplateData' {} a -> s {dryRun = a} :: GetLaunchTemplateData)

-- | The ID of the instance.
getLaunchTemplateData_instanceId :: Lens.Lens' GetLaunchTemplateData Core.Text
getLaunchTemplateData_instanceId = Lens.lens (\GetLaunchTemplateData' {instanceId} -> instanceId) (\s@GetLaunchTemplateData' {} a -> s {instanceId = a} :: GetLaunchTemplateData)

instance Core.AWSRequest GetLaunchTemplateData where
  type
    AWSResponse GetLaunchTemplateData =
      GetLaunchTemplateDataResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetLaunchTemplateDataResponse'
            Core.<$> (x Core..@? "launchTemplateData")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetLaunchTemplateData

instance Core.NFData GetLaunchTemplateData

instance Core.ToHeaders GetLaunchTemplateData where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetLaunchTemplateData where
  toPath = Core.const "/"

instance Core.ToQuery GetLaunchTemplateData where
  toQuery GetLaunchTemplateData' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("GetLaunchTemplateData" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "InstanceId" Core.=: instanceId
      ]

-- | /See:/ 'newGetLaunchTemplateDataResponse' smart constructor.
data GetLaunchTemplateDataResponse = GetLaunchTemplateDataResponse'
  { -- | The instance data.
    launchTemplateData :: Core.Maybe ResponseLaunchTemplateData,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetLaunchTemplateDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplateData', 'getLaunchTemplateDataResponse_launchTemplateData' - The instance data.
--
-- 'httpStatus', 'getLaunchTemplateDataResponse_httpStatus' - The response's http status code.
newGetLaunchTemplateDataResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetLaunchTemplateDataResponse
newGetLaunchTemplateDataResponse pHttpStatus_ =
  GetLaunchTemplateDataResponse'
    { launchTemplateData =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The instance data.
getLaunchTemplateDataResponse_launchTemplateData :: Lens.Lens' GetLaunchTemplateDataResponse (Core.Maybe ResponseLaunchTemplateData)
getLaunchTemplateDataResponse_launchTemplateData = Lens.lens (\GetLaunchTemplateDataResponse' {launchTemplateData} -> launchTemplateData) (\s@GetLaunchTemplateDataResponse' {} a -> s {launchTemplateData = a} :: GetLaunchTemplateDataResponse)

-- | The response's http status code.
getLaunchTemplateDataResponse_httpStatus :: Lens.Lens' GetLaunchTemplateDataResponse Core.Int
getLaunchTemplateDataResponse_httpStatus = Lens.lens (\GetLaunchTemplateDataResponse' {httpStatus} -> httpStatus) (\s@GetLaunchTemplateDataResponse' {} a -> s {httpStatus = a} :: GetLaunchTemplateDataResponse)

instance Core.NFData GetLaunchTemplateDataResponse
