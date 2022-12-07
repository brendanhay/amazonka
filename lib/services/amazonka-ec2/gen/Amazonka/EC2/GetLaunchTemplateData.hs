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
-- Module      : Amazonka.EC2.GetLaunchTemplateData
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- following actions in your IAM policy: @DescribeSpotInstanceRequests@,
-- @DescribeInstanceCreditSpecifications@, @DescribeVolumes@,
-- @DescribeInstanceAttribute@, and @DescribeElasticGpus@. Or, you can
-- allow @describe*@ depending on your instance requirements.
module Amazonka.EC2.GetLaunchTemplateData
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLaunchTemplateData' smart constructor.
data GetLaunchTemplateData = GetLaunchTemplateData'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetLaunchTemplateData
newGetLaunchTemplateData pInstanceId_ =
  GetLaunchTemplateData'
    { dryRun = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getLaunchTemplateData_dryRun :: Lens.Lens' GetLaunchTemplateData (Prelude.Maybe Prelude.Bool)
getLaunchTemplateData_dryRun = Lens.lens (\GetLaunchTemplateData' {dryRun} -> dryRun) (\s@GetLaunchTemplateData' {} a -> s {dryRun = a} :: GetLaunchTemplateData)

-- | The ID of the instance.
getLaunchTemplateData_instanceId :: Lens.Lens' GetLaunchTemplateData Prelude.Text
getLaunchTemplateData_instanceId = Lens.lens (\GetLaunchTemplateData' {instanceId} -> instanceId) (\s@GetLaunchTemplateData' {} a -> s {instanceId = a} :: GetLaunchTemplateData)

instance Core.AWSRequest GetLaunchTemplateData where
  type
    AWSResponse GetLaunchTemplateData =
      GetLaunchTemplateDataResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetLaunchTemplateDataResponse'
            Prelude.<$> (x Data..@? "launchTemplateData")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLaunchTemplateData where
  hashWithSalt _salt GetLaunchTemplateData' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData GetLaunchTemplateData where
  rnf GetLaunchTemplateData' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders GetLaunchTemplateData where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetLaunchTemplateData where
  toPath = Prelude.const "/"

instance Data.ToQuery GetLaunchTemplateData where
  toQuery GetLaunchTemplateData' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("GetLaunchTemplateData" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "InstanceId" Data.=: instanceId
      ]

-- | /See:/ 'newGetLaunchTemplateDataResponse' smart constructor.
data GetLaunchTemplateDataResponse = GetLaunchTemplateDataResponse'
  { -- | The instance data.
    launchTemplateData :: Prelude.Maybe ResponseLaunchTemplateData,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetLaunchTemplateDataResponse
newGetLaunchTemplateDataResponse pHttpStatus_ =
  GetLaunchTemplateDataResponse'
    { launchTemplateData =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The instance data.
getLaunchTemplateDataResponse_launchTemplateData :: Lens.Lens' GetLaunchTemplateDataResponse (Prelude.Maybe ResponseLaunchTemplateData)
getLaunchTemplateDataResponse_launchTemplateData = Lens.lens (\GetLaunchTemplateDataResponse' {launchTemplateData} -> launchTemplateData) (\s@GetLaunchTemplateDataResponse' {} a -> s {launchTemplateData = a} :: GetLaunchTemplateDataResponse)

-- | The response's http status code.
getLaunchTemplateDataResponse_httpStatus :: Lens.Lens' GetLaunchTemplateDataResponse Prelude.Int
getLaunchTemplateDataResponse_httpStatus = Lens.lens (\GetLaunchTemplateDataResponse' {httpStatus} -> httpStatus) (\s@GetLaunchTemplateDataResponse' {} a -> s {httpStatus = a} :: GetLaunchTemplateDataResponse)

instance Prelude.NFData GetLaunchTemplateDataResponse where
  rnf GetLaunchTemplateDataResponse' {..} =
    Prelude.rnf launchTemplateData
      `Prelude.seq` Prelude.rnf httpStatus
