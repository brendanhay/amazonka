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
-- Module      : Amazonka.SnowDeviceManagement.DescribeDeviceEc2Instances
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Checks the current state of the Amazon EC2 instances. The output is
-- similar to @describeDevice@, but the results are sourced from the device
-- cache in the Amazon Web Services Cloud and include a subset of the
-- available fields.
module Amazonka.SnowDeviceManagement.DescribeDeviceEc2Instances
  ( -- * Creating a Request
    DescribeDeviceEc2Instances (..),
    newDescribeDeviceEc2Instances,

    -- * Request Lenses
    describeDeviceEc2Instances_instanceIds,
    describeDeviceEc2Instances_managedDeviceId,

    -- * Destructuring the Response
    DescribeDeviceEc2InstancesResponse (..),
    newDescribeDeviceEc2InstancesResponse,

    -- * Response Lenses
    describeDeviceEc2InstancesResponse_instances,
    describeDeviceEc2InstancesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SnowDeviceManagement.Types

-- | /See:/ 'newDescribeDeviceEc2Instances' smart constructor.
data DescribeDeviceEc2Instances = DescribeDeviceEc2Instances'
  { -- | A list of instance IDs associated with the managed device.
    instanceIds :: [Prelude.Text],
    -- | The ID of the managed device.
    managedDeviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDeviceEc2Instances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceIds', 'describeDeviceEc2Instances_instanceIds' - A list of instance IDs associated with the managed device.
--
-- 'managedDeviceId', 'describeDeviceEc2Instances_managedDeviceId' - The ID of the managed device.
newDescribeDeviceEc2Instances ::
  -- | 'managedDeviceId'
  Prelude.Text ->
  DescribeDeviceEc2Instances
newDescribeDeviceEc2Instances pManagedDeviceId_ =
  DescribeDeviceEc2Instances'
    { instanceIds =
        Prelude.mempty,
      managedDeviceId = pManagedDeviceId_
    }

-- | A list of instance IDs associated with the managed device.
describeDeviceEc2Instances_instanceIds :: Lens.Lens' DescribeDeviceEc2Instances [Prelude.Text]
describeDeviceEc2Instances_instanceIds = Lens.lens (\DescribeDeviceEc2Instances' {instanceIds} -> instanceIds) (\s@DescribeDeviceEc2Instances' {} a -> s {instanceIds = a} :: DescribeDeviceEc2Instances) Prelude.. Lens.coerced

-- | The ID of the managed device.
describeDeviceEc2Instances_managedDeviceId :: Lens.Lens' DescribeDeviceEc2Instances Prelude.Text
describeDeviceEc2Instances_managedDeviceId = Lens.lens (\DescribeDeviceEc2Instances' {managedDeviceId} -> managedDeviceId) (\s@DescribeDeviceEc2Instances' {} a -> s {managedDeviceId = a} :: DescribeDeviceEc2Instances)

instance Core.AWSRequest DescribeDeviceEc2Instances where
  type
    AWSResponse DescribeDeviceEc2Instances =
      DescribeDeviceEc2InstancesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDeviceEc2InstancesResponse'
            Prelude.<$> (x Data..?> "instances" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDeviceEc2Instances where
  hashWithSalt _salt DescribeDeviceEc2Instances' {..} =
    _salt `Prelude.hashWithSalt` instanceIds
      `Prelude.hashWithSalt` managedDeviceId

instance Prelude.NFData DescribeDeviceEc2Instances where
  rnf DescribeDeviceEc2Instances' {..} =
    Prelude.rnf instanceIds
      `Prelude.seq` Prelude.rnf managedDeviceId

instance Data.ToHeaders DescribeDeviceEc2Instances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeDeviceEc2Instances where
  toJSON DescribeDeviceEc2Instances' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("instanceIds" Data..= instanceIds)]
      )

instance Data.ToPath DescribeDeviceEc2Instances where
  toPath DescribeDeviceEc2Instances' {..} =
    Prelude.mconcat
      [ "/managed-device/",
        Data.toBS managedDeviceId,
        "/resources/ec2/describe"
      ]

instance Data.ToQuery DescribeDeviceEc2Instances where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDeviceEc2InstancesResponse' smart constructor.
data DescribeDeviceEc2InstancesResponse = DescribeDeviceEc2InstancesResponse'
  { -- | A list of structures containing information about each instance.
    instances :: Prelude.Maybe [InstanceSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDeviceEc2InstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instances', 'describeDeviceEc2InstancesResponse_instances' - A list of structures containing information about each instance.
--
-- 'httpStatus', 'describeDeviceEc2InstancesResponse_httpStatus' - The response's http status code.
newDescribeDeviceEc2InstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDeviceEc2InstancesResponse
newDescribeDeviceEc2InstancesResponse pHttpStatus_ =
  DescribeDeviceEc2InstancesResponse'
    { instances =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of structures containing information about each instance.
describeDeviceEc2InstancesResponse_instances :: Lens.Lens' DescribeDeviceEc2InstancesResponse (Prelude.Maybe [InstanceSummary])
describeDeviceEc2InstancesResponse_instances = Lens.lens (\DescribeDeviceEc2InstancesResponse' {instances} -> instances) (\s@DescribeDeviceEc2InstancesResponse' {} a -> s {instances = a} :: DescribeDeviceEc2InstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeDeviceEc2InstancesResponse_httpStatus :: Lens.Lens' DescribeDeviceEc2InstancesResponse Prelude.Int
describeDeviceEc2InstancesResponse_httpStatus = Lens.lens (\DescribeDeviceEc2InstancesResponse' {httpStatus} -> httpStatus) (\s@DescribeDeviceEc2InstancesResponse' {} a -> s {httpStatus = a} :: DescribeDeviceEc2InstancesResponse)

instance
  Prelude.NFData
    DescribeDeviceEc2InstancesResponse
  where
  rnf DescribeDeviceEc2InstancesResponse' {..} =
    Prelude.rnf instances
      `Prelude.seq` Prelude.rnf httpStatus
