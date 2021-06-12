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
-- Module      : Network.AWS.GameLift.DescribeEC2InstanceLimits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the following information for the specified EC2 instance type:
--
-- -   Maximum number of instances allowed per AWS account (service limit).
--
-- -   Current usage for the AWS account.
--
-- To learn more about the capabilities of each instance type, see
-- <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>.
-- Note that the instance types offered may vary depending on the region.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift Fleets>
--
-- __Related operations__
--
-- -   CreateFleet
--
-- -   ListFleets
--
-- -   DeleteFleet
--
-- -   DescribeFleetAttributes
--
-- -   UpdateFleetAttributes
--
-- -   StartFleetActions or StopFleetActions
module Network.AWS.GameLift.DescribeEC2InstanceLimits
  ( -- * Creating a Request
    DescribeEC2InstanceLimits (..),
    newDescribeEC2InstanceLimits,

    -- * Request Lenses
    describeEC2InstanceLimits_eC2InstanceType,

    -- * Destructuring the Response
    DescribeEC2InstanceLimitsResponse (..),
    newDescribeEC2InstanceLimitsResponse,

    -- * Response Lenses
    describeEC2InstanceLimitsResponse_eC2InstanceLimits,
    describeEC2InstanceLimitsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDescribeEC2InstanceLimits' smart constructor.
data DescribeEC2InstanceLimits = DescribeEC2InstanceLimits'
  { -- | Name of an EC2 instance type that is supported in Amazon GameLift. A
    -- fleet instance type determines the computing resources of each instance
    -- in the fleet, including CPU, memory, storage, and networking capacity.
    -- Amazon GameLift supports the following EC2 instance types. See
    -- <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>
    -- for detailed descriptions. Leave this parameter blank to retrieve limits
    -- for all types.
    eC2InstanceType :: Core.Maybe EC2InstanceType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEC2InstanceLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eC2InstanceType', 'describeEC2InstanceLimits_eC2InstanceType' - Name of an EC2 instance type that is supported in Amazon GameLift. A
-- fleet instance type determines the computing resources of each instance
-- in the fleet, including CPU, memory, storage, and networking capacity.
-- Amazon GameLift supports the following EC2 instance types. See
-- <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>
-- for detailed descriptions. Leave this parameter blank to retrieve limits
-- for all types.
newDescribeEC2InstanceLimits ::
  DescribeEC2InstanceLimits
newDescribeEC2InstanceLimits =
  DescribeEC2InstanceLimits'
    { eC2InstanceType =
        Core.Nothing
    }

-- | Name of an EC2 instance type that is supported in Amazon GameLift. A
-- fleet instance type determines the computing resources of each instance
-- in the fleet, including CPU, memory, storage, and networking capacity.
-- Amazon GameLift supports the following EC2 instance types. See
-- <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>
-- for detailed descriptions. Leave this parameter blank to retrieve limits
-- for all types.
describeEC2InstanceLimits_eC2InstanceType :: Lens.Lens' DescribeEC2InstanceLimits (Core.Maybe EC2InstanceType)
describeEC2InstanceLimits_eC2InstanceType = Lens.lens (\DescribeEC2InstanceLimits' {eC2InstanceType} -> eC2InstanceType) (\s@DescribeEC2InstanceLimits' {} a -> s {eC2InstanceType = a} :: DescribeEC2InstanceLimits)

instance Core.AWSRequest DescribeEC2InstanceLimits where
  type
    AWSResponse DescribeEC2InstanceLimits =
      DescribeEC2InstanceLimitsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEC2InstanceLimitsResponse'
            Core.<$> (x Core..?> "EC2InstanceLimits" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeEC2InstanceLimits

instance Core.NFData DescribeEC2InstanceLimits

instance Core.ToHeaders DescribeEC2InstanceLimits where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.DescribeEC2InstanceLimits" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeEC2InstanceLimits where
  toJSON DescribeEC2InstanceLimits' {..} =
    Core.object
      ( Core.catMaybes
          [ ("EC2InstanceType" Core..=)
              Core.<$> eC2InstanceType
          ]
      )

instance Core.ToPath DescribeEC2InstanceLimits where
  toPath = Core.const "/"

instance Core.ToQuery DescribeEC2InstanceLimits where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newDescribeEC2InstanceLimitsResponse' smart constructor.
data DescribeEC2InstanceLimitsResponse = DescribeEC2InstanceLimitsResponse'
  { -- | The maximum number of instances for the specified instance type.
    eC2InstanceLimits :: Core.Maybe [EC2InstanceLimit],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEC2InstanceLimitsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eC2InstanceLimits', 'describeEC2InstanceLimitsResponse_eC2InstanceLimits' - The maximum number of instances for the specified instance type.
--
-- 'httpStatus', 'describeEC2InstanceLimitsResponse_httpStatus' - The response's http status code.
newDescribeEC2InstanceLimitsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeEC2InstanceLimitsResponse
newDescribeEC2InstanceLimitsResponse pHttpStatus_ =
  DescribeEC2InstanceLimitsResponse'
    { eC2InstanceLimits =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The maximum number of instances for the specified instance type.
describeEC2InstanceLimitsResponse_eC2InstanceLimits :: Lens.Lens' DescribeEC2InstanceLimitsResponse (Core.Maybe [EC2InstanceLimit])
describeEC2InstanceLimitsResponse_eC2InstanceLimits = Lens.lens (\DescribeEC2InstanceLimitsResponse' {eC2InstanceLimits} -> eC2InstanceLimits) (\s@DescribeEC2InstanceLimitsResponse' {} a -> s {eC2InstanceLimits = a} :: DescribeEC2InstanceLimitsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeEC2InstanceLimitsResponse_httpStatus :: Lens.Lens' DescribeEC2InstanceLimitsResponse Core.Int
describeEC2InstanceLimitsResponse_httpStatus = Lens.lens (\DescribeEC2InstanceLimitsResponse' {httpStatus} -> httpStatus) (\s@DescribeEC2InstanceLimitsResponse' {} a -> s {httpStatus = a} :: DescribeEC2InstanceLimitsResponse)

instance
  Core.NFData
    DescribeEC2InstanceLimitsResponse
