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
-- Module      : Amazonka.OpsWorks.DescribeRdsDbInstances
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes Amazon RDS instances.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information about
-- user permissions, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- This call accepts only one resource-identifying parameter.
module Amazonka.OpsWorks.DescribeRdsDbInstances
  ( -- * Creating a Request
    DescribeRdsDbInstances (..),
    newDescribeRdsDbInstances,

    -- * Request Lenses
    describeRdsDbInstances_rdsDbInstanceArns,
    describeRdsDbInstances_stackId,

    -- * Destructuring the Response
    DescribeRdsDbInstancesResponse (..),
    newDescribeRdsDbInstancesResponse,

    -- * Response Lenses
    describeRdsDbInstancesResponse_rdsDbInstances,
    describeRdsDbInstancesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRdsDbInstances' smart constructor.
data DescribeRdsDbInstances = DescribeRdsDbInstances'
  { -- | An array containing the ARNs of the instances to be described.
    rdsDbInstanceArns :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the stack with which the instances are registered. The
    -- operation returns descriptions of all registered Amazon RDS instances.
    stackId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRdsDbInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rdsDbInstanceArns', 'describeRdsDbInstances_rdsDbInstanceArns' - An array containing the ARNs of the instances to be described.
--
-- 'stackId', 'describeRdsDbInstances_stackId' - The ID of the stack with which the instances are registered. The
-- operation returns descriptions of all registered Amazon RDS instances.
newDescribeRdsDbInstances ::
  -- | 'stackId'
  Prelude.Text ->
  DescribeRdsDbInstances
newDescribeRdsDbInstances pStackId_ =
  DescribeRdsDbInstances'
    { rdsDbInstanceArns =
        Prelude.Nothing,
      stackId = pStackId_
    }

-- | An array containing the ARNs of the instances to be described.
describeRdsDbInstances_rdsDbInstanceArns :: Lens.Lens' DescribeRdsDbInstances (Prelude.Maybe [Prelude.Text])
describeRdsDbInstances_rdsDbInstanceArns = Lens.lens (\DescribeRdsDbInstances' {rdsDbInstanceArns} -> rdsDbInstanceArns) (\s@DescribeRdsDbInstances' {} a -> s {rdsDbInstanceArns = a} :: DescribeRdsDbInstances) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the stack with which the instances are registered. The
-- operation returns descriptions of all registered Amazon RDS instances.
describeRdsDbInstances_stackId :: Lens.Lens' DescribeRdsDbInstances Prelude.Text
describeRdsDbInstances_stackId = Lens.lens (\DescribeRdsDbInstances' {stackId} -> stackId) (\s@DescribeRdsDbInstances' {} a -> s {stackId = a} :: DescribeRdsDbInstances)

instance Core.AWSRequest DescribeRdsDbInstances where
  type
    AWSResponse DescribeRdsDbInstances =
      DescribeRdsDbInstancesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRdsDbInstancesResponse'
            Prelude.<$> (x Core..?> "RdsDbInstances" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeRdsDbInstances where
  hashWithSalt _salt DescribeRdsDbInstances' {..} =
    _salt `Prelude.hashWithSalt` rdsDbInstanceArns
      `Prelude.hashWithSalt` stackId

instance Prelude.NFData DescribeRdsDbInstances where
  rnf DescribeRdsDbInstances' {..} =
    Prelude.rnf rdsDbInstanceArns
      `Prelude.seq` Prelude.rnf stackId

instance Core.ToHeaders DescribeRdsDbInstances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.DescribeRdsDbInstances" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeRdsDbInstances where
  toJSON DescribeRdsDbInstances' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RdsDbInstanceArns" Core..=)
              Prelude.<$> rdsDbInstanceArns,
            Prelude.Just ("StackId" Core..= stackId)
          ]
      )

instance Core.ToPath DescribeRdsDbInstances where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeRdsDbInstances where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the response to a @DescribeRdsDbInstances@ request.
--
-- /See:/ 'newDescribeRdsDbInstancesResponse' smart constructor.
data DescribeRdsDbInstancesResponse = DescribeRdsDbInstancesResponse'
  { -- | An a array of @RdsDbInstance@ objects that describe the instances.
    rdsDbInstances :: Prelude.Maybe [RdsDbInstance],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRdsDbInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rdsDbInstances', 'describeRdsDbInstancesResponse_rdsDbInstances' - An a array of @RdsDbInstance@ objects that describe the instances.
--
-- 'httpStatus', 'describeRdsDbInstancesResponse_httpStatus' - The response's http status code.
newDescribeRdsDbInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRdsDbInstancesResponse
newDescribeRdsDbInstancesResponse pHttpStatus_ =
  DescribeRdsDbInstancesResponse'
    { rdsDbInstances =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An a array of @RdsDbInstance@ objects that describe the instances.
describeRdsDbInstancesResponse_rdsDbInstances :: Lens.Lens' DescribeRdsDbInstancesResponse (Prelude.Maybe [RdsDbInstance])
describeRdsDbInstancesResponse_rdsDbInstances = Lens.lens (\DescribeRdsDbInstancesResponse' {rdsDbInstances} -> rdsDbInstances) (\s@DescribeRdsDbInstancesResponse' {} a -> s {rdsDbInstances = a} :: DescribeRdsDbInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeRdsDbInstancesResponse_httpStatus :: Lens.Lens' DescribeRdsDbInstancesResponse Prelude.Int
describeRdsDbInstancesResponse_httpStatus = Lens.lens (\DescribeRdsDbInstancesResponse' {httpStatus} -> httpStatus) (\s@DescribeRdsDbInstancesResponse' {} a -> s {httpStatus = a} :: DescribeRdsDbInstancesResponse)

instance
  Prelude.NFData
    DescribeRdsDbInstancesResponse
  where
  rnf DescribeRdsDbInstancesResponse' {..} =
    Prelude.rnf rdsDbInstances
      `Prelude.seq` Prelude.rnf httpStatus
