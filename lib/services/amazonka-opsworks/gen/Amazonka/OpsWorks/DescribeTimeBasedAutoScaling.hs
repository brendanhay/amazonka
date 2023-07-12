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
-- Module      : Amazonka.OpsWorks.DescribeTimeBasedAutoScaling
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes time-based auto scaling configurations for specified
-- instances.
--
-- You must specify at least one of the parameters.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information about
-- user permissions, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Amazonka.OpsWorks.DescribeTimeBasedAutoScaling
  ( -- * Creating a Request
    DescribeTimeBasedAutoScaling (..),
    newDescribeTimeBasedAutoScaling,

    -- * Request Lenses
    describeTimeBasedAutoScaling_instanceIds,

    -- * Destructuring the Response
    DescribeTimeBasedAutoScalingResponse (..),
    newDescribeTimeBasedAutoScalingResponse,

    -- * Response Lenses
    describeTimeBasedAutoScalingResponse_timeBasedAutoScalingConfigurations,
    describeTimeBasedAutoScalingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTimeBasedAutoScaling' smart constructor.
data DescribeTimeBasedAutoScaling = DescribeTimeBasedAutoScaling'
  { -- | An array of instance IDs.
    instanceIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTimeBasedAutoScaling' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceIds', 'describeTimeBasedAutoScaling_instanceIds' - An array of instance IDs.
newDescribeTimeBasedAutoScaling ::
  DescribeTimeBasedAutoScaling
newDescribeTimeBasedAutoScaling =
  DescribeTimeBasedAutoScaling'
    { instanceIds =
        Prelude.mempty
    }

-- | An array of instance IDs.
describeTimeBasedAutoScaling_instanceIds :: Lens.Lens' DescribeTimeBasedAutoScaling [Prelude.Text]
describeTimeBasedAutoScaling_instanceIds = Lens.lens (\DescribeTimeBasedAutoScaling' {instanceIds} -> instanceIds) (\s@DescribeTimeBasedAutoScaling' {} a -> s {instanceIds = a} :: DescribeTimeBasedAutoScaling) Prelude.. Lens.coerced

instance Core.AWSRequest DescribeTimeBasedAutoScaling where
  type
    AWSResponse DescribeTimeBasedAutoScaling =
      DescribeTimeBasedAutoScalingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTimeBasedAutoScalingResponse'
            Prelude.<$> ( x
                            Data..?> "TimeBasedAutoScalingConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeTimeBasedAutoScaling
  where
  hashWithSalt _salt DescribeTimeBasedAutoScaling' {..} =
    _salt `Prelude.hashWithSalt` instanceIds

instance Prelude.NFData DescribeTimeBasedAutoScaling where
  rnf DescribeTimeBasedAutoScaling' {..} =
    Prelude.rnf instanceIds

instance Data.ToHeaders DescribeTimeBasedAutoScaling where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.DescribeTimeBasedAutoScaling" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeTimeBasedAutoScaling where
  toJSON DescribeTimeBasedAutoScaling' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("InstanceIds" Data..= instanceIds)]
      )

instance Data.ToPath DescribeTimeBasedAutoScaling where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeTimeBasedAutoScaling where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the response to a @DescribeTimeBasedAutoScaling@ request.
--
-- /See:/ 'newDescribeTimeBasedAutoScalingResponse' smart constructor.
data DescribeTimeBasedAutoScalingResponse = DescribeTimeBasedAutoScalingResponse'
  { -- | An array of @TimeBasedAutoScalingConfiguration@ objects that describe
    -- the configuration for the specified instances.
    timeBasedAutoScalingConfigurations :: Prelude.Maybe [TimeBasedAutoScalingConfiguration],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTimeBasedAutoScalingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeBasedAutoScalingConfigurations', 'describeTimeBasedAutoScalingResponse_timeBasedAutoScalingConfigurations' - An array of @TimeBasedAutoScalingConfiguration@ objects that describe
-- the configuration for the specified instances.
--
-- 'httpStatus', 'describeTimeBasedAutoScalingResponse_httpStatus' - The response's http status code.
newDescribeTimeBasedAutoScalingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTimeBasedAutoScalingResponse
newDescribeTimeBasedAutoScalingResponse pHttpStatus_ =
  DescribeTimeBasedAutoScalingResponse'
    { timeBasedAutoScalingConfigurations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @TimeBasedAutoScalingConfiguration@ objects that describe
-- the configuration for the specified instances.
describeTimeBasedAutoScalingResponse_timeBasedAutoScalingConfigurations :: Lens.Lens' DescribeTimeBasedAutoScalingResponse (Prelude.Maybe [TimeBasedAutoScalingConfiguration])
describeTimeBasedAutoScalingResponse_timeBasedAutoScalingConfigurations = Lens.lens (\DescribeTimeBasedAutoScalingResponse' {timeBasedAutoScalingConfigurations} -> timeBasedAutoScalingConfigurations) (\s@DescribeTimeBasedAutoScalingResponse' {} a -> s {timeBasedAutoScalingConfigurations = a} :: DescribeTimeBasedAutoScalingResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeTimeBasedAutoScalingResponse_httpStatus :: Lens.Lens' DescribeTimeBasedAutoScalingResponse Prelude.Int
describeTimeBasedAutoScalingResponse_httpStatus = Lens.lens (\DescribeTimeBasedAutoScalingResponse' {httpStatus} -> httpStatus) (\s@DescribeTimeBasedAutoScalingResponse' {} a -> s {httpStatus = a} :: DescribeTimeBasedAutoScalingResponse)

instance
  Prelude.NFData
    DescribeTimeBasedAutoScalingResponse
  where
  rnf DescribeTimeBasedAutoScalingResponse' {..} =
    Prelude.rnf timeBasedAutoScalingConfigurations
      `Prelude.seq` Prelude.rnf httpStatus
