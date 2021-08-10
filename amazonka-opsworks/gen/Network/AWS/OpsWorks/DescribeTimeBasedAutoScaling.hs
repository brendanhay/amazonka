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
-- Module      : Network.AWS.OpsWorks.DescribeTimeBasedAutoScaling
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.OpsWorks.DescribeTimeBasedAutoScaling
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
describeTimeBasedAutoScaling_instanceIds = Lens.lens (\DescribeTimeBasedAutoScaling' {instanceIds} -> instanceIds) (\s@DescribeTimeBasedAutoScaling' {} a -> s {instanceIds = a} :: DescribeTimeBasedAutoScaling) Prelude.. Lens._Coerce

instance Core.AWSRequest DescribeTimeBasedAutoScaling where
  type
    AWSResponse DescribeTimeBasedAutoScaling =
      DescribeTimeBasedAutoScalingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTimeBasedAutoScalingResponse'
            Prelude.<$> ( x Core..?> "TimeBasedAutoScalingConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeTimeBasedAutoScaling

instance Prelude.NFData DescribeTimeBasedAutoScaling

instance Core.ToHeaders DescribeTimeBasedAutoScaling where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.DescribeTimeBasedAutoScaling" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeTimeBasedAutoScaling where
  toJSON DescribeTimeBasedAutoScaling' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("InstanceIds" Core..= instanceIds)]
      )

instance Core.ToPath DescribeTimeBasedAutoScaling where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeTimeBasedAutoScaling where
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
describeTimeBasedAutoScalingResponse_timeBasedAutoScalingConfigurations = Lens.lens (\DescribeTimeBasedAutoScalingResponse' {timeBasedAutoScalingConfigurations} -> timeBasedAutoScalingConfigurations) (\s@DescribeTimeBasedAutoScalingResponse' {} a -> s {timeBasedAutoScalingConfigurations = a} :: DescribeTimeBasedAutoScalingResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeTimeBasedAutoScalingResponse_httpStatus :: Lens.Lens' DescribeTimeBasedAutoScalingResponse Prelude.Int
describeTimeBasedAutoScalingResponse_httpStatus = Lens.lens (\DescribeTimeBasedAutoScalingResponse' {httpStatus} -> httpStatus) (\s@DescribeTimeBasedAutoScalingResponse' {} a -> s {httpStatus = a} :: DescribeTimeBasedAutoScalingResponse)

instance
  Prelude.NFData
    DescribeTimeBasedAutoScalingResponse
