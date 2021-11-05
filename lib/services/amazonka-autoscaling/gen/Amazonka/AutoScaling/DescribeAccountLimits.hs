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
-- Module      : Amazonka.AutoScaling.DescribeAccountLimits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the current Amazon EC2 Auto Scaling resource quotas for your
-- account.
--
-- When you establish an Amazon Web Services account, the account has
-- initial quotas on the maximum number of Auto Scaling groups and launch
-- configurations that you can create in a given Region. For more
-- information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-account-limits.html Amazon EC2 Auto Scaling service quotas>
-- in the /Amazon EC2 Auto Scaling User Guide/.
module Amazonka.AutoScaling.DescribeAccountLimits
  ( -- * Creating a Request
    DescribeAccountLimits (..),
    newDescribeAccountLimits,

    -- * Destructuring the Response
    DescribeAccountLimitsResponse (..),
    newDescribeAccountLimitsResponse,

    -- * Response Lenses
    describeAccountLimitsResponse_numberOfLaunchConfigurations,
    describeAccountLimitsResponse_numberOfAutoScalingGroups,
    describeAccountLimitsResponse_maxNumberOfAutoScalingGroups,
    describeAccountLimitsResponse_maxNumberOfLaunchConfigurations,
    describeAccountLimitsResponse_httpStatus,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAccountLimits' smart constructor.
data DescribeAccountLimits = DescribeAccountLimits'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeAccountLimits ::
  DescribeAccountLimits
newDescribeAccountLimits = DescribeAccountLimits'

instance Core.AWSRequest DescribeAccountLimits where
  type
    AWSResponse DescribeAccountLimits =
      DescribeAccountLimitsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeAccountLimitsResult"
      ( \s h x ->
          DescribeAccountLimitsResponse'
            Prelude.<$> (x Core..@? "NumberOfLaunchConfigurations")
            Prelude.<*> (x Core..@? "NumberOfAutoScalingGroups")
            Prelude.<*> (x Core..@? "MaxNumberOfAutoScalingGroups")
            Prelude.<*> (x Core..@? "MaxNumberOfLaunchConfigurations")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAccountLimits

instance Prelude.NFData DescribeAccountLimits

instance Core.ToHeaders DescribeAccountLimits where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeAccountLimits where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeAccountLimits where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Core.=: ("DescribeAccountLimits" :: Prelude.ByteString),
            "Version"
              Core.=: ("2011-01-01" :: Prelude.ByteString)
          ]
      )

-- | /See:/ 'newDescribeAccountLimitsResponse' smart constructor.
data DescribeAccountLimitsResponse = DescribeAccountLimitsResponse'
  { -- | The current number of launch configurations for your account.
    numberOfLaunchConfigurations :: Prelude.Maybe Prelude.Int,
    -- | The current number of groups for your account.
    numberOfAutoScalingGroups :: Prelude.Maybe Prelude.Int,
    -- | The maximum number of groups allowed for your account. The default is
    -- 200 groups per Region.
    maxNumberOfAutoScalingGroups :: Prelude.Maybe Prelude.Int,
    -- | The maximum number of launch configurations allowed for your account.
    -- The default is 200 launch configurations per Region.
    maxNumberOfLaunchConfigurations :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountLimitsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numberOfLaunchConfigurations', 'describeAccountLimitsResponse_numberOfLaunchConfigurations' - The current number of launch configurations for your account.
--
-- 'numberOfAutoScalingGroups', 'describeAccountLimitsResponse_numberOfAutoScalingGroups' - The current number of groups for your account.
--
-- 'maxNumberOfAutoScalingGroups', 'describeAccountLimitsResponse_maxNumberOfAutoScalingGroups' - The maximum number of groups allowed for your account. The default is
-- 200 groups per Region.
--
-- 'maxNumberOfLaunchConfigurations', 'describeAccountLimitsResponse_maxNumberOfLaunchConfigurations' - The maximum number of launch configurations allowed for your account.
-- The default is 200 launch configurations per Region.
--
-- 'httpStatus', 'describeAccountLimitsResponse_httpStatus' - The response's http status code.
newDescribeAccountLimitsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAccountLimitsResponse
newDescribeAccountLimitsResponse pHttpStatus_ =
  DescribeAccountLimitsResponse'
    { numberOfLaunchConfigurations =
        Prelude.Nothing,
      numberOfAutoScalingGroups = Prelude.Nothing,
      maxNumberOfAutoScalingGroups =
        Prelude.Nothing,
      maxNumberOfLaunchConfigurations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current number of launch configurations for your account.
describeAccountLimitsResponse_numberOfLaunchConfigurations :: Lens.Lens' DescribeAccountLimitsResponse (Prelude.Maybe Prelude.Int)
describeAccountLimitsResponse_numberOfLaunchConfigurations = Lens.lens (\DescribeAccountLimitsResponse' {numberOfLaunchConfigurations} -> numberOfLaunchConfigurations) (\s@DescribeAccountLimitsResponse' {} a -> s {numberOfLaunchConfigurations = a} :: DescribeAccountLimitsResponse)

-- | The current number of groups for your account.
describeAccountLimitsResponse_numberOfAutoScalingGroups :: Lens.Lens' DescribeAccountLimitsResponse (Prelude.Maybe Prelude.Int)
describeAccountLimitsResponse_numberOfAutoScalingGroups = Lens.lens (\DescribeAccountLimitsResponse' {numberOfAutoScalingGroups} -> numberOfAutoScalingGroups) (\s@DescribeAccountLimitsResponse' {} a -> s {numberOfAutoScalingGroups = a} :: DescribeAccountLimitsResponse)

-- | The maximum number of groups allowed for your account. The default is
-- 200 groups per Region.
describeAccountLimitsResponse_maxNumberOfAutoScalingGroups :: Lens.Lens' DescribeAccountLimitsResponse (Prelude.Maybe Prelude.Int)
describeAccountLimitsResponse_maxNumberOfAutoScalingGroups = Lens.lens (\DescribeAccountLimitsResponse' {maxNumberOfAutoScalingGroups} -> maxNumberOfAutoScalingGroups) (\s@DescribeAccountLimitsResponse' {} a -> s {maxNumberOfAutoScalingGroups = a} :: DescribeAccountLimitsResponse)

-- | The maximum number of launch configurations allowed for your account.
-- The default is 200 launch configurations per Region.
describeAccountLimitsResponse_maxNumberOfLaunchConfigurations :: Lens.Lens' DescribeAccountLimitsResponse (Prelude.Maybe Prelude.Int)
describeAccountLimitsResponse_maxNumberOfLaunchConfigurations = Lens.lens (\DescribeAccountLimitsResponse' {maxNumberOfLaunchConfigurations} -> maxNumberOfLaunchConfigurations) (\s@DescribeAccountLimitsResponse' {} a -> s {maxNumberOfLaunchConfigurations = a} :: DescribeAccountLimitsResponse)

-- | The response's http status code.
describeAccountLimitsResponse_httpStatus :: Lens.Lens' DescribeAccountLimitsResponse Prelude.Int
describeAccountLimitsResponse_httpStatus = Lens.lens (\DescribeAccountLimitsResponse' {httpStatus} -> httpStatus) (\s@DescribeAccountLimitsResponse' {} a -> s {httpStatus = a} :: DescribeAccountLimitsResponse)

instance Prelude.NFData DescribeAccountLimitsResponse
