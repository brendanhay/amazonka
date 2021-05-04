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
-- Module      : Network.AWS.AutoScaling.DescribeAccountLimits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the current Amazon EC2 Auto Scaling resource quotas for your
-- AWS account.
--
-- For information about requesting an increase, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-account-limits.html Amazon EC2 Auto Scaling service quotas>
-- in the /Amazon EC2 Auto Scaling User Guide/.
module Network.AWS.AutoScaling.DescribeAccountLimits
  ( -- * Creating a Request
    DescribeAccountLimits (..),
    newDescribeAccountLimits,

    -- * Destructuring the Response
    DescribeAccountLimitsResponse (..),
    newDescribeAccountLimitsResponse,

    -- * Response Lenses
    describeAccountLimitsResponse_numberOfAutoScalingGroups,
    describeAccountLimitsResponse_maxNumberOfLaunchConfigurations,
    describeAccountLimitsResponse_numberOfLaunchConfigurations,
    describeAccountLimitsResponse_maxNumberOfAutoScalingGroups,
    describeAccountLimitsResponse_httpStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeAccountLimits' smart constructor.
data DescribeAccountLimits = DescribeAccountLimits'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeAccountLimits ::
  DescribeAccountLimits
newDescribeAccountLimits = DescribeAccountLimits'

instance Prelude.AWSRequest DescribeAccountLimits where
  type
    Rs DescribeAccountLimits =
      DescribeAccountLimitsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeAccountLimitsResult"
      ( \s h x ->
          DescribeAccountLimitsResponse'
            Prelude.<$> (x Prelude..@? "NumberOfAutoScalingGroups")
            Prelude.<*> (x Prelude..@? "MaxNumberOfLaunchConfigurations")
            Prelude.<*> (x Prelude..@? "NumberOfLaunchConfigurations")
            Prelude.<*> (x Prelude..@? "MaxNumberOfAutoScalingGroups")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAccountLimits

instance Prelude.NFData DescribeAccountLimits

instance Prelude.ToHeaders DescribeAccountLimits where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeAccountLimits where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeAccountLimits where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Prelude.=: ("DescribeAccountLimits" :: Prelude.ByteString),
            "Version"
              Prelude.=: ("2011-01-01" :: Prelude.ByteString)
          ]
      )

-- | /See:/ 'newDescribeAccountLimitsResponse' smart constructor.
data DescribeAccountLimitsResponse = DescribeAccountLimitsResponse'
  { -- | The current number of groups for your AWS account.
    numberOfAutoScalingGroups :: Prelude.Maybe Prelude.Int,
    -- | The maximum number of launch configurations allowed for your AWS
    -- account. The default is 200 launch configurations per AWS Region.
    maxNumberOfLaunchConfigurations :: Prelude.Maybe Prelude.Int,
    -- | The current number of launch configurations for your AWS account.
    numberOfLaunchConfigurations :: Prelude.Maybe Prelude.Int,
    -- | The maximum number of groups allowed for your AWS account. The default
    -- is 200 groups per AWS Region.
    maxNumberOfAutoScalingGroups :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountLimitsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numberOfAutoScalingGroups', 'describeAccountLimitsResponse_numberOfAutoScalingGroups' - The current number of groups for your AWS account.
--
-- 'maxNumberOfLaunchConfigurations', 'describeAccountLimitsResponse_maxNumberOfLaunchConfigurations' - The maximum number of launch configurations allowed for your AWS
-- account. The default is 200 launch configurations per AWS Region.
--
-- 'numberOfLaunchConfigurations', 'describeAccountLimitsResponse_numberOfLaunchConfigurations' - The current number of launch configurations for your AWS account.
--
-- 'maxNumberOfAutoScalingGroups', 'describeAccountLimitsResponse_maxNumberOfAutoScalingGroups' - The maximum number of groups allowed for your AWS account. The default
-- is 200 groups per AWS Region.
--
-- 'httpStatus', 'describeAccountLimitsResponse_httpStatus' - The response's http status code.
newDescribeAccountLimitsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAccountLimitsResponse
newDescribeAccountLimitsResponse pHttpStatus_ =
  DescribeAccountLimitsResponse'
    { numberOfAutoScalingGroups =
        Prelude.Nothing,
      maxNumberOfLaunchConfigurations =
        Prelude.Nothing,
      numberOfLaunchConfigurations =
        Prelude.Nothing,
      maxNumberOfAutoScalingGroups =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current number of groups for your AWS account.
describeAccountLimitsResponse_numberOfAutoScalingGroups :: Lens.Lens' DescribeAccountLimitsResponse (Prelude.Maybe Prelude.Int)
describeAccountLimitsResponse_numberOfAutoScalingGroups = Lens.lens (\DescribeAccountLimitsResponse' {numberOfAutoScalingGroups} -> numberOfAutoScalingGroups) (\s@DescribeAccountLimitsResponse' {} a -> s {numberOfAutoScalingGroups = a} :: DescribeAccountLimitsResponse)

-- | The maximum number of launch configurations allowed for your AWS
-- account. The default is 200 launch configurations per AWS Region.
describeAccountLimitsResponse_maxNumberOfLaunchConfigurations :: Lens.Lens' DescribeAccountLimitsResponse (Prelude.Maybe Prelude.Int)
describeAccountLimitsResponse_maxNumberOfLaunchConfigurations = Lens.lens (\DescribeAccountLimitsResponse' {maxNumberOfLaunchConfigurations} -> maxNumberOfLaunchConfigurations) (\s@DescribeAccountLimitsResponse' {} a -> s {maxNumberOfLaunchConfigurations = a} :: DescribeAccountLimitsResponse)

-- | The current number of launch configurations for your AWS account.
describeAccountLimitsResponse_numberOfLaunchConfigurations :: Lens.Lens' DescribeAccountLimitsResponse (Prelude.Maybe Prelude.Int)
describeAccountLimitsResponse_numberOfLaunchConfigurations = Lens.lens (\DescribeAccountLimitsResponse' {numberOfLaunchConfigurations} -> numberOfLaunchConfigurations) (\s@DescribeAccountLimitsResponse' {} a -> s {numberOfLaunchConfigurations = a} :: DescribeAccountLimitsResponse)

-- | The maximum number of groups allowed for your AWS account. The default
-- is 200 groups per AWS Region.
describeAccountLimitsResponse_maxNumberOfAutoScalingGroups :: Lens.Lens' DescribeAccountLimitsResponse (Prelude.Maybe Prelude.Int)
describeAccountLimitsResponse_maxNumberOfAutoScalingGroups = Lens.lens (\DescribeAccountLimitsResponse' {maxNumberOfAutoScalingGroups} -> maxNumberOfAutoScalingGroups) (\s@DescribeAccountLimitsResponse' {} a -> s {maxNumberOfAutoScalingGroups = a} :: DescribeAccountLimitsResponse)

-- | The response's http status code.
describeAccountLimitsResponse_httpStatus :: Lens.Lens' DescribeAccountLimitsResponse Prelude.Int
describeAccountLimitsResponse_httpStatus = Lens.lens (\DescribeAccountLimitsResponse' {httpStatus} -> httpStatus) (\s@DescribeAccountLimitsResponse' {} a -> s {httpStatus = a} :: DescribeAccountLimitsResponse)

instance Prelude.NFData DescribeAccountLimitsResponse
