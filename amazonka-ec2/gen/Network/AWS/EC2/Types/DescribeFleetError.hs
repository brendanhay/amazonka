{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DescribeFleetError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DescribeFleetError where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceLifecycle
import Network.AWS.EC2.Types.LaunchTemplateAndOverridesResponse
import qualified Network.AWS.Lens as Lens

-- | Describes the instances that could not be launched by the fleet.
--
-- /See:/ 'newDescribeFleetError' smart constructor.
data DescribeFleetError = DescribeFleetError'
  { -- | The launch templates and overrides that were used for launching the
    -- instances. The values that you specify in the Overrides replace the
    -- values in the launch template.
    launchTemplateAndOverrides :: Core.Maybe LaunchTemplateAndOverridesResponse,
    -- | Indicates if the instance that could not be launched was a Spot Instance
    -- or On-Demand Instance.
    lifecycle :: Core.Maybe InstanceLifecycle,
    -- | The error message that describes why the instance could not be launched.
    -- For more information about error messages, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes>.
    errorMessage :: Core.Maybe Core.Text,
    -- | The error code that indicates why the instance could not be launched.
    -- For more information about error codes, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes>.
    errorCode :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeFleetError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplateAndOverrides', 'describeFleetError_launchTemplateAndOverrides' - The launch templates and overrides that were used for launching the
-- instances. The values that you specify in the Overrides replace the
-- values in the launch template.
--
-- 'lifecycle', 'describeFleetError_lifecycle' - Indicates if the instance that could not be launched was a Spot Instance
-- or On-Demand Instance.
--
-- 'errorMessage', 'describeFleetError_errorMessage' - The error message that describes why the instance could not be launched.
-- For more information about error messages, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes>.
--
-- 'errorCode', 'describeFleetError_errorCode' - The error code that indicates why the instance could not be launched.
-- For more information about error codes, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes>.
newDescribeFleetError ::
  DescribeFleetError
newDescribeFleetError =
  DescribeFleetError'
    { launchTemplateAndOverrides =
        Core.Nothing,
      lifecycle = Core.Nothing,
      errorMessage = Core.Nothing,
      errorCode = Core.Nothing
    }

-- | The launch templates and overrides that were used for launching the
-- instances. The values that you specify in the Overrides replace the
-- values in the launch template.
describeFleetError_launchTemplateAndOverrides :: Lens.Lens' DescribeFleetError (Core.Maybe LaunchTemplateAndOverridesResponse)
describeFleetError_launchTemplateAndOverrides = Lens.lens (\DescribeFleetError' {launchTemplateAndOverrides} -> launchTemplateAndOverrides) (\s@DescribeFleetError' {} a -> s {launchTemplateAndOverrides = a} :: DescribeFleetError)

-- | Indicates if the instance that could not be launched was a Spot Instance
-- or On-Demand Instance.
describeFleetError_lifecycle :: Lens.Lens' DescribeFleetError (Core.Maybe InstanceLifecycle)
describeFleetError_lifecycle = Lens.lens (\DescribeFleetError' {lifecycle} -> lifecycle) (\s@DescribeFleetError' {} a -> s {lifecycle = a} :: DescribeFleetError)

-- | The error message that describes why the instance could not be launched.
-- For more information about error messages, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes>.
describeFleetError_errorMessage :: Lens.Lens' DescribeFleetError (Core.Maybe Core.Text)
describeFleetError_errorMessage = Lens.lens (\DescribeFleetError' {errorMessage} -> errorMessage) (\s@DescribeFleetError' {} a -> s {errorMessage = a} :: DescribeFleetError)

-- | The error code that indicates why the instance could not be launched.
-- For more information about error codes, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes>.
describeFleetError_errorCode :: Lens.Lens' DescribeFleetError (Core.Maybe Core.Text)
describeFleetError_errorCode = Lens.lens (\DescribeFleetError' {errorCode} -> errorCode) (\s@DescribeFleetError' {} a -> s {errorCode = a} :: DescribeFleetError)

instance Core.FromXML DescribeFleetError where
  parseXML x =
    DescribeFleetError'
      Core.<$> (x Core..@? "launchTemplateAndOverrides")
      Core.<*> (x Core..@? "lifecycle")
      Core.<*> (x Core..@? "errorMessage")
      Core.<*> (x Core..@? "errorCode")

instance Core.Hashable DescribeFleetError

instance Core.NFData DescribeFleetError
