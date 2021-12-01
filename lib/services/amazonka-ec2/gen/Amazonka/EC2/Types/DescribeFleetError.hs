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
-- Module      : Amazonka.EC2.Types.DescribeFleetError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.DescribeFleetError where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.InstanceLifecycle
import Amazonka.EC2.Types.LaunchTemplateAndOverridesResponse
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the instances that could not be launched by the fleet.
--
-- /See:/ 'newDescribeFleetError' smart constructor.
data DescribeFleetError = DescribeFleetError'
  { -- | Indicates if the instance that could not be launched was a Spot Instance
    -- or On-Demand Instance.
    lifecycle :: Prelude.Maybe InstanceLifecycle,
    -- | The launch templates and overrides that were used for launching the
    -- instances. The values that you specify in the Overrides replace the
    -- values in the launch template.
    launchTemplateAndOverrides :: Prelude.Maybe LaunchTemplateAndOverridesResponse,
    -- | The error code that indicates why the instance could not be launched.
    -- For more information about error codes, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes>.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The error message that describes why the instance could not be launched.
    -- For more information about error messages, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes>.
    errorMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFleetError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lifecycle', 'describeFleetError_lifecycle' - Indicates if the instance that could not be launched was a Spot Instance
-- or On-Demand Instance.
--
-- 'launchTemplateAndOverrides', 'describeFleetError_launchTemplateAndOverrides' - The launch templates and overrides that were used for launching the
-- instances. The values that you specify in the Overrides replace the
-- values in the launch template.
--
-- 'errorCode', 'describeFleetError_errorCode' - The error code that indicates why the instance could not be launched.
-- For more information about error codes, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes>.
--
-- 'errorMessage', 'describeFleetError_errorMessage' - The error message that describes why the instance could not be launched.
-- For more information about error messages, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes>.
newDescribeFleetError ::
  DescribeFleetError
newDescribeFleetError =
  DescribeFleetError'
    { lifecycle = Prelude.Nothing,
      launchTemplateAndOverrides = Prelude.Nothing,
      errorCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | Indicates if the instance that could not be launched was a Spot Instance
-- or On-Demand Instance.
describeFleetError_lifecycle :: Lens.Lens' DescribeFleetError (Prelude.Maybe InstanceLifecycle)
describeFleetError_lifecycle = Lens.lens (\DescribeFleetError' {lifecycle} -> lifecycle) (\s@DescribeFleetError' {} a -> s {lifecycle = a} :: DescribeFleetError)

-- | The launch templates and overrides that were used for launching the
-- instances. The values that you specify in the Overrides replace the
-- values in the launch template.
describeFleetError_launchTemplateAndOverrides :: Lens.Lens' DescribeFleetError (Prelude.Maybe LaunchTemplateAndOverridesResponse)
describeFleetError_launchTemplateAndOverrides = Lens.lens (\DescribeFleetError' {launchTemplateAndOverrides} -> launchTemplateAndOverrides) (\s@DescribeFleetError' {} a -> s {launchTemplateAndOverrides = a} :: DescribeFleetError)

-- | The error code that indicates why the instance could not be launched.
-- For more information about error codes, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes>.
describeFleetError_errorCode :: Lens.Lens' DescribeFleetError (Prelude.Maybe Prelude.Text)
describeFleetError_errorCode = Lens.lens (\DescribeFleetError' {errorCode} -> errorCode) (\s@DescribeFleetError' {} a -> s {errorCode = a} :: DescribeFleetError)

-- | The error message that describes why the instance could not be launched.
-- For more information about error messages, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes>.
describeFleetError_errorMessage :: Lens.Lens' DescribeFleetError (Prelude.Maybe Prelude.Text)
describeFleetError_errorMessage = Lens.lens (\DescribeFleetError' {errorMessage} -> errorMessage) (\s@DescribeFleetError' {} a -> s {errorMessage = a} :: DescribeFleetError)

instance Core.FromXML DescribeFleetError where
  parseXML x =
    DescribeFleetError'
      Prelude.<$> (x Core..@? "lifecycle")
      Prelude.<*> (x Core..@? "launchTemplateAndOverrides")
      Prelude.<*> (x Core..@? "errorCode")
      Prelude.<*> (x Core..@? "errorMessage")

instance Prelude.Hashable DescribeFleetError where
  hashWithSalt salt' DescribeFleetError' {..} =
    salt' `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` launchTemplateAndOverrides
      `Prelude.hashWithSalt` lifecycle

instance Prelude.NFData DescribeFleetError where
  rnf DescribeFleetError' {..} =
    Prelude.rnf lifecycle
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf launchTemplateAndOverrides
