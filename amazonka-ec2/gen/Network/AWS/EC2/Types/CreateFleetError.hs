{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.CreateFleetError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CreateFleetError where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceLifecycle
import Network.AWS.EC2.Types.LaunchTemplateAndOverridesResponse
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the instances that could not be launched by the fleet.
--
-- /See:/ 'newCreateFleetError' smart constructor.
data CreateFleetError = CreateFleetError'
  { -- | The launch templates and overrides that were used for launching the
    -- instances. The values that you specify in the Overrides replace the
    -- values in the launch template.
    launchTemplateAndOverrides :: Prelude.Maybe LaunchTemplateAndOverridesResponse,
    -- | Indicates if the instance that could not be launched was a Spot Instance
    -- or On-Demand Instance.
    lifecycle :: Prelude.Maybe InstanceLifecycle,
    -- | The error message that describes why the instance could not be launched.
    -- For more information about error messages, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes>.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The error code that indicates why the instance could not be launched.
    -- For more information about error codes, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes>.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateFleetError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplateAndOverrides', 'createFleetError_launchTemplateAndOverrides' - The launch templates and overrides that were used for launching the
-- instances. The values that you specify in the Overrides replace the
-- values in the launch template.
--
-- 'lifecycle', 'createFleetError_lifecycle' - Indicates if the instance that could not be launched was a Spot Instance
-- or On-Demand Instance.
--
-- 'errorMessage', 'createFleetError_errorMessage' - The error message that describes why the instance could not be launched.
-- For more information about error messages, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes>.
--
-- 'errorCode', 'createFleetError_errorCode' - The error code that indicates why the instance could not be launched.
-- For more information about error codes, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes>.
newCreateFleetError ::
  CreateFleetError
newCreateFleetError =
  CreateFleetError'
    { launchTemplateAndOverrides =
        Prelude.Nothing,
      lifecycle = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The launch templates and overrides that were used for launching the
-- instances. The values that you specify in the Overrides replace the
-- values in the launch template.
createFleetError_launchTemplateAndOverrides :: Lens.Lens' CreateFleetError (Prelude.Maybe LaunchTemplateAndOverridesResponse)
createFleetError_launchTemplateAndOverrides = Lens.lens (\CreateFleetError' {launchTemplateAndOverrides} -> launchTemplateAndOverrides) (\s@CreateFleetError' {} a -> s {launchTemplateAndOverrides = a} :: CreateFleetError)

-- | Indicates if the instance that could not be launched was a Spot Instance
-- or On-Demand Instance.
createFleetError_lifecycle :: Lens.Lens' CreateFleetError (Prelude.Maybe InstanceLifecycle)
createFleetError_lifecycle = Lens.lens (\CreateFleetError' {lifecycle} -> lifecycle) (\s@CreateFleetError' {} a -> s {lifecycle = a} :: CreateFleetError)

-- | The error message that describes why the instance could not be launched.
-- For more information about error messages, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes>.
createFleetError_errorMessage :: Lens.Lens' CreateFleetError (Prelude.Maybe Prelude.Text)
createFleetError_errorMessage = Lens.lens (\CreateFleetError' {errorMessage} -> errorMessage) (\s@CreateFleetError' {} a -> s {errorMessage = a} :: CreateFleetError)

-- | The error code that indicates why the instance could not be launched.
-- For more information about error codes, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes>.
createFleetError_errorCode :: Lens.Lens' CreateFleetError (Prelude.Maybe Prelude.Text)
createFleetError_errorCode = Lens.lens (\CreateFleetError' {errorCode} -> errorCode) (\s@CreateFleetError' {} a -> s {errorCode = a} :: CreateFleetError)

instance Prelude.FromXML CreateFleetError where
  parseXML x =
    CreateFleetError'
      Prelude.<$> (x Prelude..@? "launchTemplateAndOverrides")
      Prelude.<*> (x Prelude..@? "lifecycle")
      Prelude.<*> (x Prelude..@? "errorMessage")
      Prelude.<*> (x Prelude..@? "errorCode")

instance Prelude.Hashable CreateFleetError

instance Prelude.NFData CreateFleetError
