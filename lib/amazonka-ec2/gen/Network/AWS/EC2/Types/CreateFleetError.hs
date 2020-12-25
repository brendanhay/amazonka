{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CreateFleetError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CreateFleetError
  ( CreateFleetError (..),

    -- * Smart constructor
    mkCreateFleetError,

    -- * Lenses
    cfeErrorCode,
    cfeErrorMessage,
    cfeLaunchTemplateAndOverrides,
    cfeLifecycle,
  )
where

import qualified Network.AWS.EC2.Types.InstanceLifecycle as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateAndOverridesResponse as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the instances that could not be launched by the fleet.
--
-- /See:/ 'mkCreateFleetError' smart constructor.
data CreateFleetError = CreateFleetError'
  { -- | The error code that indicates why the instance could not be launched. For more information about error codes, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
    errorCode :: Core.Maybe Types.String,
    -- | The error message that describes why the instance could not be launched. For more information about error messages, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
    errorMessage :: Core.Maybe Types.String,
    -- | The launch templates and overrides that were used for launching the instances. The values that you specify in the Overrides replace the values in the launch template.
    launchTemplateAndOverrides :: Core.Maybe Types.LaunchTemplateAndOverridesResponse,
    -- | Indicates if the instance that could not be launched was a Spot Instance or On-Demand Instance.
    lifecycle :: Core.Maybe Types.InstanceLifecycle
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateFleetError' value with any optional fields omitted.
mkCreateFleetError ::
  CreateFleetError
mkCreateFleetError =
  CreateFleetError'
    { errorCode = Core.Nothing,
      errorMessage = Core.Nothing,
      launchTemplateAndOverrides = Core.Nothing,
      lifecycle = Core.Nothing
    }

-- | The error code that indicates why the instance could not be launched. For more information about error codes, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfeErrorCode :: Lens.Lens' CreateFleetError (Core.Maybe Types.String)
cfeErrorCode = Lens.field @"errorCode"
{-# DEPRECATED cfeErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The error message that describes why the instance could not be launched. For more information about error messages, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfeErrorMessage :: Lens.Lens' CreateFleetError (Core.Maybe Types.String)
cfeErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED cfeErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The launch templates and overrides that were used for launching the instances. The values that you specify in the Overrides replace the values in the launch template.
--
-- /Note:/ Consider using 'launchTemplateAndOverrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfeLaunchTemplateAndOverrides :: Lens.Lens' CreateFleetError (Core.Maybe Types.LaunchTemplateAndOverridesResponse)
cfeLaunchTemplateAndOverrides = Lens.field @"launchTemplateAndOverrides"
{-# DEPRECATED cfeLaunchTemplateAndOverrides "Use generic-lens or generic-optics with 'launchTemplateAndOverrides' instead." #-}

-- | Indicates if the instance that could not be launched was a Spot Instance or On-Demand Instance.
--
-- /Note:/ Consider using 'lifecycle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfeLifecycle :: Lens.Lens' CreateFleetError (Core.Maybe Types.InstanceLifecycle)
cfeLifecycle = Lens.field @"lifecycle"
{-# DEPRECATED cfeLifecycle "Use generic-lens or generic-optics with 'lifecycle' instead." #-}

instance Core.FromXML CreateFleetError where
  parseXML x =
    CreateFleetError'
      Core.<$> (x Core..@? "errorCode")
      Core.<*> (x Core..@? "errorMessage")
      Core.<*> (x Core..@? "launchTemplateAndOverrides")
      Core.<*> (x Core..@? "lifecycle")
