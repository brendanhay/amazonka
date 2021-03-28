{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DescribeFleetError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.DescribeFleetError
  ( DescribeFleetError (..)
  -- * Smart constructor
  , mkDescribeFleetError
  -- * Lenses
  , dfeErrorCode
  , dfeErrorMessage
  , dfeLaunchTemplateAndOverrides
  , dfeLifecycle
  ) where

import qualified Network.AWS.EC2.Types.InstanceLifecycle as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateAndOverridesResponse as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the instances that could not be launched by the fleet.
--
-- /See:/ 'mkDescribeFleetError' smart constructor.
data DescribeFleetError = DescribeFleetError'
  { errorCode :: Core.Maybe Core.Text
    -- ^ The error code that indicates why the instance could not be launched. For more information about error codes, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
  , errorMessage :: Core.Maybe Core.Text
    -- ^ The error message that describes why the instance could not be launched. For more information about error messages, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
  , launchTemplateAndOverrides :: Core.Maybe Types.LaunchTemplateAndOverridesResponse
    -- ^ The launch templates and overrides that were used for launching the instances. The values that you specify in the Overrides replace the values in the launch template.
  , lifecycle :: Core.Maybe Types.InstanceLifecycle
    -- ^ Indicates if the instance that could not be launched was a Spot Instance or On-Demand Instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeFleetError' value with any optional fields omitted.
mkDescribeFleetError
    :: DescribeFleetError
mkDescribeFleetError
  = DescribeFleetError'{errorCode = Core.Nothing,
                        errorMessage = Core.Nothing,
                        launchTemplateAndOverrides = Core.Nothing,
                        lifecycle = Core.Nothing}

-- | The error code that indicates why the instance could not be launched. For more information about error codes, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfeErrorCode :: Lens.Lens' DescribeFleetError (Core.Maybe Core.Text)
dfeErrorCode = Lens.field @"errorCode"
{-# INLINEABLE dfeErrorCode #-}
{-# DEPRECATED errorCode "Use generic-lens or generic-optics with 'errorCode' instead"  #-}

-- | The error message that describes why the instance could not be launched. For more information about error messages, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfeErrorMessage :: Lens.Lens' DescribeFleetError (Core.Maybe Core.Text)
dfeErrorMessage = Lens.field @"errorMessage"
{-# INLINEABLE dfeErrorMessage #-}
{-# DEPRECATED errorMessage "Use generic-lens or generic-optics with 'errorMessage' instead"  #-}

-- | The launch templates and overrides that were used for launching the instances. The values that you specify in the Overrides replace the values in the launch template.
--
-- /Note:/ Consider using 'launchTemplateAndOverrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfeLaunchTemplateAndOverrides :: Lens.Lens' DescribeFleetError (Core.Maybe Types.LaunchTemplateAndOverridesResponse)
dfeLaunchTemplateAndOverrides = Lens.field @"launchTemplateAndOverrides"
{-# INLINEABLE dfeLaunchTemplateAndOverrides #-}
{-# DEPRECATED launchTemplateAndOverrides "Use generic-lens or generic-optics with 'launchTemplateAndOverrides' instead"  #-}

-- | Indicates if the instance that could not be launched was a Spot Instance or On-Demand Instance.
--
-- /Note:/ Consider using 'lifecycle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfeLifecycle :: Lens.Lens' DescribeFleetError (Core.Maybe Types.InstanceLifecycle)
dfeLifecycle = Lens.field @"lifecycle"
{-# INLINEABLE dfeLifecycle #-}
{-# DEPRECATED lifecycle "Use generic-lens or generic-optics with 'lifecycle' instead"  #-}

instance Core.FromXML DescribeFleetError where
        parseXML x
          = DescribeFleetError' Core.<$>
              (x Core..@? "errorCode") Core.<*> x Core..@? "errorMessage"
                Core.<*> x Core..@? "launchTemplateAndOverrides"
                Core.<*> x Core..@? "lifecycle"
