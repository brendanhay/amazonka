{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.LaunchTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.LaunchTemplate
  ( LaunchTemplate (..)
  -- * Smart constructor
  , mkLaunchTemplate
  -- * Lenses
  , ltLaunchTemplateSpecification
  , ltOverrides
  ) where

import qualified Network.AWS.AutoScaling.Types.LaunchTemplateOverrides as Types
import qualified Network.AWS.AutoScaling.Types.LaunchTemplateSpecification as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a launch template and overrides. 
--
-- You specify these parameters as part of a mixed instances policy. 
-- When you update the launch template or overrides, existing Amazon EC2 instances continue to run. When scale out occurs, Amazon EC2 Auto Scaling launches instances to match the new settings. When scale in occurs, Amazon EC2 Auto Scaling terminates instances according to the group's termination policies.
--
-- /See:/ 'mkLaunchTemplate' smart constructor.
data LaunchTemplate = LaunchTemplate'
  { launchTemplateSpecification :: Core.Maybe Types.LaunchTemplateSpecification
    -- ^ The launch template to use.
  , overrides :: Core.Maybe [Types.LaunchTemplateOverrides]
    -- ^ Any parameters that you specify override the same parameters in the launch template. If not provided, Amazon EC2 Auto Scaling uses the instance type specified in the launch template when it launches an instance. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplate' value with any optional fields omitted.
mkLaunchTemplate
    :: LaunchTemplate
mkLaunchTemplate
  = LaunchTemplate'{launchTemplateSpecification = Core.Nothing,
                    overrides = Core.Nothing}

-- | The launch template to use.
--
-- /Note:/ Consider using 'launchTemplateSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltLaunchTemplateSpecification :: Lens.Lens' LaunchTemplate (Core.Maybe Types.LaunchTemplateSpecification)
ltLaunchTemplateSpecification = Lens.field @"launchTemplateSpecification"
{-# INLINEABLE ltLaunchTemplateSpecification #-}
{-# DEPRECATED launchTemplateSpecification "Use generic-lens or generic-optics with 'launchTemplateSpecification' instead"  #-}

-- | Any parameters that you specify override the same parameters in the launch template. If not provided, Amazon EC2 Auto Scaling uses the instance type specified in the launch template when it launches an instance. 
--
-- /Note:/ Consider using 'overrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltOverrides :: Lens.Lens' LaunchTemplate (Core.Maybe [Types.LaunchTemplateOverrides])
ltOverrides = Lens.field @"overrides"
{-# INLINEABLE ltOverrides #-}
{-# DEPRECATED overrides "Use generic-lens or generic-optics with 'overrides' instead"  #-}

instance Core.ToQuery LaunchTemplate where
        toQuery LaunchTemplate{..}
          = Core.maybe Core.mempty
              (Core.toQueryPair "LaunchTemplateSpecification")
              launchTemplateSpecification
              Core.<>
              Core.toQueryPair "Overrides"
                (Core.maybe Core.mempty (Core.toQueryList "member") overrides)

instance Core.FromXML LaunchTemplate where
        parseXML x
          = LaunchTemplate' Core.<$>
              (x Core..@? "LaunchTemplateSpecification") Core.<*>
                x Core..@? "Overrides" Core..<@> Core.parseXMLList "member"
