{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentResourceDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.EnvironmentResourceDescription
  ( EnvironmentResourceDescription (..)
  -- * Smart constructor
  , mkEnvironmentResourceDescription
  -- * Lenses
  , erdAutoScalingGroups
  , erdEnvironmentName
  , erdInstances
  , erdLaunchConfigurations
  , erdLaunchTemplates
  , erdLoadBalancers
  , erdQueues
  , erdTriggers
  ) where

import qualified Network.AWS.ElasticBeanstalk.Types.AutoScalingGroup as Types
import qualified Network.AWS.ElasticBeanstalk.Types.EnvironmentName as Types
import qualified Network.AWS.ElasticBeanstalk.Types.Instance as Types
import qualified Network.AWS.ElasticBeanstalk.Types.LaunchConfiguration as Types
import qualified Network.AWS.ElasticBeanstalk.Types.LaunchTemplate as Types
import qualified Network.AWS.ElasticBeanstalk.Types.LoadBalancer as Types
import qualified Network.AWS.ElasticBeanstalk.Types.Queue as Types
import qualified Network.AWS.ElasticBeanstalk.Types.Trigger as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the AWS resources in use by this environment. This data is live.
--
-- /See:/ 'mkEnvironmentResourceDescription' smart constructor.
data EnvironmentResourceDescription = EnvironmentResourceDescription'
  { autoScalingGroups :: Core.Maybe [Types.AutoScalingGroup]
    -- ^ The @AutoScalingGroups@ used by this environment. 
  , environmentName :: Core.Maybe Types.EnvironmentName
    -- ^ The name of the environment.
  , instances :: Core.Maybe [Types.Instance]
    -- ^ The Amazon EC2 instances used by this environment.
  , launchConfigurations :: Core.Maybe [Types.LaunchConfiguration]
    -- ^ The Auto Scaling launch configurations in use by this environment.
  , launchTemplates :: Core.Maybe [Types.LaunchTemplate]
    -- ^ The Amazon EC2 launch templates in use by this environment.
  , loadBalancers :: Core.Maybe [Types.LoadBalancer]
    -- ^ The LoadBalancers in use by this environment.
  , queues :: Core.Maybe [Types.Queue]
    -- ^ The queues used by this environment.
  , triggers :: Core.Maybe [Types.Trigger]
    -- ^ The @AutoScaling@ triggers in use by this environment. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnvironmentResourceDescription' value with any optional fields omitted.
mkEnvironmentResourceDescription
    :: EnvironmentResourceDescription
mkEnvironmentResourceDescription
  = EnvironmentResourceDescription'{autoScalingGroups = Core.Nothing,
                                    environmentName = Core.Nothing, instances = Core.Nothing,
                                    launchConfigurations = Core.Nothing,
                                    launchTemplates = Core.Nothing, loadBalancers = Core.Nothing,
                                    queues = Core.Nothing, triggers = Core.Nothing}

-- | The @AutoScalingGroups@ used by this environment. 
--
-- /Note:/ Consider using 'autoScalingGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erdAutoScalingGroups :: Lens.Lens' EnvironmentResourceDescription (Core.Maybe [Types.AutoScalingGroup])
erdAutoScalingGroups = Lens.field @"autoScalingGroups"
{-# INLINEABLE erdAutoScalingGroups #-}
{-# DEPRECATED autoScalingGroups "Use generic-lens or generic-optics with 'autoScalingGroups' instead"  #-}

-- | The name of the environment.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erdEnvironmentName :: Lens.Lens' EnvironmentResourceDescription (Core.Maybe Types.EnvironmentName)
erdEnvironmentName = Lens.field @"environmentName"
{-# INLINEABLE erdEnvironmentName #-}
{-# DEPRECATED environmentName "Use generic-lens or generic-optics with 'environmentName' instead"  #-}

-- | The Amazon EC2 instances used by this environment.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erdInstances :: Lens.Lens' EnvironmentResourceDescription (Core.Maybe [Types.Instance])
erdInstances = Lens.field @"instances"
{-# INLINEABLE erdInstances #-}
{-# DEPRECATED instances "Use generic-lens or generic-optics with 'instances' instead"  #-}

-- | The Auto Scaling launch configurations in use by this environment.
--
-- /Note:/ Consider using 'launchConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erdLaunchConfigurations :: Lens.Lens' EnvironmentResourceDescription (Core.Maybe [Types.LaunchConfiguration])
erdLaunchConfigurations = Lens.field @"launchConfigurations"
{-# INLINEABLE erdLaunchConfigurations #-}
{-# DEPRECATED launchConfigurations "Use generic-lens or generic-optics with 'launchConfigurations' instead"  #-}

-- | The Amazon EC2 launch templates in use by this environment.
--
-- /Note:/ Consider using 'launchTemplates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erdLaunchTemplates :: Lens.Lens' EnvironmentResourceDescription (Core.Maybe [Types.LaunchTemplate])
erdLaunchTemplates = Lens.field @"launchTemplates"
{-# INLINEABLE erdLaunchTemplates #-}
{-# DEPRECATED launchTemplates "Use generic-lens or generic-optics with 'launchTemplates' instead"  #-}

-- | The LoadBalancers in use by this environment.
--
-- /Note:/ Consider using 'loadBalancers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erdLoadBalancers :: Lens.Lens' EnvironmentResourceDescription (Core.Maybe [Types.LoadBalancer])
erdLoadBalancers = Lens.field @"loadBalancers"
{-# INLINEABLE erdLoadBalancers #-}
{-# DEPRECATED loadBalancers "Use generic-lens or generic-optics with 'loadBalancers' instead"  #-}

-- | The queues used by this environment.
--
-- /Note:/ Consider using 'queues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erdQueues :: Lens.Lens' EnvironmentResourceDescription (Core.Maybe [Types.Queue])
erdQueues = Lens.field @"queues"
{-# INLINEABLE erdQueues #-}
{-# DEPRECATED queues "Use generic-lens or generic-optics with 'queues' instead"  #-}

-- | The @AutoScaling@ triggers in use by this environment. 
--
-- /Note:/ Consider using 'triggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erdTriggers :: Lens.Lens' EnvironmentResourceDescription (Core.Maybe [Types.Trigger])
erdTriggers = Lens.field @"triggers"
{-# INLINEABLE erdTriggers #-}
{-# DEPRECATED triggers "Use generic-lens or generic-optics with 'triggers' instead"  #-}

instance Core.FromXML EnvironmentResourceDescription where
        parseXML x
          = EnvironmentResourceDescription' Core.<$>
              (x Core..@? "AutoScalingGroups" Core..<@>
                 Core.parseXMLList "member")
                Core.<*> x Core..@? "EnvironmentName"
                Core.<*>
                x Core..@? "Instances" Core..<@> Core.parseXMLList "member"
                Core.<*>
                x Core..@? "LaunchConfigurations" Core..<@>
                  Core.parseXMLList "member"
                Core.<*>
                x Core..@? "LaunchTemplates" Core..<@> Core.parseXMLList "member"
                Core.<*>
                x Core..@? "LoadBalancers" Core..<@> Core.parseXMLList "member"
                Core.<*> x Core..@? "Queues" Core..<@> Core.parseXMLList "member"
                Core.<*> x Core..@? "Triggers" Core..<@> Core.parseXMLList "member"
