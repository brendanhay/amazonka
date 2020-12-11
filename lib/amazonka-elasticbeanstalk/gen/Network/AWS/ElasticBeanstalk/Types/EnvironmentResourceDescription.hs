-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentResourceDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentResourceDescription
  ( EnvironmentResourceDescription (..),

    -- * Smart constructor
    mkEnvironmentResourceDescription,

    -- * Lenses
    erdQueues,
    erdTriggers,
    erdLaunchTemplates,
    erdLoadBalancers,
    erdEnvironmentName,
    erdInstances,
    erdLaunchConfigurations,
    erdAutoScalingGroups,
  )
where

import Network.AWS.ElasticBeanstalk.Types.AutoScalingGroup
import Network.AWS.ElasticBeanstalk.Types.Instance
import Network.AWS.ElasticBeanstalk.Types.LaunchConfiguration
import Network.AWS.ElasticBeanstalk.Types.LaunchTemplate
import Network.AWS.ElasticBeanstalk.Types.LoadBalancer
import Network.AWS.ElasticBeanstalk.Types.Queue
import Network.AWS.ElasticBeanstalk.Types.Trigger
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the AWS resources in use by this environment. This data is live.
--
-- /See:/ 'mkEnvironmentResourceDescription' smart constructor.
data EnvironmentResourceDescription = EnvironmentResourceDescription'
  { queues ::
      Lude.Maybe [Queue],
    triggers ::
      Lude.Maybe [Trigger],
    launchTemplates ::
      Lude.Maybe [LaunchTemplate],
    loadBalancers ::
      Lude.Maybe [LoadBalancer],
    environmentName ::
      Lude.Maybe Lude.Text,
    instances ::
      Lude.Maybe [Instance],
    launchConfigurations ::
      Lude.Maybe
        [LaunchConfiguration],
    autoScalingGroups ::
      Lude.Maybe [AutoScalingGroup]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnvironmentResourceDescription' with the minimum fields required to make a request.
--
-- * 'autoScalingGroups' - The @AutoScalingGroups@ used by this environment.
-- * 'environmentName' - The name of the environment.
-- * 'instances' - The Amazon EC2 instances used by this environment.
-- * 'launchConfigurations' - The Auto Scaling launch configurations in use by this environment.
-- * 'launchTemplates' - The Amazon EC2 launch templates in use by this environment.
-- * 'loadBalancers' - The LoadBalancers in use by this environment.
-- * 'queues' - The queues used by this environment.
-- * 'triggers' - The @AutoScaling@ triggers in use by this environment.
mkEnvironmentResourceDescription ::
  EnvironmentResourceDescription
mkEnvironmentResourceDescription =
  EnvironmentResourceDescription'
    { queues = Lude.Nothing,
      triggers = Lude.Nothing,
      launchTemplates = Lude.Nothing,
      loadBalancers = Lude.Nothing,
      environmentName = Lude.Nothing,
      instances = Lude.Nothing,
      launchConfigurations = Lude.Nothing,
      autoScalingGroups = Lude.Nothing
    }

-- | The queues used by this environment.
--
-- /Note:/ Consider using 'queues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erdQueues :: Lens.Lens' EnvironmentResourceDescription (Lude.Maybe [Queue])
erdQueues = Lens.lens (queues :: EnvironmentResourceDescription -> Lude.Maybe [Queue]) (\s a -> s {queues = a} :: EnvironmentResourceDescription)
{-# DEPRECATED erdQueues "Use generic-lens or generic-optics with 'queues' instead." #-}

-- | The @AutoScaling@ triggers in use by this environment.
--
-- /Note:/ Consider using 'triggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erdTriggers :: Lens.Lens' EnvironmentResourceDescription (Lude.Maybe [Trigger])
erdTriggers = Lens.lens (triggers :: EnvironmentResourceDescription -> Lude.Maybe [Trigger]) (\s a -> s {triggers = a} :: EnvironmentResourceDescription)
{-# DEPRECATED erdTriggers "Use generic-lens or generic-optics with 'triggers' instead." #-}

-- | The Amazon EC2 launch templates in use by this environment.
--
-- /Note:/ Consider using 'launchTemplates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erdLaunchTemplates :: Lens.Lens' EnvironmentResourceDescription (Lude.Maybe [LaunchTemplate])
erdLaunchTemplates = Lens.lens (launchTemplates :: EnvironmentResourceDescription -> Lude.Maybe [LaunchTemplate]) (\s a -> s {launchTemplates = a} :: EnvironmentResourceDescription)
{-# DEPRECATED erdLaunchTemplates "Use generic-lens or generic-optics with 'launchTemplates' instead." #-}

-- | The LoadBalancers in use by this environment.
--
-- /Note:/ Consider using 'loadBalancers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erdLoadBalancers :: Lens.Lens' EnvironmentResourceDescription (Lude.Maybe [LoadBalancer])
erdLoadBalancers = Lens.lens (loadBalancers :: EnvironmentResourceDescription -> Lude.Maybe [LoadBalancer]) (\s a -> s {loadBalancers = a} :: EnvironmentResourceDescription)
{-# DEPRECATED erdLoadBalancers "Use generic-lens or generic-optics with 'loadBalancers' instead." #-}

-- | The name of the environment.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erdEnvironmentName :: Lens.Lens' EnvironmentResourceDescription (Lude.Maybe Lude.Text)
erdEnvironmentName = Lens.lens (environmentName :: EnvironmentResourceDescription -> Lude.Maybe Lude.Text) (\s a -> s {environmentName = a} :: EnvironmentResourceDescription)
{-# DEPRECATED erdEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | The Amazon EC2 instances used by this environment.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erdInstances :: Lens.Lens' EnvironmentResourceDescription (Lude.Maybe [Instance])
erdInstances = Lens.lens (instances :: EnvironmentResourceDescription -> Lude.Maybe [Instance]) (\s a -> s {instances = a} :: EnvironmentResourceDescription)
{-# DEPRECATED erdInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

-- | The Auto Scaling launch configurations in use by this environment.
--
-- /Note:/ Consider using 'launchConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erdLaunchConfigurations :: Lens.Lens' EnvironmentResourceDescription (Lude.Maybe [LaunchConfiguration])
erdLaunchConfigurations = Lens.lens (launchConfigurations :: EnvironmentResourceDescription -> Lude.Maybe [LaunchConfiguration]) (\s a -> s {launchConfigurations = a} :: EnvironmentResourceDescription)
{-# DEPRECATED erdLaunchConfigurations "Use generic-lens or generic-optics with 'launchConfigurations' instead." #-}

-- | The @AutoScalingGroups@ used by this environment.
--
-- /Note:/ Consider using 'autoScalingGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erdAutoScalingGroups :: Lens.Lens' EnvironmentResourceDescription (Lude.Maybe [AutoScalingGroup])
erdAutoScalingGroups = Lens.lens (autoScalingGroups :: EnvironmentResourceDescription -> Lude.Maybe [AutoScalingGroup]) (\s a -> s {autoScalingGroups = a} :: EnvironmentResourceDescription)
{-# DEPRECATED erdAutoScalingGroups "Use generic-lens or generic-optics with 'autoScalingGroups' instead." #-}

instance Lude.FromXML EnvironmentResourceDescription where
  parseXML x =
    EnvironmentResourceDescription'
      Lude.<$> ( x Lude..@? "Queues" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> ( x Lude..@? "Triggers" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> ( x Lude..@? "LaunchTemplates" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> ( x Lude..@? "LoadBalancers" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "EnvironmentName")
      Lude.<*> ( x Lude..@? "Instances" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> ( x Lude..@? "LaunchConfigurations" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> ( x Lude..@? "AutoScalingGroups" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
