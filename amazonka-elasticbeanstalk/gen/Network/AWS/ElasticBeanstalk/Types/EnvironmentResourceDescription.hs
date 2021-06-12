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
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentResourceDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentResourceDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types.AutoScalingGroup
import Network.AWS.ElasticBeanstalk.Types.Instance
import Network.AWS.ElasticBeanstalk.Types.LaunchConfiguration
import Network.AWS.ElasticBeanstalk.Types.LaunchTemplate
import Network.AWS.ElasticBeanstalk.Types.LoadBalancer
import Network.AWS.ElasticBeanstalk.Types.Queue
import Network.AWS.ElasticBeanstalk.Types.Trigger
import qualified Network.AWS.Lens as Lens

-- | Describes the AWS resources in use by this environment. This data is
-- live.
--
-- /See:/ 'newEnvironmentResourceDescription' smart constructor.
data EnvironmentResourceDescription = EnvironmentResourceDescription'
  { -- | The Auto Scaling launch configurations in use by this environment.
    launchConfigurations :: Core.Maybe [LaunchConfiguration],
    -- | The Amazon EC2 launch templates in use by this environment.
    launchTemplates :: Core.Maybe [LaunchTemplate],
    -- | The @AutoScaling@ triggers in use by this environment.
    triggers :: Core.Maybe [Trigger],
    -- | The Amazon EC2 instances used by this environment.
    instances :: Core.Maybe [Instance],
    -- | The name of the environment.
    environmentName :: Core.Maybe Core.Text,
    -- | The queues used by this environment.
    queues :: Core.Maybe [Queue],
    -- | The LoadBalancers in use by this environment.
    loadBalancers :: Core.Maybe [LoadBalancer],
    -- | The @AutoScalingGroups@ used by this environment.
    autoScalingGroups :: Core.Maybe [AutoScalingGroup]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnvironmentResourceDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchConfigurations', 'environmentResourceDescription_launchConfigurations' - The Auto Scaling launch configurations in use by this environment.
--
-- 'launchTemplates', 'environmentResourceDescription_launchTemplates' - The Amazon EC2 launch templates in use by this environment.
--
-- 'triggers', 'environmentResourceDescription_triggers' - The @AutoScaling@ triggers in use by this environment.
--
-- 'instances', 'environmentResourceDescription_instances' - The Amazon EC2 instances used by this environment.
--
-- 'environmentName', 'environmentResourceDescription_environmentName' - The name of the environment.
--
-- 'queues', 'environmentResourceDescription_queues' - The queues used by this environment.
--
-- 'loadBalancers', 'environmentResourceDescription_loadBalancers' - The LoadBalancers in use by this environment.
--
-- 'autoScalingGroups', 'environmentResourceDescription_autoScalingGroups' - The @AutoScalingGroups@ used by this environment.
newEnvironmentResourceDescription ::
  EnvironmentResourceDescription
newEnvironmentResourceDescription =
  EnvironmentResourceDescription'
    { launchConfigurations =
        Core.Nothing,
      launchTemplates = Core.Nothing,
      triggers = Core.Nothing,
      instances = Core.Nothing,
      environmentName = Core.Nothing,
      queues = Core.Nothing,
      loadBalancers = Core.Nothing,
      autoScalingGroups = Core.Nothing
    }

-- | The Auto Scaling launch configurations in use by this environment.
environmentResourceDescription_launchConfigurations :: Lens.Lens' EnvironmentResourceDescription (Core.Maybe [LaunchConfiguration])
environmentResourceDescription_launchConfigurations = Lens.lens (\EnvironmentResourceDescription' {launchConfigurations} -> launchConfigurations) (\s@EnvironmentResourceDescription' {} a -> s {launchConfigurations = a} :: EnvironmentResourceDescription) Core.. Lens.mapping Lens._Coerce

-- | The Amazon EC2 launch templates in use by this environment.
environmentResourceDescription_launchTemplates :: Lens.Lens' EnvironmentResourceDescription (Core.Maybe [LaunchTemplate])
environmentResourceDescription_launchTemplates = Lens.lens (\EnvironmentResourceDescription' {launchTemplates} -> launchTemplates) (\s@EnvironmentResourceDescription' {} a -> s {launchTemplates = a} :: EnvironmentResourceDescription) Core.. Lens.mapping Lens._Coerce

-- | The @AutoScaling@ triggers in use by this environment.
environmentResourceDescription_triggers :: Lens.Lens' EnvironmentResourceDescription (Core.Maybe [Trigger])
environmentResourceDescription_triggers = Lens.lens (\EnvironmentResourceDescription' {triggers} -> triggers) (\s@EnvironmentResourceDescription' {} a -> s {triggers = a} :: EnvironmentResourceDescription) Core.. Lens.mapping Lens._Coerce

-- | The Amazon EC2 instances used by this environment.
environmentResourceDescription_instances :: Lens.Lens' EnvironmentResourceDescription (Core.Maybe [Instance])
environmentResourceDescription_instances = Lens.lens (\EnvironmentResourceDescription' {instances} -> instances) (\s@EnvironmentResourceDescription' {} a -> s {instances = a} :: EnvironmentResourceDescription) Core.. Lens.mapping Lens._Coerce

-- | The name of the environment.
environmentResourceDescription_environmentName :: Lens.Lens' EnvironmentResourceDescription (Core.Maybe Core.Text)
environmentResourceDescription_environmentName = Lens.lens (\EnvironmentResourceDescription' {environmentName} -> environmentName) (\s@EnvironmentResourceDescription' {} a -> s {environmentName = a} :: EnvironmentResourceDescription)

-- | The queues used by this environment.
environmentResourceDescription_queues :: Lens.Lens' EnvironmentResourceDescription (Core.Maybe [Queue])
environmentResourceDescription_queues = Lens.lens (\EnvironmentResourceDescription' {queues} -> queues) (\s@EnvironmentResourceDescription' {} a -> s {queues = a} :: EnvironmentResourceDescription) Core.. Lens.mapping Lens._Coerce

-- | The LoadBalancers in use by this environment.
environmentResourceDescription_loadBalancers :: Lens.Lens' EnvironmentResourceDescription (Core.Maybe [LoadBalancer])
environmentResourceDescription_loadBalancers = Lens.lens (\EnvironmentResourceDescription' {loadBalancers} -> loadBalancers) (\s@EnvironmentResourceDescription' {} a -> s {loadBalancers = a} :: EnvironmentResourceDescription) Core.. Lens.mapping Lens._Coerce

-- | The @AutoScalingGroups@ used by this environment.
environmentResourceDescription_autoScalingGroups :: Lens.Lens' EnvironmentResourceDescription (Core.Maybe [AutoScalingGroup])
environmentResourceDescription_autoScalingGroups = Lens.lens (\EnvironmentResourceDescription' {autoScalingGroups} -> autoScalingGroups) (\s@EnvironmentResourceDescription' {} a -> s {autoScalingGroups = a} :: EnvironmentResourceDescription) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML EnvironmentResourceDescription where
  parseXML x =
    EnvironmentResourceDescription'
      Core.<$> ( x Core..@? "LaunchConfigurations"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> ( x Core..@? "LaunchTemplates" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> ( x Core..@? "Triggers" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> ( x Core..@? "Instances" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "EnvironmentName")
      Core.<*> ( x Core..@? "Queues" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> ( x Core..@? "LoadBalancers" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> ( x Core..@? "AutoScalingGroups" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )

instance Core.Hashable EnvironmentResourceDescription

instance Core.NFData EnvironmentResourceDescription
