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
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentResourceDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentResourceDescription where

import Network.AWS.ElasticBeanstalk.Types.AutoScalingGroup
import Network.AWS.ElasticBeanstalk.Types.Instance
import Network.AWS.ElasticBeanstalk.Types.LaunchConfiguration
import Network.AWS.ElasticBeanstalk.Types.LaunchTemplate
import Network.AWS.ElasticBeanstalk.Types.LoadBalancer
import Network.AWS.ElasticBeanstalk.Types.Queue
import Network.AWS.ElasticBeanstalk.Types.Trigger
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the AWS resources in use by this environment. This data is
-- live.
--
-- /See:/ 'newEnvironmentResourceDescription' smart constructor.
data EnvironmentResourceDescription = EnvironmentResourceDescription'
  { -- | The Auto Scaling launch configurations in use by this environment.
    launchConfigurations :: Prelude.Maybe [LaunchConfiguration],
    -- | The Amazon EC2 launch templates in use by this environment.
    launchTemplates :: Prelude.Maybe [LaunchTemplate],
    -- | The @AutoScaling@ triggers in use by this environment.
    triggers :: Prelude.Maybe [Trigger],
    -- | The Amazon EC2 instances used by this environment.
    instances :: Prelude.Maybe [Instance],
    -- | The name of the environment.
    environmentName :: Prelude.Maybe Prelude.Text,
    -- | The queues used by this environment.
    queues :: Prelude.Maybe [Queue],
    -- | The LoadBalancers in use by this environment.
    loadBalancers :: Prelude.Maybe [LoadBalancer],
    -- | The @AutoScalingGroups@ used by this environment.
    autoScalingGroups :: Prelude.Maybe [AutoScalingGroup]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      launchTemplates = Prelude.Nothing,
      triggers = Prelude.Nothing,
      instances = Prelude.Nothing,
      environmentName = Prelude.Nothing,
      queues = Prelude.Nothing,
      loadBalancers = Prelude.Nothing,
      autoScalingGroups = Prelude.Nothing
    }

-- | The Auto Scaling launch configurations in use by this environment.
environmentResourceDescription_launchConfigurations :: Lens.Lens' EnvironmentResourceDescription (Prelude.Maybe [LaunchConfiguration])
environmentResourceDescription_launchConfigurations = Lens.lens (\EnvironmentResourceDescription' {launchConfigurations} -> launchConfigurations) (\s@EnvironmentResourceDescription' {} a -> s {launchConfigurations = a} :: EnvironmentResourceDescription) Prelude.. Lens.mapping Prelude._Coerce

-- | The Amazon EC2 launch templates in use by this environment.
environmentResourceDescription_launchTemplates :: Lens.Lens' EnvironmentResourceDescription (Prelude.Maybe [LaunchTemplate])
environmentResourceDescription_launchTemplates = Lens.lens (\EnvironmentResourceDescription' {launchTemplates} -> launchTemplates) (\s@EnvironmentResourceDescription' {} a -> s {launchTemplates = a} :: EnvironmentResourceDescription) Prelude.. Lens.mapping Prelude._Coerce

-- | The @AutoScaling@ triggers in use by this environment.
environmentResourceDescription_triggers :: Lens.Lens' EnvironmentResourceDescription (Prelude.Maybe [Trigger])
environmentResourceDescription_triggers = Lens.lens (\EnvironmentResourceDescription' {triggers} -> triggers) (\s@EnvironmentResourceDescription' {} a -> s {triggers = a} :: EnvironmentResourceDescription) Prelude.. Lens.mapping Prelude._Coerce

-- | The Amazon EC2 instances used by this environment.
environmentResourceDescription_instances :: Lens.Lens' EnvironmentResourceDescription (Prelude.Maybe [Instance])
environmentResourceDescription_instances = Lens.lens (\EnvironmentResourceDescription' {instances} -> instances) (\s@EnvironmentResourceDescription' {} a -> s {instances = a} :: EnvironmentResourceDescription) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the environment.
environmentResourceDescription_environmentName :: Lens.Lens' EnvironmentResourceDescription (Prelude.Maybe Prelude.Text)
environmentResourceDescription_environmentName = Lens.lens (\EnvironmentResourceDescription' {environmentName} -> environmentName) (\s@EnvironmentResourceDescription' {} a -> s {environmentName = a} :: EnvironmentResourceDescription)

-- | The queues used by this environment.
environmentResourceDescription_queues :: Lens.Lens' EnvironmentResourceDescription (Prelude.Maybe [Queue])
environmentResourceDescription_queues = Lens.lens (\EnvironmentResourceDescription' {queues} -> queues) (\s@EnvironmentResourceDescription' {} a -> s {queues = a} :: EnvironmentResourceDescription) Prelude.. Lens.mapping Prelude._Coerce

-- | The LoadBalancers in use by this environment.
environmentResourceDescription_loadBalancers :: Lens.Lens' EnvironmentResourceDescription (Prelude.Maybe [LoadBalancer])
environmentResourceDescription_loadBalancers = Lens.lens (\EnvironmentResourceDescription' {loadBalancers} -> loadBalancers) (\s@EnvironmentResourceDescription' {} a -> s {loadBalancers = a} :: EnvironmentResourceDescription) Prelude.. Lens.mapping Prelude._Coerce

-- | The @AutoScalingGroups@ used by this environment.
environmentResourceDescription_autoScalingGroups :: Lens.Lens' EnvironmentResourceDescription (Prelude.Maybe [AutoScalingGroup])
environmentResourceDescription_autoScalingGroups = Lens.lens (\EnvironmentResourceDescription' {autoScalingGroups} -> autoScalingGroups) (\s@EnvironmentResourceDescription' {} a -> s {autoScalingGroups = a} :: EnvironmentResourceDescription) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.FromXML
    EnvironmentResourceDescription
  where
  parseXML x =
    EnvironmentResourceDescription'
      Prelude.<$> ( x Prelude..@? "LaunchConfigurations"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> ( x Prelude..@? "LaunchTemplates"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> ( x Prelude..@? "Triggers" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> ( x Prelude..@? "Instances" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "EnvironmentName")
      Prelude.<*> ( x Prelude..@? "Queues" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> ( x Prelude..@? "LoadBalancers"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> ( x Prelude..@? "AutoScalingGroups"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )

instance
  Prelude.Hashable
    EnvironmentResourceDescription

instance
  Prelude.NFData
    EnvironmentResourceDescription
