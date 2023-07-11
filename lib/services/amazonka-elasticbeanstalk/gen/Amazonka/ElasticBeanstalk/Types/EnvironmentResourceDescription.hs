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
-- Module      : Amazonka.ElasticBeanstalk.Types.EnvironmentResourceDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.EnvironmentResourceDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types.AutoScalingGroup
import Amazonka.ElasticBeanstalk.Types.Instance
import Amazonka.ElasticBeanstalk.Types.LaunchConfiguration
import Amazonka.ElasticBeanstalk.Types.LaunchTemplate
import Amazonka.ElasticBeanstalk.Types.LoadBalancer
import Amazonka.ElasticBeanstalk.Types.Queue
import Amazonka.ElasticBeanstalk.Types.Trigger
import qualified Amazonka.Prelude as Prelude

-- | Describes the AWS resources in use by this environment. This data is
-- live.
--
-- /See:/ 'newEnvironmentResourceDescription' smart constructor.
data EnvironmentResourceDescription = EnvironmentResourceDescription'
  { -- | The @AutoScalingGroups@ used by this environment.
    autoScalingGroups :: Prelude.Maybe [AutoScalingGroup],
    -- | The name of the environment.
    environmentName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon EC2 instances used by this environment.
    instances :: Prelude.Maybe [Instance],
    -- | The Auto Scaling launch configurations in use by this environment.
    launchConfigurations :: Prelude.Maybe [LaunchConfiguration],
    -- | The Amazon EC2 launch templates in use by this environment.
    launchTemplates :: Prelude.Maybe [LaunchTemplate],
    -- | The LoadBalancers in use by this environment.
    loadBalancers :: Prelude.Maybe [LoadBalancer],
    -- | The queues used by this environment.
    queues :: Prelude.Maybe [Queue],
    -- | The @AutoScaling@ triggers in use by this environment.
    triggers :: Prelude.Maybe [Trigger]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentResourceDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingGroups', 'environmentResourceDescription_autoScalingGroups' - The @AutoScalingGroups@ used by this environment.
--
-- 'environmentName', 'environmentResourceDescription_environmentName' - The name of the environment.
--
-- 'instances', 'environmentResourceDescription_instances' - The Amazon EC2 instances used by this environment.
--
-- 'launchConfigurations', 'environmentResourceDescription_launchConfigurations' - The Auto Scaling launch configurations in use by this environment.
--
-- 'launchTemplates', 'environmentResourceDescription_launchTemplates' - The Amazon EC2 launch templates in use by this environment.
--
-- 'loadBalancers', 'environmentResourceDescription_loadBalancers' - The LoadBalancers in use by this environment.
--
-- 'queues', 'environmentResourceDescription_queues' - The queues used by this environment.
--
-- 'triggers', 'environmentResourceDescription_triggers' - The @AutoScaling@ triggers in use by this environment.
newEnvironmentResourceDescription ::
  EnvironmentResourceDescription
newEnvironmentResourceDescription =
  EnvironmentResourceDescription'
    { autoScalingGroups =
        Prelude.Nothing,
      environmentName = Prelude.Nothing,
      instances = Prelude.Nothing,
      launchConfigurations = Prelude.Nothing,
      launchTemplates = Prelude.Nothing,
      loadBalancers = Prelude.Nothing,
      queues = Prelude.Nothing,
      triggers = Prelude.Nothing
    }

-- | The @AutoScalingGroups@ used by this environment.
environmentResourceDescription_autoScalingGroups :: Lens.Lens' EnvironmentResourceDescription (Prelude.Maybe [AutoScalingGroup])
environmentResourceDescription_autoScalingGroups = Lens.lens (\EnvironmentResourceDescription' {autoScalingGroups} -> autoScalingGroups) (\s@EnvironmentResourceDescription' {} a -> s {autoScalingGroups = a} :: EnvironmentResourceDescription) Prelude.. Lens.mapping Lens.coerced

-- | The name of the environment.
environmentResourceDescription_environmentName :: Lens.Lens' EnvironmentResourceDescription (Prelude.Maybe Prelude.Text)
environmentResourceDescription_environmentName = Lens.lens (\EnvironmentResourceDescription' {environmentName} -> environmentName) (\s@EnvironmentResourceDescription' {} a -> s {environmentName = a} :: EnvironmentResourceDescription)

-- | The Amazon EC2 instances used by this environment.
environmentResourceDescription_instances :: Lens.Lens' EnvironmentResourceDescription (Prelude.Maybe [Instance])
environmentResourceDescription_instances = Lens.lens (\EnvironmentResourceDescription' {instances} -> instances) (\s@EnvironmentResourceDescription' {} a -> s {instances = a} :: EnvironmentResourceDescription) Prelude.. Lens.mapping Lens.coerced

-- | The Auto Scaling launch configurations in use by this environment.
environmentResourceDescription_launchConfigurations :: Lens.Lens' EnvironmentResourceDescription (Prelude.Maybe [LaunchConfiguration])
environmentResourceDescription_launchConfigurations = Lens.lens (\EnvironmentResourceDescription' {launchConfigurations} -> launchConfigurations) (\s@EnvironmentResourceDescription' {} a -> s {launchConfigurations = a} :: EnvironmentResourceDescription) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon EC2 launch templates in use by this environment.
environmentResourceDescription_launchTemplates :: Lens.Lens' EnvironmentResourceDescription (Prelude.Maybe [LaunchTemplate])
environmentResourceDescription_launchTemplates = Lens.lens (\EnvironmentResourceDescription' {launchTemplates} -> launchTemplates) (\s@EnvironmentResourceDescription' {} a -> s {launchTemplates = a} :: EnvironmentResourceDescription) Prelude.. Lens.mapping Lens.coerced

-- | The LoadBalancers in use by this environment.
environmentResourceDescription_loadBalancers :: Lens.Lens' EnvironmentResourceDescription (Prelude.Maybe [LoadBalancer])
environmentResourceDescription_loadBalancers = Lens.lens (\EnvironmentResourceDescription' {loadBalancers} -> loadBalancers) (\s@EnvironmentResourceDescription' {} a -> s {loadBalancers = a} :: EnvironmentResourceDescription) Prelude.. Lens.mapping Lens.coerced

-- | The queues used by this environment.
environmentResourceDescription_queues :: Lens.Lens' EnvironmentResourceDescription (Prelude.Maybe [Queue])
environmentResourceDescription_queues = Lens.lens (\EnvironmentResourceDescription' {queues} -> queues) (\s@EnvironmentResourceDescription' {} a -> s {queues = a} :: EnvironmentResourceDescription) Prelude.. Lens.mapping Lens.coerced

-- | The @AutoScaling@ triggers in use by this environment.
environmentResourceDescription_triggers :: Lens.Lens' EnvironmentResourceDescription (Prelude.Maybe [Trigger])
environmentResourceDescription_triggers = Lens.lens (\EnvironmentResourceDescription' {triggers} -> triggers) (\s@EnvironmentResourceDescription' {} a -> s {triggers = a} :: EnvironmentResourceDescription) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML EnvironmentResourceDescription where
  parseXML x =
    EnvironmentResourceDescription'
      Prelude.<$> ( x
                      Data..@? "AutoScalingGroups"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "EnvironmentName")
      Prelude.<*> ( x
                      Data..@? "Instances"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> ( x
                      Data..@? "LaunchConfigurations"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> ( x
                      Data..@? "LaunchTemplates"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> ( x
                      Data..@? "LoadBalancers"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> ( x
                      Data..@? "Queues"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> ( x
                      Data..@? "Triggers"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )

instance
  Prelude.Hashable
    EnvironmentResourceDescription
  where
  hashWithSalt
    _salt
    EnvironmentResourceDescription' {..} =
      _salt
        `Prelude.hashWithSalt` autoScalingGroups
        `Prelude.hashWithSalt` environmentName
        `Prelude.hashWithSalt` instances
        `Prelude.hashWithSalt` launchConfigurations
        `Prelude.hashWithSalt` launchTemplates
        `Prelude.hashWithSalt` loadBalancers
        `Prelude.hashWithSalt` queues
        `Prelude.hashWithSalt` triggers

instance
  Prelude.NFData
    EnvironmentResourceDescription
  where
  rnf EnvironmentResourceDescription' {..} =
    Prelude.rnf autoScalingGroups
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf instances
      `Prelude.seq` Prelude.rnf launchConfigurations
      `Prelude.seq` Prelude.rnf launchTemplates
      `Prelude.seq` Prelude.rnf loadBalancers
      `Prelude.seq` Prelude.rnf queues
      `Prelude.seq` Prelude.rnf triggers
