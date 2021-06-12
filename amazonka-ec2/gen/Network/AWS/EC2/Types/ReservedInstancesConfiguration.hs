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
-- Module      : Network.AWS.EC2.Types.ReservedInstancesConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservedInstancesConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.Scope
import qualified Network.AWS.Lens as Lens

-- | Describes the configuration settings for the modified Reserved
-- Instances.
--
-- /See:/ 'newReservedInstancesConfiguration' smart constructor.
data ReservedInstancesConfiguration = ReservedInstancesConfiguration'
  { -- | The network platform of the modified Reserved Instances, which is either
    -- EC2-Classic or EC2-VPC.
    platform :: Core.Maybe Core.Text,
    -- | The instance type for the modified Reserved Instances.
    instanceType :: Core.Maybe InstanceType,
    -- | Whether the Reserved Instance is applied to instances in a Region or
    -- instances in a specific Availability Zone.
    scope :: Core.Maybe Scope,
    -- | The Availability Zone for the modified Reserved Instances.
    availabilityZone :: Core.Maybe Core.Text,
    -- | The number of modified Reserved Instances.
    --
    -- This is a required field for a request.
    instanceCount :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReservedInstancesConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platform', 'reservedInstancesConfiguration_platform' - The network platform of the modified Reserved Instances, which is either
-- EC2-Classic or EC2-VPC.
--
-- 'instanceType', 'reservedInstancesConfiguration_instanceType' - The instance type for the modified Reserved Instances.
--
-- 'scope', 'reservedInstancesConfiguration_scope' - Whether the Reserved Instance is applied to instances in a Region or
-- instances in a specific Availability Zone.
--
-- 'availabilityZone', 'reservedInstancesConfiguration_availabilityZone' - The Availability Zone for the modified Reserved Instances.
--
-- 'instanceCount', 'reservedInstancesConfiguration_instanceCount' - The number of modified Reserved Instances.
--
-- This is a required field for a request.
newReservedInstancesConfiguration ::
  ReservedInstancesConfiguration
newReservedInstancesConfiguration =
  ReservedInstancesConfiguration'
    { platform =
        Core.Nothing,
      instanceType = Core.Nothing,
      scope = Core.Nothing,
      availabilityZone = Core.Nothing,
      instanceCount = Core.Nothing
    }

-- | The network platform of the modified Reserved Instances, which is either
-- EC2-Classic or EC2-VPC.
reservedInstancesConfiguration_platform :: Lens.Lens' ReservedInstancesConfiguration (Core.Maybe Core.Text)
reservedInstancesConfiguration_platform = Lens.lens (\ReservedInstancesConfiguration' {platform} -> platform) (\s@ReservedInstancesConfiguration' {} a -> s {platform = a} :: ReservedInstancesConfiguration)

-- | The instance type for the modified Reserved Instances.
reservedInstancesConfiguration_instanceType :: Lens.Lens' ReservedInstancesConfiguration (Core.Maybe InstanceType)
reservedInstancesConfiguration_instanceType = Lens.lens (\ReservedInstancesConfiguration' {instanceType} -> instanceType) (\s@ReservedInstancesConfiguration' {} a -> s {instanceType = a} :: ReservedInstancesConfiguration)

-- | Whether the Reserved Instance is applied to instances in a Region or
-- instances in a specific Availability Zone.
reservedInstancesConfiguration_scope :: Lens.Lens' ReservedInstancesConfiguration (Core.Maybe Scope)
reservedInstancesConfiguration_scope = Lens.lens (\ReservedInstancesConfiguration' {scope} -> scope) (\s@ReservedInstancesConfiguration' {} a -> s {scope = a} :: ReservedInstancesConfiguration)

-- | The Availability Zone for the modified Reserved Instances.
reservedInstancesConfiguration_availabilityZone :: Lens.Lens' ReservedInstancesConfiguration (Core.Maybe Core.Text)
reservedInstancesConfiguration_availabilityZone = Lens.lens (\ReservedInstancesConfiguration' {availabilityZone} -> availabilityZone) (\s@ReservedInstancesConfiguration' {} a -> s {availabilityZone = a} :: ReservedInstancesConfiguration)

-- | The number of modified Reserved Instances.
--
-- This is a required field for a request.
reservedInstancesConfiguration_instanceCount :: Lens.Lens' ReservedInstancesConfiguration (Core.Maybe Core.Int)
reservedInstancesConfiguration_instanceCount = Lens.lens (\ReservedInstancesConfiguration' {instanceCount} -> instanceCount) (\s@ReservedInstancesConfiguration' {} a -> s {instanceCount = a} :: ReservedInstancesConfiguration)

instance Core.FromXML ReservedInstancesConfiguration where
  parseXML x =
    ReservedInstancesConfiguration'
      Core.<$> (x Core..@? "platform")
      Core.<*> (x Core..@? "instanceType")
      Core.<*> (x Core..@? "scope")
      Core.<*> (x Core..@? "availabilityZone")
      Core.<*> (x Core..@? "instanceCount")

instance Core.Hashable ReservedInstancesConfiguration

instance Core.NFData ReservedInstancesConfiguration

instance Core.ToQuery ReservedInstancesConfiguration where
  toQuery ReservedInstancesConfiguration' {..} =
    Core.mconcat
      [ "Platform" Core.=: platform,
        "InstanceType" Core.=: instanceType,
        "Scope" Core.=: scope,
        "AvailabilityZone" Core.=: availabilityZone,
        "InstanceCount" Core.=: instanceCount
      ]
