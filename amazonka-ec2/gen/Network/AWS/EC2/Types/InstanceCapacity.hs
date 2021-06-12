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
-- Module      : Network.AWS.EC2.Types.InstanceCapacity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceCapacity where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Information about the number of instances that can be launched onto the
-- Dedicated Host.
--
-- /See:/ 'newInstanceCapacity' smart constructor.
data InstanceCapacity = InstanceCapacity'
  { -- | The instance type supported by the Dedicated Host.
    instanceType :: Core.Maybe Core.Text,
    -- | The number of instances that can be launched onto the Dedicated Host
    -- based on the host\'s available capacity.
    availableCapacity :: Core.Maybe Core.Int,
    -- | The total number of instances that can be launched onto the Dedicated
    -- Host if there are no instances running on it.
    totalCapacity :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceCapacity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceType', 'instanceCapacity_instanceType' - The instance type supported by the Dedicated Host.
--
-- 'availableCapacity', 'instanceCapacity_availableCapacity' - The number of instances that can be launched onto the Dedicated Host
-- based on the host\'s available capacity.
--
-- 'totalCapacity', 'instanceCapacity_totalCapacity' - The total number of instances that can be launched onto the Dedicated
-- Host if there are no instances running on it.
newInstanceCapacity ::
  InstanceCapacity
newInstanceCapacity =
  InstanceCapacity'
    { instanceType = Core.Nothing,
      availableCapacity = Core.Nothing,
      totalCapacity = Core.Nothing
    }

-- | The instance type supported by the Dedicated Host.
instanceCapacity_instanceType :: Lens.Lens' InstanceCapacity (Core.Maybe Core.Text)
instanceCapacity_instanceType = Lens.lens (\InstanceCapacity' {instanceType} -> instanceType) (\s@InstanceCapacity' {} a -> s {instanceType = a} :: InstanceCapacity)

-- | The number of instances that can be launched onto the Dedicated Host
-- based on the host\'s available capacity.
instanceCapacity_availableCapacity :: Lens.Lens' InstanceCapacity (Core.Maybe Core.Int)
instanceCapacity_availableCapacity = Lens.lens (\InstanceCapacity' {availableCapacity} -> availableCapacity) (\s@InstanceCapacity' {} a -> s {availableCapacity = a} :: InstanceCapacity)

-- | The total number of instances that can be launched onto the Dedicated
-- Host if there are no instances running on it.
instanceCapacity_totalCapacity :: Lens.Lens' InstanceCapacity (Core.Maybe Core.Int)
instanceCapacity_totalCapacity = Lens.lens (\InstanceCapacity' {totalCapacity} -> totalCapacity) (\s@InstanceCapacity' {} a -> s {totalCapacity = a} :: InstanceCapacity)

instance Core.FromXML InstanceCapacity where
  parseXML x =
    InstanceCapacity'
      Core.<$> (x Core..@? "instanceType")
      Core.<*> (x Core..@? "availableCapacity")
      Core.<*> (x Core..@? "totalCapacity")

instance Core.Hashable InstanceCapacity

instance Core.NFData InstanceCapacity
