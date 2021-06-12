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
-- Module      : Network.AWS.EC2.Types.AvailableCapacity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AvailableCapacity where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceCapacity
import qualified Network.AWS.Lens as Lens

-- | The capacity information for instances that can be launched onto the
-- Dedicated Host.
--
-- /See:/ 'newAvailableCapacity' smart constructor.
data AvailableCapacity = AvailableCapacity'
  { -- | The number of instances that can be launched onto the Dedicated Host
    -- depending on the host\'s available capacity. For Dedicated Hosts that
    -- support multiple instance types, this parameter represents the number of
    -- instances for each instance size that is supported on the host.
    availableInstanceCapacity :: Core.Maybe [InstanceCapacity],
    -- | The number of vCPUs available for launching instances onto the Dedicated
    -- Host.
    availableVCpus :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AvailableCapacity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availableInstanceCapacity', 'availableCapacity_availableInstanceCapacity' - The number of instances that can be launched onto the Dedicated Host
-- depending on the host\'s available capacity. For Dedicated Hosts that
-- support multiple instance types, this parameter represents the number of
-- instances for each instance size that is supported on the host.
--
-- 'availableVCpus', 'availableCapacity_availableVCpus' - The number of vCPUs available for launching instances onto the Dedicated
-- Host.
newAvailableCapacity ::
  AvailableCapacity
newAvailableCapacity =
  AvailableCapacity'
    { availableInstanceCapacity =
        Core.Nothing,
      availableVCpus = Core.Nothing
    }

-- | The number of instances that can be launched onto the Dedicated Host
-- depending on the host\'s available capacity. For Dedicated Hosts that
-- support multiple instance types, this parameter represents the number of
-- instances for each instance size that is supported on the host.
availableCapacity_availableInstanceCapacity :: Lens.Lens' AvailableCapacity (Core.Maybe [InstanceCapacity])
availableCapacity_availableInstanceCapacity = Lens.lens (\AvailableCapacity' {availableInstanceCapacity} -> availableInstanceCapacity) (\s@AvailableCapacity' {} a -> s {availableInstanceCapacity = a} :: AvailableCapacity) Core.. Lens.mapping Lens._Coerce

-- | The number of vCPUs available for launching instances onto the Dedicated
-- Host.
availableCapacity_availableVCpus :: Lens.Lens' AvailableCapacity (Core.Maybe Core.Int)
availableCapacity_availableVCpus = Lens.lens (\AvailableCapacity' {availableVCpus} -> availableVCpus) (\s@AvailableCapacity' {} a -> s {availableVCpus = a} :: AvailableCapacity)

instance Core.FromXML AvailableCapacity where
  parseXML x =
    AvailableCapacity'
      Core.<$> ( x Core..@? "availableInstanceCapacity"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "availableVCpus")

instance Core.Hashable AvailableCapacity

instance Core.NFData AvailableCapacity
