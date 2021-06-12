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
-- Module      : Network.AWS.EC2.Types.HostProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.HostProperties where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes the properties of a Dedicated Host.
--
-- /See:/ 'newHostProperties' smart constructor.
data HostProperties = HostProperties'
  { -- | The instance family supported by the Dedicated Host. For example, @m5@.
    instanceFamily :: Core.Maybe Core.Text,
    -- | The total number of vCPUs on the Dedicated Host.
    totalVCpus :: Core.Maybe Core.Int,
    -- | The instance type supported by the Dedicated Host. For example,
    -- @m5.large@. If the host supports multiple instance types, no
    -- __instanceType__ is returned.
    instanceType :: Core.Maybe Core.Text,
    -- | The number of cores on the Dedicated Host.
    cores :: Core.Maybe Core.Int,
    -- | The number of sockets on the Dedicated Host.
    sockets :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HostProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceFamily', 'hostProperties_instanceFamily' - The instance family supported by the Dedicated Host. For example, @m5@.
--
-- 'totalVCpus', 'hostProperties_totalVCpus' - The total number of vCPUs on the Dedicated Host.
--
-- 'instanceType', 'hostProperties_instanceType' - The instance type supported by the Dedicated Host. For example,
-- @m5.large@. If the host supports multiple instance types, no
-- __instanceType__ is returned.
--
-- 'cores', 'hostProperties_cores' - The number of cores on the Dedicated Host.
--
-- 'sockets', 'hostProperties_sockets' - The number of sockets on the Dedicated Host.
newHostProperties ::
  HostProperties
newHostProperties =
  HostProperties'
    { instanceFamily = Core.Nothing,
      totalVCpus = Core.Nothing,
      instanceType = Core.Nothing,
      cores = Core.Nothing,
      sockets = Core.Nothing
    }

-- | The instance family supported by the Dedicated Host. For example, @m5@.
hostProperties_instanceFamily :: Lens.Lens' HostProperties (Core.Maybe Core.Text)
hostProperties_instanceFamily = Lens.lens (\HostProperties' {instanceFamily} -> instanceFamily) (\s@HostProperties' {} a -> s {instanceFamily = a} :: HostProperties)

-- | The total number of vCPUs on the Dedicated Host.
hostProperties_totalVCpus :: Lens.Lens' HostProperties (Core.Maybe Core.Int)
hostProperties_totalVCpus = Lens.lens (\HostProperties' {totalVCpus} -> totalVCpus) (\s@HostProperties' {} a -> s {totalVCpus = a} :: HostProperties)

-- | The instance type supported by the Dedicated Host. For example,
-- @m5.large@. If the host supports multiple instance types, no
-- __instanceType__ is returned.
hostProperties_instanceType :: Lens.Lens' HostProperties (Core.Maybe Core.Text)
hostProperties_instanceType = Lens.lens (\HostProperties' {instanceType} -> instanceType) (\s@HostProperties' {} a -> s {instanceType = a} :: HostProperties)

-- | The number of cores on the Dedicated Host.
hostProperties_cores :: Lens.Lens' HostProperties (Core.Maybe Core.Int)
hostProperties_cores = Lens.lens (\HostProperties' {cores} -> cores) (\s@HostProperties' {} a -> s {cores = a} :: HostProperties)

-- | The number of sockets on the Dedicated Host.
hostProperties_sockets :: Lens.Lens' HostProperties (Core.Maybe Core.Int)
hostProperties_sockets = Lens.lens (\HostProperties' {sockets} -> sockets) (\s@HostProperties' {} a -> s {sockets = a} :: HostProperties)

instance Core.FromXML HostProperties where
  parseXML x =
    HostProperties'
      Core.<$> (x Core..@? "instanceFamily")
      Core.<*> (x Core..@? "totalVCpus")
      Core.<*> (x Core..@? "instanceType")
      Core.<*> (x Core..@? "cores")
      Core.<*> (x Core..@? "sockets")

instance Core.Hashable HostProperties

instance Core.NFData HostProperties
