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
-- Module      : Network.AWS.DeviceFarm.Types.DeviceMinutes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.DeviceMinutes where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents the total (metered or unmetered) minutes used by the resource
-- to run tests. Contains the sum of minutes consumed by all children.
--
-- /See:/ 'newDeviceMinutes' smart constructor.
data DeviceMinutes = DeviceMinutes'
  { -- | When specified, represents only the sum of unmetered minutes used by the
    -- resource to run tests.
    unmetered :: Core.Maybe Core.Double,
    -- | When specified, represents only the sum of metered minutes used by the
    -- resource to run tests.
    metered :: Core.Maybe Core.Double,
    -- | When specified, represents the total minutes used by the resource to run
    -- tests.
    total :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeviceMinutes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unmetered', 'deviceMinutes_unmetered' - When specified, represents only the sum of unmetered minutes used by the
-- resource to run tests.
--
-- 'metered', 'deviceMinutes_metered' - When specified, represents only the sum of metered minutes used by the
-- resource to run tests.
--
-- 'total', 'deviceMinutes_total' - When specified, represents the total minutes used by the resource to run
-- tests.
newDeviceMinutes ::
  DeviceMinutes
newDeviceMinutes =
  DeviceMinutes'
    { unmetered = Core.Nothing,
      metered = Core.Nothing,
      total = Core.Nothing
    }

-- | When specified, represents only the sum of unmetered minutes used by the
-- resource to run tests.
deviceMinutes_unmetered :: Lens.Lens' DeviceMinutes (Core.Maybe Core.Double)
deviceMinutes_unmetered = Lens.lens (\DeviceMinutes' {unmetered} -> unmetered) (\s@DeviceMinutes' {} a -> s {unmetered = a} :: DeviceMinutes)

-- | When specified, represents only the sum of metered minutes used by the
-- resource to run tests.
deviceMinutes_metered :: Lens.Lens' DeviceMinutes (Core.Maybe Core.Double)
deviceMinutes_metered = Lens.lens (\DeviceMinutes' {metered} -> metered) (\s@DeviceMinutes' {} a -> s {metered = a} :: DeviceMinutes)

-- | When specified, represents the total minutes used by the resource to run
-- tests.
deviceMinutes_total :: Lens.Lens' DeviceMinutes (Core.Maybe Core.Double)
deviceMinutes_total = Lens.lens (\DeviceMinutes' {total} -> total) (\s@DeviceMinutes' {} a -> s {total = a} :: DeviceMinutes)

instance Core.FromJSON DeviceMinutes where
  parseJSON =
    Core.withObject
      "DeviceMinutes"
      ( \x ->
          DeviceMinutes'
            Core.<$> (x Core..:? "unmetered")
            Core.<*> (x Core..:? "metered")
            Core.<*> (x Core..:? "total")
      )

instance Core.Hashable DeviceMinutes

instance Core.NFData DeviceMinutes
