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
-- Module      : Network.AWS.AlexaBusiness.Types.SmartHomeAppliance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.SmartHomeAppliance where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A smart home appliance that can connect to a central system. Any
-- domestic device can be a smart appliance.
--
-- /See:/ 'newSmartHomeAppliance' smart constructor.
data SmartHomeAppliance = SmartHomeAppliance'
  { -- | The friendly name of the smart home appliance.
    friendlyName :: Core.Maybe Core.Text,
    -- | The description of the smart home appliance.
    description :: Core.Maybe Core.Text,
    -- | The name of the manufacturer of the smart home appliance.
    manufacturerName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SmartHomeAppliance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'friendlyName', 'smartHomeAppliance_friendlyName' - The friendly name of the smart home appliance.
--
-- 'description', 'smartHomeAppliance_description' - The description of the smart home appliance.
--
-- 'manufacturerName', 'smartHomeAppliance_manufacturerName' - The name of the manufacturer of the smart home appliance.
newSmartHomeAppliance ::
  SmartHomeAppliance
newSmartHomeAppliance =
  SmartHomeAppliance'
    { friendlyName = Core.Nothing,
      description = Core.Nothing,
      manufacturerName = Core.Nothing
    }

-- | The friendly name of the smart home appliance.
smartHomeAppliance_friendlyName :: Lens.Lens' SmartHomeAppliance (Core.Maybe Core.Text)
smartHomeAppliance_friendlyName = Lens.lens (\SmartHomeAppliance' {friendlyName} -> friendlyName) (\s@SmartHomeAppliance' {} a -> s {friendlyName = a} :: SmartHomeAppliance)

-- | The description of the smart home appliance.
smartHomeAppliance_description :: Lens.Lens' SmartHomeAppliance (Core.Maybe Core.Text)
smartHomeAppliance_description = Lens.lens (\SmartHomeAppliance' {description} -> description) (\s@SmartHomeAppliance' {} a -> s {description = a} :: SmartHomeAppliance)

-- | The name of the manufacturer of the smart home appliance.
smartHomeAppliance_manufacturerName :: Lens.Lens' SmartHomeAppliance (Core.Maybe Core.Text)
smartHomeAppliance_manufacturerName = Lens.lens (\SmartHomeAppliance' {manufacturerName} -> manufacturerName) (\s@SmartHomeAppliance' {} a -> s {manufacturerName = a} :: SmartHomeAppliance)

instance Core.FromJSON SmartHomeAppliance where
  parseJSON =
    Core.withObject
      "SmartHomeAppliance"
      ( \x ->
          SmartHomeAppliance'
            Core.<$> (x Core..:? "FriendlyName")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "ManufacturerName")
      )

instance Core.Hashable SmartHomeAppliance

instance Core.NFData SmartHomeAppliance
