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
-- Module      : Amazonka.AlexaBusiness.Types.SmartHomeAppliance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.SmartHomeAppliance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A smart home appliance that can connect to a central system. Any
-- domestic device can be a smart appliance.
--
-- /See:/ 'newSmartHomeAppliance' smart constructor.
data SmartHomeAppliance = SmartHomeAppliance'
  { -- | The description of the smart home appliance.
    description :: Prelude.Maybe Prelude.Text,
    -- | The friendly name of the smart home appliance.
    friendlyName :: Prelude.Maybe Prelude.Text,
    -- | The name of the manufacturer of the smart home appliance.
    manufacturerName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SmartHomeAppliance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'smartHomeAppliance_description' - The description of the smart home appliance.
--
-- 'friendlyName', 'smartHomeAppliance_friendlyName' - The friendly name of the smart home appliance.
--
-- 'manufacturerName', 'smartHomeAppliance_manufacturerName' - The name of the manufacturer of the smart home appliance.
newSmartHomeAppliance ::
  SmartHomeAppliance
newSmartHomeAppliance =
  SmartHomeAppliance'
    { description = Prelude.Nothing,
      friendlyName = Prelude.Nothing,
      manufacturerName = Prelude.Nothing
    }

-- | The description of the smart home appliance.
smartHomeAppliance_description :: Lens.Lens' SmartHomeAppliance (Prelude.Maybe Prelude.Text)
smartHomeAppliance_description = Lens.lens (\SmartHomeAppliance' {description} -> description) (\s@SmartHomeAppliance' {} a -> s {description = a} :: SmartHomeAppliance)

-- | The friendly name of the smart home appliance.
smartHomeAppliance_friendlyName :: Lens.Lens' SmartHomeAppliance (Prelude.Maybe Prelude.Text)
smartHomeAppliance_friendlyName = Lens.lens (\SmartHomeAppliance' {friendlyName} -> friendlyName) (\s@SmartHomeAppliance' {} a -> s {friendlyName = a} :: SmartHomeAppliance)

-- | The name of the manufacturer of the smart home appliance.
smartHomeAppliance_manufacturerName :: Lens.Lens' SmartHomeAppliance (Prelude.Maybe Prelude.Text)
smartHomeAppliance_manufacturerName = Lens.lens (\SmartHomeAppliance' {manufacturerName} -> manufacturerName) (\s@SmartHomeAppliance' {} a -> s {manufacturerName = a} :: SmartHomeAppliance)

instance Data.FromJSON SmartHomeAppliance where
  parseJSON =
    Data.withObject
      "SmartHomeAppliance"
      ( \x ->
          SmartHomeAppliance'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "FriendlyName")
            Prelude.<*> (x Data..:? "ManufacturerName")
      )

instance Prelude.Hashable SmartHomeAppliance where
  hashWithSalt _salt SmartHomeAppliance' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` friendlyName
      `Prelude.hashWithSalt` manufacturerName

instance Prelude.NFData SmartHomeAppliance where
  rnf SmartHomeAppliance' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf friendlyName
      `Prelude.seq` Prelude.rnf manufacturerName
