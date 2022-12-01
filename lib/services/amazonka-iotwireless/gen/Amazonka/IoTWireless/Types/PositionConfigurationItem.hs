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
-- Module      : Amazonka.IoTWireless.Types.PositionConfigurationItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.PositionConfigurationItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types.PositionResourceType
import Amazonka.IoTWireless.Types.PositionSolverDetails
import qualified Amazonka.Prelude as Prelude

-- | The wrapper for a position configuration.
--
-- /See:/ 'newPositionConfigurationItem' smart constructor.
data PositionConfigurationItem = PositionConfigurationItem'
  { -- | The position data destination that describes the AWS IoT rule that
    -- processes the device\'s position data for use by AWS IoT Core for
    -- LoRaWAN.
    destination :: Prelude.Maybe Prelude.Text,
    -- | Resource type of the resource for the position configuration.
    resourceType :: Prelude.Maybe PositionResourceType,
    -- | Resource identifier for the position configuration.
    resourceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The details of the positioning solver object used to compute the
    -- location.
    solvers :: Prelude.Maybe PositionSolverDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PositionConfigurationItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'positionConfigurationItem_destination' - The position data destination that describes the AWS IoT rule that
-- processes the device\'s position data for use by AWS IoT Core for
-- LoRaWAN.
--
-- 'resourceType', 'positionConfigurationItem_resourceType' - Resource type of the resource for the position configuration.
--
-- 'resourceIdentifier', 'positionConfigurationItem_resourceIdentifier' - Resource identifier for the position configuration.
--
-- 'solvers', 'positionConfigurationItem_solvers' - The details of the positioning solver object used to compute the
-- location.
newPositionConfigurationItem ::
  PositionConfigurationItem
newPositionConfigurationItem =
  PositionConfigurationItem'
    { destination =
        Prelude.Nothing,
      resourceType = Prelude.Nothing,
      resourceIdentifier = Prelude.Nothing,
      solvers = Prelude.Nothing
    }

-- | The position data destination that describes the AWS IoT rule that
-- processes the device\'s position data for use by AWS IoT Core for
-- LoRaWAN.
positionConfigurationItem_destination :: Lens.Lens' PositionConfigurationItem (Prelude.Maybe Prelude.Text)
positionConfigurationItem_destination = Lens.lens (\PositionConfigurationItem' {destination} -> destination) (\s@PositionConfigurationItem' {} a -> s {destination = a} :: PositionConfigurationItem)

-- | Resource type of the resource for the position configuration.
positionConfigurationItem_resourceType :: Lens.Lens' PositionConfigurationItem (Prelude.Maybe PositionResourceType)
positionConfigurationItem_resourceType = Lens.lens (\PositionConfigurationItem' {resourceType} -> resourceType) (\s@PositionConfigurationItem' {} a -> s {resourceType = a} :: PositionConfigurationItem)

-- | Resource identifier for the position configuration.
positionConfigurationItem_resourceIdentifier :: Lens.Lens' PositionConfigurationItem (Prelude.Maybe Prelude.Text)
positionConfigurationItem_resourceIdentifier = Lens.lens (\PositionConfigurationItem' {resourceIdentifier} -> resourceIdentifier) (\s@PositionConfigurationItem' {} a -> s {resourceIdentifier = a} :: PositionConfigurationItem)

-- | The details of the positioning solver object used to compute the
-- location.
positionConfigurationItem_solvers :: Lens.Lens' PositionConfigurationItem (Prelude.Maybe PositionSolverDetails)
positionConfigurationItem_solvers = Lens.lens (\PositionConfigurationItem' {solvers} -> solvers) (\s@PositionConfigurationItem' {} a -> s {solvers = a} :: PositionConfigurationItem)

instance Core.FromJSON PositionConfigurationItem where
  parseJSON =
    Core.withObject
      "PositionConfigurationItem"
      ( \x ->
          PositionConfigurationItem'
            Prelude.<$> (x Core..:? "Destination")
            Prelude.<*> (x Core..:? "ResourceType")
            Prelude.<*> (x Core..:? "ResourceIdentifier")
            Prelude.<*> (x Core..:? "Solvers")
      )

instance Prelude.Hashable PositionConfigurationItem where
  hashWithSalt _salt PositionConfigurationItem' {..} =
    _salt `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resourceIdentifier
      `Prelude.hashWithSalt` solvers

instance Prelude.NFData PositionConfigurationItem where
  rnf PositionConfigurationItem' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf resourceIdentifier
      `Prelude.seq` Prelude.rnf solvers
