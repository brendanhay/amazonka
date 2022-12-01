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
-- Module      : Amazonka.DirectConnect.Types.Location
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types.Location where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about an Direct Connect location.
--
-- /See:/ 'newLocation' smart constructor.
data Location = Location'
  { -- | The available port speeds for the location.
    availablePortSpeeds :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Web Services Region for the location.
    region :: Prelude.Maybe Prelude.Text,
    -- | The name of the service provider for the location.
    availableProviders :: Prelude.Maybe [Prelude.Text],
    -- | The name of the location. This includes the name of the colocation
    -- partner and the physical site of the building.
    locationName :: Prelude.Maybe Prelude.Text,
    -- | The code for the location.
    locationCode :: Prelude.Maybe Prelude.Text,
    -- | The available MAC Security (MACsec) port speeds for the location.
    availableMacSecPortSpeeds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Location' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availablePortSpeeds', 'location_availablePortSpeeds' - The available port speeds for the location.
--
-- 'region', 'location_region' - The Amazon Web Services Region for the location.
--
-- 'availableProviders', 'location_availableProviders' - The name of the service provider for the location.
--
-- 'locationName', 'location_locationName' - The name of the location. This includes the name of the colocation
-- partner and the physical site of the building.
--
-- 'locationCode', 'location_locationCode' - The code for the location.
--
-- 'availableMacSecPortSpeeds', 'location_availableMacSecPortSpeeds' - The available MAC Security (MACsec) port speeds for the location.
newLocation ::
  Location
newLocation =
  Location'
    { availablePortSpeeds = Prelude.Nothing,
      region = Prelude.Nothing,
      availableProviders = Prelude.Nothing,
      locationName = Prelude.Nothing,
      locationCode = Prelude.Nothing,
      availableMacSecPortSpeeds = Prelude.Nothing
    }

-- | The available port speeds for the location.
location_availablePortSpeeds :: Lens.Lens' Location (Prelude.Maybe [Prelude.Text])
location_availablePortSpeeds = Lens.lens (\Location' {availablePortSpeeds} -> availablePortSpeeds) (\s@Location' {} a -> s {availablePortSpeeds = a} :: Location) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services Region for the location.
location_region :: Lens.Lens' Location (Prelude.Maybe Prelude.Text)
location_region = Lens.lens (\Location' {region} -> region) (\s@Location' {} a -> s {region = a} :: Location)

-- | The name of the service provider for the location.
location_availableProviders :: Lens.Lens' Location (Prelude.Maybe [Prelude.Text])
location_availableProviders = Lens.lens (\Location' {availableProviders} -> availableProviders) (\s@Location' {} a -> s {availableProviders = a} :: Location) Prelude.. Lens.mapping Lens.coerced

-- | The name of the location. This includes the name of the colocation
-- partner and the physical site of the building.
location_locationName :: Lens.Lens' Location (Prelude.Maybe Prelude.Text)
location_locationName = Lens.lens (\Location' {locationName} -> locationName) (\s@Location' {} a -> s {locationName = a} :: Location)

-- | The code for the location.
location_locationCode :: Lens.Lens' Location (Prelude.Maybe Prelude.Text)
location_locationCode = Lens.lens (\Location' {locationCode} -> locationCode) (\s@Location' {} a -> s {locationCode = a} :: Location)

-- | The available MAC Security (MACsec) port speeds for the location.
location_availableMacSecPortSpeeds :: Lens.Lens' Location (Prelude.Maybe [Prelude.Text])
location_availableMacSecPortSpeeds = Lens.lens (\Location' {availableMacSecPortSpeeds} -> availableMacSecPortSpeeds) (\s@Location' {} a -> s {availableMacSecPortSpeeds = a} :: Location) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Location where
  parseJSON =
    Core.withObject
      "Location"
      ( \x ->
          Location'
            Prelude.<$> ( x Core..:? "availablePortSpeeds"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "region")
            Prelude.<*> ( x Core..:? "availableProviders"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "locationName")
            Prelude.<*> (x Core..:? "locationCode")
            Prelude.<*> ( x Core..:? "availableMacSecPortSpeeds"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Location where
  hashWithSalt _salt Location' {..} =
    _salt `Prelude.hashWithSalt` availablePortSpeeds
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` availableProviders
      `Prelude.hashWithSalt` locationName
      `Prelude.hashWithSalt` locationCode
      `Prelude.hashWithSalt` availableMacSecPortSpeeds

instance Prelude.NFData Location where
  rnf Location' {..} =
    Prelude.rnf availablePortSpeeds
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf availableProviders
      `Prelude.seq` Prelude.rnf locationName
      `Prelude.seq` Prelude.rnf locationCode
      `Prelude.seq` Prelude.rnf availableMacSecPortSpeeds
