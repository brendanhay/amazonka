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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types.Location where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an Direct Connect location.
--
-- /See:/ 'newLocation' smart constructor.
data Location = Location'
  { -- | The available MAC Security (MACsec) port speeds for the location.
    availableMacSecPortSpeeds :: Prelude.Maybe [Prelude.Text],
    -- | The available port speeds for the location.
    availablePortSpeeds :: Prelude.Maybe [Prelude.Text],
    -- | The name of the service provider for the location.
    availableProviders :: Prelude.Maybe [Prelude.Text],
    -- | The code for the location.
    locationCode :: Prelude.Maybe Prelude.Text,
    -- | The name of the location. This includes the name of the colocation
    -- partner and the physical site of the building.
    locationName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region for the location.
    region :: Prelude.Maybe Prelude.Text
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
-- 'availableMacSecPortSpeeds', 'location_availableMacSecPortSpeeds' - The available MAC Security (MACsec) port speeds for the location.
--
-- 'availablePortSpeeds', 'location_availablePortSpeeds' - The available port speeds for the location.
--
-- 'availableProviders', 'location_availableProviders' - The name of the service provider for the location.
--
-- 'locationCode', 'location_locationCode' - The code for the location.
--
-- 'locationName', 'location_locationName' - The name of the location. This includes the name of the colocation
-- partner and the physical site of the building.
--
-- 'region', 'location_region' - The Amazon Web Services Region for the location.
newLocation ::
  Location
newLocation =
  Location'
    { availableMacSecPortSpeeds =
        Prelude.Nothing,
      availablePortSpeeds = Prelude.Nothing,
      availableProviders = Prelude.Nothing,
      locationCode = Prelude.Nothing,
      locationName = Prelude.Nothing,
      region = Prelude.Nothing
    }

-- | The available MAC Security (MACsec) port speeds for the location.
location_availableMacSecPortSpeeds :: Lens.Lens' Location (Prelude.Maybe [Prelude.Text])
location_availableMacSecPortSpeeds = Lens.lens (\Location' {availableMacSecPortSpeeds} -> availableMacSecPortSpeeds) (\s@Location' {} a -> s {availableMacSecPortSpeeds = a} :: Location) Prelude.. Lens.mapping Lens.coerced

-- | The available port speeds for the location.
location_availablePortSpeeds :: Lens.Lens' Location (Prelude.Maybe [Prelude.Text])
location_availablePortSpeeds = Lens.lens (\Location' {availablePortSpeeds} -> availablePortSpeeds) (\s@Location' {} a -> s {availablePortSpeeds = a} :: Location) Prelude.. Lens.mapping Lens.coerced

-- | The name of the service provider for the location.
location_availableProviders :: Lens.Lens' Location (Prelude.Maybe [Prelude.Text])
location_availableProviders = Lens.lens (\Location' {availableProviders} -> availableProviders) (\s@Location' {} a -> s {availableProviders = a} :: Location) Prelude.. Lens.mapping Lens.coerced

-- | The code for the location.
location_locationCode :: Lens.Lens' Location (Prelude.Maybe Prelude.Text)
location_locationCode = Lens.lens (\Location' {locationCode} -> locationCode) (\s@Location' {} a -> s {locationCode = a} :: Location)

-- | The name of the location. This includes the name of the colocation
-- partner and the physical site of the building.
location_locationName :: Lens.Lens' Location (Prelude.Maybe Prelude.Text)
location_locationName = Lens.lens (\Location' {locationName} -> locationName) (\s@Location' {} a -> s {locationName = a} :: Location)

-- | The Amazon Web Services Region for the location.
location_region :: Lens.Lens' Location (Prelude.Maybe Prelude.Text)
location_region = Lens.lens (\Location' {region} -> region) (\s@Location' {} a -> s {region = a} :: Location)

instance Data.FromJSON Location where
  parseJSON =
    Data.withObject
      "Location"
      ( \x ->
          Location'
            Prelude.<$> ( x
                            Data..:? "availableMacSecPortSpeeds"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "availablePortSpeeds"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "availableProviders"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "locationCode")
            Prelude.<*> (x Data..:? "locationName")
            Prelude.<*> (x Data..:? "region")
      )

instance Prelude.Hashable Location where
  hashWithSalt _salt Location' {..} =
    _salt
      `Prelude.hashWithSalt` availableMacSecPortSpeeds
      `Prelude.hashWithSalt` availablePortSpeeds
      `Prelude.hashWithSalt` availableProviders
      `Prelude.hashWithSalt` locationCode
      `Prelude.hashWithSalt` locationName
      `Prelude.hashWithSalt` region

instance Prelude.NFData Location where
  rnf Location' {..} =
    Prelude.rnf availableMacSecPortSpeeds
      `Prelude.seq` Prelude.rnf availablePortSpeeds
      `Prelude.seq` Prelude.rnf availableProviders
      `Prelude.seq` Prelude.rnf locationCode
      `Prelude.seq` Prelude.rnf locationName
      `Prelude.seq` Prelude.rnf region
