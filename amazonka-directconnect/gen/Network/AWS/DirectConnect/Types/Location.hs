{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DirectConnect.Types.Location
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.Location where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an AWS Direct Connect location.
--
-- /See:/ 'newLocation' smart constructor.
data Location = Location'
  { -- | The available port speeds for the location.
    availablePortSpeeds :: Prelude.Maybe [Prelude.Text],
    -- | The name of the service provider for the location.
    availableProviders :: Prelude.Maybe [Prelude.Text],
    -- | The code for the location.
    locationCode :: Prelude.Maybe Prelude.Text,
    -- | The AWS Region for the location.
    region :: Prelude.Maybe Prelude.Text,
    -- | The name of the location. This includes the name of the colocation
    -- partner and the physical site of the building.
    locationName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'availableProviders', 'location_availableProviders' - The name of the service provider for the location.
--
-- 'locationCode', 'location_locationCode' - The code for the location.
--
-- 'region', 'location_region' - The AWS Region for the location.
--
-- 'locationName', 'location_locationName' - The name of the location. This includes the name of the colocation
-- partner and the physical site of the building.
newLocation ::
  Location
newLocation =
  Location'
    { availablePortSpeeds = Prelude.Nothing,
      availableProviders = Prelude.Nothing,
      locationCode = Prelude.Nothing,
      region = Prelude.Nothing,
      locationName = Prelude.Nothing
    }

-- | The available port speeds for the location.
location_availablePortSpeeds :: Lens.Lens' Location (Prelude.Maybe [Prelude.Text])
location_availablePortSpeeds = Lens.lens (\Location' {availablePortSpeeds} -> availablePortSpeeds) (\s@Location' {} a -> s {availablePortSpeeds = a} :: Location) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the service provider for the location.
location_availableProviders :: Lens.Lens' Location (Prelude.Maybe [Prelude.Text])
location_availableProviders = Lens.lens (\Location' {availableProviders} -> availableProviders) (\s@Location' {} a -> s {availableProviders = a} :: Location) Prelude.. Lens.mapping Prelude._Coerce

-- | The code for the location.
location_locationCode :: Lens.Lens' Location (Prelude.Maybe Prelude.Text)
location_locationCode = Lens.lens (\Location' {locationCode} -> locationCode) (\s@Location' {} a -> s {locationCode = a} :: Location)

-- | The AWS Region for the location.
location_region :: Lens.Lens' Location (Prelude.Maybe Prelude.Text)
location_region = Lens.lens (\Location' {region} -> region) (\s@Location' {} a -> s {region = a} :: Location)

-- | The name of the location. This includes the name of the colocation
-- partner and the physical site of the building.
location_locationName :: Lens.Lens' Location (Prelude.Maybe Prelude.Text)
location_locationName = Lens.lens (\Location' {locationName} -> locationName) (\s@Location' {} a -> s {locationName = a} :: Location)

instance Prelude.FromJSON Location where
  parseJSON =
    Prelude.withObject
      "Location"
      ( \x ->
          Location'
            Prelude.<$> ( x Prelude..:? "availablePortSpeeds"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "availableProviders"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "locationCode")
            Prelude.<*> (x Prelude..:? "region")
            Prelude.<*> (x Prelude..:? "locationName")
      )

instance Prelude.Hashable Location

instance Prelude.NFData Location
