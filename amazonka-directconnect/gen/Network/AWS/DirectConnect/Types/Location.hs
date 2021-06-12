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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about an AWS Direct Connect location.
--
-- /See:/ 'newLocation' smart constructor.
data Location = Location'
  { -- | The available port speeds for the location.
    availablePortSpeeds :: Core.Maybe [Core.Text],
    -- | The name of the service provider for the location.
    availableProviders :: Core.Maybe [Core.Text],
    -- | The code for the location.
    locationCode :: Core.Maybe Core.Text,
    -- | The AWS Region for the location.
    region :: Core.Maybe Core.Text,
    -- | The name of the location. This includes the name of the colocation
    -- partner and the physical site of the building.
    locationName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { availablePortSpeeds = Core.Nothing,
      availableProviders = Core.Nothing,
      locationCode = Core.Nothing,
      region = Core.Nothing,
      locationName = Core.Nothing
    }

-- | The available port speeds for the location.
location_availablePortSpeeds :: Lens.Lens' Location (Core.Maybe [Core.Text])
location_availablePortSpeeds = Lens.lens (\Location' {availablePortSpeeds} -> availablePortSpeeds) (\s@Location' {} a -> s {availablePortSpeeds = a} :: Location) Core.. Lens.mapping Lens._Coerce

-- | The name of the service provider for the location.
location_availableProviders :: Lens.Lens' Location (Core.Maybe [Core.Text])
location_availableProviders = Lens.lens (\Location' {availableProviders} -> availableProviders) (\s@Location' {} a -> s {availableProviders = a} :: Location) Core.. Lens.mapping Lens._Coerce

-- | The code for the location.
location_locationCode :: Lens.Lens' Location (Core.Maybe Core.Text)
location_locationCode = Lens.lens (\Location' {locationCode} -> locationCode) (\s@Location' {} a -> s {locationCode = a} :: Location)

-- | The AWS Region for the location.
location_region :: Lens.Lens' Location (Core.Maybe Core.Text)
location_region = Lens.lens (\Location' {region} -> region) (\s@Location' {} a -> s {region = a} :: Location)

-- | The name of the location. This includes the name of the colocation
-- partner and the physical site of the building.
location_locationName :: Lens.Lens' Location (Core.Maybe Core.Text)
location_locationName = Lens.lens (\Location' {locationName} -> locationName) (\s@Location' {} a -> s {locationName = a} :: Location)

instance Core.FromJSON Location where
  parseJSON =
    Core.withObject
      "Location"
      ( \x ->
          Location'
            Core.<$> ( x Core..:? "availablePortSpeeds"
                         Core..!= Core.mempty
                     )
            Core.<*> ( x Core..:? "availableProviders"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "locationCode")
            Core.<*> (x Core..:? "region")
            Core.<*> (x Core..:? "locationName")
      )

instance Core.Hashable Location

instance Core.NFData Location
