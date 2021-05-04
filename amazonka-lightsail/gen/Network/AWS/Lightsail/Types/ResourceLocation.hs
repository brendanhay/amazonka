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
-- Module      : Network.AWS.Lightsail.Types.ResourceLocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ResourceLocation where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.RegionName
import qualified Network.AWS.Prelude as Prelude

-- | Describes the resource location.
--
-- /See:/ 'newResourceLocation' smart constructor.
data ResourceLocation = ResourceLocation'
  { -- | The AWS Region name.
    regionName :: Prelude.Maybe RegionName,
    -- | The Availability Zone. Follows the format @us-east-2a@ (case-sensitive).
    availabilityZone :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResourceLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regionName', 'resourceLocation_regionName' - The AWS Region name.
--
-- 'availabilityZone', 'resourceLocation_availabilityZone' - The Availability Zone. Follows the format @us-east-2a@ (case-sensitive).
newResourceLocation ::
  ResourceLocation
newResourceLocation =
  ResourceLocation'
    { regionName = Prelude.Nothing,
      availabilityZone = Prelude.Nothing
    }

-- | The AWS Region name.
resourceLocation_regionName :: Lens.Lens' ResourceLocation (Prelude.Maybe RegionName)
resourceLocation_regionName = Lens.lens (\ResourceLocation' {regionName} -> regionName) (\s@ResourceLocation' {} a -> s {regionName = a} :: ResourceLocation)

-- | The Availability Zone. Follows the format @us-east-2a@ (case-sensitive).
resourceLocation_availabilityZone :: Lens.Lens' ResourceLocation (Prelude.Maybe Prelude.Text)
resourceLocation_availabilityZone = Lens.lens (\ResourceLocation' {availabilityZone} -> availabilityZone) (\s@ResourceLocation' {} a -> s {availabilityZone = a} :: ResourceLocation)

instance Prelude.FromJSON ResourceLocation where
  parseJSON =
    Prelude.withObject
      "ResourceLocation"
      ( \x ->
          ResourceLocation'
            Prelude.<$> (x Prelude..:? "regionName")
            Prelude.<*> (x Prelude..:? "availabilityZone")
      )

instance Prelude.Hashable ResourceLocation

instance Prelude.NFData ResourceLocation
