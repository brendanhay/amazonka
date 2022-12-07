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
-- Module      : Amazonka.Lightsail.Types.ResourceLocation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.ResourceLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.RegionName
import qualified Amazonka.Prelude as Prelude

-- | Describes the resource location.
--
-- /See:/ 'newResourceLocation' smart constructor.
data ResourceLocation = ResourceLocation'
  { -- | The Availability Zone. Follows the format @us-east-2a@ (case-sensitive).
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region name.
    regionName :: Prelude.Maybe RegionName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZone', 'resourceLocation_availabilityZone' - The Availability Zone. Follows the format @us-east-2a@ (case-sensitive).
--
-- 'regionName', 'resourceLocation_regionName' - The Amazon Web Services Region name.
newResourceLocation ::
  ResourceLocation
newResourceLocation =
  ResourceLocation'
    { availabilityZone =
        Prelude.Nothing,
      regionName = Prelude.Nothing
    }

-- | The Availability Zone. Follows the format @us-east-2a@ (case-sensitive).
resourceLocation_availabilityZone :: Lens.Lens' ResourceLocation (Prelude.Maybe Prelude.Text)
resourceLocation_availabilityZone = Lens.lens (\ResourceLocation' {availabilityZone} -> availabilityZone) (\s@ResourceLocation' {} a -> s {availabilityZone = a} :: ResourceLocation)

-- | The Amazon Web Services Region name.
resourceLocation_regionName :: Lens.Lens' ResourceLocation (Prelude.Maybe RegionName)
resourceLocation_regionName = Lens.lens (\ResourceLocation' {regionName} -> regionName) (\s@ResourceLocation' {} a -> s {regionName = a} :: ResourceLocation)

instance Data.FromJSON ResourceLocation where
  parseJSON =
    Data.withObject
      "ResourceLocation"
      ( \x ->
          ResourceLocation'
            Prelude.<$> (x Data..:? "availabilityZone")
            Prelude.<*> (x Data..:? "regionName")
      )

instance Prelude.Hashable ResourceLocation where
  hashWithSalt _salt ResourceLocation' {..} =
    _salt `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` regionName

instance Prelude.NFData ResourceLocation where
  rnf ResourceLocation' {..} =
    Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf regionName
