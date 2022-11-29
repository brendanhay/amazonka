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
-- Module      : Amazonka.Outposts.Types.AssetLocation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.AssetLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the position of the asset in a rack.
--
-- /See:/ 'newAssetLocation' smart constructor.
data AssetLocation = AssetLocation'
  { -- | The position of an asset in a rack measured in rack units.
    rackElevation :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rackElevation', 'assetLocation_rackElevation' - The position of an asset in a rack measured in rack units.
newAssetLocation ::
  AssetLocation
newAssetLocation =
  AssetLocation' {rackElevation = Prelude.Nothing}

-- | The position of an asset in a rack measured in rack units.
assetLocation_rackElevation :: Lens.Lens' AssetLocation (Prelude.Maybe Prelude.Double)
assetLocation_rackElevation = Lens.lens (\AssetLocation' {rackElevation} -> rackElevation) (\s@AssetLocation' {} a -> s {rackElevation = a} :: AssetLocation)

instance Core.FromJSON AssetLocation where
  parseJSON =
    Core.withObject
      "AssetLocation"
      ( \x ->
          AssetLocation'
            Prelude.<$> (x Core..:? "RackElevation")
      )

instance Prelude.Hashable AssetLocation where
  hashWithSalt _salt AssetLocation' {..} =
    _salt `Prelude.hashWithSalt` rackElevation

instance Prelude.NFData AssetLocation where
  rnf AssetLocation' {..} = Prelude.rnf rackElevation
