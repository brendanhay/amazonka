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
-- Module      : Amazonka.Outposts.Types.AssetInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.AssetInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Outposts.Types.AssetLocation
import Amazonka.Outposts.Types.AssetType
import Amazonka.Outposts.Types.ComputeAttributes
import qualified Amazonka.Prelude as Prelude

-- | Information about hardware assets.
--
-- /See:/ 'newAssetInfo' smart constructor.
data AssetInfo = AssetInfo'
  { -- | The position of an asset in a rack.
    assetLocation :: Prelude.Maybe AssetLocation,
    -- | The ID of the asset.
    assetId :: Prelude.Maybe Prelude.Text,
    -- | Information about compute hardware assets.
    computeAttributes :: Prelude.Maybe ComputeAttributes,
    -- | The type of the asset.
    assetType :: Prelude.Maybe AssetType,
    -- | The rack ID of the asset.
    rackId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assetLocation', 'assetInfo_assetLocation' - The position of an asset in a rack.
--
-- 'assetId', 'assetInfo_assetId' - The ID of the asset.
--
-- 'computeAttributes', 'assetInfo_computeAttributes' - Information about compute hardware assets.
--
-- 'assetType', 'assetInfo_assetType' - The type of the asset.
--
-- 'rackId', 'assetInfo_rackId' - The rack ID of the asset.
newAssetInfo ::
  AssetInfo
newAssetInfo =
  AssetInfo'
    { assetLocation = Prelude.Nothing,
      assetId = Prelude.Nothing,
      computeAttributes = Prelude.Nothing,
      assetType = Prelude.Nothing,
      rackId = Prelude.Nothing
    }

-- | The position of an asset in a rack.
assetInfo_assetLocation :: Lens.Lens' AssetInfo (Prelude.Maybe AssetLocation)
assetInfo_assetLocation = Lens.lens (\AssetInfo' {assetLocation} -> assetLocation) (\s@AssetInfo' {} a -> s {assetLocation = a} :: AssetInfo)

-- | The ID of the asset.
assetInfo_assetId :: Lens.Lens' AssetInfo (Prelude.Maybe Prelude.Text)
assetInfo_assetId = Lens.lens (\AssetInfo' {assetId} -> assetId) (\s@AssetInfo' {} a -> s {assetId = a} :: AssetInfo)

-- | Information about compute hardware assets.
assetInfo_computeAttributes :: Lens.Lens' AssetInfo (Prelude.Maybe ComputeAttributes)
assetInfo_computeAttributes = Lens.lens (\AssetInfo' {computeAttributes} -> computeAttributes) (\s@AssetInfo' {} a -> s {computeAttributes = a} :: AssetInfo)

-- | The type of the asset.
assetInfo_assetType :: Lens.Lens' AssetInfo (Prelude.Maybe AssetType)
assetInfo_assetType = Lens.lens (\AssetInfo' {assetType} -> assetType) (\s@AssetInfo' {} a -> s {assetType = a} :: AssetInfo)

-- | The rack ID of the asset.
assetInfo_rackId :: Lens.Lens' AssetInfo (Prelude.Maybe Prelude.Text)
assetInfo_rackId = Lens.lens (\AssetInfo' {rackId} -> rackId) (\s@AssetInfo' {} a -> s {rackId = a} :: AssetInfo)

instance Data.FromJSON AssetInfo where
  parseJSON =
    Data.withObject
      "AssetInfo"
      ( \x ->
          AssetInfo'
            Prelude.<$> (x Data..:? "AssetLocation")
            Prelude.<*> (x Data..:? "AssetId")
            Prelude.<*> (x Data..:? "ComputeAttributes")
            Prelude.<*> (x Data..:? "AssetType")
            Prelude.<*> (x Data..:? "RackId")
      )

instance Prelude.Hashable AssetInfo where
  hashWithSalt _salt AssetInfo' {..} =
    _salt `Prelude.hashWithSalt` assetLocation
      `Prelude.hashWithSalt` assetId
      `Prelude.hashWithSalt` computeAttributes
      `Prelude.hashWithSalt` assetType
      `Prelude.hashWithSalt` rackId

instance Prelude.NFData AssetInfo where
  rnf AssetInfo' {..} =
    Prelude.rnf assetLocation
      `Prelude.seq` Prelude.rnf assetId
      `Prelude.seq` Prelude.rnf computeAttributes
      `Prelude.seq` Prelude.rnf assetType
      `Prelude.seq` Prelude.rnf rackId
