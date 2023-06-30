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
-- Module      : Amazonka.IoTSiteWise.Types.AssetHierarchyInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.AssetHierarchyInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a parent asset and a child asset that are
-- related through an asset hierarchy.
--
-- /See:/ 'newAssetHierarchyInfo' smart constructor.
data AssetHierarchyInfo = AssetHierarchyInfo'
  { -- | The ID of the child asset in this asset relationship.
    childAssetId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the parent asset in this asset relationship.
    parentAssetId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetHierarchyInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'childAssetId', 'assetHierarchyInfo_childAssetId' - The ID of the child asset in this asset relationship.
--
-- 'parentAssetId', 'assetHierarchyInfo_parentAssetId' - The ID of the parent asset in this asset relationship.
newAssetHierarchyInfo ::
  AssetHierarchyInfo
newAssetHierarchyInfo =
  AssetHierarchyInfo'
    { childAssetId = Prelude.Nothing,
      parentAssetId = Prelude.Nothing
    }

-- | The ID of the child asset in this asset relationship.
assetHierarchyInfo_childAssetId :: Lens.Lens' AssetHierarchyInfo (Prelude.Maybe Prelude.Text)
assetHierarchyInfo_childAssetId = Lens.lens (\AssetHierarchyInfo' {childAssetId} -> childAssetId) (\s@AssetHierarchyInfo' {} a -> s {childAssetId = a} :: AssetHierarchyInfo)

-- | The ID of the parent asset in this asset relationship.
assetHierarchyInfo_parentAssetId :: Lens.Lens' AssetHierarchyInfo (Prelude.Maybe Prelude.Text)
assetHierarchyInfo_parentAssetId = Lens.lens (\AssetHierarchyInfo' {parentAssetId} -> parentAssetId) (\s@AssetHierarchyInfo' {} a -> s {parentAssetId = a} :: AssetHierarchyInfo)

instance Data.FromJSON AssetHierarchyInfo where
  parseJSON =
    Data.withObject
      "AssetHierarchyInfo"
      ( \x ->
          AssetHierarchyInfo'
            Prelude.<$> (x Data..:? "childAssetId")
            Prelude.<*> (x Data..:? "parentAssetId")
      )

instance Prelude.Hashable AssetHierarchyInfo where
  hashWithSalt _salt AssetHierarchyInfo' {..} =
    _salt
      `Prelude.hashWithSalt` childAssetId
      `Prelude.hashWithSalt` parentAssetId

instance Prelude.NFData AssetHierarchyInfo where
  rnf AssetHierarchyInfo' {..} =
    Prelude.rnf childAssetId
      `Prelude.seq` Prelude.rnf parentAssetId
