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
-- Module      : Amazonka.IoTSiteWise.Types.AssetModelHierarchy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.AssetModelHierarchy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an asset hierarchy that contains a hierarchy\'s name, ID, and
-- child asset model ID that specifies the type of asset that can be in
-- this hierarchy.
--
-- /See:/ 'newAssetModelHierarchy' smart constructor.
data AssetModelHierarchy = AssetModelHierarchy'
  { -- | The ID of the asset model hierarchy. This ID is a @hierarchyId@.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the asset model hierarchy that you specify by using the
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_CreateAssetModel.html CreateAssetModel>
    -- or
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_UpdateAssetModel.html UpdateAssetModel>
    -- API operation.
    name :: Prelude.Text,
    -- | The ID of the asset model. All assets in this hierarchy must be
    -- instances of the @childAssetModelId@ asset model.
    childAssetModelId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetModelHierarchy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'assetModelHierarchy_id' - The ID of the asset model hierarchy. This ID is a @hierarchyId@.
--
-- 'name', 'assetModelHierarchy_name' - The name of the asset model hierarchy that you specify by using the
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_CreateAssetModel.html CreateAssetModel>
-- or
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_UpdateAssetModel.html UpdateAssetModel>
-- API operation.
--
-- 'childAssetModelId', 'assetModelHierarchy_childAssetModelId' - The ID of the asset model. All assets in this hierarchy must be
-- instances of the @childAssetModelId@ asset model.
newAssetModelHierarchy ::
  -- | 'name'
  Prelude.Text ->
  -- | 'childAssetModelId'
  Prelude.Text ->
  AssetModelHierarchy
newAssetModelHierarchy pName_ pChildAssetModelId_ =
  AssetModelHierarchy'
    { id = Prelude.Nothing,
      name = pName_,
      childAssetModelId = pChildAssetModelId_
    }

-- | The ID of the asset model hierarchy. This ID is a @hierarchyId@.
assetModelHierarchy_id :: Lens.Lens' AssetModelHierarchy (Prelude.Maybe Prelude.Text)
assetModelHierarchy_id = Lens.lens (\AssetModelHierarchy' {id} -> id) (\s@AssetModelHierarchy' {} a -> s {id = a} :: AssetModelHierarchy)

-- | The name of the asset model hierarchy that you specify by using the
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_CreateAssetModel.html CreateAssetModel>
-- or
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_UpdateAssetModel.html UpdateAssetModel>
-- API operation.
assetModelHierarchy_name :: Lens.Lens' AssetModelHierarchy Prelude.Text
assetModelHierarchy_name = Lens.lens (\AssetModelHierarchy' {name} -> name) (\s@AssetModelHierarchy' {} a -> s {name = a} :: AssetModelHierarchy)

-- | The ID of the asset model. All assets in this hierarchy must be
-- instances of the @childAssetModelId@ asset model.
assetModelHierarchy_childAssetModelId :: Lens.Lens' AssetModelHierarchy Prelude.Text
assetModelHierarchy_childAssetModelId = Lens.lens (\AssetModelHierarchy' {childAssetModelId} -> childAssetModelId) (\s@AssetModelHierarchy' {} a -> s {childAssetModelId = a} :: AssetModelHierarchy)

instance Core.FromJSON AssetModelHierarchy where
  parseJSON =
    Core.withObject
      "AssetModelHierarchy"
      ( \x ->
          AssetModelHierarchy'
            Prelude.<$> (x Core..:? "id")
            Prelude.<*> (x Core..: "name")
            Prelude.<*> (x Core..: "childAssetModelId")
      )

instance Prelude.Hashable AssetModelHierarchy where
  hashWithSalt _salt AssetModelHierarchy' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` childAssetModelId

instance Prelude.NFData AssetModelHierarchy where
  rnf AssetModelHierarchy' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf childAssetModelId

instance Core.ToJSON AssetModelHierarchy where
  toJSON AssetModelHierarchy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("id" Core..=) Prelude.<$> id,
            Prelude.Just ("name" Core..= name),
            Prelude.Just
              ("childAssetModelId" Core..= childAssetModelId)
          ]
      )
