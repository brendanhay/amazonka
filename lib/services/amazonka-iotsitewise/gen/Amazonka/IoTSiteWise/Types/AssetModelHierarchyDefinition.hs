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
-- Module      : Amazonka.IoTSiteWise.Types.AssetModelHierarchyDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.AssetModelHierarchyDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains an asset model hierarchy used in asset model creation. An asset
-- model hierarchy determines the kind (or type) of asset that can belong
-- to a hierarchy.
--
-- /See:/ 'newAssetModelHierarchyDefinition' smart constructor.
data AssetModelHierarchyDefinition = AssetModelHierarchyDefinition'
  { -- | The name of the asset model hierarchy definition (as specified in the
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_CreateAssetModel.html CreateAssetModel>
    -- or
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_UpdateAssetModel.html UpdateAssetModel>
    -- API operation).
    name :: Prelude.Text,
    -- | The ID of an asset model for this hierarchy.
    childAssetModelId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetModelHierarchyDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'assetModelHierarchyDefinition_name' - The name of the asset model hierarchy definition (as specified in the
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_CreateAssetModel.html CreateAssetModel>
-- or
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_UpdateAssetModel.html UpdateAssetModel>
-- API operation).
--
-- 'childAssetModelId', 'assetModelHierarchyDefinition_childAssetModelId' - The ID of an asset model for this hierarchy.
newAssetModelHierarchyDefinition ::
  -- | 'name'
  Prelude.Text ->
  -- | 'childAssetModelId'
  Prelude.Text ->
  AssetModelHierarchyDefinition
newAssetModelHierarchyDefinition
  pName_
  pChildAssetModelId_ =
    AssetModelHierarchyDefinition'
      { name = pName_,
        childAssetModelId = pChildAssetModelId_
      }

-- | The name of the asset model hierarchy definition (as specified in the
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_CreateAssetModel.html CreateAssetModel>
-- or
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_UpdateAssetModel.html UpdateAssetModel>
-- API operation).
assetModelHierarchyDefinition_name :: Lens.Lens' AssetModelHierarchyDefinition Prelude.Text
assetModelHierarchyDefinition_name = Lens.lens (\AssetModelHierarchyDefinition' {name} -> name) (\s@AssetModelHierarchyDefinition' {} a -> s {name = a} :: AssetModelHierarchyDefinition)

-- | The ID of an asset model for this hierarchy.
assetModelHierarchyDefinition_childAssetModelId :: Lens.Lens' AssetModelHierarchyDefinition Prelude.Text
assetModelHierarchyDefinition_childAssetModelId = Lens.lens (\AssetModelHierarchyDefinition' {childAssetModelId} -> childAssetModelId) (\s@AssetModelHierarchyDefinition' {} a -> s {childAssetModelId = a} :: AssetModelHierarchyDefinition)

instance
  Prelude.Hashable
    AssetModelHierarchyDefinition
  where
  hashWithSalt _salt AssetModelHierarchyDefinition' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` childAssetModelId

instance Prelude.NFData AssetModelHierarchyDefinition where
  rnf AssetModelHierarchyDefinition' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf childAssetModelId

instance Data.ToJSON AssetModelHierarchyDefinition where
  toJSON AssetModelHierarchyDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just
              ("childAssetModelId" Data..= childAssetModelId)
          ]
      )
