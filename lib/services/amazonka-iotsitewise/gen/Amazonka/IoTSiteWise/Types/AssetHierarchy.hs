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
-- Module      : Amazonka.IoTSiteWise.Types.AssetHierarchy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.AssetHierarchy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an asset hierarchy that contains a hierarchy\'s name and ID.
--
-- /See:/ 'newAssetHierarchy' smart constructor.
data AssetHierarchy = AssetHierarchy'
  { -- | The ID of the hierarchy. This ID is a @hierarchyId@.
    id :: Prelude.Maybe Prelude.Text,
    -- | The hierarchy name provided in the
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_CreateAssetModel.html CreateAssetModel>
    -- or
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_UpdateAssetModel.html UpdateAssetModel>
    -- API operation.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetHierarchy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'assetHierarchy_id' - The ID of the hierarchy. This ID is a @hierarchyId@.
--
-- 'name', 'assetHierarchy_name' - The hierarchy name provided in the
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_CreateAssetModel.html CreateAssetModel>
-- or
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_UpdateAssetModel.html UpdateAssetModel>
-- API operation.
newAssetHierarchy ::
  -- | 'name'
  Prelude.Text ->
  AssetHierarchy
newAssetHierarchy pName_ =
  AssetHierarchy'
    { id = Prelude.Nothing,
      name = pName_
    }

-- | The ID of the hierarchy. This ID is a @hierarchyId@.
assetHierarchy_id :: Lens.Lens' AssetHierarchy (Prelude.Maybe Prelude.Text)
assetHierarchy_id = Lens.lens (\AssetHierarchy' {id} -> id) (\s@AssetHierarchy' {} a -> s {id = a} :: AssetHierarchy)

-- | The hierarchy name provided in the
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_CreateAssetModel.html CreateAssetModel>
-- or
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_UpdateAssetModel.html UpdateAssetModel>
-- API operation.
assetHierarchy_name :: Lens.Lens' AssetHierarchy Prelude.Text
assetHierarchy_name = Lens.lens (\AssetHierarchy' {name} -> name) (\s@AssetHierarchy' {} a -> s {name = a} :: AssetHierarchy)

instance Core.FromJSON AssetHierarchy where
  parseJSON =
    Core.withObject
      "AssetHierarchy"
      ( \x ->
          AssetHierarchy'
            Prelude.<$> (x Core..:? "id") Prelude.<*> (x Core..: "name")
      )

instance Prelude.Hashable AssetHierarchy where
  hashWithSalt _salt AssetHierarchy' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name

instance Prelude.NFData AssetHierarchy where
  rnf AssetHierarchy' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf name
