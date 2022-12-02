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
-- Module      : Amazonka.IoTSiteWise.Types.AssetModelPropertySummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.AssetModelPropertySummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.PropertyDataType
import Amazonka.IoTSiteWise.Types.PropertyType
import qualified Amazonka.Prelude as Prelude

-- | Contains a summary of a property associated with a model.
--
-- /See:/ 'newAssetModelPropertySummary' smart constructor.
data AssetModelPropertySummary = AssetModelPropertySummary'
  { -- | The data type of the structure for this property. This parameter exists
    -- on properties that have the @STRUCT@ data type.
    dataTypeSpec :: Prelude.Maybe Prelude.Text,
    -- | The ID of the composite model that contains the asset model property.
    assetModelCompositeModelId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the property.
    id :: Prelude.Maybe Prelude.Text,
    -- | The unit (such as @Newtons@ or @RPM@) of the property.
    unit :: Prelude.Maybe Prelude.Text,
    -- | The name of the property.
    name :: Prelude.Text,
    -- | The data type of the property.
    dataType :: PropertyDataType,
    type' :: PropertyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetModelPropertySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataTypeSpec', 'assetModelPropertySummary_dataTypeSpec' - The data type of the structure for this property. This parameter exists
-- on properties that have the @STRUCT@ data type.
--
-- 'assetModelCompositeModelId', 'assetModelPropertySummary_assetModelCompositeModelId' - The ID of the composite model that contains the asset model property.
--
-- 'id', 'assetModelPropertySummary_id' - The ID of the property.
--
-- 'unit', 'assetModelPropertySummary_unit' - The unit (such as @Newtons@ or @RPM@) of the property.
--
-- 'name', 'assetModelPropertySummary_name' - The name of the property.
--
-- 'dataType', 'assetModelPropertySummary_dataType' - The data type of the property.
--
-- 'type'', 'assetModelPropertySummary_type' - Undocumented member.
newAssetModelPropertySummary ::
  -- | 'name'
  Prelude.Text ->
  -- | 'dataType'
  PropertyDataType ->
  -- | 'type''
  PropertyType ->
  AssetModelPropertySummary
newAssetModelPropertySummary pName_ pDataType_ pType_ =
  AssetModelPropertySummary'
    { dataTypeSpec =
        Prelude.Nothing,
      assetModelCompositeModelId = Prelude.Nothing,
      id = Prelude.Nothing,
      unit = Prelude.Nothing,
      name = pName_,
      dataType = pDataType_,
      type' = pType_
    }

-- | The data type of the structure for this property. This parameter exists
-- on properties that have the @STRUCT@ data type.
assetModelPropertySummary_dataTypeSpec :: Lens.Lens' AssetModelPropertySummary (Prelude.Maybe Prelude.Text)
assetModelPropertySummary_dataTypeSpec = Lens.lens (\AssetModelPropertySummary' {dataTypeSpec} -> dataTypeSpec) (\s@AssetModelPropertySummary' {} a -> s {dataTypeSpec = a} :: AssetModelPropertySummary)

-- | The ID of the composite model that contains the asset model property.
assetModelPropertySummary_assetModelCompositeModelId :: Lens.Lens' AssetModelPropertySummary (Prelude.Maybe Prelude.Text)
assetModelPropertySummary_assetModelCompositeModelId = Lens.lens (\AssetModelPropertySummary' {assetModelCompositeModelId} -> assetModelCompositeModelId) (\s@AssetModelPropertySummary' {} a -> s {assetModelCompositeModelId = a} :: AssetModelPropertySummary)

-- | The ID of the property.
assetModelPropertySummary_id :: Lens.Lens' AssetModelPropertySummary (Prelude.Maybe Prelude.Text)
assetModelPropertySummary_id = Lens.lens (\AssetModelPropertySummary' {id} -> id) (\s@AssetModelPropertySummary' {} a -> s {id = a} :: AssetModelPropertySummary)

-- | The unit (such as @Newtons@ or @RPM@) of the property.
assetModelPropertySummary_unit :: Lens.Lens' AssetModelPropertySummary (Prelude.Maybe Prelude.Text)
assetModelPropertySummary_unit = Lens.lens (\AssetModelPropertySummary' {unit} -> unit) (\s@AssetModelPropertySummary' {} a -> s {unit = a} :: AssetModelPropertySummary)

-- | The name of the property.
assetModelPropertySummary_name :: Lens.Lens' AssetModelPropertySummary Prelude.Text
assetModelPropertySummary_name = Lens.lens (\AssetModelPropertySummary' {name} -> name) (\s@AssetModelPropertySummary' {} a -> s {name = a} :: AssetModelPropertySummary)

-- | The data type of the property.
assetModelPropertySummary_dataType :: Lens.Lens' AssetModelPropertySummary PropertyDataType
assetModelPropertySummary_dataType = Lens.lens (\AssetModelPropertySummary' {dataType} -> dataType) (\s@AssetModelPropertySummary' {} a -> s {dataType = a} :: AssetModelPropertySummary)

-- | Undocumented member.
assetModelPropertySummary_type :: Lens.Lens' AssetModelPropertySummary PropertyType
assetModelPropertySummary_type = Lens.lens (\AssetModelPropertySummary' {type'} -> type') (\s@AssetModelPropertySummary' {} a -> s {type' = a} :: AssetModelPropertySummary)

instance Data.FromJSON AssetModelPropertySummary where
  parseJSON =
    Data.withObject
      "AssetModelPropertySummary"
      ( \x ->
          AssetModelPropertySummary'
            Prelude.<$> (x Data..:? "dataTypeSpec")
            Prelude.<*> (x Data..:? "assetModelCompositeModelId")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "unit")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "dataType")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable AssetModelPropertySummary where
  hashWithSalt _salt AssetModelPropertySummary' {..} =
    _salt `Prelude.hashWithSalt` dataTypeSpec
      `Prelude.hashWithSalt` assetModelCompositeModelId
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` dataType
      `Prelude.hashWithSalt` type'

instance Prelude.NFData AssetModelPropertySummary where
  rnf AssetModelPropertySummary' {..} =
    Prelude.rnf dataTypeSpec
      `Prelude.seq` Prelude.rnf assetModelCompositeModelId
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf dataType
      `Prelude.seq` Prelude.rnf type'
