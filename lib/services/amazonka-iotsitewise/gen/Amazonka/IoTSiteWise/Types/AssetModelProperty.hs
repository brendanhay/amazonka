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
-- Module      : Amazonka.IoTSiteWise.Types.AssetModelProperty
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.AssetModelProperty where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.PropertyDataType
import Amazonka.IoTSiteWise.Types.PropertyType
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an asset model property.
--
-- /See:/ 'newAssetModelProperty' smart constructor.
data AssetModelProperty = AssetModelProperty'
  { -- | The data type of the structure for this property. This parameter exists
    -- on properties that have the @STRUCT@ data type.
    dataTypeSpec :: Prelude.Maybe Prelude.Text,
    -- | The ID of the asset model property.
    id :: Prelude.Maybe Prelude.Text,
    -- | The unit of the asset model property, such as @Newtons@ or @RPM@.
    unit :: Prelude.Maybe Prelude.Text,
    -- | The name of the asset model property.
    name :: Prelude.Text,
    -- | The data type of the asset model property.
    dataType :: PropertyDataType,
    -- | The property type (see @PropertyType@).
    type' :: PropertyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetModelProperty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataTypeSpec', 'assetModelProperty_dataTypeSpec' - The data type of the structure for this property. This parameter exists
-- on properties that have the @STRUCT@ data type.
--
-- 'id', 'assetModelProperty_id' - The ID of the asset model property.
--
-- 'unit', 'assetModelProperty_unit' - The unit of the asset model property, such as @Newtons@ or @RPM@.
--
-- 'name', 'assetModelProperty_name' - The name of the asset model property.
--
-- 'dataType', 'assetModelProperty_dataType' - The data type of the asset model property.
--
-- 'type'', 'assetModelProperty_type' - The property type (see @PropertyType@).
newAssetModelProperty ::
  -- | 'name'
  Prelude.Text ->
  -- | 'dataType'
  PropertyDataType ->
  -- | 'type''
  PropertyType ->
  AssetModelProperty
newAssetModelProperty pName_ pDataType_ pType_ =
  AssetModelProperty'
    { dataTypeSpec = Prelude.Nothing,
      id = Prelude.Nothing,
      unit = Prelude.Nothing,
      name = pName_,
      dataType = pDataType_,
      type' = pType_
    }

-- | The data type of the structure for this property. This parameter exists
-- on properties that have the @STRUCT@ data type.
assetModelProperty_dataTypeSpec :: Lens.Lens' AssetModelProperty (Prelude.Maybe Prelude.Text)
assetModelProperty_dataTypeSpec = Lens.lens (\AssetModelProperty' {dataTypeSpec} -> dataTypeSpec) (\s@AssetModelProperty' {} a -> s {dataTypeSpec = a} :: AssetModelProperty)

-- | The ID of the asset model property.
assetModelProperty_id :: Lens.Lens' AssetModelProperty (Prelude.Maybe Prelude.Text)
assetModelProperty_id = Lens.lens (\AssetModelProperty' {id} -> id) (\s@AssetModelProperty' {} a -> s {id = a} :: AssetModelProperty)

-- | The unit of the asset model property, such as @Newtons@ or @RPM@.
assetModelProperty_unit :: Lens.Lens' AssetModelProperty (Prelude.Maybe Prelude.Text)
assetModelProperty_unit = Lens.lens (\AssetModelProperty' {unit} -> unit) (\s@AssetModelProperty' {} a -> s {unit = a} :: AssetModelProperty)

-- | The name of the asset model property.
assetModelProperty_name :: Lens.Lens' AssetModelProperty Prelude.Text
assetModelProperty_name = Lens.lens (\AssetModelProperty' {name} -> name) (\s@AssetModelProperty' {} a -> s {name = a} :: AssetModelProperty)

-- | The data type of the asset model property.
assetModelProperty_dataType :: Lens.Lens' AssetModelProperty PropertyDataType
assetModelProperty_dataType = Lens.lens (\AssetModelProperty' {dataType} -> dataType) (\s@AssetModelProperty' {} a -> s {dataType = a} :: AssetModelProperty)

-- | The property type (see @PropertyType@).
assetModelProperty_type :: Lens.Lens' AssetModelProperty PropertyType
assetModelProperty_type = Lens.lens (\AssetModelProperty' {type'} -> type') (\s@AssetModelProperty' {} a -> s {type' = a} :: AssetModelProperty)

instance Data.FromJSON AssetModelProperty where
  parseJSON =
    Data.withObject
      "AssetModelProperty"
      ( \x ->
          AssetModelProperty'
            Prelude.<$> (x Data..:? "dataTypeSpec")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "unit")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "dataType")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable AssetModelProperty where
  hashWithSalt _salt AssetModelProperty' {..} =
    _salt
      `Prelude.hashWithSalt` dataTypeSpec
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` dataType
      `Prelude.hashWithSalt` type'

instance Prelude.NFData AssetModelProperty where
  rnf AssetModelProperty' {..} =
    Prelude.rnf dataTypeSpec
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf dataType
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON AssetModelProperty where
  toJSON AssetModelProperty' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("dataTypeSpec" Data..=) Prelude.<$> dataTypeSpec,
            ("id" Data..=) Prelude.<$> id,
            ("unit" Data..=) Prelude.<$> unit,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("dataType" Data..= dataType),
            Prelude.Just ("type" Data..= type')
          ]
      )
