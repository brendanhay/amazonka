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
-- Module      : Amazonka.IoTSiteWise.Types.AssetModelPropertyDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.AssetModelPropertyDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTSiteWise.Types.PropertyDataType
import Amazonka.IoTSiteWise.Types.PropertyType
import qualified Amazonka.Prelude as Prelude

-- | Contains an asset model property definition. This property definition is
-- applied to all assets created from the asset model.
--
-- /See:/ 'newAssetModelPropertyDefinition' smart constructor.
data AssetModelPropertyDefinition = AssetModelPropertyDefinition'
  { -- | The data type of the structure for this property. This parameter is
    -- required on properties that have the @STRUCT@ data type.
    --
    -- The options for this parameter depend on the type of the composite model
    -- in which you define this property. Use @AWS\/ALARM_STATE@ for alarm
    -- state in alarm composite models.
    dataTypeSpec :: Prelude.Maybe Prelude.Text,
    -- | The unit of the property definition, such as @Newtons@ or @RPM@.
    unit :: Prelude.Maybe Prelude.Text,
    -- | The name of the property definition.
    name :: Prelude.Text,
    -- | The data type of the property definition.
    --
    -- If you specify @STRUCT@, you must also specify @dataTypeSpec@ to
    -- identify the type of the structure for this property.
    dataType :: PropertyDataType,
    -- | The property definition type (see @PropertyType@). You can only specify
    -- one type in a property definition.
    type' :: PropertyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetModelPropertyDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataTypeSpec', 'assetModelPropertyDefinition_dataTypeSpec' - The data type of the structure for this property. This parameter is
-- required on properties that have the @STRUCT@ data type.
--
-- The options for this parameter depend on the type of the composite model
-- in which you define this property. Use @AWS\/ALARM_STATE@ for alarm
-- state in alarm composite models.
--
-- 'unit', 'assetModelPropertyDefinition_unit' - The unit of the property definition, such as @Newtons@ or @RPM@.
--
-- 'name', 'assetModelPropertyDefinition_name' - The name of the property definition.
--
-- 'dataType', 'assetModelPropertyDefinition_dataType' - The data type of the property definition.
--
-- If you specify @STRUCT@, you must also specify @dataTypeSpec@ to
-- identify the type of the structure for this property.
--
-- 'type'', 'assetModelPropertyDefinition_type' - The property definition type (see @PropertyType@). You can only specify
-- one type in a property definition.
newAssetModelPropertyDefinition ::
  -- | 'name'
  Prelude.Text ->
  -- | 'dataType'
  PropertyDataType ->
  -- | 'type''
  PropertyType ->
  AssetModelPropertyDefinition
newAssetModelPropertyDefinition
  pName_
  pDataType_
  pType_ =
    AssetModelPropertyDefinition'
      { dataTypeSpec =
          Prelude.Nothing,
        unit = Prelude.Nothing,
        name = pName_,
        dataType = pDataType_,
        type' = pType_
      }

-- | The data type of the structure for this property. This parameter is
-- required on properties that have the @STRUCT@ data type.
--
-- The options for this parameter depend on the type of the composite model
-- in which you define this property. Use @AWS\/ALARM_STATE@ for alarm
-- state in alarm composite models.
assetModelPropertyDefinition_dataTypeSpec :: Lens.Lens' AssetModelPropertyDefinition (Prelude.Maybe Prelude.Text)
assetModelPropertyDefinition_dataTypeSpec = Lens.lens (\AssetModelPropertyDefinition' {dataTypeSpec} -> dataTypeSpec) (\s@AssetModelPropertyDefinition' {} a -> s {dataTypeSpec = a} :: AssetModelPropertyDefinition)

-- | The unit of the property definition, such as @Newtons@ or @RPM@.
assetModelPropertyDefinition_unit :: Lens.Lens' AssetModelPropertyDefinition (Prelude.Maybe Prelude.Text)
assetModelPropertyDefinition_unit = Lens.lens (\AssetModelPropertyDefinition' {unit} -> unit) (\s@AssetModelPropertyDefinition' {} a -> s {unit = a} :: AssetModelPropertyDefinition)

-- | The name of the property definition.
assetModelPropertyDefinition_name :: Lens.Lens' AssetModelPropertyDefinition Prelude.Text
assetModelPropertyDefinition_name = Lens.lens (\AssetModelPropertyDefinition' {name} -> name) (\s@AssetModelPropertyDefinition' {} a -> s {name = a} :: AssetModelPropertyDefinition)

-- | The data type of the property definition.
--
-- If you specify @STRUCT@, you must also specify @dataTypeSpec@ to
-- identify the type of the structure for this property.
assetModelPropertyDefinition_dataType :: Lens.Lens' AssetModelPropertyDefinition PropertyDataType
assetModelPropertyDefinition_dataType = Lens.lens (\AssetModelPropertyDefinition' {dataType} -> dataType) (\s@AssetModelPropertyDefinition' {} a -> s {dataType = a} :: AssetModelPropertyDefinition)

-- | The property definition type (see @PropertyType@). You can only specify
-- one type in a property definition.
assetModelPropertyDefinition_type :: Lens.Lens' AssetModelPropertyDefinition PropertyType
assetModelPropertyDefinition_type = Lens.lens (\AssetModelPropertyDefinition' {type'} -> type') (\s@AssetModelPropertyDefinition' {} a -> s {type' = a} :: AssetModelPropertyDefinition)

instance
  Prelude.Hashable
    AssetModelPropertyDefinition
  where
  hashWithSalt _salt AssetModelPropertyDefinition' {..} =
    _salt `Prelude.hashWithSalt` dataTypeSpec
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` dataType
      `Prelude.hashWithSalt` type'

instance Prelude.NFData AssetModelPropertyDefinition where
  rnf AssetModelPropertyDefinition' {..} =
    Prelude.rnf dataTypeSpec
      `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf dataType
      `Prelude.seq` Prelude.rnf type'

instance Core.ToJSON AssetModelPropertyDefinition where
  toJSON AssetModelPropertyDefinition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("dataTypeSpec" Core..=) Prelude.<$> dataTypeSpec,
            ("unit" Core..=) Prelude.<$> unit,
            Prelude.Just ("name" Core..= name),
            Prelude.Just ("dataType" Core..= dataType),
            Prelude.Just ("type" Core..= type')
          ]
      )
