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
-- Module      : Amazonka.IoTSiteWise.Types.Property
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.Property where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.PropertyDataType
import Amazonka.IoTSiteWise.Types.PropertyNotification
import Amazonka.IoTSiteWise.Types.PropertyType
import qualified Amazonka.Prelude as Prelude

-- | Contains asset property information.
--
-- /See:/ 'newProperty' smart constructor.
data Property = Property'
  { -- | The alias that identifies the property, such as an OPC-UA server data
    -- stream path (for example,
    -- @\/company\/windfarm\/3\/turbine\/7\/temperature@). For more
    -- information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/connect-data-streams.html Mapping industrial data streams to asset properties>
    -- in the /IoT SiteWise User Guide/.
    alias :: Prelude.Maybe Prelude.Text,
    -- | The asset property\'s notification topic and state. For more
    -- information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_UpdateAssetProperty.html UpdateAssetProperty>.
    notification :: Prelude.Maybe PropertyNotification,
    -- | The property type (see @PropertyType@). A property contains one type.
    type' :: Prelude.Maybe PropertyType,
    -- | The unit (such as @Newtons@ or @RPM@) of the asset property.
    unit :: Prelude.Maybe Prelude.Text,
    -- | The ID of the asset property.
    id :: Prelude.Text,
    -- | The name of the property.
    name :: Prelude.Text,
    -- | The property data type.
    dataType :: PropertyDataType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Property' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alias', 'property_alias' - The alias that identifies the property, such as an OPC-UA server data
-- stream path (for example,
-- @\/company\/windfarm\/3\/turbine\/7\/temperature@). For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/connect-data-streams.html Mapping industrial data streams to asset properties>
-- in the /IoT SiteWise User Guide/.
--
-- 'notification', 'property_notification' - The asset property\'s notification topic and state. For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_UpdateAssetProperty.html UpdateAssetProperty>.
--
-- 'type'', 'property_type' - The property type (see @PropertyType@). A property contains one type.
--
-- 'unit', 'property_unit' - The unit (such as @Newtons@ or @RPM@) of the asset property.
--
-- 'id', 'property_id' - The ID of the asset property.
--
-- 'name', 'property_name' - The name of the property.
--
-- 'dataType', 'property_dataType' - The property data type.
newProperty ::
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'dataType'
  PropertyDataType ->
  Property
newProperty pId_ pName_ pDataType_ =
  Property'
    { alias = Prelude.Nothing,
      notification = Prelude.Nothing,
      type' = Prelude.Nothing,
      unit = Prelude.Nothing,
      id = pId_,
      name = pName_,
      dataType = pDataType_
    }

-- | The alias that identifies the property, such as an OPC-UA server data
-- stream path (for example,
-- @\/company\/windfarm\/3\/turbine\/7\/temperature@). For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/connect-data-streams.html Mapping industrial data streams to asset properties>
-- in the /IoT SiteWise User Guide/.
property_alias :: Lens.Lens' Property (Prelude.Maybe Prelude.Text)
property_alias = Lens.lens (\Property' {alias} -> alias) (\s@Property' {} a -> s {alias = a} :: Property)

-- | The asset property\'s notification topic and state. For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_UpdateAssetProperty.html UpdateAssetProperty>.
property_notification :: Lens.Lens' Property (Prelude.Maybe PropertyNotification)
property_notification = Lens.lens (\Property' {notification} -> notification) (\s@Property' {} a -> s {notification = a} :: Property)

-- | The property type (see @PropertyType@). A property contains one type.
property_type :: Lens.Lens' Property (Prelude.Maybe PropertyType)
property_type = Lens.lens (\Property' {type'} -> type') (\s@Property' {} a -> s {type' = a} :: Property)

-- | The unit (such as @Newtons@ or @RPM@) of the asset property.
property_unit :: Lens.Lens' Property (Prelude.Maybe Prelude.Text)
property_unit = Lens.lens (\Property' {unit} -> unit) (\s@Property' {} a -> s {unit = a} :: Property)

-- | The ID of the asset property.
property_id :: Lens.Lens' Property Prelude.Text
property_id = Lens.lens (\Property' {id} -> id) (\s@Property' {} a -> s {id = a} :: Property)

-- | The name of the property.
property_name :: Lens.Lens' Property Prelude.Text
property_name = Lens.lens (\Property' {name} -> name) (\s@Property' {} a -> s {name = a} :: Property)

-- | The property data type.
property_dataType :: Lens.Lens' Property PropertyDataType
property_dataType = Lens.lens (\Property' {dataType} -> dataType) (\s@Property' {} a -> s {dataType = a} :: Property)

instance Data.FromJSON Property where
  parseJSON =
    Data.withObject
      "Property"
      ( \x ->
          Property'
            Prelude.<$> (x Data..:? "alias")
            Prelude.<*> (x Data..:? "notification")
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..:? "unit")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "dataType")
      )

instance Prelude.Hashable Property where
  hashWithSalt _salt Property' {..} =
    _salt
      `Prelude.hashWithSalt` alias
      `Prelude.hashWithSalt` notification
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` dataType

instance Prelude.NFData Property where
  rnf Property' {..} =
    Prelude.rnf alias
      `Prelude.seq` Prelude.rnf notification
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf dataType
