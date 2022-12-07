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
-- Module      : Amazonka.IotTwinMaker.Types.PropertyDefinitionRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.PropertyDefinitionRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.DataType
import Amazonka.IotTwinMaker.Types.DataValue
import qualified Amazonka.Prelude as Prelude

-- | An object that sets information about a property.
--
-- /See:/ 'newPropertyDefinitionRequest' smart constructor.
data PropertyDefinitionRequest = PropertyDefinitionRequest'
  { -- | A Boolean value that specifies whether the property ID comes from an
    -- external data store.
    isExternalId :: Prelude.Maybe Prelude.Bool,
    -- | A Boolean value that specifies whether the property is stored
    -- externally.
    isStoredExternally :: Prelude.Maybe Prelude.Bool,
    -- | A mapping that specifies configuration information about the property.
    -- Use this field to specify information that you read from and write to an
    -- external source.
    configuration :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | An object that contains the default value.
    defaultValue :: Prelude.Maybe DataValue,
    -- | A Boolean value that specifies whether the property is required.
    isRequiredInEntity :: Prelude.Maybe Prelude.Bool,
    -- | An object that contains information about the data type.
    dataType :: Prelude.Maybe DataType,
    -- | A Boolean value that specifies whether the property consists of time
    -- series data.
    isTimeSeries :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PropertyDefinitionRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isExternalId', 'propertyDefinitionRequest_isExternalId' - A Boolean value that specifies whether the property ID comes from an
-- external data store.
--
-- 'isStoredExternally', 'propertyDefinitionRequest_isStoredExternally' - A Boolean value that specifies whether the property is stored
-- externally.
--
-- 'configuration', 'propertyDefinitionRequest_configuration' - A mapping that specifies configuration information about the property.
-- Use this field to specify information that you read from and write to an
-- external source.
--
-- 'defaultValue', 'propertyDefinitionRequest_defaultValue' - An object that contains the default value.
--
-- 'isRequiredInEntity', 'propertyDefinitionRequest_isRequiredInEntity' - A Boolean value that specifies whether the property is required.
--
-- 'dataType', 'propertyDefinitionRequest_dataType' - An object that contains information about the data type.
--
-- 'isTimeSeries', 'propertyDefinitionRequest_isTimeSeries' - A Boolean value that specifies whether the property consists of time
-- series data.
newPropertyDefinitionRequest ::
  PropertyDefinitionRequest
newPropertyDefinitionRequest =
  PropertyDefinitionRequest'
    { isExternalId =
        Prelude.Nothing,
      isStoredExternally = Prelude.Nothing,
      configuration = Prelude.Nothing,
      defaultValue = Prelude.Nothing,
      isRequiredInEntity = Prelude.Nothing,
      dataType = Prelude.Nothing,
      isTimeSeries = Prelude.Nothing
    }

-- | A Boolean value that specifies whether the property ID comes from an
-- external data store.
propertyDefinitionRequest_isExternalId :: Lens.Lens' PropertyDefinitionRequest (Prelude.Maybe Prelude.Bool)
propertyDefinitionRequest_isExternalId = Lens.lens (\PropertyDefinitionRequest' {isExternalId} -> isExternalId) (\s@PropertyDefinitionRequest' {} a -> s {isExternalId = a} :: PropertyDefinitionRequest)

-- | A Boolean value that specifies whether the property is stored
-- externally.
propertyDefinitionRequest_isStoredExternally :: Lens.Lens' PropertyDefinitionRequest (Prelude.Maybe Prelude.Bool)
propertyDefinitionRequest_isStoredExternally = Lens.lens (\PropertyDefinitionRequest' {isStoredExternally} -> isStoredExternally) (\s@PropertyDefinitionRequest' {} a -> s {isStoredExternally = a} :: PropertyDefinitionRequest)

-- | A mapping that specifies configuration information about the property.
-- Use this field to specify information that you read from and write to an
-- external source.
propertyDefinitionRequest_configuration :: Lens.Lens' PropertyDefinitionRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
propertyDefinitionRequest_configuration = Lens.lens (\PropertyDefinitionRequest' {configuration} -> configuration) (\s@PropertyDefinitionRequest' {} a -> s {configuration = a} :: PropertyDefinitionRequest) Prelude.. Lens.mapping Lens.coerced

-- | An object that contains the default value.
propertyDefinitionRequest_defaultValue :: Lens.Lens' PropertyDefinitionRequest (Prelude.Maybe DataValue)
propertyDefinitionRequest_defaultValue = Lens.lens (\PropertyDefinitionRequest' {defaultValue} -> defaultValue) (\s@PropertyDefinitionRequest' {} a -> s {defaultValue = a} :: PropertyDefinitionRequest)

-- | A Boolean value that specifies whether the property is required.
propertyDefinitionRequest_isRequiredInEntity :: Lens.Lens' PropertyDefinitionRequest (Prelude.Maybe Prelude.Bool)
propertyDefinitionRequest_isRequiredInEntity = Lens.lens (\PropertyDefinitionRequest' {isRequiredInEntity} -> isRequiredInEntity) (\s@PropertyDefinitionRequest' {} a -> s {isRequiredInEntity = a} :: PropertyDefinitionRequest)

-- | An object that contains information about the data type.
propertyDefinitionRequest_dataType :: Lens.Lens' PropertyDefinitionRequest (Prelude.Maybe DataType)
propertyDefinitionRequest_dataType = Lens.lens (\PropertyDefinitionRequest' {dataType} -> dataType) (\s@PropertyDefinitionRequest' {} a -> s {dataType = a} :: PropertyDefinitionRequest)

-- | A Boolean value that specifies whether the property consists of time
-- series data.
propertyDefinitionRequest_isTimeSeries :: Lens.Lens' PropertyDefinitionRequest (Prelude.Maybe Prelude.Bool)
propertyDefinitionRequest_isTimeSeries = Lens.lens (\PropertyDefinitionRequest' {isTimeSeries} -> isTimeSeries) (\s@PropertyDefinitionRequest' {} a -> s {isTimeSeries = a} :: PropertyDefinitionRequest)

instance Prelude.Hashable PropertyDefinitionRequest where
  hashWithSalt _salt PropertyDefinitionRequest' {..} =
    _salt `Prelude.hashWithSalt` isExternalId
      `Prelude.hashWithSalt` isStoredExternally
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` isRequiredInEntity
      `Prelude.hashWithSalt` dataType
      `Prelude.hashWithSalt` isTimeSeries

instance Prelude.NFData PropertyDefinitionRequest where
  rnf PropertyDefinitionRequest' {..} =
    Prelude.rnf isExternalId
      `Prelude.seq` Prelude.rnf isStoredExternally
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf isRequiredInEntity
      `Prelude.seq` Prelude.rnf dataType
      `Prelude.seq` Prelude.rnf isTimeSeries

instance Data.ToJSON PropertyDefinitionRequest where
  toJSON PropertyDefinitionRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("isExternalId" Data..=) Prelude.<$> isExternalId,
            ("isStoredExternally" Data..=)
              Prelude.<$> isStoredExternally,
            ("configuration" Data..=) Prelude.<$> configuration,
            ("defaultValue" Data..=) Prelude.<$> defaultValue,
            ("isRequiredInEntity" Data..=)
              Prelude.<$> isRequiredInEntity,
            ("dataType" Data..=) Prelude.<$> dataType,
            ("isTimeSeries" Data..=) Prelude.<$> isTimeSeries
          ]
      )
