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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
  { -- | A mapping that specifies configuration information about the property.
    -- Use this field to specify information that you read from and write to an
    -- external source.
    configuration :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | An object that contains information about the data type.
    dataType :: Prelude.Maybe DataType,
    -- | An object that contains the default value.
    defaultValue :: Prelude.Maybe DataValue,
    -- | A friendly name for the property.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value that specifies whether the property ID comes from an
    -- external data store.
    isExternalId :: Prelude.Maybe Prelude.Bool,
    -- | A Boolean value that specifies whether the property is required.
    isRequiredInEntity :: Prelude.Maybe Prelude.Bool,
    -- | A Boolean value that specifies whether the property is stored
    -- externally.
    isStoredExternally :: Prelude.Maybe Prelude.Bool,
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
-- 'configuration', 'propertyDefinitionRequest_configuration' - A mapping that specifies configuration information about the property.
-- Use this field to specify information that you read from and write to an
-- external source.
--
-- 'dataType', 'propertyDefinitionRequest_dataType' - An object that contains information about the data type.
--
-- 'defaultValue', 'propertyDefinitionRequest_defaultValue' - An object that contains the default value.
--
-- 'displayName', 'propertyDefinitionRequest_displayName' - A friendly name for the property.
--
-- 'isExternalId', 'propertyDefinitionRequest_isExternalId' - A Boolean value that specifies whether the property ID comes from an
-- external data store.
--
-- 'isRequiredInEntity', 'propertyDefinitionRequest_isRequiredInEntity' - A Boolean value that specifies whether the property is required.
--
-- 'isStoredExternally', 'propertyDefinitionRequest_isStoredExternally' - A Boolean value that specifies whether the property is stored
-- externally.
--
-- 'isTimeSeries', 'propertyDefinitionRequest_isTimeSeries' - A Boolean value that specifies whether the property consists of time
-- series data.
newPropertyDefinitionRequest ::
  PropertyDefinitionRequest
newPropertyDefinitionRequest =
  PropertyDefinitionRequest'
    { configuration =
        Prelude.Nothing,
      dataType = Prelude.Nothing,
      defaultValue = Prelude.Nothing,
      displayName = Prelude.Nothing,
      isExternalId = Prelude.Nothing,
      isRequiredInEntity = Prelude.Nothing,
      isStoredExternally = Prelude.Nothing,
      isTimeSeries = Prelude.Nothing
    }

-- | A mapping that specifies configuration information about the property.
-- Use this field to specify information that you read from and write to an
-- external source.
propertyDefinitionRequest_configuration :: Lens.Lens' PropertyDefinitionRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
propertyDefinitionRequest_configuration = Lens.lens (\PropertyDefinitionRequest' {configuration} -> configuration) (\s@PropertyDefinitionRequest' {} a -> s {configuration = a} :: PropertyDefinitionRequest) Prelude.. Lens.mapping Lens.coerced

-- | An object that contains information about the data type.
propertyDefinitionRequest_dataType :: Lens.Lens' PropertyDefinitionRequest (Prelude.Maybe DataType)
propertyDefinitionRequest_dataType = Lens.lens (\PropertyDefinitionRequest' {dataType} -> dataType) (\s@PropertyDefinitionRequest' {} a -> s {dataType = a} :: PropertyDefinitionRequest)

-- | An object that contains the default value.
propertyDefinitionRequest_defaultValue :: Lens.Lens' PropertyDefinitionRequest (Prelude.Maybe DataValue)
propertyDefinitionRequest_defaultValue = Lens.lens (\PropertyDefinitionRequest' {defaultValue} -> defaultValue) (\s@PropertyDefinitionRequest' {} a -> s {defaultValue = a} :: PropertyDefinitionRequest)

-- | A friendly name for the property.
propertyDefinitionRequest_displayName :: Lens.Lens' PropertyDefinitionRequest (Prelude.Maybe Prelude.Text)
propertyDefinitionRequest_displayName = Lens.lens (\PropertyDefinitionRequest' {displayName} -> displayName) (\s@PropertyDefinitionRequest' {} a -> s {displayName = a} :: PropertyDefinitionRequest)

-- | A Boolean value that specifies whether the property ID comes from an
-- external data store.
propertyDefinitionRequest_isExternalId :: Lens.Lens' PropertyDefinitionRequest (Prelude.Maybe Prelude.Bool)
propertyDefinitionRequest_isExternalId = Lens.lens (\PropertyDefinitionRequest' {isExternalId} -> isExternalId) (\s@PropertyDefinitionRequest' {} a -> s {isExternalId = a} :: PropertyDefinitionRequest)

-- | A Boolean value that specifies whether the property is required.
propertyDefinitionRequest_isRequiredInEntity :: Lens.Lens' PropertyDefinitionRequest (Prelude.Maybe Prelude.Bool)
propertyDefinitionRequest_isRequiredInEntity = Lens.lens (\PropertyDefinitionRequest' {isRequiredInEntity} -> isRequiredInEntity) (\s@PropertyDefinitionRequest' {} a -> s {isRequiredInEntity = a} :: PropertyDefinitionRequest)

-- | A Boolean value that specifies whether the property is stored
-- externally.
propertyDefinitionRequest_isStoredExternally :: Lens.Lens' PropertyDefinitionRequest (Prelude.Maybe Prelude.Bool)
propertyDefinitionRequest_isStoredExternally = Lens.lens (\PropertyDefinitionRequest' {isStoredExternally} -> isStoredExternally) (\s@PropertyDefinitionRequest' {} a -> s {isStoredExternally = a} :: PropertyDefinitionRequest)

-- | A Boolean value that specifies whether the property consists of time
-- series data.
propertyDefinitionRequest_isTimeSeries :: Lens.Lens' PropertyDefinitionRequest (Prelude.Maybe Prelude.Bool)
propertyDefinitionRequest_isTimeSeries = Lens.lens (\PropertyDefinitionRequest' {isTimeSeries} -> isTimeSeries) (\s@PropertyDefinitionRequest' {} a -> s {isTimeSeries = a} :: PropertyDefinitionRequest)

instance Prelude.Hashable PropertyDefinitionRequest where
  hashWithSalt _salt PropertyDefinitionRequest' {..} =
    _salt
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` dataType
      `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` isExternalId
      `Prelude.hashWithSalt` isRequiredInEntity
      `Prelude.hashWithSalt` isStoredExternally
      `Prelude.hashWithSalt` isTimeSeries

instance Prelude.NFData PropertyDefinitionRequest where
  rnf PropertyDefinitionRequest' {..} =
    Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf dataType
      `Prelude.seq` Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf isExternalId
      `Prelude.seq` Prelude.rnf isRequiredInEntity
      `Prelude.seq` Prelude.rnf isStoredExternally
      `Prelude.seq` Prelude.rnf isTimeSeries

instance Data.ToJSON PropertyDefinitionRequest where
  toJSON PropertyDefinitionRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("configuration" Data..=) Prelude.<$> configuration,
            ("dataType" Data..=) Prelude.<$> dataType,
            ("defaultValue" Data..=) Prelude.<$> defaultValue,
            ("displayName" Data..=) Prelude.<$> displayName,
            ("isExternalId" Data..=) Prelude.<$> isExternalId,
            ("isRequiredInEntity" Data..=)
              Prelude.<$> isRequiredInEntity,
            ("isStoredExternally" Data..=)
              Prelude.<$> isStoredExternally,
            ("isTimeSeries" Data..=) Prelude.<$> isTimeSeries
          ]
      )
