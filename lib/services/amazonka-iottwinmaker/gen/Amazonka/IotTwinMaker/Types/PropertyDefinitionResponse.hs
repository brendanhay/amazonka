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
-- Module      : Amazonka.IotTwinMaker.Types.PropertyDefinitionResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.PropertyDefinitionResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.DataType
import Amazonka.IotTwinMaker.Types.DataValue
import qualified Amazonka.Prelude as Prelude

-- | An object that contains response data from a property definition
-- request.
--
-- /See:/ 'newPropertyDefinitionResponse' smart constructor.
data PropertyDefinitionResponse = PropertyDefinitionResponse'
  { -- | A mapping that specifies configuration information about the property.
    configuration :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | An object that contains the default value.
    defaultValue :: Prelude.Maybe DataValue,
    -- | A friendly name for the property.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | An object that contains information about the data type.
    dataType :: DataType,
    -- | A Boolean value that specifies whether the property consists of time
    -- series data.
    isTimeSeries :: Prelude.Bool,
    -- | A Boolean value that specifies whether the property is required in an
    -- entity.
    isRequiredInEntity :: Prelude.Bool,
    -- | A Boolean value that specifies whether the property ID comes from an
    -- external data store.
    isExternalId :: Prelude.Bool,
    -- | A Boolean value that specifies whether the property is stored
    -- externally.
    isStoredExternally :: Prelude.Bool,
    -- | A Boolean value that specifies whether the property definition is
    -- imported from an external data store.
    isImported :: Prelude.Bool,
    -- | A Boolean value that specifies whether the property definition can be
    -- updated.
    isFinal :: Prelude.Bool,
    -- | A Boolean value that specifies whether the property definition is
    -- inherited from a parent entity.
    isInherited :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PropertyDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuration', 'propertyDefinitionResponse_configuration' - A mapping that specifies configuration information about the property.
--
-- 'defaultValue', 'propertyDefinitionResponse_defaultValue' - An object that contains the default value.
--
-- 'displayName', 'propertyDefinitionResponse_displayName' - A friendly name for the property.
--
-- 'dataType', 'propertyDefinitionResponse_dataType' - An object that contains information about the data type.
--
-- 'isTimeSeries', 'propertyDefinitionResponse_isTimeSeries' - A Boolean value that specifies whether the property consists of time
-- series data.
--
-- 'isRequiredInEntity', 'propertyDefinitionResponse_isRequiredInEntity' - A Boolean value that specifies whether the property is required in an
-- entity.
--
-- 'isExternalId', 'propertyDefinitionResponse_isExternalId' - A Boolean value that specifies whether the property ID comes from an
-- external data store.
--
-- 'isStoredExternally', 'propertyDefinitionResponse_isStoredExternally' - A Boolean value that specifies whether the property is stored
-- externally.
--
-- 'isImported', 'propertyDefinitionResponse_isImported' - A Boolean value that specifies whether the property definition is
-- imported from an external data store.
--
-- 'isFinal', 'propertyDefinitionResponse_isFinal' - A Boolean value that specifies whether the property definition can be
-- updated.
--
-- 'isInherited', 'propertyDefinitionResponse_isInherited' - A Boolean value that specifies whether the property definition is
-- inherited from a parent entity.
newPropertyDefinitionResponse ::
  -- | 'dataType'
  DataType ->
  -- | 'isTimeSeries'
  Prelude.Bool ->
  -- | 'isRequiredInEntity'
  Prelude.Bool ->
  -- | 'isExternalId'
  Prelude.Bool ->
  -- | 'isStoredExternally'
  Prelude.Bool ->
  -- | 'isImported'
  Prelude.Bool ->
  -- | 'isFinal'
  Prelude.Bool ->
  -- | 'isInherited'
  Prelude.Bool ->
  PropertyDefinitionResponse
newPropertyDefinitionResponse
  pDataType_
  pIsTimeSeries_
  pIsRequiredInEntity_
  pIsExternalId_
  pIsStoredExternally_
  pIsImported_
  pIsFinal_
  pIsInherited_ =
    PropertyDefinitionResponse'
      { configuration =
          Prelude.Nothing,
        defaultValue = Prelude.Nothing,
        displayName = Prelude.Nothing,
        dataType = pDataType_,
        isTimeSeries = pIsTimeSeries_,
        isRequiredInEntity = pIsRequiredInEntity_,
        isExternalId = pIsExternalId_,
        isStoredExternally = pIsStoredExternally_,
        isImported = pIsImported_,
        isFinal = pIsFinal_,
        isInherited = pIsInherited_
      }

-- | A mapping that specifies configuration information about the property.
propertyDefinitionResponse_configuration :: Lens.Lens' PropertyDefinitionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
propertyDefinitionResponse_configuration = Lens.lens (\PropertyDefinitionResponse' {configuration} -> configuration) (\s@PropertyDefinitionResponse' {} a -> s {configuration = a} :: PropertyDefinitionResponse) Prelude.. Lens.mapping Lens.coerced

-- | An object that contains the default value.
propertyDefinitionResponse_defaultValue :: Lens.Lens' PropertyDefinitionResponse (Prelude.Maybe DataValue)
propertyDefinitionResponse_defaultValue = Lens.lens (\PropertyDefinitionResponse' {defaultValue} -> defaultValue) (\s@PropertyDefinitionResponse' {} a -> s {defaultValue = a} :: PropertyDefinitionResponse)

-- | A friendly name for the property.
propertyDefinitionResponse_displayName :: Lens.Lens' PropertyDefinitionResponse (Prelude.Maybe Prelude.Text)
propertyDefinitionResponse_displayName = Lens.lens (\PropertyDefinitionResponse' {displayName} -> displayName) (\s@PropertyDefinitionResponse' {} a -> s {displayName = a} :: PropertyDefinitionResponse)

-- | An object that contains information about the data type.
propertyDefinitionResponse_dataType :: Lens.Lens' PropertyDefinitionResponse DataType
propertyDefinitionResponse_dataType = Lens.lens (\PropertyDefinitionResponse' {dataType} -> dataType) (\s@PropertyDefinitionResponse' {} a -> s {dataType = a} :: PropertyDefinitionResponse)

-- | A Boolean value that specifies whether the property consists of time
-- series data.
propertyDefinitionResponse_isTimeSeries :: Lens.Lens' PropertyDefinitionResponse Prelude.Bool
propertyDefinitionResponse_isTimeSeries = Lens.lens (\PropertyDefinitionResponse' {isTimeSeries} -> isTimeSeries) (\s@PropertyDefinitionResponse' {} a -> s {isTimeSeries = a} :: PropertyDefinitionResponse)

-- | A Boolean value that specifies whether the property is required in an
-- entity.
propertyDefinitionResponse_isRequiredInEntity :: Lens.Lens' PropertyDefinitionResponse Prelude.Bool
propertyDefinitionResponse_isRequiredInEntity = Lens.lens (\PropertyDefinitionResponse' {isRequiredInEntity} -> isRequiredInEntity) (\s@PropertyDefinitionResponse' {} a -> s {isRequiredInEntity = a} :: PropertyDefinitionResponse)

-- | A Boolean value that specifies whether the property ID comes from an
-- external data store.
propertyDefinitionResponse_isExternalId :: Lens.Lens' PropertyDefinitionResponse Prelude.Bool
propertyDefinitionResponse_isExternalId = Lens.lens (\PropertyDefinitionResponse' {isExternalId} -> isExternalId) (\s@PropertyDefinitionResponse' {} a -> s {isExternalId = a} :: PropertyDefinitionResponse)

-- | A Boolean value that specifies whether the property is stored
-- externally.
propertyDefinitionResponse_isStoredExternally :: Lens.Lens' PropertyDefinitionResponse Prelude.Bool
propertyDefinitionResponse_isStoredExternally = Lens.lens (\PropertyDefinitionResponse' {isStoredExternally} -> isStoredExternally) (\s@PropertyDefinitionResponse' {} a -> s {isStoredExternally = a} :: PropertyDefinitionResponse)

-- | A Boolean value that specifies whether the property definition is
-- imported from an external data store.
propertyDefinitionResponse_isImported :: Lens.Lens' PropertyDefinitionResponse Prelude.Bool
propertyDefinitionResponse_isImported = Lens.lens (\PropertyDefinitionResponse' {isImported} -> isImported) (\s@PropertyDefinitionResponse' {} a -> s {isImported = a} :: PropertyDefinitionResponse)

-- | A Boolean value that specifies whether the property definition can be
-- updated.
propertyDefinitionResponse_isFinal :: Lens.Lens' PropertyDefinitionResponse Prelude.Bool
propertyDefinitionResponse_isFinal = Lens.lens (\PropertyDefinitionResponse' {isFinal} -> isFinal) (\s@PropertyDefinitionResponse' {} a -> s {isFinal = a} :: PropertyDefinitionResponse)

-- | A Boolean value that specifies whether the property definition is
-- inherited from a parent entity.
propertyDefinitionResponse_isInherited :: Lens.Lens' PropertyDefinitionResponse Prelude.Bool
propertyDefinitionResponse_isInherited = Lens.lens (\PropertyDefinitionResponse' {isInherited} -> isInherited) (\s@PropertyDefinitionResponse' {} a -> s {isInherited = a} :: PropertyDefinitionResponse)

instance Data.FromJSON PropertyDefinitionResponse where
  parseJSON =
    Data.withObject
      "PropertyDefinitionResponse"
      ( \x ->
          PropertyDefinitionResponse'
            Prelude.<$> (x Data..:? "configuration" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "defaultValue")
            Prelude.<*> (x Data..:? "displayName")
            Prelude.<*> (x Data..: "dataType")
            Prelude.<*> (x Data..: "isTimeSeries")
            Prelude.<*> (x Data..: "isRequiredInEntity")
            Prelude.<*> (x Data..: "isExternalId")
            Prelude.<*> (x Data..: "isStoredExternally")
            Prelude.<*> (x Data..: "isImported")
            Prelude.<*> (x Data..: "isFinal")
            Prelude.<*> (x Data..: "isInherited")
      )

instance Prelude.Hashable PropertyDefinitionResponse where
  hashWithSalt _salt PropertyDefinitionResponse' {..} =
    _salt `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` dataType
      `Prelude.hashWithSalt` isTimeSeries
      `Prelude.hashWithSalt` isRequiredInEntity
      `Prelude.hashWithSalt` isExternalId
      `Prelude.hashWithSalt` isStoredExternally
      `Prelude.hashWithSalt` isImported
      `Prelude.hashWithSalt` isFinal
      `Prelude.hashWithSalt` isInherited

instance Prelude.NFData PropertyDefinitionResponse where
  rnf PropertyDefinitionResponse' {..} =
    Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf dataType
      `Prelude.seq` Prelude.rnf isTimeSeries
      `Prelude.seq` Prelude.rnf isRequiredInEntity
      `Prelude.seq` Prelude.rnf isExternalId
      `Prelude.seq` Prelude.rnf isStoredExternally
      `Prelude.seq` Prelude.rnf isImported
      `Prelude.seq` Prelude.rnf isFinal
      `Prelude.seq` Prelude.rnf isInherited
