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
-- Module      : Amazonka.AppFlow.Types.ConnectorEntityField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ConnectorEntityField where

import Amazonka.AppFlow.Types.DestinationFieldProperties
import Amazonka.AppFlow.Types.SourceFieldProperties
import Amazonka.AppFlow.Types.SupportedFieldTypeDetails
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the data model of a connector field. For example, for an
-- /account/ entity, the fields would be /account name/, /account ID/, and
-- so on.
--
-- /See:/ 'newConnectorEntityField' smart constructor.
data ConnectorEntityField = ConnectorEntityField'
  { -- | A map that has specific properties related to the ConnectorEntityField.
    customProperties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Default value that can be assigned to this field.
    defaultValue :: Prelude.Maybe Prelude.Text,
    -- | A description of the connector entity field.
    description :: Prelude.Maybe Prelude.Text,
    -- | The properties applied to a field when the connector is being used as a
    -- destination.
    destinationProperties :: Prelude.Maybe DestinationFieldProperties,
    -- | Booelan value that indicates whether this field is deprecated or not.
    isDeprecated :: Prelude.Maybe Prelude.Bool,
    -- | Booelan value that indicates whether this field can be used as a primary
    -- key.
    isPrimaryKey :: Prelude.Maybe Prelude.Bool,
    -- | The label applied to a connector entity field.
    label :: Prelude.Maybe Prelude.Text,
    -- | The parent identifier of the connector field.
    parentIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The properties that can be applied to a field when the connector is
    -- being used as a source.
    sourceProperties :: Prelude.Maybe SourceFieldProperties,
    -- | Contains details regarding the supported @FieldType@, including the
    -- corresponding @filterOperators@ and @supportedValues@.
    supportedFieldTypeDetails :: Prelude.Maybe SupportedFieldTypeDetails,
    -- | The unique identifier of the connector field.
    identifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectorEntityField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customProperties', 'connectorEntityField_customProperties' - A map that has specific properties related to the ConnectorEntityField.
--
-- 'defaultValue', 'connectorEntityField_defaultValue' - Default value that can be assigned to this field.
--
-- 'description', 'connectorEntityField_description' - A description of the connector entity field.
--
-- 'destinationProperties', 'connectorEntityField_destinationProperties' - The properties applied to a field when the connector is being used as a
-- destination.
--
-- 'isDeprecated', 'connectorEntityField_isDeprecated' - Booelan value that indicates whether this field is deprecated or not.
--
-- 'isPrimaryKey', 'connectorEntityField_isPrimaryKey' - Booelan value that indicates whether this field can be used as a primary
-- key.
--
-- 'label', 'connectorEntityField_label' - The label applied to a connector entity field.
--
-- 'parentIdentifier', 'connectorEntityField_parentIdentifier' - The parent identifier of the connector field.
--
-- 'sourceProperties', 'connectorEntityField_sourceProperties' - The properties that can be applied to a field when the connector is
-- being used as a source.
--
-- 'supportedFieldTypeDetails', 'connectorEntityField_supportedFieldTypeDetails' - Contains details regarding the supported @FieldType@, including the
-- corresponding @filterOperators@ and @supportedValues@.
--
-- 'identifier', 'connectorEntityField_identifier' - The unique identifier of the connector field.
newConnectorEntityField ::
  -- | 'identifier'
  Prelude.Text ->
  ConnectorEntityField
newConnectorEntityField pIdentifier_ =
  ConnectorEntityField'
    { customProperties =
        Prelude.Nothing,
      defaultValue = Prelude.Nothing,
      description = Prelude.Nothing,
      destinationProperties = Prelude.Nothing,
      isDeprecated = Prelude.Nothing,
      isPrimaryKey = Prelude.Nothing,
      label = Prelude.Nothing,
      parentIdentifier = Prelude.Nothing,
      sourceProperties = Prelude.Nothing,
      supportedFieldTypeDetails = Prelude.Nothing,
      identifier = pIdentifier_
    }

-- | A map that has specific properties related to the ConnectorEntityField.
connectorEntityField_customProperties :: Lens.Lens' ConnectorEntityField (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
connectorEntityField_customProperties = Lens.lens (\ConnectorEntityField' {customProperties} -> customProperties) (\s@ConnectorEntityField' {} a -> s {customProperties = a} :: ConnectorEntityField) Prelude.. Lens.mapping Lens.coerced

-- | Default value that can be assigned to this field.
connectorEntityField_defaultValue :: Lens.Lens' ConnectorEntityField (Prelude.Maybe Prelude.Text)
connectorEntityField_defaultValue = Lens.lens (\ConnectorEntityField' {defaultValue} -> defaultValue) (\s@ConnectorEntityField' {} a -> s {defaultValue = a} :: ConnectorEntityField)

-- | A description of the connector entity field.
connectorEntityField_description :: Lens.Lens' ConnectorEntityField (Prelude.Maybe Prelude.Text)
connectorEntityField_description = Lens.lens (\ConnectorEntityField' {description} -> description) (\s@ConnectorEntityField' {} a -> s {description = a} :: ConnectorEntityField)

-- | The properties applied to a field when the connector is being used as a
-- destination.
connectorEntityField_destinationProperties :: Lens.Lens' ConnectorEntityField (Prelude.Maybe DestinationFieldProperties)
connectorEntityField_destinationProperties = Lens.lens (\ConnectorEntityField' {destinationProperties} -> destinationProperties) (\s@ConnectorEntityField' {} a -> s {destinationProperties = a} :: ConnectorEntityField)

-- | Booelan value that indicates whether this field is deprecated or not.
connectorEntityField_isDeprecated :: Lens.Lens' ConnectorEntityField (Prelude.Maybe Prelude.Bool)
connectorEntityField_isDeprecated = Lens.lens (\ConnectorEntityField' {isDeprecated} -> isDeprecated) (\s@ConnectorEntityField' {} a -> s {isDeprecated = a} :: ConnectorEntityField)

-- | Booelan value that indicates whether this field can be used as a primary
-- key.
connectorEntityField_isPrimaryKey :: Lens.Lens' ConnectorEntityField (Prelude.Maybe Prelude.Bool)
connectorEntityField_isPrimaryKey = Lens.lens (\ConnectorEntityField' {isPrimaryKey} -> isPrimaryKey) (\s@ConnectorEntityField' {} a -> s {isPrimaryKey = a} :: ConnectorEntityField)

-- | The label applied to a connector entity field.
connectorEntityField_label :: Lens.Lens' ConnectorEntityField (Prelude.Maybe Prelude.Text)
connectorEntityField_label = Lens.lens (\ConnectorEntityField' {label} -> label) (\s@ConnectorEntityField' {} a -> s {label = a} :: ConnectorEntityField)

-- | The parent identifier of the connector field.
connectorEntityField_parentIdentifier :: Lens.Lens' ConnectorEntityField (Prelude.Maybe Prelude.Text)
connectorEntityField_parentIdentifier = Lens.lens (\ConnectorEntityField' {parentIdentifier} -> parentIdentifier) (\s@ConnectorEntityField' {} a -> s {parentIdentifier = a} :: ConnectorEntityField)

-- | The properties that can be applied to a field when the connector is
-- being used as a source.
connectorEntityField_sourceProperties :: Lens.Lens' ConnectorEntityField (Prelude.Maybe SourceFieldProperties)
connectorEntityField_sourceProperties = Lens.lens (\ConnectorEntityField' {sourceProperties} -> sourceProperties) (\s@ConnectorEntityField' {} a -> s {sourceProperties = a} :: ConnectorEntityField)

-- | Contains details regarding the supported @FieldType@, including the
-- corresponding @filterOperators@ and @supportedValues@.
connectorEntityField_supportedFieldTypeDetails :: Lens.Lens' ConnectorEntityField (Prelude.Maybe SupportedFieldTypeDetails)
connectorEntityField_supportedFieldTypeDetails = Lens.lens (\ConnectorEntityField' {supportedFieldTypeDetails} -> supportedFieldTypeDetails) (\s@ConnectorEntityField' {} a -> s {supportedFieldTypeDetails = a} :: ConnectorEntityField)

-- | The unique identifier of the connector field.
connectorEntityField_identifier :: Lens.Lens' ConnectorEntityField Prelude.Text
connectorEntityField_identifier = Lens.lens (\ConnectorEntityField' {identifier} -> identifier) (\s@ConnectorEntityField' {} a -> s {identifier = a} :: ConnectorEntityField)

instance Data.FromJSON ConnectorEntityField where
  parseJSON =
    Data.withObject
      "ConnectorEntityField"
      ( \x ->
          ConnectorEntityField'
            Prelude.<$> ( x
                            Data..:? "customProperties"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "defaultValue")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "destinationProperties")
            Prelude.<*> (x Data..:? "isDeprecated")
            Prelude.<*> (x Data..:? "isPrimaryKey")
            Prelude.<*> (x Data..:? "label")
            Prelude.<*> (x Data..:? "parentIdentifier")
            Prelude.<*> (x Data..:? "sourceProperties")
            Prelude.<*> (x Data..:? "supportedFieldTypeDetails")
            Prelude.<*> (x Data..: "identifier")
      )

instance Prelude.Hashable ConnectorEntityField where
  hashWithSalt _salt ConnectorEntityField' {..} =
    _salt
      `Prelude.hashWithSalt` customProperties
      `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` destinationProperties
      `Prelude.hashWithSalt` isDeprecated
      `Prelude.hashWithSalt` isPrimaryKey
      `Prelude.hashWithSalt` label
      `Prelude.hashWithSalt` parentIdentifier
      `Prelude.hashWithSalt` sourceProperties
      `Prelude.hashWithSalt` supportedFieldTypeDetails
      `Prelude.hashWithSalt` identifier

instance Prelude.NFData ConnectorEntityField where
  rnf ConnectorEntityField' {..} =
    Prelude.rnf customProperties
      `Prelude.seq` Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf destinationProperties
      `Prelude.seq` Prelude.rnf isDeprecated
      `Prelude.seq` Prelude.rnf isPrimaryKey
      `Prelude.seq` Prelude.rnf label
      `Prelude.seq` Prelude.rnf parentIdentifier
      `Prelude.seq` Prelude.rnf sourceProperties
      `Prelude.seq` Prelude.rnf supportedFieldTypeDetails
      `Prelude.seq` Prelude.rnf identifier
