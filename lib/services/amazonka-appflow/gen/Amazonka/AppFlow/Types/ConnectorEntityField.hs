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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ConnectorEntityField where

import Amazonka.AppFlow.Types.DestinationFieldProperties
import Amazonka.AppFlow.Types.SourceFieldProperties
import Amazonka.AppFlow.Types.SupportedFieldTypeDetails
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the data model of a connector field. For example, for an
-- /account/ entity, the fields would be /account name/, /account ID/, and
-- so on.
--
-- /See:/ 'newConnectorEntityField' smart constructor.
data ConnectorEntityField = ConnectorEntityField'
  { -- | The label applied to a connector entity field.
    label :: Prelude.Maybe Prelude.Text,
    -- | A description of the connector entity field.
    description :: Prelude.Maybe Prelude.Text,
    -- | The properties applied to a field when the connector is being used as a
    -- destination.
    destinationProperties :: Prelude.Maybe DestinationFieldProperties,
    -- | Contains details regarding the supported @FieldType@, including the
    -- corresponding @filterOperators@ and @supportedValues@.
    supportedFieldTypeDetails :: Prelude.Maybe SupportedFieldTypeDetails,
    -- | The properties that can be applied to a field when the connector is
    -- being used as a source.
    sourceProperties :: Prelude.Maybe SourceFieldProperties,
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
-- 'label', 'connectorEntityField_label' - The label applied to a connector entity field.
--
-- 'description', 'connectorEntityField_description' - A description of the connector entity field.
--
-- 'destinationProperties', 'connectorEntityField_destinationProperties' - The properties applied to a field when the connector is being used as a
-- destination.
--
-- 'supportedFieldTypeDetails', 'connectorEntityField_supportedFieldTypeDetails' - Contains details regarding the supported @FieldType@, including the
-- corresponding @filterOperators@ and @supportedValues@.
--
-- 'sourceProperties', 'connectorEntityField_sourceProperties' - The properties that can be applied to a field when the connector is
-- being used as a source.
--
-- 'identifier', 'connectorEntityField_identifier' - The unique identifier of the connector field.
newConnectorEntityField ::
  -- | 'identifier'
  Prelude.Text ->
  ConnectorEntityField
newConnectorEntityField pIdentifier_ =
  ConnectorEntityField'
    { label = Prelude.Nothing,
      description = Prelude.Nothing,
      destinationProperties = Prelude.Nothing,
      supportedFieldTypeDetails = Prelude.Nothing,
      sourceProperties = Prelude.Nothing,
      identifier = pIdentifier_
    }

-- | The label applied to a connector entity field.
connectorEntityField_label :: Lens.Lens' ConnectorEntityField (Prelude.Maybe Prelude.Text)
connectorEntityField_label = Lens.lens (\ConnectorEntityField' {label} -> label) (\s@ConnectorEntityField' {} a -> s {label = a} :: ConnectorEntityField)

-- | A description of the connector entity field.
connectorEntityField_description :: Lens.Lens' ConnectorEntityField (Prelude.Maybe Prelude.Text)
connectorEntityField_description = Lens.lens (\ConnectorEntityField' {description} -> description) (\s@ConnectorEntityField' {} a -> s {description = a} :: ConnectorEntityField)

-- | The properties applied to a field when the connector is being used as a
-- destination.
connectorEntityField_destinationProperties :: Lens.Lens' ConnectorEntityField (Prelude.Maybe DestinationFieldProperties)
connectorEntityField_destinationProperties = Lens.lens (\ConnectorEntityField' {destinationProperties} -> destinationProperties) (\s@ConnectorEntityField' {} a -> s {destinationProperties = a} :: ConnectorEntityField)

-- | Contains details regarding the supported @FieldType@, including the
-- corresponding @filterOperators@ and @supportedValues@.
connectorEntityField_supportedFieldTypeDetails :: Lens.Lens' ConnectorEntityField (Prelude.Maybe SupportedFieldTypeDetails)
connectorEntityField_supportedFieldTypeDetails = Lens.lens (\ConnectorEntityField' {supportedFieldTypeDetails} -> supportedFieldTypeDetails) (\s@ConnectorEntityField' {} a -> s {supportedFieldTypeDetails = a} :: ConnectorEntityField)

-- | The properties that can be applied to a field when the connector is
-- being used as a source.
connectorEntityField_sourceProperties :: Lens.Lens' ConnectorEntityField (Prelude.Maybe SourceFieldProperties)
connectorEntityField_sourceProperties = Lens.lens (\ConnectorEntityField' {sourceProperties} -> sourceProperties) (\s@ConnectorEntityField' {} a -> s {sourceProperties = a} :: ConnectorEntityField)

-- | The unique identifier of the connector field.
connectorEntityField_identifier :: Lens.Lens' ConnectorEntityField Prelude.Text
connectorEntityField_identifier = Lens.lens (\ConnectorEntityField' {identifier} -> identifier) (\s@ConnectorEntityField' {} a -> s {identifier = a} :: ConnectorEntityField)

instance Core.FromJSON ConnectorEntityField where
  parseJSON =
    Core.withObject
      "ConnectorEntityField"
      ( \x ->
          ConnectorEntityField'
            Prelude.<$> (x Core..:? "label")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "destinationProperties")
            Prelude.<*> (x Core..:? "supportedFieldTypeDetails")
            Prelude.<*> (x Core..:? "sourceProperties")
            Prelude.<*> (x Core..: "identifier")
      )

instance Prelude.Hashable ConnectorEntityField where
  hashWithSalt _salt ConnectorEntityField' {..} =
    _salt `Prelude.hashWithSalt` label
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` destinationProperties
      `Prelude.hashWithSalt` supportedFieldTypeDetails
      `Prelude.hashWithSalt` sourceProperties
      `Prelude.hashWithSalt` identifier

instance Prelude.NFData ConnectorEntityField where
  rnf ConnectorEntityField' {..} =
    Prelude.rnf label
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf destinationProperties
      `Prelude.seq` Prelude.rnf supportedFieldTypeDetails
      `Prelude.seq` Prelude.rnf sourceProperties
      `Prelude.seq` Prelude.rnf identifier
