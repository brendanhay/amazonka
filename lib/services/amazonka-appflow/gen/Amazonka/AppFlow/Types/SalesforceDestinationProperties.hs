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
-- Module      : Amazonka.AppFlow.Types.SalesforceDestinationProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.SalesforceDestinationProperties where

import Amazonka.AppFlow.Types.ErrorHandlingConfig
import Amazonka.AppFlow.Types.WriteOperationType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The properties that are applied when Salesforce is being used as a
-- destination.
--
-- /See:/ 'newSalesforceDestinationProperties' smart constructor.
data SalesforceDestinationProperties = SalesforceDestinationProperties'
  { -- | The settings that determine how Amazon AppFlow handles an error when
    -- placing data in the Salesforce destination. For example, this setting
    -- would determine if the flow should fail after one insertion error, or
    -- continue and attempt to insert every record regardless of the initial
    -- failure. @ErrorHandlingConfig@ is a part of the destination connector
    -- details.
    errorHandlingConfig :: Prelude.Maybe ErrorHandlingConfig,
    -- | The name of the field that Amazon AppFlow uses as an ID when performing
    -- a write operation such as update or delete.
    idFieldNames :: Prelude.Maybe [Prelude.Text],
    -- | This specifies the type of write operation to be performed in
    -- Salesforce. When the value is @UPSERT@, then @idFieldNames@ is required.
    writeOperationType :: Prelude.Maybe WriteOperationType,
    -- | The object specified in the Salesforce flow destination.
    object' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SalesforceDestinationProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorHandlingConfig', 'salesforceDestinationProperties_errorHandlingConfig' - The settings that determine how Amazon AppFlow handles an error when
-- placing data in the Salesforce destination. For example, this setting
-- would determine if the flow should fail after one insertion error, or
-- continue and attempt to insert every record regardless of the initial
-- failure. @ErrorHandlingConfig@ is a part of the destination connector
-- details.
--
-- 'idFieldNames', 'salesforceDestinationProperties_idFieldNames' - The name of the field that Amazon AppFlow uses as an ID when performing
-- a write operation such as update or delete.
--
-- 'writeOperationType', 'salesforceDestinationProperties_writeOperationType' - This specifies the type of write operation to be performed in
-- Salesforce. When the value is @UPSERT@, then @idFieldNames@ is required.
--
-- 'object'', 'salesforceDestinationProperties_object' - The object specified in the Salesforce flow destination.
newSalesforceDestinationProperties ::
  -- | 'object''
  Prelude.Text ->
  SalesforceDestinationProperties
newSalesforceDestinationProperties pObject_ =
  SalesforceDestinationProperties'
    { errorHandlingConfig =
        Prelude.Nothing,
      idFieldNames = Prelude.Nothing,
      writeOperationType = Prelude.Nothing,
      object' = pObject_
    }

-- | The settings that determine how Amazon AppFlow handles an error when
-- placing data in the Salesforce destination. For example, this setting
-- would determine if the flow should fail after one insertion error, or
-- continue and attempt to insert every record regardless of the initial
-- failure. @ErrorHandlingConfig@ is a part of the destination connector
-- details.
salesforceDestinationProperties_errorHandlingConfig :: Lens.Lens' SalesforceDestinationProperties (Prelude.Maybe ErrorHandlingConfig)
salesforceDestinationProperties_errorHandlingConfig = Lens.lens (\SalesforceDestinationProperties' {errorHandlingConfig} -> errorHandlingConfig) (\s@SalesforceDestinationProperties' {} a -> s {errorHandlingConfig = a} :: SalesforceDestinationProperties)

-- | The name of the field that Amazon AppFlow uses as an ID when performing
-- a write operation such as update or delete.
salesforceDestinationProperties_idFieldNames :: Lens.Lens' SalesforceDestinationProperties (Prelude.Maybe [Prelude.Text])
salesforceDestinationProperties_idFieldNames = Lens.lens (\SalesforceDestinationProperties' {idFieldNames} -> idFieldNames) (\s@SalesforceDestinationProperties' {} a -> s {idFieldNames = a} :: SalesforceDestinationProperties) Prelude.. Lens.mapping Lens.coerced

-- | This specifies the type of write operation to be performed in
-- Salesforce. When the value is @UPSERT@, then @idFieldNames@ is required.
salesforceDestinationProperties_writeOperationType :: Lens.Lens' SalesforceDestinationProperties (Prelude.Maybe WriteOperationType)
salesforceDestinationProperties_writeOperationType = Lens.lens (\SalesforceDestinationProperties' {writeOperationType} -> writeOperationType) (\s@SalesforceDestinationProperties' {} a -> s {writeOperationType = a} :: SalesforceDestinationProperties)

-- | The object specified in the Salesforce flow destination.
salesforceDestinationProperties_object :: Lens.Lens' SalesforceDestinationProperties Prelude.Text
salesforceDestinationProperties_object = Lens.lens (\SalesforceDestinationProperties' {object'} -> object') (\s@SalesforceDestinationProperties' {} a -> s {object' = a} :: SalesforceDestinationProperties)

instance
  Core.FromJSON
    SalesforceDestinationProperties
  where
  parseJSON =
    Core.withObject
      "SalesforceDestinationProperties"
      ( \x ->
          SalesforceDestinationProperties'
            Prelude.<$> (x Core..:? "errorHandlingConfig")
            Prelude.<*> (x Core..:? "idFieldNames" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "writeOperationType")
            Prelude.<*> (x Core..: "object")
      )

instance
  Prelude.Hashable
    SalesforceDestinationProperties
  where
  hashWithSalt
    _salt
    SalesforceDestinationProperties' {..} =
      _salt `Prelude.hashWithSalt` errorHandlingConfig
        `Prelude.hashWithSalt` idFieldNames
        `Prelude.hashWithSalt` writeOperationType
        `Prelude.hashWithSalt` object'

instance
  Prelude.NFData
    SalesforceDestinationProperties
  where
  rnf SalesforceDestinationProperties' {..} =
    Prelude.rnf errorHandlingConfig
      `Prelude.seq` Prelude.rnf idFieldNames
      `Prelude.seq` Prelude.rnf writeOperationType
      `Prelude.seq` Prelude.rnf object'

instance Core.ToJSON SalesforceDestinationProperties where
  toJSON SalesforceDestinationProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("errorHandlingConfig" Core..=)
              Prelude.<$> errorHandlingConfig,
            ("idFieldNames" Core..=) Prelude.<$> idFieldNames,
            ("writeOperationType" Core..=)
              Prelude.<$> writeOperationType,
            Prelude.Just ("object" Core..= object')
          ]
      )
