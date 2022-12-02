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
import Amazonka.AppFlow.Types.SalesforceDataTransferApi
import Amazonka.AppFlow.Types.WriteOperationType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
    -- | Specifies which Salesforce API is used by Amazon AppFlow when your flow
    -- transfers data to Salesforce.
    --
    -- [AUTOMATIC]
    --     The default. Amazon AppFlow selects which API to use based on the
    --     number of records that your flow transfers to Salesforce. If your
    --     flow transfers fewer than 1,000 records, Amazon AppFlow uses
    --     Salesforce REST API. If your flow transfers 1,000 records or more,
    --     Amazon AppFlow uses Salesforce Bulk API 2.0.
    --
    --     Each of these Salesforce APIs structures data differently. If Amazon
    --     AppFlow selects the API automatically, be aware that, for recurring
    --     flows, the data output might vary from one flow run to the next. For
    --     example, if a flow runs daily, it might use REST API on one day to
    --     transfer 900 records, and it might use Bulk API 2.0 on the next day
    --     to transfer 1,100 records. For each of these flow runs, the
    --     respective Salesforce API formats the data differently. Some of the
    --     differences include how dates are formatted and null values are
    --     represented. Also, Bulk API 2.0 doesn\'t transfer Salesforce
    --     compound fields.
    --
    --     By choosing this option, you optimize flow performance for both
    --     small and large data transfers, but the tradeoff is inconsistent
    --     formatting in the output.
    --
    -- [BULKV2]
    --     Amazon AppFlow uses only Salesforce Bulk API 2.0. This API runs
    --     asynchronous data transfers, and it\'s optimal for large sets of
    --     data. By choosing this option, you ensure that your flow writes
    --     consistent output, but you optimize performance only for large data
    --     transfers.
    --
    --     Note that Bulk API 2.0 does not transfer Salesforce compound fields.
    --
    -- [REST_SYNC]
    --     Amazon AppFlow uses only Salesforce REST API. By choosing this
    --     option, you ensure that your flow writes consistent output, but you
    --     decrease performance for large data transfers that are better suited
    --     for Bulk API 2.0. In some cases, if your flow attempts to transfer a
    --     vary large set of data, it might fail with a timed out error.
    dataTransferApi :: Prelude.Maybe SalesforceDataTransferApi,
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
-- 'dataTransferApi', 'salesforceDestinationProperties_dataTransferApi' - Specifies which Salesforce API is used by Amazon AppFlow when your flow
-- transfers data to Salesforce.
--
-- [AUTOMATIC]
--     The default. Amazon AppFlow selects which API to use based on the
--     number of records that your flow transfers to Salesforce. If your
--     flow transfers fewer than 1,000 records, Amazon AppFlow uses
--     Salesforce REST API. If your flow transfers 1,000 records or more,
--     Amazon AppFlow uses Salesforce Bulk API 2.0.
--
--     Each of these Salesforce APIs structures data differently. If Amazon
--     AppFlow selects the API automatically, be aware that, for recurring
--     flows, the data output might vary from one flow run to the next. For
--     example, if a flow runs daily, it might use REST API on one day to
--     transfer 900 records, and it might use Bulk API 2.0 on the next day
--     to transfer 1,100 records. For each of these flow runs, the
--     respective Salesforce API formats the data differently. Some of the
--     differences include how dates are formatted and null values are
--     represented. Also, Bulk API 2.0 doesn\'t transfer Salesforce
--     compound fields.
--
--     By choosing this option, you optimize flow performance for both
--     small and large data transfers, but the tradeoff is inconsistent
--     formatting in the output.
--
-- [BULKV2]
--     Amazon AppFlow uses only Salesforce Bulk API 2.0. This API runs
--     asynchronous data transfers, and it\'s optimal for large sets of
--     data. By choosing this option, you ensure that your flow writes
--     consistent output, but you optimize performance only for large data
--     transfers.
--
--     Note that Bulk API 2.0 does not transfer Salesforce compound fields.
--
-- [REST_SYNC]
--     Amazon AppFlow uses only Salesforce REST API. By choosing this
--     option, you ensure that your flow writes consistent output, but you
--     decrease performance for large data transfers that are better suited
--     for Bulk API 2.0. In some cases, if your flow attempts to transfer a
--     vary large set of data, it might fail with a timed out error.
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
      dataTransferApi = Prelude.Nothing,
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

-- | Specifies which Salesforce API is used by Amazon AppFlow when your flow
-- transfers data to Salesforce.
--
-- [AUTOMATIC]
--     The default. Amazon AppFlow selects which API to use based on the
--     number of records that your flow transfers to Salesforce. If your
--     flow transfers fewer than 1,000 records, Amazon AppFlow uses
--     Salesforce REST API. If your flow transfers 1,000 records or more,
--     Amazon AppFlow uses Salesforce Bulk API 2.0.
--
--     Each of these Salesforce APIs structures data differently. If Amazon
--     AppFlow selects the API automatically, be aware that, for recurring
--     flows, the data output might vary from one flow run to the next. For
--     example, if a flow runs daily, it might use REST API on one day to
--     transfer 900 records, and it might use Bulk API 2.0 on the next day
--     to transfer 1,100 records. For each of these flow runs, the
--     respective Salesforce API formats the data differently. Some of the
--     differences include how dates are formatted and null values are
--     represented. Also, Bulk API 2.0 doesn\'t transfer Salesforce
--     compound fields.
--
--     By choosing this option, you optimize flow performance for both
--     small and large data transfers, but the tradeoff is inconsistent
--     formatting in the output.
--
-- [BULKV2]
--     Amazon AppFlow uses only Salesforce Bulk API 2.0. This API runs
--     asynchronous data transfers, and it\'s optimal for large sets of
--     data. By choosing this option, you ensure that your flow writes
--     consistent output, but you optimize performance only for large data
--     transfers.
--
--     Note that Bulk API 2.0 does not transfer Salesforce compound fields.
--
-- [REST_SYNC]
--     Amazon AppFlow uses only Salesforce REST API. By choosing this
--     option, you ensure that your flow writes consistent output, but you
--     decrease performance for large data transfers that are better suited
--     for Bulk API 2.0. In some cases, if your flow attempts to transfer a
--     vary large set of data, it might fail with a timed out error.
salesforceDestinationProperties_dataTransferApi :: Lens.Lens' SalesforceDestinationProperties (Prelude.Maybe SalesforceDataTransferApi)
salesforceDestinationProperties_dataTransferApi = Lens.lens (\SalesforceDestinationProperties' {dataTransferApi} -> dataTransferApi) (\s@SalesforceDestinationProperties' {} a -> s {dataTransferApi = a} :: SalesforceDestinationProperties)

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
  Data.FromJSON
    SalesforceDestinationProperties
  where
  parseJSON =
    Data.withObject
      "SalesforceDestinationProperties"
      ( \x ->
          SalesforceDestinationProperties'
            Prelude.<$> (x Data..:? "errorHandlingConfig")
            Prelude.<*> (x Data..:? "dataTransferApi")
            Prelude.<*> (x Data..:? "idFieldNames" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "writeOperationType")
            Prelude.<*> (x Data..: "object")
      )

instance
  Prelude.Hashable
    SalesforceDestinationProperties
  where
  hashWithSalt
    _salt
    SalesforceDestinationProperties' {..} =
      _salt `Prelude.hashWithSalt` errorHandlingConfig
        `Prelude.hashWithSalt` dataTransferApi
        `Prelude.hashWithSalt` idFieldNames
        `Prelude.hashWithSalt` writeOperationType
        `Prelude.hashWithSalt` object'

instance
  Prelude.NFData
    SalesforceDestinationProperties
  where
  rnf SalesforceDestinationProperties' {..} =
    Prelude.rnf errorHandlingConfig
      `Prelude.seq` Prelude.rnf dataTransferApi
      `Prelude.seq` Prelude.rnf idFieldNames
      `Prelude.seq` Prelude.rnf writeOperationType
      `Prelude.seq` Prelude.rnf object'

instance Data.ToJSON SalesforceDestinationProperties where
  toJSON SalesforceDestinationProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("errorHandlingConfig" Data..=)
              Prelude.<$> errorHandlingConfig,
            ("dataTransferApi" Data..=)
              Prelude.<$> dataTransferApi,
            ("idFieldNames" Data..=) Prelude.<$> idFieldNames,
            ("writeOperationType" Data..=)
              Prelude.<$> writeOperationType,
            Prelude.Just ("object" Data..= object')
          ]
      )
