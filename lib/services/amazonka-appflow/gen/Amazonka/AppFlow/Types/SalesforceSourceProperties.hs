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
-- Module      : Amazonka.AppFlow.Types.SalesforceSourceProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.SalesforceSourceProperties where

import Amazonka.AppFlow.Types.SalesforceDataTransferApi
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The properties that are applied when Salesforce is being used as a
-- source.
--
-- /See:/ 'newSalesforceSourceProperties' smart constructor.
data SalesforceSourceProperties = SalesforceSourceProperties'
  { -- | Indicates whether Amazon AppFlow includes deleted files in the flow run.
    includeDeletedRecords :: Prelude.Maybe Prelude.Bool,
    -- | Specifies which Salesforce API is used by Amazon AppFlow when your flow
    -- transfers data from Salesforce.
    --
    -- [AUTOMATIC]
    --     The default. Amazon AppFlow selects which API to use based on the
    --     number of records that your flow transfers from Salesforce. If your
    --     flow transfers fewer than 1,000,000 records, Amazon AppFlow uses
    --     Salesforce REST API. If your flow transfers 1,000,000 records or
    --     more, Amazon AppFlow uses Salesforce Bulk API 2.0.
    --
    --     Each of these Salesforce APIs structures data differently. If Amazon
    --     AppFlow selects the API automatically, be aware that, for recurring
    --     flows, the data output might vary from one flow run to the next. For
    --     example, if a flow runs daily, it might use REST API on one day to
    --     transfer 900,000 records, and it might use Bulk API 2.0 on the next
    --     day to transfer 1,100,000 records. For each of these flow runs, the
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
    --     vary large set of data, it might fail wituh a timed out error.
    dataTransferApi :: Prelude.Maybe SalesforceDataTransferApi,
    -- | The flag that enables dynamic fetching of new (recently added) fields in
    -- the Salesforce objects while running a flow.
    enableDynamicFieldUpdate :: Prelude.Maybe Prelude.Bool,
    -- | The object specified in the Salesforce flow source.
    object' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SalesforceSourceProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeDeletedRecords', 'salesforceSourceProperties_includeDeletedRecords' - Indicates whether Amazon AppFlow includes deleted files in the flow run.
--
-- 'dataTransferApi', 'salesforceSourceProperties_dataTransferApi' - Specifies which Salesforce API is used by Amazon AppFlow when your flow
-- transfers data from Salesforce.
--
-- [AUTOMATIC]
--     The default. Amazon AppFlow selects which API to use based on the
--     number of records that your flow transfers from Salesforce. If your
--     flow transfers fewer than 1,000,000 records, Amazon AppFlow uses
--     Salesforce REST API. If your flow transfers 1,000,000 records or
--     more, Amazon AppFlow uses Salesforce Bulk API 2.0.
--
--     Each of these Salesforce APIs structures data differently. If Amazon
--     AppFlow selects the API automatically, be aware that, for recurring
--     flows, the data output might vary from one flow run to the next. For
--     example, if a flow runs daily, it might use REST API on one day to
--     transfer 900,000 records, and it might use Bulk API 2.0 on the next
--     day to transfer 1,100,000 records. For each of these flow runs, the
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
--     vary large set of data, it might fail wituh a timed out error.
--
-- 'enableDynamicFieldUpdate', 'salesforceSourceProperties_enableDynamicFieldUpdate' - The flag that enables dynamic fetching of new (recently added) fields in
-- the Salesforce objects while running a flow.
--
-- 'object'', 'salesforceSourceProperties_object' - The object specified in the Salesforce flow source.
newSalesforceSourceProperties ::
  -- | 'object''
  Prelude.Text ->
  SalesforceSourceProperties
newSalesforceSourceProperties pObject_ =
  SalesforceSourceProperties'
    { includeDeletedRecords =
        Prelude.Nothing,
      dataTransferApi = Prelude.Nothing,
      enableDynamicFieldUpdate = Prelude.Nothing,
      object' = pObject_
    }

-- | Indicates whether Amazon AppFlow includes deleted files in the flow run.
salesforceSourceProperties_includeDeletedRecords :: Lens.Lens' SalesforceSourceProperties (Prelude.Maybe Prelude.Bool)
salesforceSourceProperties_includeDeletedRecords = Lens.lens (\SalesforceSourceProperties' {includeDeletedRecords} -> includeDeletedRecords) (\s@SalesforceSourceProperties' {} a -> s {includeDeletedRecords = a} :: SalesforceSourceProperties)

-- | Specifies which Salesforce API is used by Amazon AppFlow when your flow
-- transfers data from Salesforce.
--
-- [AUTOMATIC]
--     The default. Amazon AppFlow selects which API to use based on the
--     number of records that your flow transfers from Salesforce. If your
--     flow transfers fewer than 1,000,000 records, Amazon AppFlow uses
--     Salesforce REST API. If your flow transfers 1,000,000 records or
--     more, Amazon AppFlow uses Salesforce Bulk API 2.0.
--
--     Each of these Salesforce APIs structures data differently. If Amazon
--     AppFlow selects the API automatically, be aware that, for recurring
--     flows, the data output might vary from one flow run to the next. For
--     example, if a flow runs daily, it might use REST API on one day to
--     transfer 900,000 records, and it might use Bulk API 2.0 on the next
--     day to transfer 1,100,000 records. For each of these flow runs, the
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
--     vary large set of data, it might fail wituh a timed out error.
salesforceSourceProperties_dataTransferApi :: Lens.Lens' SalesforceSourceProperties (Prelude.Maybe SalesforceDataTransferApi)
salesforceSourceProperties_dataTransferApi = Lens.lens (\SalesforceSourceProperties' {dataTransferApi} -> dataTransferApi) (\s@SalesforceSourceProperties' {} a -> s {dataTransferApi = a} :: SalesforceSourceProperties)

-- | The flag that enables dynamic fetching of new (recently added) fields in
-- the Salesforce objects while running a flow.
salesforceSourceProperties_enableDynamicFieldUpdate :: Lens.Lens' SalesforceSourceProperties (Prelude.Maybe Prelude.Bool)
salesforceSourceProperties_enableDynamicFieldUpdate = Lens.lens (\SalesforceSourceProperties' {enableDynamicFieldUpdate} -> enableDynamicFieldUpdate) (\s@SalesforceSourceProperties' {} a -> s {enableDynamicFieldUpdate = a} :: SalesforceSourceProperties)

-- | The object specified in the Salesforce flow source.
salesforceSourceProperties_object :: Lens.Lens' SalesforceSourceProperties Prelude.Text
salesforceSourceProperties_object = Lens.lens (\SalesforceSourceProperties' {object'} -> object') (\s@SalesforceSourceProperties' {} a -> s {object' = a} :: SalesforceSourceProperties)

instance Data.FromJSON SalesforceSourceProperties where
  parseJSON =
    Data.withObject
      "SalesforceSourceProperties"
      ( \x ->
          SalesforceSourceProperties'
            Prelude.<$> (x Data..:? "includeDeletedRecords")
            Prelude.<*> (x Data..:? "dataTransferApi")
            Prelude.<*> (x Data..:? "enableDynamicFieldUpdate")
            Prelude.<*> (x Data..: "object")
      )

instance Prelude.Hashable SalesforceSourceProperties where
  hashWithSalt _salt SalesforceSourceProperties' {..} =
    _salt `Prelude.hashWithSalt` includeDeletedRecords
      `Prelude.hashWithSalt` dataTransferApi
      `Prelude.hashWithSalt` enableDynamicFieldUpdate
      `Prelude.hashWithSalt` object'

instance Prelude.NFData SalesforceSourceProperties where
  rnf SalesforceSourceProperties' {..} =
    Prelude.rnf includeDeletedRecords
      `Prelude.seq` Prelude.rnf dataTransferApi
      `Prelude.seq` Prelude.rnf enableDynamicFieldUpdate
      `Prelude.seq` Prelude.rnf object'

instance Data.ToJSON SalesforceSourceProperties where
  toJSON SalesforceSourceProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("includeDeletedRecords" Data..=)
              Prelude.<$> includeDeletedRecords,
            ("dataTransferApi" Data..=)
              Prelude.<$> dataTransferApi,
            ("enableDynamicFieldUpdate" Data..=)
              Prelude.<$> enableDynamicFieldUpdate,
            Prelude.Just ("object" Data..= object')
          ]
      )
