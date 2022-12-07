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
-- Module      : Amazonka.LakeFormation.Types.QueryPlanningContext
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.QueryPlanningContext where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure containing information about the query plan.
--
-- /See:/ 'newQueryPlanningContext' smart constructor.
data QueryPlanningContext = QueryPlanningContext'
  { -- | The time as of when to read the table contents. If not set, the most
    -- recent transaction commit time will be used. Cannot be specified along
    -- with @TransactionId@.
    queryAsOfTime :: Prelude.Maybe Data.POSIX,
    -- | A map consisting of key-value pairs.
    queryParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ID of the Data Catalog where the partition in question resides. If
    -- none is provided, the Amazon Web Services account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The transaction ID at which to read the table contents. If this
    -- transaction is not committed, the read will be treated as part of that
    -- transaction and will see its writes. If this transaction has aborted, an
    -- error will be returned. If not set, defaults to the most recent
    -- committed transaction. Cannot be specified along with @QueryAsOfTime@.
    transactionId :: Prelude.Maybe Prelude.Text,
    -- | The database containing the table.
    databaseName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryPlanningContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryAsOfTime', 'queryPlanningContext_queryAsOfTime' - The time as of when to read the table contents. If not set, the most
-- recent transaction commit time will be used. Cannot be specified along
-- with @TransactionId@.
--
-- 'queryParameters', 'queryPlanningContext_queryParameters' - A map consisting of key-value pairs.
--
-- 'catalogId', 'queryPlanningContext_catalogId' - The ID of the Data Catalog where the partition in question resides. If
-- none is provided, the Amazon Web Services account ID is used by default.
--
-- 'transactionId', 'queryPlanningContext_transactionId' - The transaction ID at which to read the table contents. If this
-- transaction is not committed, the read will be treated as part of that
-- transaction and will see its writes. If this transaction has aborted, an
-- error will be returned. If not set, defaults to the most recent
-- committed transaction. Cannot be specified along with @QueryAsOfTime@.
--
-- 'databaseName', 'queryPlanningContext_databaseName' - The database containing the table.
newQueryPlanningContext ::
  -- | 'databaseName'
  Prelude.Text ->
  QueryPlanningContext
newQueryPlanningContext pDatabaseName_ =
  QueryPlanningContext'
    { queryAsOfTime =
        Prelude.Nothing,
      queryParameters = Prelude.Nothing,
      catalogId = Prelude.Nothing,
      transactionId = Prelude.Nothing,
      databaseName = pDatabaseName_
    }

-- | The time as of when to read the table contents. If not set, the most
-- recent transaction commit time will be used. Cannot be specified along
-- with @TransactionId@.
queryPlanningContext_queryAsOfTime :: Lens.Lens' QueryPlanningContext (Prelude.Maybe Prelude.UTCTime)
queryPlanningContext_queryAsOfTime = Lens.lens (\QueryPlanningContext' {queryAsOfTime} -> queryAsOfTime) (\s@QueryPlanningContext' {} a -> s {queryAsOfTime = a} :: QueryPlanningContext) Prelude.. Lens.mapping Data._Time

-- | A map consisting of key-value pairs.
queryPlanningContext_queryParameters :: Lens.Lens' QueryPlanningContext (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
queryPlanningContext_queryParameters = Lens.lens (\QueryPlanningContext' {queryParameters} -> queryParameters) (\s@QueryPlanningContext' {} a -> s {queryParameters = a} :: QueryPlanningContext) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Data Catalog where the partition in question resides. If
-- none is provided, the Amazon Web Services account ID is used by default.
queryPlanningContext_catalogId :: Lens.Lens' QueryPlanningContext (Prelude.Maybe Prelude.Text)
queryPlanningContext_catalogId = Lens.lens (\QueryPlanningContext' {catalogId} -> catalogId) (\s@QueryPlanningContext' {} a -> s {catalogId = a} :: QueryPlanningContext)

-- | The transaction ID at which to read the table contents. If this
-- transaction is not committed, the read will be treated as part of that
-- transaction and will see its writes. If this transaction has aborted, an
-- error will be returned. If not set, defaults to the most recent
-- committed transaction. Cannot be specified along with @QueryAsOfTime@.
queryPlanningContext_transactionId :: Lens.Lens' QueryPlanningContext (Prelude.Maybe Prelude.Text)
queryPlanningContext_transactionId = Lens.lens (\QueryPlanningContext' {transactionId} -> transactionId) (\s@QueryPlanningContext' {} a -> s {transactionId = a} :: QueryPlanningContext)

-- | The database containing the table.
queryPlanningContext_databaseName :: Lens.Lens' QueryPlanningContext Prelude.Text
queryPlanningContext_databaseName = Lens.lens (\QueryPlanningContext' {databaseName} -> databaseName) (\s@QueryPlanningContext' {} a -> s {databaseName = a} :: QueryPlanningContext)

instance Prelude.Hashable QueryPlanningContext where
  hashWithSalt _salt QueryPlanningContext' {..} =
    _salt `Prelude.hashWithSalt` queryAsOfTime
      `Prelude.hashWithSalt` queryParameters
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` transactionId
      `Prelude.hashWithSalt` databaseName

instance Prelude.NFData QueryPlanningContext where
  rnf QueryPlanningContext' {..} =
    Prelude.rnf queryAsOfTime
      `Prelude.seq` Prelude.rnf queryParameters
      `Prelude.seq` Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf transactionId
      `Prelude.seq` Prelude.rnf databaseName

instance Data.ToJSON QueryPlanningContext where
  toJSON QueryPlanningContext' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("QueryAsOfTime" Data..=) Prelude.<$> queryAsOfTime,
            ("QueryParameters" Data..=)
              Prelude.<$> queryParameters,
            ("CatalogId" Data..=) Prelude.<$> catalogId,
            ("TransactionId" Data..=) Prelude.<$> transactionId,
            Prelude.Just ("DatabaseName" Data..= databaseName)
          ]
      )
