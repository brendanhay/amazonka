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
-- Module      : Amazonka.Glue.Types.CatalogTarget
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.CatalogTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies an Glue Data Catalog target.
--
-- /See:/ 'newCatalogTarget' smart constructor.
data CatalogTarget = CatalogTarget'
  { -- | A valid Amazon dead-letter SQS ARN. For example,
    -- @arn:aws:sqs:region:account:deadLetterQueue@.
    dlqEventQueueArn :: Prelude.Maybe Prelude.Text,
    -- | A valid Amazon SQS ARN. For example, @arn:aws:sqs:region:account:sqs@.
    eventQueueArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the connection for an Amazon S3-backed Data Catalog table to
    -- be a target of the crawl when using a @Catalog@ connection type paired
    -- with a @NETWORK@ Connection type.
    connectionName :: Prelude.Maybe Prelude.Text,
    -- | The name of the database to be synchronized.
    databaseName :: Prelude.Text,
    -- | A list of the tables to be synchronized.
    tables :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CatalogTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dlqEventQueueArn', 'catalogTarget_dlqEventQueueArn' - A valid Amazon dead-letter SQS ARN. For example,
-- @arn:aws:sqs:region:account:deadLetterQueue@.
--
-- 'eventQueueArn', 'catalogTarget_eventQueueArn' - A valid Amazon SQS ARN. For example, @arn:aws:sqs:region:account:sqs@.
--
-- 'connectionName', 'catalogTarget_connectionName' - The name of the connection for an Amazon S3-backed Data Catalog table to
-- be a target of the crawl when using a @Catalog@ connection type paired
-- with a @NETWORK@ Connection type.
--
-- 'databaseName', 'catalogTarget_databaseName' - The name of the database to be synchronized.
--
-- 'tables', 'catalogTarget_tables' - A list of the tables to be synchronized.
newCatalogTarget ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tables'
  Prelude.NonEmpty Prelude.Text ->
  CatalogTarget
newCatalogTarget pDatabaseName_ pTables_ =
  CatalogTarget'
    { dlqEventQueueArn = Prelude.Nothing,
      eventQueueArn = Prelude.Nothing,
      connectionName = Prelude.Nothing,
      databaseName = pDatabaseName_,
      tables = Lens.coerced Lens.# pTables_
    }

-- | A valid Amazon dead-letter SQS ARN. For example,
-- @arn:aws:sqs:region:account:deadLetterQueue@.
catalogTarget_dlqEventQueueArn :: Lens.Lens' CatalogTarget (Prelude.Maybe Prelude.Text)
catalogTarget_dlqEventQueueArn = Lens.lens (\CatalogTarget' {dlqEventQueueArn} -> dlqEventQueueArn) (\s@CatalogTarget' {} a -> s {dlqEventQueueArn = a} :: CatalogTarget)

-- | A valid Amazon SQS ARN. For example, @arn:aws:sqs:region:account:sqs@.
catalogTarget_eventQueueArn :: Lens.Lens' CatalogTarget (Prelude.Maybe Prelude.Text)
catalogTarget_eventQueueArn = Lens.lens (\CatalogTarget' {eventQueueArn} -> eventQueueArn) (\s@CatalogTarget' {} a -> s {eventQueueArn = a} :: CatalogTarget)

-- | The name of the connection for an Amazon S3-backed Data Catalog table to
-- be a target of the crawl when using a @Catalog@ connection type paired
-- with a @NETWORK@ Connection type.
catalogTarget_connectionName :: Lens.Lens' CatalogTarget (Prelude.Maybe Prelude.Text)
catalogTarget_connectionName = Lens.lens (\CatalogTarget' {connectionName} -> connectionName) (\s@CatalogTarget' {} a -> s {connectionName = a} :: CatalogTarget)

-- | The name of the database to be synchronized.
catalogTarget_databaseName :: Lens.Lens' CatalogTarget Prelude.Text
catalogTarget_databaseName = Lens.lens (\CatalogTarget' {databaseName} -> databaseName) (\s@CatalogTarget' {} a -> s {databaseName = a} :: CatalogTarget)

-- | A list of the tables to be synchronized.
catalogTarget_tables :: Lens.Lens' CatalogTarget (Prelude.NonEmpty Prelude.Text)
catalogTarget_tables = Lens.lens (\CatalogTarget' {tables} -> tables) (\s@CatalogTarget' {} a -> s {tables = a} :: CatalogTarget) Prelude.. Lens.coerced

instance Data.FromJSON CatalogTarget where
  parseJSON =
    Data.withObject
      "CatalogTarget"
      ( \x ->
          CatalogTarget'
            Prelude.<$> (x Data..:? "DlqEventQueueArn")
            Prelude.<*> (x Data..:? "EventQueueArn")
            Prelude.<*> (x Data..:? "ConnectionName")
            Prelude.<*> (x Data..: "DatabaseName")
            Prelude.<*> (x Data..: "Tables")
      )

instance Prelude.Hashable CatalogTarget where
  hashWithSalt _salt CatalogTarget' {..} =
    _salt `Prelude.hashWithSalt` dlqEventQueueArn
      `Prelude.hashWithSalt` eventQueueArn
      `Prelude.hashWithSalt` connectionName
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tables

instance Prelude.NFData CatalogTarget where
  rnf CatalogTarget' {..} =
    Prelude.rnf dlqEventQueueArn
      `Prelude.seq` Prelude.rnf eventQueueArn
      `Prelude.seq` Prelude.rnf connectionName
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tables

instance Data.ToJSON CatalogTarget where
  toJSON CatalogTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DlqEventQueueArn" Data..=)
              Prelude.<$> dlqEventQueueArn,
            ("EventQueueArn" Data..=) Prelude.<$> eventQueueArn,
            ("ConnectionName" Data..=)
              Prelude.<$> connectionName,
            Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just ("Tables" Data..= tables)
          ]
      )
