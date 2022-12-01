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
-- Module      : Amazonka.Glue.Types.MongoDBTarget
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.MongoDBTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies an Amazon DocumentDB or MongoDB data store to crawl.
--
-- /See:/ 'newMongoDBTarget' smart constructor.
data MongoDBTarget = MongoDBTarget'
  { -- | Indicates whether to scan all the records, or to sample rows from the
    -- table. Scanning all the records can take a long time when the table is
    -- not a high throughput table.
    --
    -- A value of @true@ means to scan all records, while a value of @false@
    -- means to sample the records. If no value is specified, the value
    -- defaults to @true@.
    scanAll :: Prelude.Maybe Prelude.Bool,
    -- | The path of the Amazon DocumentDB or MongoDB target
    -- (database\/collection).
    path :: Prelude.Maybe Prelude.Text,
    -- | The name of the connection to use to connect to the Amazon DocumentDB or
    -- MongoDB target.
    connectionName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MongoDBTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scanAll', 'mongoDBTarget_scanAll' - Indicates whether to scan all the records, or to sample rows from the
-- table. Scanning all the records can take a long time when the table is
-- not a high throughput table.
--
-- A value of @true@ means to scan all records, while a value of @false@
-- means to sample the records. If no value is specified, the value
-- defaults to @true@.
--
-- 'path', 'mongoDBTarget_path' - The path of the Amazon DocumentDB or MongoDB target
-- (database\/collection).
--
-- 'connectionName', 'mongoDBTarget_connectionName' - The name of the connection to use to connect to the Amazon DocumentDB or
-- MongoDB target.
newMongoDBTarget ::
  MongoDBTarget
newMongoDBTarget =
  MongoDBTarget'
    { scanAll = Prelude.Nothing,
      path = Prelude.Nothing,
      connectionName = Prelude.Nothing
    }

-- | Indicates whether to scan all the records, or to sample rows from the
-- table. Scanning all the records can take a long time when the table is
-- not a high throughput table.
--
-- A value of @true@ means to scan all records, while a value of @false@
-- means to sample the records. If no value is specified, the value
-- defaults to @true@.
mongoDBTarget_scanAll :: Lens.Lens' MongoDBTarget (Prelude.Maybe Prelude.Bool)
mongoDBTarget_scanAll = Lens.lens (\MongoDBTarget' {scanAll} -> scanAll) (\s@MongoDBTarget' {} a -> s {scanAll = a} :: MongoDBTarget)

-- | The path of the Amazon DocumentDB or MongoDB target
-- (database\/collection).
mongoDBTarget_path :: Lens.Lens' MongoDBTarget (Prelude.Maybe Prelude.Text)
mongoDBTarget_path = Lens.lens (\MongoDBTarget' {path} -> path) (\s@MongoDBTarget' {} a -> s {path = a} :: MongoDBTarget)

-- | The name of the connection to use to connect to the Amazon DocumentDB or
-- MongoDB target.
mongoDBTarget_connectionName :: Lens.Lens' MongoDBTarget (Prelude.Maybe Prelude.Text)
mongoDBTarget_connectionName = Lens.lens (\MongoDBTarget' {connectionName} -> connectionName) (\s@MongoDBTarget' {} a -> s {connectionName = a} :: MongoDBTarget)

instance Core.FromJSON MongoDBTarget where
  parseJSON =
    Core.withObject
      "MongoDBTarget"
      ( \x ->
          MongoDBTarget'
            Prelude.<$> (x Core..:? "ScanAll")
            Prelude.<*> (x Core..:? "Path")
            Prelude.<*> (x Core..:? "ConnectionName")
      )

instance Prelude.Hashable MongoDBTarget where
  hashWithSalt _salt MongoDBTarget' {..} =
    _salt `Prelude.hashWithSalt` scanAll
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` connectionName

instance Prelude.NFData MongoDBTarget where
  rnf MongoDBTarget' {..} =
    Prelude.rnf scanAll
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf connectionName

instance Core.ToJSON MongoDBTarget where
  toJSON MongoDBTarget' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ScanAll" Core..=) Prelude.<$> scanAll,
            ("Path" Core..=) Prelude.<$> path,
            ("ConnectionName" Core..=)
              Prelude.<$> connectionName
          ]
      )
