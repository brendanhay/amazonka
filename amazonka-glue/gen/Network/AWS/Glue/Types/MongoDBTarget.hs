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
-- Module      : Network.AWS.Glue.Types.MongoDBTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.MongoDBTarget where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies an Amazon DocumentDB or MongoDB data store to crawl.
--
-- /See:/ 'newMongoDBTarget' smart constructor.
data MongoDBTarget = MongoDBTarget'
  { -- | The name of the connection to use to connect to the Amazon DocumentDB or
    -- MongoDB target.
    connectionName :: Core.Maybe Core.Text,
    -- | Indicates whether to scan all the records, or to sample rows from the
    -- table. Scanning all the records can take a long time when the table is
    -- not a high throughput table.
    --
    -- A value of @true@ means to scan all records, while a value of @false@
    -- means to sample the records. If no value is specified, the value
    -- defaults to @true@.
    scanAll :: Core.Maybe Core.Bool,
    -- | The path of the Amazon DocumentDB or MongoDB target
    -- (database\/collection).
    path :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MongoDBTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionName', 'mongoDBTarget_connectionName' - The name of the connection to use to connect to the Amazon DocumentDB or
-- MongoDB target.
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
newMongoDBTarget ::
  MongoDBTarget
newMongoDBTarget =
  MongoDBTarget'
    { connectionName = Core.Nothing,
      scanAll = Core.Nothing,
      path = Core.Nothing
    }

-- | The name of the connection to use to connect to the Amazon DocumentDB or
-- MongoDB target.
mongoDBTarget_connectionName :: Lens.Lens' MongoDBTarget (Core.Maybe Core.Text)
mongoDBTarget_connectionName = Lens.lens (\MongoDBTarget' {connectionName} -> connectionName) (\s@MongoDBTarget' {} a -> s {connectionName = a} :: MongoDBTarget)

-- | Indicates whether to scan all the records, or to sample rows from the
-- table. Scanning all the records can take a long time when the table is
-- not a high throughput table.
--
-- A value of @true@ means to scan all records, while a value of @false@
-- means to sample the records. If no value is specified, the value
-- defaults to @true@.
mongoDBTarget_scanAll :: Lens.Lens' MongoDBTarget (Core.Maybe Core.Bool)
mongoDBTarget_scanAll = Lens.lens (\MongoDBTarget' {scanAll} -> scanAll) (\s@MongoDBTarget' {} a -> s {scanAll = a} :: MongoDBTarget)

-- | The path of the Amazon DocumentDB or MongoDB target
-- (database\/collection).
mongoDBTarget_path :: Lens.Lens' MongoDBTarget (Core.Maybe Core.Text)
mongoDBTarget_path = Lens.lens (\MongoDBTarget' {path} -> path) (\s@MongoDBTarget' {} a -> s {path = a} :: MongoDBTarget)

instance Core.FromJSON MongoDBTarget where
  parseJSON =
    Core.withObject
      "MongoDBTarget"
      ( \x ->
          MongoDBTarget'
            Core.<$> (x Core..:? "ConnectionName")
            Core.<*> (x Core..:? "ScanAll")
            Core.<*> (x Core..:? "Path")
      )

instance Core.Hashable MongoDBTarget

instance Core.NFData MongoDBTarget

instance Core.ToJSON MongoDBTarget where
  toJSON MongoDBTarget' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ConnectionName" Core..=) Core.<$> connectionName,
            ("ScanAll" Core..=) Core.<$> scanAll,
            ("Path" Core..=) Core.<$> path
          ]
      )
