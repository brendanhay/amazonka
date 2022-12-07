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
-- Module      : Amazonka.Glue.Types.DynamoDBTarget
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DynamoDBTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies an Amazon DynamoDB table to crawl.
--
-- /See:/ 'newDynamoDBTarget' smart constructor.
data DynamoDBTarget = DynamoDBTarget'
  { -- | Indicates whether to scan all the records, or to sample rows from the
    -- table. Scanning all the records can take a long time when the table is
    -- not a high throughput table.
    --
    -- A value of @true@ means to scan all records, while a value of @false@
    -- means to sample the records. If no value is specified, the value
    -- defaults to @true@.
    scanAll :: Prelude.Maybe Prelude.Bool,
    -- | The name of the DynamoDB table to crawl.
    path :: Prelude.Maybe Prelude.Text,
    -- | The percentage of the configured read capacity units to use by the Glue
    -- crawler. Read capacity units is a term defined by DynamoDB, and is a
    -- numeric value that acts as rate limiter for the number of reads that can
    -- be performed on that table per second.
    --
    -- The valid values are null or a value between 0.1 to 1.5. A null value is
    -- used when user does not provide a value, and defaults to 0.5 of the
    -- configured Read Capacity Unit (for provisioned tables), or 0.25 of the
    -- max configured Read Capacity Unit (for tables using on-demand mode).
    scanRate :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DynamoDBTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scanAll', 'dynamoDBTarget_scanAll' - Indicates whether to scan all the records, or to sample rows from the
-- table. Scanning all the records can take a long time when the table is
-- not a high throughput table.
--
-- A value of @true@ means to scan all records, while a value of @false@
-- means to sample the records. If no value is specified, the value
-- defaults to @true@.
--
-- 'path', 'dynamoDBTarget_path' - The name of the DynamoDB table to crawl.
--
-- 'scanRate', 'dynamoDBTarget_scanRate' - The percentage of the configured read capacity units to use by the Glue
-- crawler. Read capacity units is a term defined by DynamoDB, and is a
-- numeric value that acts as rate limiter for the number of reads that can
-- be performed on that table per second.
--
-- The valid values are null or a value between 0.1 to 1.5. A null value is
-- used when user does not provide a value, and defaults to 0.5 of the
-- configured Read Capacity Unit (for provisioned tables), or 0.25 of the
-- max configured Read Capacity Unit (for tables using on-demand mode).
newDynamoDBTarget ::
  DynamoDBTarget
newDynamoDBTarget =
  DynamoDBTarget'
    { scanAll = Prelude.Nothing,
      path = Prelude.Nothing,
      scanRate = Prelude.Nothing
    }

-- | Indicates whether to scan all the records, or to sample rows from the
-- table. Scanning all the records can take a long time when the table is
-- not a high throughput table.
--
-- A value of @true@ means to scan all records, while a value of @false@
-- means to sample the records. If no value is specified, the value
-- defaults to @true@.
dynamoDBTarget_scanAll :: Lens.Lens' DynamoDBTarget (Prelude.Maybe Prelude.Bool)
dynamoDBTarget_scanAll = Lens.lens (\DynamoDBTarget' {scanAll} -> scanAll) (\s@DynamoDBTarget' {} a -> s {scanAll = a} :: DynamoDBTarget)

-- | The name of the DynamoDB table to crawl.
dynamoDBTarget_path :: Lens.Lens' DynamoDBTarget (Prelude.Maybe Prelude.Text)
dynamoDBTarget_path = Lens.lens (\DynamoDBTarget' {path} -> path) (\s@DynamoDBTarget' {} a -> s {path = a} :: DynamoDBTarget)

-- | The percentage of the configured read capacity units to use by the Glue
-- crawler. Read capacity units is a term defined by DynamoDB, and is a
-- numeric value that acts as rate limiter for the number of reads that can
-- be performed on that table per second.
--
-- The valid values are null or a value between 0.1 to 1.5. A null value is
-- used when user does not provide a value, and defaults to 0.5 of the
-- configured Read Capacity Unit (for provisioned tables), or 0.25 of the
-- max configured Read Capacity Unit (for tables using on-demand mode).
dynamoDBTarget_scanRate :: Lens.Lens' DynamoDBTarget (Prelude.Maybe Prelude.Double)
dynamoDBTarget_scanRate = Lens.lens (\DynamoDBTarget' {scanRate} -> scanRate) (\s@DynamoDBTarget' {} a -> s {scanRate = a} :: DynamoDBTarget)

instance Data.FromJSON DynamoDBTarget where
  parseJSON =
    Data.withObject
      "DynamoDBTarget"
      ( \x ->
          DynamoDBTarget'
            Prelude.<$> (x Data..:? "scanAll")
            Prelude.<*> (x Data..:? "Path")
            Prelude.<*> (x Data..:? "scanRate")
      )

instance Prelude.Hashable DynamoDBTarget where
  hashWithSalt _salt DynamoDBTarget' {..} =
    _salt `Prelude.hashWithSalt` scanAll
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` scanRate

instance Prelude.NFData DynamoDBTarget where
  rnf DynamoDBTarget' {..} =
    Prelude.rnf scanAll
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf scanRate

instance Data.ToJSON DynamoDBTarget where
  toJSON DynamoDBTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("scanAll" Data..=) Prelude.<$> scanAll,
            ("Path" Data..=) Prelude.<$> path,
            ("scanRate" Data..=) Prelude.<$> scanRate
          ]
      )
