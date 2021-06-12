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
-- Module      : Network.AWS.Glue.Types.DynamoDBTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DynamoDBTarget where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

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
    scanAll :: Core.Maybe Core.Bool,
    -- | The percentage of the configured read capacity units to use by the AWS
    -- Glue crawler. Read capacity units is a term defined by DynamoDB, and is
    -- a numeric value that acts as rate limiter for the number of reads that
    -- can be performed on that table per second.
    --
    -- The valid values are null or a value between 0.1 to 1.5. A null value is
    -- used when user does not provide a value, and defaults to 0.5 of the
    -- configured Read Capacity Unit (for provisioned tables), or 0.25 of the
    -- max configured Read Capacity Unit (for tables using on-demand mode).
    scanRate :: Core.Maybe Core.Double,
    -- | The name of the DynamoDB table to crawl.
    path :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'scanRate', 'dynamoDBTarget_scanRate' - The percentage of the configured read capacity units to use by the AWS
-- Glue crawler. Read capacity units is a term defined by DynamoDB, and is
-- a numeric value that acts as rate limiter for the number of reads that
-- can be performed on that table per second.
--
-- The valid values are null or a value between 0.1 to 1.5. A null value is
-- used when user does not provide a value, and defaults to 0.5 of the
-- configured Read Capacity Unit (for provisioned tables), or 0.25 of the
-- max configured Read Capacity Unit (for tables using on-demand mode).
--
-- 'path', 'dynamoDBTarget_path' - The name of the DynamoDB table to crawl.
newDynamoDBTarget ::
  DynamoDBTarget
newDynamoDBTarget =
  DynamoDBTarget'
    { scanAll = Core.Nothing,
      scanRate = Core.Nothing,
      path = Core.Nothing
    }

-- | Indicates whether to scan all the records, or to sample rows from the
-- table. Scanning all the records can take a long time when the table is
-- not a high throughput table.
--
-- A value of @true@ means to scan all records, while a value of @false@
-- means to sample the records. If no value is specified, the value
-- defaults to @true@.
dynamoDBTarget_scanAll :: Lens.Lens' DynamoDBTarget (Core.Maybe Core.Bool)
dynamoDBTarget_scanAll = Lens.lens (\DynamoDBTarget' {scanAll} -> scanAll) (\s@DynamoDBTarget' {} a -> s {scanAll = a} :: DynamoDBTarget)

-- | The percentage of the configured read capacity units to use by the AWS
-- Glue crawler. Read capacity units is a term defined by DynamoDB, and is
-- a numeric value that acts as rate limiter for the number of reads that
-- can be performed on that table per second.
--
-- The valid values are null or a value between 0.1 to 1.5. A null value is
-- used when user does not provide a value, and defaults to 0.5 of the
-- configured Read Capacity Unit (for provisioned tables), or 0.25 of the
-- max configured Read Capacity Unit (for tables using on-demand mode).
dynamoDBTarget_scanRate :: Lens.Lens' DynamoDBTarget (Core.Maybe Core.Double)
dynamoDBTarget_scanRate = Lens.lens (\DynamoDBTarget' {scanRate} -> scanRate) (\s@DynamoDBTarget' {} a -> s {scanRate = a} :: DynamoDBTarget)

-- | The name of the DynamoDB table to crawl.
dynamoDBTarget_path :: Lens.Lens' DynamoDBTarget (Core.Maybe Core.Text)
dynamoDBTarget_path = Lens.lens (\DynamoDBTarget' {path} -> path) (\s@DynamoDBTarget' {} a -> s {path = a} :: DynamoDBTarget)

instance Core.FromJSON DynamoDBTarget where
  parseJSON =
    Core.withObject
      "DynamoDBTarget"
      ( \x ->
          DynamoDBTarget'
            Core.<$> (x Core..:? "scanAll")
            Core.<*> (x Core..:? "scanRate")
            Core.<*> (x Core..:? "Path")
      )

instance Core.Hashable DynamoDBTarget

instance Core.NFData DynamoDBTarget

instance Core.ToJSON DynamoDBTarget where
  toJSON DynamoDBTarget' {..} =
    Core.object
      ( Core.catMaybes
          [ ("scanAll" Core..=) Core.<$> scanAll,
            ("scanRate" Core..=) Core.<$> scanRate,
            ("Path" Core..=) Core.<$> path
          ]
      )
