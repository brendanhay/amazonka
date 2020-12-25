{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DynamoDBTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DynamoDBTarget
  ( DynamoDBTarget (..),

    -- * Smart constructor
    mkDynamoDBTarget,

    -- * Lenses
    ddbtPath,
    ddbtScanAll,
    ddbtScanRate,
  )
where

import qualified Network.AWS.Glue.Types.Path as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies an Amazon DynamoDB table to crawl.
--
-- /See:/ 'mkDynamoDBTarget' smart constructor.
data DynamoDBTarget = DynamoDBTarget'
  { -- | The name of the DynamoDB table to crawl.
    path :: Core.Maybe Types.Path,
    -- | Indicates whether to scan all the records, or to sample rows from the table. Scanning all the records can take a long time when the table is not a high throughput table.
    --
    -- A value of @true@ means to scan all records, while a value of @false@ means to sample the records. If no value is specified, the value defaults to @true@ .
    scanAll :: Core.Maybe Core.Bool,
    -- | The percentage of the configured read capacity units to use by the AWS Glue crawler. Read capacity units is a term defined by DynamoDB, and is a numeric value that acts as rate limiter for the number of reads that can be performed on that table per second.
    --
    -- The valid values are null or a value between 0.1 to 1.5. A null value is used when user does not provide a value, and defaults to 0.5 of the configured Read Capacity Unit (for provisioned tables), or 0.25 of the max configured Read Capacity Unit (for tables using on-demand mode).
    scanRate :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DynamoDBTarget' value with any optional fields omitted.
mkDynamoDBTarget ::
  DynamoDBTarget
mkDynamoDBTarget =
  DynamoDBTarget'
    { path = Core.Nothing,
      scanAll = Core.Nothing,
      scanRate = Core.Nothing
    }

-- | The name of the DynamoDB table to crawl.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbtPath :: Lens.Lens' DynamoDBTarget (Core.Maybe Types.Path)
ddbtPath = Lens.field @"path"
{-# DEPRECATED ddbtPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | Indicates whether to scan all the records, or to sample rows from the table. Scanning all the records can take a long time when the table is not a high throughput table.
--
-- A value of @true@ means to scan all records, while a value of @false@ means to sample the records. If no value is specified, the value defaults to @true@ .
--
-- /Note:/ Consider using 'scanAll' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbtScanAll :: Lens.Lens' DynamoDBTarget (Core.Maybe Core.Bool)
ddbtScanAll = Lens.field @"scanAll"
{-# DEPRECATED ddbtScanAll "Use generic-lens or generic-optics with 'scanAll' instead." #-}

-- | The percentage of the configured read capacity units to use by the AWS Glue crawler. Read capacity units is a term defined by DynamoDB, and is a numeric value that acts as rate limiter for the number of reads that can be performed on that table per second.
--
-- The valid values are null or a value between 0.1 to 1.5. A null value is used when user does not provide a value, and defaults to 0.5 of the configured Read Capacity Unit (for provisioned tables), or 0.25 of the max configured Read Capacity Unit (for tables using on-demand mode).
--
-- /Note:/ Consider using 'scanRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbtScanRate :: Lens.Lens' DynamoDBTarget (Core.Maybe Core.Double)
ddbtScanRate = Lens.field @"scanRate"
{-# DEPRECATED ddbtScanRate "Use generic-lens or generic-optics with 'scanRate' instead." #-}

instance Core.FromJSON DynamoDBTarget where
  toJSON DynamoDBTarget {..} =
    Core.object
      ( Core.catMaybes
          [ ("Path" Core..=) Core.<$> path,
            ("scanAll" Core..=) Core.<$> scanAll,
            ("scanRate" Core..=) Core.<$> scanRate
          ]
      )

instance Core.FromJSON DynamoDBTarget where
  parseJSON =
    Core.withObject "DynamoDBTarget" Core.$
      \x ->
        DynamoDBTarget'
          Core.<$> (x Core..:? "Path")
          Core.<*> (x Core..:? "scanAll")
          Core.<*> (x Core..:? "scanRate")
