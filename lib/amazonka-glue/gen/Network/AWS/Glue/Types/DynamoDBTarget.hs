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
    ddtPath,
    ddtScanRate,
    ddtScanAll,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies an Amazon DynamoDB table to crawl.
--
-- /See:/ 'mkDynamoDBTarget' smart constructor.
data DynamoDBTarget = DynamoDBTarget'
  { -- | The name of the DynamoDB table to crawl.
    path :: Lude.Maybe Lude.Text,
    -- | The percentage of the configured read capacity units to use by the AWS Glue crawler. Read capacity units is a term defined by DynamoDB, and is a numeric value that acts as rate limiter for the number of reads that can be performed on that table per second.
    --
    -- The valid values are null or a value between 0.1 to 1.5. A null value is used when user does not provide a value, and defaults to 0.5 of the configured Read Capacity Unit (for provisioned tables), or 0.25 of the max configured Read Capacity Unit (for tables using on-demand mode).
    scanRate :: Lude.Maybe Lude.Double,
    -- | Indicates whether to scan all the records, or to sample rows from the table. Scanning all the records can take a long time when the table is not a high throughput table.
    --
    -- A value of @true@ means to scan all records, while a value of @false@ means to sample the records. If no value is specified, the value defaults to @true@ .
    scanAll :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DynamoDBTarget' with the minimum fields required to make a request.
--
-- * 'path' - The name of the DynamoDB table to crawl.
-- * 'scanRate' - The percentage of the configured read capacity units to use by the AWS Glue crawler. Read capacity units is a term defined by DynamoDB, and is a numeric value that acts as rate limiter for the number of reads that can be performed on that table per second.
--
-- The valid values are null or a value between 0.1 to 1.5. A null value is used when user does not provide a value, and defaults to 0.5 of the configured Read Capacity Unit (for provisioned tables), or 0.25 of the max configured Read Capacity Unit (for tables using on-demand mode).
-- * 'scanAll' - Indicates whether to scan all the records, or to sample rows from the table. Scanning all the records can take a long time when the table is not a high throughput table.
--
-- A value of @true@ means to scan all records, while a value of @false@ means to sample the records. If no value is specified, the value defaults to @true@ .
mkDynamoDBTarget ::
  DynamoDBTarget
mkDynamoDBTarget =
  DynamoDBTarget'
    { path = Lude.Nothing,
      scanRate = Lude.Nothing,
      scanAll = Lude.Nothing
    }

-- | The name of the DynamoDB table to crawl.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddtPath :: Lens.Lens' DynamoDBTarget (Lude.Maybe Lude.Text)
ddtPath = Lens.lens (path :: DynamoDBTarget -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: DynamoDBTarget)
{-# DEPRECATED ddtPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The percentage of the configured read capacity units to use by the AWS Glue crawler. Read capacity units is a term defined by DynamoDB, and is a numeric value that acts as rate limiter for the number of reads that can be performed on that table per second.
--
-- The valid values are null or a value between 0.1 to 1.5. A null value is used when user does not provide a value, and defaults to 0.5 of the configured Read Capacity Unit (for provisioned tables), or 0.25 of the max configured Read Capacity Unit (for tables using on-demand mode).
--
-- /Note:/ Consider using 'scanRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddtScanRate :: Lens.Lens' DynamoDBTarget (Lude.Maybe Lude.Double)
ddtScanRate = Lens.lens (scanRate :: DynamoDBTarget -> Lude.Maybe Lude.Double) (\s a -> s {scanRate = a} :: DynamoDBTarget)
{-# DEPRECATED ddtScanRate "Use generic-lens or generic-optics with 'scanRate' instead." #-}

-- | Indicates whether to scan all the records, or to sample rows from the table. Scanning all the records can take a long time when the table is not a high throughput table.
--
-- A value of @true@ means to scan all records, while a value of @false@ means to sample the records. If no value is specified, the value defaults to @true@ .
--
-- /Note:/ Consider using 'scanAll' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddtScanAll :: Lens.Lens' DynamoDBTarget (Lude.Maybe Lude.Bool)
ddtScanAll = Lens.lens (scanAll :: DynamoDBTarget -> Lude.Maybe Lude.Bool) (\s a -> s {scanAll = a} :: DynamoDBTarget)
{-# DEPRECATED ddtScanAll "Use generic-lens or generic-optics with 'scanAll' instead." #-}

instance Lude.FromJSON DynamoDBTarget where
  parseJSON =
    Lude.withObject
      "DynamoDBTarget"
      ( \x ->
          DynamoDBTarget'
            Lude.<$> (x Lude..:? "Path")
            Lude.<*> (x Lude..:? "scanRate")
            Lude.<*> (x Lude..:? "scanAll")
      )

instance Lude.ToJSON DynamoDBTarget where
  toJSON DynamoDBTarget' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Path" Lude..=) Lude.<$> path,
            ("scanRate" Lude..=) Lude.<$> scanRate,
            ("scanAll" Lude..=) Lude.<$> scanAll
          ]
      )
