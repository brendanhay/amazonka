{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.SourceTableDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.SourceTableDetails
  ( SourceTableDetails (..),

    -- * Smart constructor
    mkSourceTableDetails,

    -- * Lenses
    stdTableName,
    stdTableId,
    stdKeySchema,
    stdTableCreationDateTime,
    stdProvisionedThroughput,
    stdBillingMode,
    stdItemCount,
    stdTableArn,
    stdTableSizeBytes,
  )
where

import qualified Network.AWS.DynamoDB.Types.BillingMode as Types
import qualified Network.AWS.DynamoDB.Types.KeySchemaElement as Types
import qualified Network.AWS.DynamoDB.Types.ProvisionedThroughput as Types
import qualified Network.AWS.DynamoDB.Types.TableArn as Types
import qualified Network.AWS.DynamoDB.Types.TableId as Types
import qualified Network.AWS.DynamoDB.Types.TableName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the details of the table when the backup was created.
--
-- /See:/ 'mkSourceTableDetails' smart constructor.
data SourceTableDetails = SourceTableDetails'
  { -- | The name of the table for which the backup was created.
    tableName :: Types.TableName,
    -- | Unique identifier for the table for which the backup was created.
    tableId :: Types.TableId,
    -- | Schema of the table.
    keySchema :: Core.NonEmpty Types.KeySchemaElement,
    -- | Time when the source table was created.
    tableCreationDateTime :: Core.NominalDiffTime,
    -- | Read IOPs and Write IOPS on the table when the backup was created.
    provisionedThroughput :: Types.ProvisionedThroughput,
    -- | Controls how you are charged for read and write throughput and how you manage capacity. This setting can be changed later.
    --
    --
    --     * @PROVISIONED@ - Sets the read/write capacity mode to @PROVISIONED@ . We recommend using @PROVISIONED@ for predictable workloads.
    --
    --
    --     * @PAY_PER_REQUEST@ - Sets the read/write capacity mode to @PAY_PER_REQUEST@ . We recommend using @PAY_PER_REQUEST@ for unpredictable workloads.
    billingMode :: Core.Maybe Types.BillingMode,
    -- | Number of items in the table. Note that this is an approximate value.
    itemCount :: Core.Maybe Core.Natural,
    -- | ARN of the table for which backup was created.
    tableArn :: Core.Maybe Types.TableArn,
    -- | Size of the table in bytes. Note that this is an approximate value.
    tableSizeBytes :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SourceTableDetails' value with any optional fields omitted.
mkSourceTableDetails ::
  -- | 'tableName'
  Types.TableName ->
  -- | 'tableId'
  Types.TableId ->
  -- | 'keySchema'
  Core.NonEmpty Types.KeySchemaElement ->
  -- | 'tableCreationDateTime'
  Core.NominalDiffTime ->
  -- | 'provisionedThroughput'
  Types.ProvisionedThroughput ->
  SourceTableDetails
mkSourceTableDetails
  tableName
  tableId
  keySchema
  tableCreationDateTime
  provisionedThroughput =
    SourceTableDetails'
      { tableName,
        tableId,
        keySchema,
        tableCreationDateTime,
        provisionedThroughput,
        billingMode = Core.Nothing,
        itemCount = Core.Nothing,
        tableArn = Core.Nothing,
        tableSizeBytes = Core.Nothing
      }

-- | The name of the table for which the backup was created.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdTableName :: Lens.Lens' SourceTableDetails Types.TableName
stdTableName = Lens.field @"tableName"
{-# DEPRECATED stdTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | Unique identifier for the table for which the backup was created.
--
-- /Note:/ Consider using 'tableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdTableId :: Lens.Lens' SourceTableDetails Types.TableId
stdTableId = Lens.field @"tableId"
{-# DEPRECATED stdTableId "Use generic-lens or generic-optics with 'tableId' instead." #-}

-- | Schema of the table.
--
-- /Note:/ Consider using 'keySchema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdKeySchema :: Lens.Lens' SourceTableDetails (Core.NonEmpty Types.KeySchemaElement)
stdKeySchema = Lens.field @"keySchema"
{-# DEPRECATED stdKeySchema "Use generic-lens or generic-optics with 'keySchema' instead." #-}

-- | Time when the source table was created.
--
-- /Note:/ Consider using 'tableCreationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdTableCreationDateTime :: Lens.Lens' SourceTableDetails Core.NominalDiffTime
stdTableCreationDateTime = Lens.field @"tableCreationDateTime"
{-# DEPRECATED stdTableCreationDateTime "Use generic-lens or generic-optics with 'tableCreationDateTime' instead." #-}

-- | Read IOPs and Write IOPS on the table when the backup was created.
--
-- /Note:/ Consider using 'provisionedThroughput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdProvisionedThroughput :: Lens.Lens' SourceTableDetails Types.ProvisionedThroughput
stdProvisionedThroughput = Lens.field @"provisionedThroughput"
{-# DEPRECATED stdProvisionedThroughput "Use generic-lens or generic-optics with 'provisionedThroughput' instead." #-}

-- | Controls how you are charged for read and write throughput and how you manage capacity. This setting can be changed later.
--
--
--     * @PROVISIONED@ - Sets the read/write capacity mode to @PROVISIONED@ . We recommend using @PROVISIONED@ for predictable workloads.
--
--
--     * @PAY_PER_REQUEST@ - Sets the read/write capacity mode to @PAY_PER_REQUEST@ . We recommend using @PAY_PER_REQUEST@ for unpredictable workloads.
--
--
--
-- /Note:/ Consider using 'billingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdBillingMode :: Lens.Lens' SourceTableDetails (Core.Maybe Types.BillingMode)
stdBillingMode = Lens.field @"billingMode"
{-# DEPRECATED stdBillingMode "Use generic-lens or generic-optics with 'billingMode' instead." #-}

-- | Number of items in the table. Note that this is an approximate value.
--
-- /Note:/ Consider using 'itemCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdItemCount :: Lens.Lens' SourceTableDetails (Core.Maybe Core.Natural)
stdItemCount = Lens.field @"itemCount"
{-# DEPRECATED stdItemCount "Use generic-lens or generic-optics with 'itemCount' instead." #-}

-- | ARN of the table for which backup was created.
--
-- /Note:/ Consider using 'tableArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdTableArn :: Lens.Lens' SourceTableDetails (Core.Maybe Types.TableArn)
stdTableArn = Lens.field @"tableArn"
{-# DEPRECATED stdTableArn "Use generic-lens or generic-optics with 'tableArn' instead." #-}

-- | Size of the table in bytes. Note that this is an approximate value.
--
-- /Note:/ Consider using 'tableSizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdTableSizeBytes :: Lens.Lens' SourceTableDetails (Core.Maybe Core.Integer)
stdTableSizeBytes = Lens.field @"tableSizeBytes"
{-# DEPRECATED stdTableSizeBytes "Use generic-lens or generic-optics with 'tableSizeBytes' instead." #-}

instance Core.FromJSON SourceTableDetails where
  parseJSON =
    Core.withObject "SourceTableDetails" Core.$
      \x ->
        SourceTableDetails'
          Core.<$> (x Core..: "TableName")
          Core.<*> (x Core..: "TableId")
          Core.<*> (x Core..: "KeySchema")
          Core.<*> (x Core..: "TableCreationDateTime")
          Core.<*> (x Core..: "ProvisionedThroughput")
          Core.<*> (x Core..:? "BillingMode")
          Core.<*> (x Core..:? "ItemCount")
          Core.<*> (x Core..:? "TableArn")
          Core.<*> (x Core..:? "TableSizeBytes")
