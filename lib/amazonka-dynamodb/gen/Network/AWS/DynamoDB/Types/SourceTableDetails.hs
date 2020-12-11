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
    stdTableSizeBytes,
    stdTableARN,
    stdBillingMode,
    stdItemCount,
    stdTableName,
    stdTableId,
    stdKeySchema,
    stdTableCreationDateTime,
    stdProvisionedThroughput,
  )
where

import Network.AWS.DynamoDB.Types.BillingMode
import Network.AWS.DynamoDB.Types.KeySchemaElement
import Network.AWS.DynamoDB.Types.ProvisionedThroughput
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the details of the table when the backup was created.
--
-- /See:/ 'mkSourceTableDetails' smart constructor.
data SourceTableDetails = SourceTableDetails'
  { tableSizeBytes ::
      Lude.Maybe Lude.Integer,
    tableARN :: Lude.Maybe Lude.Text,
    billingMode :: Lude.Maybe BillingMode,
    itemCount :: Lude.Maybe Lude.Natural,
    tableName :: Lude.Text,
    tableId :: Lude.Text,
    keySchema :: Lude.NonEmpty KeySchemaElement,
    tableCreationDateTime :: Lude.Timestamp,
    provisionedThroughput :: ProvisionedThroughput
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SourceTableDetails' with the minimum fields required to make a request.
--
-- * 'billingMode' - Controls how you are charged for read and write throughput and how you manage capacity. This setting can be changed later.
--
--
--     * @PROVISIONED@ - Sets the read/write capacity mode to @PROVISIONED@ . We recommend using @PROVISIONED@ for predictable workloads.
--
--
--     * @PAY_PER_REQUEST@ - Sets the read/write capacity mode to @PAY_PER_REQUEST@ . We recommend using @PAY_PER_REQUEST@ for unpredictable workloads.
--
--
-- * 'itemCount' - Number of items in the table. Note that this is an approximate value.
-- * 'keySchema' - Schema of the table.
-- * 'provisionedThroughput' - Read IOPs and Write IOPS on the table when the backup was created.
-- * 'tableARN' - ARN of the table for which backup was created.
-- * 'tableCreationDateTime' - Time when the source table was created.
-- * 'tableId' - Unique identifier for the table for which the backup was created.
-- * 'tableName' - The name of the table for which the backup was created.
-- * 'tableSizeBytes' - Size of the table in bytes. Note that this is an approximate value.
mkSourceTableDetails ::
  -- | 'tableName'
  Lude.Text ->
  -- | 'tableId'
  Lude.Text ->
  -- | 'keySchema'
  Lude.NonEmpty KeySchemaElement ->
  -- | 'tableCreationDateTime'
  Lude.Timestamp ->
  -- | 'provisionedThroughput'
  ProvisionedThroughput ->
  SourceTableDetails
mkSourceTableDetails
  pTableName_
  pTableId_
  pKeySchema_
  pTableCreationDateTime_
  pProvisionedThroughput_ =
    SourceTableDetails'
      { tableSizeBytes = Lude.Nothing,
        tableARN = Lude.Nothing,
        billingMode = Lude.Nothing,
        itemCount = Lude.Nothing,
        tableName = pTableName_,
        tableId = pTableId_,
        keySchema = pKeySchema_,
        tableCreationDateTime = pTableCreationDateTime_,
        provisionedThroughput = pProvisionedThroughput_
      }

-- | Size of the table in bytes. Note that this is an approximate value.
--
-- /Note:/ Consider using 'tableSizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdTableSizeBytes :: Lens.Lens' SourceTableDetails (Lude.Maybe Lude.Integer)
stdTableSizeBytes = Lens.lens (tableSizeBytes :: SourceTableDetails -> Lude.Maybe Lude.Integer) (\s a -> s {tableSizeBytes = a} :: SourceTableDetails)
{-# DEPRECATED stdTableSizeBytes "Use generic-lens or generic-optics with 'tableSizeBytes' instead." #-}

-- | ARN of the table for which backup was created.
--
-- /Note:/ Consider using 'tableARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdTableARN :: Lens.Lens' SourceTableDetails (Lude.Maybe Lude.Text)
stdTableARN = Lens.lens (tableARN :: SourceTableDetails -> Lude.Maybe Lude.Text) (\s a -> s {tableARN = a} :: SourceTableDetails)
{-# DEPRECATED stdTableARN "Use generic-lens or generic-optics with 'tableARN' instead." #-}

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
stdBillingMode :: Lens.Lens' SourceTableDetails (Lude.Maybe BillingMode)
stdBillingMode = Lens.lens (billingMode :: SourceTableDetails -> Lude.Maybe BillingMode) (\s a -> s {billingMode = a} :: SourceTableDetails)
{-# DEPRECATED stdBillingMode "Use generic-lens or generic-optics with 'billingMode' instead." #-}

-- | Number of items in the table. Note that this is an approximate value.
--
-- /Note:/ Consider using 'itemCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdItemCount :: Lens.Lens' SourceTableDetails (Lude.Maybe Lude.Natural)
stdItemCount = Lens.lens (itemCount :: SourceTableDetails -> Lude.Maybe Lude.Natural) (\s a -> s {itemCount = a} :: SourceTableDetails)
{-# DEPRECATED stdItemCount "Use generic-lens or generic-optics with 'itemCount' instead." #-}

-- | The name of the table for which the backup was created.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdTableName :: Lens.Lens' SourceTableDetails Lude.Text
stdTableName = Lens.lens (tableName :: SourceTableDetails -> Lude.Text) (\s a -> s {tableName = a} :: SourceTableDetails)
{-# DEPRECATED stdTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | Unique identifier for the table for which the backup was created.
--
-- /Note:/ Consider using 'tableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdTableId :: Lens.Lens' SourceTableDetails Lude.Text
stdTableId = Lens.lens (tableId :: SourceTableDetails -> Lude.Text) (\s a -> s {tableId = a} :: SourceTableDetails)
{-# DEPRECATED stdTableId "Use generic-lens or generic-optics with 'tableId' instead." #-}

-- | Schema of the table.
--
-- /Note:/ Consider using 'keySchema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdKeySchema :: Lens.Lens' SourceTableDetails (Lude.NonEmpty KeySchemaElement)
stdKeySchema = Lens.lens (keySchema :: SourceTableDetails -> Lude.NonEmpty KeySchemaElement) (\s a -> s {keySchema = a} :: SourceTableDetails)
{-# DEPRECATED stdKeySchema "Use generic-lens or generic-optics with 'keySchema' instead." #-}

-- | Time when the source table was created.
--
-- /Note:/ Consider using 'tableCreationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdTableCreationDateTime :: Lens.Lens' SourceTableDetails Lude.Timestamp
stdTableCreationDateTime = Lens.lens (tableCreationDateTime :: SourceTableDetails -> Lude.Timestamp) (\s a -> s {tableCreationDateTime = a} :: SourceTableDetails)
{-# DEPRECATED stdTableCreationDateTime "Use generic-lens or generic-optics with 'tableCreationDateTime' instead." #-}

-- | Read IOPs and Write IOPS on the table when the backup was created.
--
-- /Note:/ Consider using 'provisionedThroughput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdProvisionedThroughput :: Lens.Lens' SourceTableDetails ProvisionedThroughput
stdProvisionedThroughput = Lens.lens (provisionedThroughput :: SourceTableDetails -> ProvisionedThroughput) (\s a -> s {provisionedThroughput = a} :: SourceTableDetails)
{-# DEPRECATED stdProvisionedThroughput "Use generic-lens or generic-optics with 'provisionedThroughput' instead." #-}

instance Lude.FromJSON SourceTableDetails where
  parseJSON =
    Lude.withObject
      "SourceTableDetails"
      ( \x ->
          SourceTableDetails'
            Lude.<$> (x Lude..:? "TableSizeBytes")
            Lude.<*> (x Lude..:? "TableArn")
            Lude.<*> (x Lude..:? "BillingMode")
            Lude.<*> (x Lude..:? "ItemCount")
            Lude.<*> (x Lude..: "TableName")
            Lude.<*> (x Lude..: "TableId")
            Lude.<*> (x Lude..: "KeySchema")
            Lude.<*> (x Lude..: "TableCreationDateTime")
            Lude.<*> (x Lude..: "ProvisionedThroughput")
      )
