-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ProvisionedThroughputDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ProvisionedThroughputDescription
  ( ProvisionedThroughputDescription (..),

    -- * Smart constructor
    mkProvisionedThroughputDescription,

    -- * Lenses
    ptdReadCapacityUnits,
    ptdLastDecreaseDateTime,
    ptdWriteCapacityUnits,
    ptdNumberOfDecreasesToday,
    ptdLastIncreaseDateTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the provisioned throughput settings for the table, consisting of read and write capacity units, along with data about increases and decreases.
--
-- /See:/ 'mkProvisionedThroughputDescription' smart constructor.
data ProvisionedThroughputDescription = ProvisionedThroughputDescription'
  { readCapacityUnits ::
      Lude.Maybe Lude.Natural,
    lastDecreaseDateTime ::
      Lude.Maybe Lude.Timestamp,
    writeCapacityUnits ::
      Lude.Maybe Lude.Natural,
    numberOfDecreasesToday ::
      Lude.Maybe Lude.Natural,
    lastIncreaseDateTime ::
      Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProvisionedThroughputDescription' with the minimum fields required to make a request.
--
-- * 'lastDecreaseDateTime' - The date and time of the last provisioned throughput decrease for this table.
-- * 'lastIncreaseDateTime' - The date and time of the last provisioned throughput increase for this table.
-- * 'numberOfDecreasesToday' - The number of provisioned throughput decreases for this table during this UTC calendar day. For current maximums on provisioned throughput decreases, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
-- * 'readCapacityUnits' - The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . Eventually consistent reads require less effort than strongly consistent reads, so a setting of 50 @ReadCapacityUnits@ per second provides 100 eventually consistent @ReadCapacityUnits@ per second.
-- * 'writeCapacityUnits' - The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ .
mkProvisionedThroughputDescription ::
  ProvisionedThroughputDescription
mkProvisionedThroughputDescription =
  ProvisionedThroughputDescription'
    { readCapacityUnits =
        Lude.Nothing,
      lastDecreaseDateTime = Lude.Nothing,
      writeCapacityUnits = Lude.Nothing,
      numberOfDecreasesToday = Lude.Nothing,
      lastIncreaseDateTime = Lude.Nothing
    }

-- | The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . Eventually consistent reads require less effort than strongly consistent reads, so a setting of 50 @ReadCapacityUnits@ per second provides 100 eventually consistent @ReadCapacityUnits@ per second.
--
-- /Note:/ Consider using 'readCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptdReadCapacityUnits :: Lens.Lens' ProvisionedThroughputDescription (Lude.Maybe Lude.Natural)
ptdReadCapacityUnits = Lens.lens (readCapacityUnits :: ProvisionedThroughputDescription -> Lude.Maybe Lude.Natural) (\s a -> s {readCapacityUnits = a} :: ProvisionedThroughputDescription)
{-# DEPRECATED ptdReadCapacityUnits "Use generic-lens or generic-optics with 'readCapacityUnits' instead." #-}

-- | The date and time of the last provisioned throughput decrease for this table.
--
-- /Note:/ Consider using 'lastDecreaseDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptdLastDecreaseDateTime :: Lens.Lens' ProvisionedThroughputDescription (Lude.Maybe Lude.Timestamp)
ptdLastDecreaseDateTime = Lens.lens (lastDecreaseDateTime :: ProvisionedThroughputDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastDecreaseDateTime = a} :: ProvisionedThroughputDescription)
{-# DEPRECATED ptdLastDecreaseDateTime "Use generic-lens or generic-optics with 'lastDecreaseDateTime' instead." #-}

-- | The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ .
--
-- /Note:/ Consider using 'writeCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptdWriteCapacityUnits :: Lens.Lens' ProvisionedThroughputDescription (Lude.Maybe Lude.Natural)
ptdWriteCapacityUnits = Lens.lens (writeCapacityUnits :: ProvisionedThroughputDescription -> Lude.Maybe Lude.Natural) (\s a -> s {writeCapacityUnits = a} :: ProvisionedThroughputDescription)
{-# DEPRECATED ptdWriteCapacityUnits "Use generic-lens or generic-optics with 'writeCapacityUnits' instead." #-}

-- | The number of provisioned throughput decreases for this table during this UTC calendar day. For current maximums on provisioned throughput decreases, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'numberOfDecreasesToday' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptdNumberOfDecreasesToday :: Lens.Lens' ProvisionedThroughputDescription (Lude.Maybe Lude.Natural)
ptdNumberOfDecreasesToday = Lens.lens (numberOfDecreasesToday :: ProvisionedThroughputDescription -> Lude.Maybe Lude.Natural) (\s a -> s {numberOfDecreasesToday = a} :: ProvisionedThroughputDescription)
{-# DEPRECATED ptdNumberOfDecreasesToday "Use generic-lens or generic-optics with 'numberOfDecreasesToday' instead." #-}

-- | The date and time of the last provisioned throughput increase for this table.
--
-- /Note:/ Consider using 'lastIncreaseDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptdLastIncreaseDateTime :: Lens.Lens' ProvisionedThroughputDescription (Lude.Maybe Lude.Timestamp)
ptdLastIncreaseDateTime = Lens.lens (lastIncreaseDateTime :: ProvisionedThroughputDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastIncreaseDateTime = a} :: ProvisionedThroughputDescription)
{-# DEPRECATED ptdLastIncreaseDateTime "Use generic-lens or generic-optics with 'lastIncreaseDateTime' instead." #-}

instance Lude.FromJSON ProvisionedThroughputDescription where
  parseJSON =
    Lude.withObject
      "ProvisionedThroughputDescription"
      ( \x ->
          ProvisionedThroughputDescription'
            Lude.<$> (x Lude..:? "ReadCapacityUnits")
            Lude.<*> (x Lude..:? "LastDecreaseDateTime")
            Lude.<*> (x Lude..:? "WriteCapacityUnits")
            Lude.<*> (x Lude..:? "NumberOfDecreasesToday")
            Lude.<*> (x Lude..:? "LastIncreaseDateTime")
      )
