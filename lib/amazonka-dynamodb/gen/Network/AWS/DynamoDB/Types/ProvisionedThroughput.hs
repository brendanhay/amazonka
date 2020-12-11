-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ProvisionedThroughput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ProvisionedThroughput
  ( ProvisionedThroughput (..),

    -- * Smart constructor
    mkProvisionedThroughput,

    -- * Lenses
    ptReadCapacityUnits,
    ptWriteCapacityUnits,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the provisioned throughput settings for a specified table or index. The settings can be modified using the @UpdateTable@ operation.
--
-- For current minimum and maximum provisioned throughput values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
--
-- /See:/ 'mkProvisionedThroughput' smart constructor.
data ProvisionedThroughput = ProvisionedThroughput'
  { readCapacityUnits ::
      Lude.Natural,
    writeCapacityUnits :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProvisionedThroughput' with the minimum fields required to make a request.
--
-- * 'readCapacityUnits' - The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
--
-- If read/write capacity mode is @PAY_PER_REQUEST@ the value is set to 0.
-- * 'writeCapacityUnits' - The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
--
-- If read/write capacity mode is @PAY_PER_REQUEST@ the value is set to 0.
mkProvisionedThroughput ::
  -- | 'readCapacityUnits'
  Lude.Natural ->
  -- | 'writeCapacityUnits'
  Lude.Natural ->
  ProvisionedThroughput
mkProvisionedThroughput pReadCapacityUnits_ pWriteCapacityUnits_ =
  ProvisionedThroughput'
    { readCapacityUnits = pReadCapacityUnits_,
      writeCapacityUnits = pWriteCapacityUnits_
    }

-- | The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
--
-- If read/write capacity mode is @PAY_PER_REQUEST@ the value is set to 0.
--
-- /Note:/ Consider using 'readCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptReadCapacityUnits :: Lens.Lens' ProvisionedThroughput Lude.Natural
ptReadCapacityUnits = Lens.lens (readCapacityUnits :: ProvisionedThroughput -> Lude.Natural) (\s a -> s {readCapacityUnits = a} :: ProvisionedThroughput)
{-# DEPRECATED ptReadCapacityUnits "Use generic-lens or generic-optics with 'readCapacityUnits' instead." #-}

-- | The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
--
-- If read/write capacity mode is @PAY_PER_REQUEST@ the value is set to 0.
--
-- /Note:/ Consider using 'writeCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptWriteCapacityUnits :: Lens.Lens' ProvisionedThroughput Lude.Natural
ptWriteCapacityUnits = Lens.lens (writeCapacityUnits :: ProvisionedThroughput -> Lude.Natural) (\s a -> s {writeCapacityUnits = a} :: ProvisionedThroughput)
{-# DEPRECATED ptWriteCapacityUnits "Use generic-lens or generic-optics with 'writeCapacityUnits' instead." #-}

instance Lude.FromJSON ProvisionedThroughput where
  parseJSON =
    Lude.withObject
      "ProvisionedThroughput"
      ( \x ->
          ProvisionedThroughput'
            Lude.<$> (x Lude..: "ReadCapacityUnits")
            Lude.<*> (x Lude..: "WriteCapacityUnits")
      )

instance Lude.ToJSON ProvisionedThroughput where
  toJSON ProvisionedThroughput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ReadCapacityUnits" Lude..= readCapacityUnits),
            Lude.Just ("WriteCapacityUnits" Lude..= writeCapacityUnits)
          ]
      )
