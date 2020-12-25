{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
import qualified Network.AWS.Prelude as Core

-- | Represents the provisioned throughput settings for a specified table or index. The settings can be modified using the @UpdateTable@ operation.
--
-- For current minimum and maximum provisioned throughput values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
--
-- /See:/ 'mkProvisionedThroughput' smart constructor.
data ProvisionedThroughput = ProvisionedThroughput'
  { -- | The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
    --
    -- If read/write capacity mode is @PAY_PER_REQUEST@ the value is set to 0.
    readCapacityUnits :: Core.Natural,
    -- | The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
    --
    -- If read/write capacity mode is @PAY_PER_REQUEST@ the value is set to 0.
    writeCapacityUnits :: Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProvisionedThroughput' value with any optional fields omitted.
mkProvisionedThroughput ::
  -- | 'readCapacityUnits'
  Core.Natural ->
  -- | 'writeCapacityUnits'
  Core.Natural ->
  ProvisionedThroughput
mkProvisionedThroughput readCapacityUnits writeCapacityUnits =
  ProvisionedThroughput' {readCapacityUnits, writeCapacityUnits}

-- | The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
--
-- If read/write capacity mode is @PAY_PER_REQUEST@ the value is set to 0.
--
-- /Note:/ Consider using 'readCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptReadCapacityUnits :: Lens.Lens' ProvisionedThroughput Core.Natural
ptReadCapacityUnits = Lens.field @"readCapacityUnits"
{-# DEPRECATED ptReadCapacityUnits "Use generic-lens or generic-optics with 'readCapacityUnits' instead." #-}

-- | The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
--
-- If read/write capacity mode is @PAY_PER_REQUEST@ the value is set to 0.
--
-- /Note:/ Consider using 'writeCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptWriteCapacityUnits :: Lens.Lens' ProvisionedThroughput Core.Natural
ptWriteCapacityUnits = Lens.field @"writeCapacityUnits"
{-# DEPRECATED ptWriteCapacityUnits "Use generic-lens or generic-optics with 'writeCapacityUnits' instead." #-}

instance Core.FromJSON ProvisionedThroughput where
  toJSON ProvisionedThroughput {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ReadCapacityUnits" Core..= readCapacityUnits),
            Core.Just ("WriteCapacityUnits" Core..= writeCapacityUnits)
          ]
      )

instance Core.FromJSON ProvisionedThroughput where
  parseJSON =
    Core.withObject "ProvisionedThroughput" Core.$
      \x ->
        ProvisionedThroughput'
          Core.<$> (x Core..: "ReadCapacityUnits")
          Core.<*> (x Core..: "WriteCapacityUnits")
