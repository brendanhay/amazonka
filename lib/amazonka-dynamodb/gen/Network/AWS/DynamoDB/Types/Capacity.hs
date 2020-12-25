{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.Capacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.Capacity
  ( Capacity (..),

    -- * Smart constructor
    mkCapacity,

    -- * Lenses
    cCapacityUnits,
    cReadCapacityUnits,
    cWriteCapacityUnits,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the amount of provisioned throughput capacity consumed on a table or an index.
--
-- /See:/ 'mkCapacity' smart constructor.
data Capacity = Capacity'
  { -- | The total number of capacity units consumed on a table or an index.
    capacityUnits :: Core.Maybe Core.Double,
    -- | The total number of read capacity units consumed on a table or an index.
    readCapacityUnits :: Core.Maybe Core.Double,
    -- | The total number of write capacity units consumed on a table or an index.
    writeCapacityUnits :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Capacity' value with any optional fields omitted.
mkCapacity ::
  Capacity
mkCapacity =
  Capacity'
    { capacityUnits = Core.Nothing,
      readCapacityUnits = Core.Nothing,
      writeCapacityUnits = Core.Nothing
    }

-- | The total number of capacity units consumed on a table or an index.
--
-- /Note:/ Consider using 'capacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCapacityUnits :: Lens.Lens' Capacity (Core.Maybe Core.Double)
cCapacityUnits = Lens.field @"capacityUnits"
{-# DEPRECATED cCapacityUnits "Use generic-lens or generic-optics with 'capacityUnits' instead." #-}

-- | The total number of read capacity units consumed on a table or an index.
--
-- /Note:/ Consider using 'readCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cReadCapacityUnits :: Lens.Lens' Capacity (Core.Maybe Core.Double)
cReadCapacityUnits = Lens.field @"readCapacityUnits"
{-# DEPRECATED cReadCapacityUnits "Use generic-lens or generic-optics with 'readCapacityUnits' instead." #-}

-- | The total number of write capacity units consumed on a table or an index.
--
-- /Note:/ Consider using 'writeCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cWriteCapacityUnits :: Lens.Lens' Capacity (Core.Maybe Core.Double)
cWriteCapacityUnits = Lens.field @"writeCapacityUnits"
{-# DEPRECATED cWriteCapacityUnits "Use generic-lens or generic-optics with 'writeCapacityUnits' instead." #-}

instance Core.FromJSON Capacity where
  parseJSON =
    Core.withObject "Capacity" Core.$
      \x ->
        Capacity'
          Core.<$> (x Core..:? "CapacityUnits")
          Core.<*> (x Core..:? "ReadCapacityUnits")
          Core.<*> (x Core..:? "WriteCapacityUnits")
