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
    capReadCapacityUnits,
    capCapacityUnits,
    capWriteCapacityUnits,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the amount of provisioned throughput capacity consumed on a table or an index.
--
-- /See:/ 'mkCapacity' smart constructor.
data Capacity = Capacity'
  { readCapacityUnits ::
      Lude.Maybe Lude.Double,
    capacityUnits :: Lude.Maybe Lude.Double,
    writeCapacityUnits :: Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Capacity' with the minimum fields required to make a request.
--
-- * 'capacityUnits' - The total number of capacity units consumed on a table or an index.
-- * 'readCapacityUnits' - The total number of read capacity units consumed on a table or an index.
-- * 'writeCapacityUnits' - The total number of write capacity units consumed on a table or an index.
mkCapacity ::
  Capacity
mkCapacity =
  Capacity'
    { readCapacityUnits = Lude.Nothing,
      capacityUnits = Lude.Nothing,
      writeCapacityUnits = Lude.Nothing
    }

-- | The total number of read capacity units consumed on a table or an index.
--
-- /Note:/ Consider using 'readCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
capReadCapacityUnits :: Lens.Lens' Capacity (Lude.Maybe Lude.Double)
capReadCapacityUnits = Lens.lens (readCapacityUnits :: Capacity -> Lude.Maybe Lude.Double) (\s a -> s {readCapacityUnits = a} :: Capacity)
{-# DEPRECATED capReadCapacityUnits "Use generic-lens or generic-optics with 'readCapacityUnits' instead." #-}

-- | The total number of capacity units consumed on a table or an index.
--
-- /Note:/ Consider using 'capacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
capCapacityUnits :: Lens.Lens' Capacity (Lude.Maybe Lude.Double)
capCapacityUnits = Lens.lens (capacityUnits :: Capacity -> Lude.Maybe Lude.Double) (\s a -> s {capacityUnits = a} :: Capacity)
{-# DEPRECATED capCapacityUnits "Use generic-lens or generic-optics with 'capacityUnits' instead." #-}

-- | The total number of write capacity units consumed on a table or an index.
--
-- /Note:/ Consider using 'writeCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
capWriteCapacityUnits :: Lens.Lens' Capacity (Lude.Maybe Lude.Double)
capWriteCapacityUnits = Lens.lens (writeCapacityUnits :: Capacity -> Lude.Maybe Lude.Double) (\s a -> s {writeCapacityUnits = a} :: Capacity)
{-# DEPRECATED capWriteCapacityUnits "Use generic-lens or generic-optics with 'writeCapacityUnits' instead." #-}

instance Lude.FromJSON Capacity where
  parseJSON =
    Lude.withObject
      "Capacity"
      ( \x ->
          Capacity'
            Lude.<$> (x Lude..:? "ReadCapacityUnits")
            Lude.<*> (x Lude..:? "CapacityUnits")
            Lude.<*> (x Lude..:? "WriteCapacityUnits")
      )
