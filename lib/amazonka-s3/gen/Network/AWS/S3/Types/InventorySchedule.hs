{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.InventorySchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.InventorySchedule
  ( InventorySchedule (..),

    -- * Smart constructor
    mkInventorySchedule,

    -- * Lenses
    isFrequency,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.InventoryFrequency

-- | Specifies the schedule for generating inventory results.
--
-- /See:/ 'mkInventorySchedule' smart constructor.
newtype InventorySchedule = InventorySchedule'
  { -- | Specifies how frequently inventory results are produced.
    frequency :: InventoryFrequency
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InventorySchedule' with the minimum fields required to make a request.
--
-- * 'frequency' - Specifies how frequently inventory results are produced.
mkInventorySchedule ::
  -- | 'frequency'
  InventoryFrequency ->
  InventorySchedule
mkInventorySchedule pFrequency_ =
  InventorySchedule' {frequency = pFrequency_}

-- | Specifies how frequently inventory results are produced.
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isFrequency :: Lens.Lens' InventorySchedule InventoryFrequency
isFrequency = Lens.lens (frequency :: InventorySchedule -> InventoryFrequency) (\s a -> s {frequency = a} :: InventorySchedule)
{-# DEPRECATED isFrequency "Use generic-lens or generic-optics with 'frequency' instead." #-}

instance Lude.FromXML InventorySchedule where
  parseXML x = InventorySchedule' Lude.<$> (x Lude..@ "Frequency")

instance Lude.ToXML InventorySchedule where
  toXML InventorySchedule' {..} =
    Lude.mconcat ["Frequency" Lude.@= frequency]
