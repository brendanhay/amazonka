{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.SlotMigration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.SlotMigration
  ( SlotMigration (..),

    -- * Smart constructor
    mkSlotMigration,

    -- * Lenses
    smProgressPercentage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the progress of an online resharding operation.
--
-- /See:/ 'mkSlotMigration' smart constructor.
newtype SlotMigration = SlotMigration'
  { progressPercentage ::
      Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SlotMigration' with the minimum fields required to make a request.
--
-- * 'progressPercentage' - The percentage of the slot migration that is complete.
mkSlotMigration ::
  SlotMigration
mkSlotMigration = SlotMigration' {progressPercentage = Lude.Nothing}

-- | The percentage of the slot migration that is complete.
--
-- /Note:/ Consider using 'progressPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smProgressPercentage :: Lens.Lens' SlotMigration (Lude.Maybe Lude.Double)
smProgressPercentage = Lens.lens (progressPercentage :: SlotMigration -> Lude.Maybe Lude.Double) (\s a -> s {progressPercentage = a} :: SlotMigration)
{-# DEPRECATED smProgressPercentage "Use generic-lens or generic-optics with 'progressPercentage' instead." #-}

instance Lude.FromXML SlotMigration where
  parseXML x =
    SlotMigration' Lude.<$> (x Lude..@? "ProgressPercentage")
