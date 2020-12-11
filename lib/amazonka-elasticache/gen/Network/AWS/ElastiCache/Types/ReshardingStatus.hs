-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ReshardingStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ReshardingStatus
  ( ReshardingStatus (..),

    -- * Smart constructor
    mkReshardingStatus,

    -- * Lenses
    rsSlotMigration,
  )
where

import Network.AWS.ElastiCache.Types.SlotMigration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The status of an online resharding operation.
--
-- /See:/ 'mkReshardingStatus' smart constructor.
newtype ReshardingStatus = ReshardingStatus'
  { slotMigration ::
      Lude.Maybe SlotMigration
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReshardingStatus' with the minimum fields required to make a request.
--
-- * 'slotMigration' - Represents the progress of an online resharding operation.
mkReshardingStatus ::
  ReshardingStatus
mkReshardingStatus =
  ReshardingStatus' {slotMigration = Lude.Nothing}

-- | Represents the progress of an online resharding operation.
--
-- /Note:/ Consider using 'slotMigration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsSlotMigration :: Lens.Lens' ReshardingStatus (Lude.Maybe SlotMigration)
rsSlotMigration = Lens.lens (slotMigration :: ReshardingStatus -> Lude.Maybe SlotMigration) (\s a -> s {slotMigration = a} :: ReshardingStatus)
{-# DEPRECATED rsSlotMigration "Use generic-lens or generic-optics with 'slotMigration' instead." #-}

instance Lude.FromXML ReshardingStatus where
  parseXML x =
    ReshardingStatus' Lude.<$> (x Lude..@? "SlotMigration")
