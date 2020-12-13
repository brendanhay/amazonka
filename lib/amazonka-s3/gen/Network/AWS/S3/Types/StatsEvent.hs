{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.StatsEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.StatsEvent
  ( StatsEvent (..),

    -- * Smart constructor
    mkStatsEvent,

    -- * Lenses
    seDetails,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Stats

-- | Container for the Stats Event.
--
-- /See:/ 'mkStatsEvent' smart constructor.
newtype StatsEvent = StatsEvent'
  { -- | The Stats event details.
    details :: Lude.Maybe Stats
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StatsEvent' with the minimum fields required to make a request.
--
-- * 'details' - The Stats event details.
mkStatsEvent ::
  StatsEvent
mkStatsEvent = StatsEvent' {details = Lude.Nothing}

-- | The Stats event details.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seDetails :: Lens.Lens' StatsEvent (Lude.Maybe Stats)
seDetails = Lens.lens (details :: StatsEvent -> Lude.Maybe Stats) (\s a -> s {details = a} :: StatsEvent)
{-# DEPRECATED seDetails "Use generic-lens or generic-optics with 'details' instead." #-}

instance Lude.FromXML StatsEvent where
  parseXML x = StatsEvent' Lude.<$> (x Lude..@? "Details")
