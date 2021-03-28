{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.MediaConcurrency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.MediaConcurrency
  ( MediaConcurrency (..)
  -- * Smart constructor
  , mkMediaConcurrency
  -- * Lenses
  , mcChannel
  , mcConcurrency
  ) where

import qualified Network.AWS.Connect.Types.Channel as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about which channels are supported, and how many contacts an agent can have on a channel simultaneously.
--
-- /See:/ 'mkMediaConcurrency' smart constructor.
data MediaConcurrency = MediaConcurrency'
  { channel :: Types.Channel
    -- ^ The channels that agents can handle in the Contact Control Panel (CCP).
  , concurrency :: Core.Natural
    -- ^ The number of contacts an agent can have on a channel simultaneously.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MediaConcurrency' value with any optional fields omitted.
mkMediaConcurrency
    :: Types.Channel -- ^ 'channel'
    -> Core.Natural -- ^ 'concurrency'
    -> MediaConcurrency
mkMediaConcurrency channel concurrency
  = MediaConcurrency'{channel, concurrency}

-- | The channels that agents can handle in the Contact Control Panel (CCP).
--
-- /Note:/ Consider using 'channel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcChannel :: Lens.Lens' MediaConcurrency Types.Channel
mcChannel = Lens.field @"channel"
{-# INLINEABLE mcChannel #-}
{-# DEPRECATED channel "Use generic-lens or generic-optics with 'channel' instead"  #-}

-- | The number of contacts an agent can have on a channel simultaneously.
--
-- /Note:/ Consider using 'concurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcConcurrency :: Lens.Lens' MediaConcurrency Core.Natural
mcConcurrency = Lens.field @"concurrency"
{-# INLINEABLE mcConcurrency #-}
{-# DEPRECATED concurrency "Use generic-lens or generic-optics with 'concurrency' instead"  #-}

instance Core.FromJSON MediaConcurrency where
        toJSON MediaConcurrency{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Channel" Core..= channel),
                  Core.Just ("Concurrency" Core..= concurrency)])

instance Core.FromJSON MediaConcurrency where
        parseJSON
          = Core.withObject "MediaConcurrency" Core.$
              \ x ->
                MediaConcurrency' Core.<$>
                  (x Core..: "Channel") Core.<*> x Core..: "Concurrency"
