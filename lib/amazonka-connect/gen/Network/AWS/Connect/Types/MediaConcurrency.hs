-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.MediaConcurrency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.MediaConcurrency
  ( MediaConcurrency (..),

    -- * Smart constructor
    mkMediaConcurrency,

    -- * Lenses
    mcChannel,
    mcConcurrency,
  )
where

import Network.AWS.Connect.Types.Channel
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about which channels are supported, and how many contacts an agent can have on a channel simultaneously.
--
-- /See:/ 'mkMediaConcurrency' smart constructor.
data MediaConcurrency = MediaConcurrency'
  { channel :: Channel,
    concurrency :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MediaConcurrency' with the minimum fields required to make a request.
--
-- * 'channel' - The channels that agents can handle in the Contact Control Panel (CCP).
-- * 'concurrency' - The number of contacts an agent can have on a channel simultaneously.
mkMediaConcurrency ::
  -- | 'channel'
  Channel ->
  -- | 'concurrency'
  Lude.Natural ->
  MediaConcurrency
mkMediaConcurrency pChannel_ pConcurrency_ =
  MediaConcurrency'
    { channel = pChannel_,
      concurrency = pConcurrency_
    }

-- | The channels that agents can handle in the Contact Control Panel (CCP).
--
-- /Note:/ Consider using 'channel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcChannel :: Lens.Lens' MediaConcurrency Channel
mcChannel = Lens.lens (channel :: MediaConcurrency -> Channel) (\s a -> s {channel = a} :: MediaConcurrency)
{-# DEPRECATED mcChannel "Use generic-lens or generic-optics with 'channel' instead." #-}

-- | The number of contacts an agent can have on a channel simultaneously.
--
-- /Note:/ Consider using 'concurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcConcurrency :: Lens.Lens' MediaConcurrency Lude.Natural
mcConcurrency = Lens.lens (concurrency :: MediaConcurrency -> Lude.Natural) (\s a -> s {concurrency = a} :: MediaConcurrency)
{-# DEPRECATED mcConcurrency "Use generic-lens or generic-optics with 'concurrency' instead." #-}

instance Lude.FromJSON MediaConcurrency where
  parseJSON =
    Lude.withObject
      "MediaConcurrency"
      ( \x ->
          MediaConcurrency'
            Lude.<$> (x Lude..: "Channel") Lude.<*> (x Lude..: "Concurrency")
      )

instance Lude.ToJSON MediaConcurrency where
  toJSON MediaConcurrency' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Channel" Lude..= channel),
            Lude.Just ("Concurrency" Lude..= concurrency)
          ]
      )
