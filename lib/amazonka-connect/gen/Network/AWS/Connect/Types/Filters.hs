-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.Filters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.Filters
  ( Filters (..),

    -- * Smart constructor
    mkFilters,

    -- * Lenses
    fQueues,
    fChannels,
  )
where

import Network.AWS.Connect.Types.Channel
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the filter to apply when retrieving metrics.
--
-- /See:/ 'mkFilters' smart constructor.
data Filters = Filters'
  { queues ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    channels :: Lude.Maybe [Channel]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Filters' with the minimum fields required to make a request.
--
-- * 'channels' - The channel to use to filter the metrics.
-- * 'queues' - The queues to use to filter the metrics. You can specify up to 100 queues per request.
mkFilters ::
  Filters
mkFilters =
  Filters' {queues = Lude.Nothing, channels = Lude.Nothing}

-- | The queues to use to filter the metrics. You can specify up to 100 queues per request.
--
-- /Note:/ Consider using 'queues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fQueues :: Lens.Lens' Filters (Lude.Maybe (Lude.NonEmpty Lude.Text))
fQueues = Lens.lens (queues :: Filters -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {queues = a} :: Filters)
{-# DEPRECATED fQueues "Use generic-lens or generic-optics with 'queues' instead." #-}

-- | The channel to use to filter the metrics.
--
-- /Note:/ Consider using 'channels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fChannels :: Lens.Lens' Filters (Lude.Maybe [Channel])
fChannels = Lens.lens (channels :: Filters -> Lude.Maybe [Channel]) (\s a -> s {channels = a} :: Filters)
{-# DEPRECATED fChannels "Use generic-lens or generic-optics with 'channels' instead." #-}

instance Lude.ToJSON Filters where
  toJSON Filters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Queues" Lude..=) Lude.<$> queues,
            ("Channels" Lude..=) Lude.<$> channels
          ]
      )
