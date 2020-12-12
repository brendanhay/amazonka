{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.Dimensions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.Dimensions
  ( Dimensions (..),

    -- * Smart constructor
    mkDimensions,

    -- * Lenses
    dChannel,
    dQueue,
  )
where

import Network.AWS.Connect.Types.Channel
import Network.AWS.Connect.Types.QueueReference
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the dimensions for a set of metrics.
--
-- /See:/ 'mkDimensions' smart constructor.
data Dimensions = Dimensions'
  { channel :: Lude.Maybe Channel,
    queue :: Lude.Maybe QueueReference
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Dimensions' with the minimum fields required to make a request.
--
-- * 'channel' - The channel used for grouping and filters.
-- * 'queue' - Information about the queue for which metrics are returned.
mkDimensions ::
  Dimensions
mkDimensions =
  Dimensions' {channel = Lude.Nothing, queue = Lude.Nothing}

-- | The channel used for grouping and filters.
--
-- /Note:/ Consider using 'channel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dChannel :: Lens.Lens' Dimensions (Lude.Maybe Channel)
dChannel = Lens.lens (channel :: Dimensions -> Lude.Maybe Channel) (\s a -> s {channel = a} :: Dimensions)
{-# DEPRECATED dChannel "Use generic-lens or generic-optics with 'channel' instead." #-}

-- | Information about the queue for which metrics are returned.
--
-- /Note:/ Consider using 'queue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dQueue :: Lens.Lens' Dimensions (Lude.Maybe QueueReference)
dQueue = Lens.lens (queue :: Dimensions -> Lude.Maybe QueueReference) (\s a -> s {queue = a} :: Dimensions)
{-# DEPRECATED dQueue "Use generic-lens or generic-optics with 'queue' instead." #-}

instance Lude.FromJSON Dimensions where
  parseJSON =
    Lude.withObject
      "Dimensions"
      ( \x ->
          Dimensions'
            Lude.<$> (x Lude..:? "Channel") Lude.<*> (x Lude..:? "Queue")
      )
