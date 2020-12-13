{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ChannelStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ChannelStatistics
  ( ChannelStatistics (..),

    -- * Smart constructor
    mkChannelStatistics,

    -- * Lenses
    csSize,
  )
where

import Network.AWS.IoTAnalytics.Types.EstimatedResourceSize
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Statistics information about the channel.
--
-- /See:/ 'mkChannelStatistics' smart constructor.
newtype ChannelStatistics = ChannelStatistics'
  { -- | The estimated size of the channel.
    size :: Lude.Maybe EstimatedResourceSize
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChannelStatistics' with the minimum fields required to make a request.
--
-- * 'size' - The estimated size of the channel.
mkChannelStatistics ::
  ChannelStatistics
mkChannelStatistics = ChannelStatistics' {size = Lude.Nothing}

-- | The estimated size of the channel.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSize :: Lens.Lens' ChannelStatistics (Lude.Maybe EstimatedResourceSize)
csSize = Lens.lens (size :: ChannelStatistics -> Lude.Maybe EstimatedResourceSize) (\s a -> s {size = a} :: ChannelStatistics)
{-# DEPRECATED csSize "Use generic-lens or generic-optics with 'size' instead." #-}

instance Lude.FromJSON ChannelStatistics where
  parseJSON =
    Lude.withObject
      "ChannelStatistics"
      (\x -> ChannelStatistics' Lude.<$> (x Lude..:? "size"))
