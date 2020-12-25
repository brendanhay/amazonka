{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexSettingsSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexSettingsSummary
  ( MultiplexSettingsSummary (..),

    -- * Smart constructor
    mkMultiplexSettingsSummary,

    -- * Lenses
    mTransportStreamBitrate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains summary configuration for a Multiplex event.
--
-- /See:/ 'mkMultiplexSettingsSummary' smart constructor.
newtype MultiplexSettingsSummary = MultiplexSettingsSummary'
  { -- | Transport stream bit rate.
    transportStreamBitrate :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'MultiplexSettingsSummary' value with any optional fields omitted.
mkMultiplexSettingsSummary ::
  MultiplexSettingsSummary
mkMultiplexSettingsSummary =
  MultiplexSettingsSummary' {transportStreamBitrate = Core.Nothing}

-- | Transport stream bit rate.
--
-- /Note:/ Consider using 'transportStreamBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mTransportStreamBitrate :: Lens.Lens' MultiplexSettingsSummary (Core.Maybe Core.Natural)
mTransportStreamBitrate = Lens.field @"transportStreamBitrate"
{-# DEPRECATED mTransportStreamBitrate "Use generic-lens or generic-optics with 'transportStreamBitrate' instead." #-}

instance Core.FromJSON MultiplexSettingsSummary where
  parseJSON =
    Core.withObject "MultiplexSettingsSummary" Core.$
      \x ->
        MultiplexSettingsSummary'
          Core.<$> (x Core..:? "transportStreamBitrate")
