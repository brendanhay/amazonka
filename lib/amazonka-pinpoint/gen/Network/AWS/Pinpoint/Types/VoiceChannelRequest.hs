{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.VoiceChannelRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.VoiceChannelRequest
  ( VoiceChannelRequest (..),

    -- * Smart constructor
    mkVoiceChannelRequest,

    -- * Lenses
    vcrEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the status and settings of the voice channel for an application.
--
-- /See:/ 'mkVoiceChannelRequest' smart constructor.
newtype VoiceChannelRequest = VoiceChannelRequest'
  { -- | Specifies whether to enable the voice channel for the application.
    enabled :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'VoiceChannelRequest' value with any optional fields omitted.
mkVoiceChannelRequest ::
  VoiceChannelRequest
mkVoiceChannelRequest =
  VoiceChannelRequest' {enabled = Core.Nothing}

-- | Specifies whether to enable the voice channel for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcrEnabled :: Lens.Lens' VoiceChannelRequest (Core.Maybe Core.Bool)
vcrEnabled = Lens.field @"enabled"
{-# DEPRECATED vcrEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Core.FromJSON VoiceChannelRequest where
  toJSON VoiceChannelRequest {..} =
    Core.object
      (Core.catMaybes [("Enabled" Core..=) Core.<$> enabled])
