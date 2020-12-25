{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.GCMChannelRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.GCMChannelRequest
  ( GCMChannelRequest (..),

    -- * Smart constructor
    mkGCMChannelRequest,

    -- * Lenses
    gApiKey,
    gEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the status and settings of the GCM channel for an application. This channel enables Amazon Pinpoint to send push notifications through the Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service.
--
-- /See:/ 'mkGCMChannelRequest' smart constructor.
data GCMChannelRequest = GCMChannelRequest'
  { -- | The Web API Key, also referred to as an /API_KEY/ or /server key/ , that you received from Google to communicate with Google services.
    apiKey :: Core.Text,
    -- | Specifies whether to enable the GCM channel for the application.
    enabled :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GCMChannelRequest' value with any optional fields omitted.
mkGCMChannelRequest ::
  -- | 'apiKey'
  Core.Text ->
  GCMChannelRequest
mkGCMChannelRequest apiKey =
  GCMChannelRequest' {apiKey, enabled = Core.Nothing}

-- | The Web API Key, also referred to as an /API_KEY/ or /server key/ , that you received from Google to communicate with Google services.
--
-- /Note:/ Consider using 'apiKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gApiKey :: Lens.Lens' GCMChannelRequest Core.Text
gApiKey = Lens.field @"apiKey"
{-# DEPRECATED gApiKey "Use generic-lens or generic-optics with 'apiKey' instead." #-}

-- | Specifies whether to enable the GCM channel for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gEnabled :: Lens.Lens' GCMChannelRequest (Core.Maybe Core.Bool)
gEnabled = Lens.field @"enabled"
{-# DEPRECATED gEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Core.FromJSON GCMChannelRequest where
  toJSON GCMChannelRequest {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ApiKey" Core..= apiKey),
            ("Enabled" Core..=) Core.<$> enabled
          ]
      )
