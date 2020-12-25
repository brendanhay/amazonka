{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ADMChannelRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ADMChannelRequest
  ( ADMChannelRequest (..),

    -- * Smart constructor
    mkADMChannelRequest,

    -- * Lenses
    admcrfClientSecret,
    admcrfClientId,
    admcrfEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the status and settings of the ADM (Amazon Device Messaging) channel for an application.
--
-- /See:/ 'mkADMChannelRequest' smart constructor.
data ADMChannelRequest = ADMChannelRequest'
  { -- | The Client Secret that you received from Amazon to send messages by using ADM.
    clientSecret :: Core.Text,
    -- | The Client ID that you received from Amazon to send messages by using ADM.
    clientId :: Core.Text,
    -- | Specifies whether to enable the ADM channel for the application.
    enabled :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ADMChannelRequest' value with any optional fields omitted.
mkADMChannelRequest ::
  -- | 'clientSecret'
  Core.Text ->
  -- | 'clientId'
  Core.Text ->
  ADMChannelRequest
mkADMChannelRequest clientSecret clientId =
  ADMChannelRequest'
    { clientSecret,
      clientId,
      enabled = Core.Nothing
    }

-- | The Client Secret that you received from Amazon to send messages by using ADM.
--
-- /Note:/ Consider using 'clientSecret' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcrfClientSecret :: Lens.Lens' ADMChannelRequest Core.Text
admcrfClientSecret = Lens.field @"clientSecret"
{-# DEPRECATED admcrfClientSecret "Use generic-lens or generic-optics with 'clientSecret' instead." #-}

-- | The Client ID that you received from Amazon to send messages by using ADM.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcrfClientId :: Lens.Lens' ADMChannelRequest Core.Text
admcrfClientId = Lens.field @"clientId"
{-# DEPRECATED admcrfClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | Specifies whether to enable the ADM channel for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcrfEnabled :: Lens.Lens' ADMChannelRequest (Core.Maybe Core.Bool)
admcrfEnabled = Lens.field @"enabled"
{-# DEPRECATED admcrfEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Core.FromJSON ADMChannelRequest where
  toJSON ADMChannelRequest {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ClientSecret" Core..= clientSecret),
            Core.Just ("ClientId" Core..= clientId),
            ("Enabled" Core..=) Core.<$> enabled
          ]
      )
