{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ADMChannelRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.ADMChannelRequest
  ( ADMChannelRequest (..)
  -- * Smart constructor
  , mkADMChannelRequest
  -- * Lenses
  , admcrfClientSecret
  , admcrfClientId
  , admcrfEnabled
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the status and settings of the ADM (Amazon Device Messaging) channel for an application.
--
-- /See:/ 'mkADMChannelRequest' smart constructor.
data ADMChannelRequest = ADMChannelRequest'
  { clientSecret :: Core.Text
    -- ^ The Client Secret that you received from Amazon to send messages by using ADM.
  , clientId :: Core.Text
    -- ^ The Client ID that you received from Amazon to send messages by using ADM.
  , enabled :: Core.Maybe Core.Bool
    -- ^ Specifies whether to enable the ADM channel for the application.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ADMChannelRequest' value with any optional fields omitted.
mkADMChannelRequest
    :: Core.Text -- ^ 'clientSecret'
    -> Core.Text -- ^ 'clientId'
    -> ADMChannelRequest
mkADMChannelRequest clientSecret clientId
  = ADMChannelRequest'{clientSecret, clientId,
                       enabled = Core.Nothing}

-- | The Client Secret that you received from Amazon to send messages by using ADM.
--
-- /Note:/ Consider using 'clientSecret' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcrfClientSecret :: Lens.Lens' ADMChannelRequest Core.Text
admcrfClientSecret = Lens.field @"clientSecret"
{-# INLINEABLE admcrfClientSecret #-}
{-# DEPRECATED clientSecret "Use generic-lens or generic-optics with 'clientSecret' instead"  #-}

-- | The Client ID that you received from Amazon to send messages by using ADM.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcrfClientId :: Lens.Lens' ADMChannelRequest Core.Text
admcrfClientId = Lens.field @"clientId"
{-# INLINEABLE admcrfClientId #-}
{-# DEPRECATED clientId "Use generic-lens or generic-optics with 'clientId' instead"  #-}

-- | Specifies whether to enable the ADM channel for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcrfEnabled :: Lens.Lens' ADMChannelRequest (Core.Maybe Core.Bool)
admcrfEnabled = Lens.field @"enabled"
{-# INLINEABLE admcrfEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

instance Core.FromJSON ADMChannelRequest where
        toJSON ADMChannelRequest{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ClientSecret" Core..= clientSecret),
                  Core.Just ("ClientId" Core..= clientId),
                  ("Enabled" Core..=) Core.<$> enabled])
