{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SMSChannelRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.SMSChannelRequest
  ( SMSChannelRequest (..)
  -- * Smart constructor
  , mkSMSChannelRequest
  -- * Lenses
  , sEnabled
  , sSenderId
  , sShortCode
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the status and settings of the SMS channel for an application.
--
-- /See:/ 'mkSMSChannelRequest' smart constructor.
data SMSChannelRequest = SMSChannelRequest'
  { enabled :: Core.Maybe Core.Bool
    -- ^ Specifies whether to enable the SMS channel for the application.
  , senderId :: Core.Maybe Core.Text
    -- ^ The identity that you want to display on recipients' devices when they receive messages from the SMS channel.
  , shortCode :: Core.Maybe Core.Text
    -- ^ The registered short code that you want to use when you send messages through the SMS channel.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SMSChannelRequest' value with any optional fields omitted.
mkSMSChannelRequest
    :: SMSChannelRequest
mkSMSChannelRequest
  = SMSChannelRequest'{enabled = Core.Nothing,
                       senderId = Core.Nothing, shortCode = Core.Nothing}

-- | Specifies whether to enable the SMS channel for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEnabled :: Lens.Lens' SMSChannelRequest (Core.Maybe Core.Bool)
sEnabled = Lens.field @"enabled"
{-# INLINEABLE sEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | The identity that you want to display on recipients' devices when they receive messages from the SMS channel.
--
-- /Note:/ Consider using 'senderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSenderId :: Lens.Lens' SMSChannelRequest (Core.Maybe Core.Text)
sSenderId = Lens.field @"senderId"
{-# INLINEABLE sSenderId #-}
{-# DEPRECATED senderId "Use generic-lens or generic-optics with 'senderId' instead"  #-}

-- | The registered short code that you want to use when you send messages through the SMS channel.
--
-- /Note:/ Consider using 'shortCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sShortCode :: Lens.Lens' SMSChannelRequest (Core.Maybe Core.Text)
sShortCode = Lens.field @"shortCode"
{-# INLINEABLE sShortCode #-}
{-# DEPRECATED shortCode "Use generic-lens or generic-optics with 'shortCode' instead"  #-}

instance Core.FromJSON SMSChannelRequest where
        toJSON SMSChannelRequest{..}
          = Core.object
              (Core.catMaybes
                 [("Enabled" Core..=) Core.<$> enabled,
                  ("SenderId" Core..=) Core.<$> senderId,
                  ("ShortCode" Core..=) Core.<$> shortCode])
