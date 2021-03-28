{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.MessageBody
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.MessageBody
  ( MessageBody (..)
  -- * Smart constructor
  , mkMessageBody
  -- * Lenses
  , mbMessage
  , mbRequestID
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about an API request or response.
--
-- /See:/ 'mkMessageBody' smart constructor.
data MessageBody = MessageBody'
  { message :: Core.Maybe Core.Text
    -- ^ The message that's returned from the API.
  , requestID :: Core.Maybe Core.Text
    -- ^ The unique identifier for the request or response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MessageBody' value with any optional fields omitted.
mkMessageBody
    :: MessageBody
mkMessageBody
  = MessageBody'{message = Core.Nothing, requestID = Core.Nothing}

-- | The message that's returned from the API.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbMessage :: Lens.Lens' MessageBody (Core.Maybe Core.Text)
mbMessage = Lens.field @"message"
{-# INLINEABLE mbMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | The unique identifier for the request or response.
--
-- /Note:/ Consider using 'requestID' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbRequestID :: Lens.Lens' MessageBody (Core.Maybe Core.Text)
mbRequestID = Lens.field @"requestID"
{-# INLINEABLE mbRequestID #-}
{-# DEPRECATED requestID "Use generic-lens or generic-optics with 'requestID' instead"  #-}

instance Core.FromJSON MessageBody where
        parseJSON
          = Core.withObject "MessageBody" Core.$
              \ x ->
                MessageBody' Core.<$>
                  (x Core..:? "Message") Core.<*> x Core..:? "RequestID"
