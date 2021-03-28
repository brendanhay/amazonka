{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CreateTemplateMessageBody
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.CreateTemplateMessageBody
  ( CreateTemplateMessageBody (..)
  -- * Smart constructor
  , mkCreateTemplateMessageBody
  -- * Lenses
  , ctmbArn
  , ctmbMessage
  , ctmbRequestID
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about a request to create a message template.
--
-- /See:/ 'mkCreateTemplateMessageBody' smart constructor.
data CreateTemplateMessageBody = CreateTemplateMessageBody'
  { arn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the message template that was created.
  , message :: Core.Maybe Core.Text
    -- ^ The message that's returned from the API for the request to create the message template.
  , requestID :: Core.Maybe Core.Text
    -- ^ The unique identifier for the request to create the message template.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTemplateMessageBody' value with any optional fields omitted.
mkCreateTemplateMessageBody
    :: CreateTemplateMessageBody
mkCreateTemplateMessageBody
  = CreateTemplateMessageBody'{arn = Core.Nothing,
                               message = Core.Nothing, requestID = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the message template that was created.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmbArn :: Lens.Lens' CreateTemplateMessageBody (Core.Maybe Core.Text)
ctmbArn = Lens.field @"arn"
{-# INLINEABLE ctmbArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The message that's returned from the API for the request to create the message template.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmbMessage :: Lens.Lens' CreateTemplateMessageBody (Core.Maybe Core.Text)
ctmbMessage = Lens.field @"message"
{-# INLINEABLE ctmbMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | The unique identifier for the request to create the message template.
--
-- /Note:/ Consider using 'requestID' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmbRequestID :: Lens.Lens' CreateTemplateMessageBody (Core.Maybe Core.Text)
ctmbRequestID = Lens.field @"requestID"
{-# INLINEABLE ctmbRequestID #-}
{-# DEPRECATED requestID "Use generic-lens or generic-optics with 'requestID' instead"  #-}

instance Core.FromJSON CreateTemplateMessageBody where
        parseJSON
          = Core.withObject "CreateTemplateMessageBody" Core.$
              \ x ->
                CreateTemplateMessageBody' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "Message" Core.<*>
                    x Core..:? "RequestID"
