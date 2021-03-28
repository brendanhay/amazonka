{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.Message
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.Message
  ( Message (..)
  -- * Smart constructor
  , mkMessage
  -- * Lenses
  , mMessageId
  , mPayload
  ) where

import qualified Network.AWS.IoTAnalytics.Types.MessageId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a message.
--
-- /See:/ 'mkMessage' smart constructor.
data Message = Message'
  { messageId :: Types.MessageId
    -- ^ The ID you want to assign to the message. Each @messageId@ must be unique within each batch sent.
  , payload :: Core.Base64
    -- ^ The payload of the message. This can be a JSON string or a base64-encoded string representing binary data, in which case you must decode it by means of a pipeline activity.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Message' value with any optional fields omitted.
mkMessage
    :: Types.MessageId -- ^ 'messageId'
    -> Core.Base64 -- ^ 'payload'
    -> Message
mkMessage messageId payload = Message'{messageId, payload}

-- | The ID you want to assign to the message. Each @messageId@ must be unique within each batch sent.
--
-- /Note:/ Consider using 'messageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mMessageId :: Lens.Lens' Message Types.MessageId
mMessageId = Lens.field @"messageId"
{-# INLINEABLE mMessageId #-}
{-# DEPRECATED messageId "Use generic-lens or generic-optics with 'messageId' instead"  #-}

-- | The payload of the message. This can be a JSON string or a base64-encoded string representing binary data, in which case you must decode it by means of a pipeline activity.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mPayload :: Lens.Lens' Message Core.Base64
mPayload = Lens.field @"payload"
{-# INLINEABLE mPayload #-}
{-# DEPRECATED payload "Use generic-lens or generic-optics with 'payload' instead"  #-}

instance Core.FromJSON Message where
        toJSON Message{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("messageId" Core..= messageId),
                  Core.Just ("payload" Core..= payload)])
