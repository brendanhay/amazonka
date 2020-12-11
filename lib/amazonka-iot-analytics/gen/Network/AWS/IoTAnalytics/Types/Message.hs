-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.Message
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.Message
  ( Message (..),

    -- * Smart constructor
    mkMessage,

    -- * Lenses
    mMessageId,
    mPayload,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a message.
--
-- /See:/ 'mkMessage' smart constructor.
data Message = Message'
  { messageId :: Lude.Text,
    payload :: Lude.Base64
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Message' with the minimum fields required to make a request.
--
-- * 'messageId' - The ID you want to assign to the message. Each @messageId@ must be unique within each batch sent.
-- * 'payload' - The payload of the message. This can be a JSON string or a base64-encoded string representing binary data, in which case you must decode it by means of a pipeline activity.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
mkMessage ::
  -- | 'messageId'
  Lude.Text ->
  -- | 'payload'
  Lude.Base64 ->
  Message
mkMessage pMessageId_ pPayload_ =
  Message' {messageId = pMessageId_, payload = pPayload_}

-- | The ID you want to assign to the message. Each @messageId@ must be unique within each batch sent.
--
-- /Note:/ Consider using 'messageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mMessageId :: Lens.Lens' Message Lude.Text
mMessageId = Lens.lens (messageId :: Message -> Lude.Text) (\s a -> s {messageId = a} :: Message)
{-# DEPRECATED mMessageId "Use generic-lens or generic-optics with 'messageId' instead." #-}

-- | The payload of the message. This can be a JSON string or a base64-encoded string representing binary data, in which case you must decode it by means of a pipeline activity.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mPayload :: Lens.Lens' Message Lude.Base64
mPayload = Lens.lens (payload :: Message -> Lude.Base64) (\s a -> s {payload = a} :: Message)
{-# DEPRECATED mPayload "Use generic-lens or generic-optics with 'payload' instead." #-}

instance Lude.ToJSON Message where
  toJSON Message' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("messageId" Lude..= messageId),
            Lude.Just ("payload" Lude..= payload)
          ]
      )
