-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.MessageBody
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.MessageBody
  ( MessageBody (..),

    -- * Smart constructor
    mkMessageBody,

    -- * Lenses
    mbRequestId,
    mbMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about an API request or response.
--
-- /See:/ 'mkMessageBody' smart constructor.
data MessageBody = MessageBody'
  { requestId :: Lude.Maybe Lude.Text,
    message :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MessageBody' with the minimum fields required to make a request.
--
-- * 'message' - The message that's returned from the API.
-- * 'requestId' - The unique identifier for the request or response.
mkMessageBody ::
  MessageBody
mkMessageBody =
  MessageBody' {requestId = Lude.Nothing, message = Lude.Nothing}

-- | The unique identifier for the request or response.
--
-- /Note:/ Consider using 'requestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbRequestId :: Lens.Lens' MessageBody (Lude.Maybe Lude.Text)
mbRequestId = Lens.lens (requestId :: MessageBody -> Lude.Maybe Lude.Text) (\s a -> s {requestId = a} :: MessageBody)
{-# DEPRECATED mbRequestId "Use generic-lens or generic-optics with 'requestId' instead." #-}

-- | The message that's returned from the API.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbMessage :: Lens.Lens' MessageBody (Lude.Maybe Lude.Text)
mbMessage = Lens.lens (message :: MessageBody -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: MessageBody)
{-# DEPRECATED mbMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON MessageBody where
  parseJSON =
    Lude.withObject
      "MessageBody"
      ( \x ->
          MessageBody'
            Lude.<$> (x Lude..:? "RequestID") Lude.<*> (x Lude..:? "Message")
      )
