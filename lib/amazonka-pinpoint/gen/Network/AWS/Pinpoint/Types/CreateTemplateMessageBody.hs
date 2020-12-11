-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CreateTemplateMessageBody
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CreateTemplateMessageBody
  ( CreateTemplateMessageBody (..),

    -- * Smart constructor
    mkCreateTemplateMessageBody,

    -- * Lenses
    ctmbRequestId,
    ctmbARN,
    ctmbMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about a request to create a message template.
--
-- /See:/ 'mkCreateTemplateMessageBody' smart constructor.
data CreateTemplateMessageBody = CreateTemplateMessageBody'
  { requestId ::
      Lude.Maybe Lude.Text,
    arn :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateTemplateMessageBody' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the message template that was created.
-- * 'message' - The message that's returned from the API for the request to create the message template.
-- * 'requestId' - The unique identifier for the request to create the message template.
mkCreateTemplateMessageBody ::
  CreateTemplateMessageBody
mkCreateTemplateMessageBody =
  CreateTemplateMessageBody'
    { requestId = Lude.Nothing,
      arn = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The unique identifier for the request to create the message template.
--
-- /Note:/ Consider using 'requestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmbRequestId :: Lens.Lens' CreateTemplateMessageBody (Lude.Maybe Lude.Text)
ctmbRequestId = Lens.lens (requestId :: CreateTemplateMessageBody -> Lude.Maybe Lude.Text) (\s a -> s {requestId = a} :: CreateTemplateMessageBody)
{-# DEPRECATED ctmbRequestId "Use generic-lens or generic-optics with 'requestId' instead." #-}

-- | The Amazon Resource Name (ARN) of the message template that was created.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmbARN :: Lens.Lens' CreateTemplateMessageBody (Lude.Maybe Lude.Text)
ctmbARN = Lens.lens (arn :: CreateTemplateMessageBody -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: CreateTemplateMessageBody)
{-# DEPRECATED ctmbARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The message that's returned from the API for the request to create the message template.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmbMessage :: Lens.Lens' CreateTemplateMessageBody (Lude.Maybe Lude.Text)
ctmbMessage = Lens.lens (message :: CreateTemplateMessageBody -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: CreateTemplateMessageBody)
{-# DEPRECATED ctmbMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON CreateTemplateMessageBody where
  parseJSON =
    Lude.withObject
      "CreateTemplateMessageBody"
      ( \x ->
          CreateTemplateMessageBody'
            Lude.<$> (x Lude..:? "RequestID")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "Message")
      )
