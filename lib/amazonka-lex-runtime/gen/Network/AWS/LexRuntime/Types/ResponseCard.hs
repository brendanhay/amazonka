-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.ResponseCard
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.ResponseCard
  ( ResponseCard (..),

    -- * Smart constructor
    mkResponseCard,

    -- * Lenses
    rcGenericAttachments,
    rcVersion,
    rcContentType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexRuntime.Types.ContentType
import Network.AWS.LexRuntime.Types.GenericAttachment
import qualified Network.AWS.Prelude as Lude

-- | If you configure a response card when creating your bots, Amazon Lex substitutes the session attributes and slot values that are available, and then returns it. The response card can also come from a Lambda function ( @dialogCodeHook@ and @fulfillmentActivity@ on an intent).
--
-- /See:/ 'mkResponseCard' smart constructor.
data ResponseCard = ResponseCard'
  { genericAttachments ::
      Lude.Maybe [GenericAttachment],
    version :: Lude.Maybe Lude.Text,
    contentType :: Lude.Maybe ContentType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResponseCard' with the minimum fields required to make a request.
--
-- * 'contentType' - The content type of the response.
-- * 'genericAttachments' - An array of attachment objects representing options.
-- * 'version' - The version of the response card format.
mkResponseCard ::
  ResponseCard
mkResponseCard =
  ResponseCard'
    { genericAttachments = Lude.Nothing,
      version = Lude.Nothing,
      contentType = Lude.Nothing
    }

-- | An array of attachment objects representing options.
--
-- /Note:/ Consider using 'genericAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcGenericAttachments :: Lens.Lens' ResponseCard (Lude.Maybe [GenericAttachment])
rcGenericAttachments = Lens.lens (genericAttachments :: ResponseCard -> Lude.Maybe [GenericAttachment]) (\s a -> s {genericAttachments = a} :: ResponseCard)
{-# DEPRECATED rcGenericAttachments "Use generic-lens or generic-optics with 'genericAttachments' instead." #-}

-- | The version of the response card format.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcVersion :: Lens.Lens' ResponseCard (Lude.Maybe Lude.Text)
rcVersion = Lens.lens (version :: ResponseCard -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: ResponseCard)
{-# DEPRECATED rcVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The content type of the response.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcContentType :: Lens.Lens' ResponseCard (Lude.Maybe ContentType)
rcContentType = Lens.lens (contentType :: ResponseCard -> Lude.Maybe ContentType) (\s a -> s {contentType = a} :: ResponseCard)
{-# DEPRECATED rcContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

instance Lude.FromJSON ResponseCard where
  parseJSON =
    Lude.withObject
      "ResponseCard"
      ( \x ->
          ResponseCard'
            Lude.<$> (x Lude..:? "genericAttachments" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "version")
            Lude.<*> (x Lude..:? "contentType")
      )
