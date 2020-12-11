-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.Body
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.Body
  ( Body (..),

    -- * Smart constructor
    mkBody,

    -- * Lenses
    bText,
    bHTML,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SES.Types.Content

-- | Represents the body of the message. You can specify text, HTML, or both. If you use both, then the message should display correctly in the widest variety of email clients.
--
-- /See:/ 'mkBody' smart constructor.
data Body = Body'
  { text :: Lude.Maybe Content,
    html :: Lude.Maybe Content
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Body' with the minimum fields required to make a request.
--
-- * 'html' - The content of the message, in HTML format. Use this for email clients that can process HTML. You can include clickable links, formatted text, and much more in an HTML message.
-- * 'text' - The content of the message, in text format. Use this for text-based email clients, or clients on high-latency networks (such as mobile devices).
mkBody ::
  Body
mkBody = Body' {text = Lude.Nothing, html = Lude.Nothing}

-- | The content of the message, in text format. Use this for text-based email clients, or clients on high-latency networks (such as mobile devices).
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bText :: Lens.Lens' Body (Lude.Maybe Content)
bText = Lens.lens (text :: Body -> Lude.Maybe Content) (\s a -> s {text = a} :: Body)
{-# DEPRECATED bText "Use generic-lens or generic-optics with 'text' instead." #-}

-- | The content of the message, in HTML format. Use this for email clients that can process HTML. You can include clickable links, formatted text, and much more in an HTML message.
--
-- /Note:/ Consider using 'html' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bHTML :: Lens.Lens' Body (Lude.Maybe Content)
bHTML = Lens.lens (html :: Body -> Lude.Maybe Content) (\s a -> s {html = a} :: Body)
{-# DEPRECATED bHTML "Use generic-lens or generic-optics with 'html' instead." #-}

instance Lude.ToQuery Body where
  toQuery Body' {..} =
    Lude.mconcat ["Text" Lude.=: text, "Html" Lude.=: html]
