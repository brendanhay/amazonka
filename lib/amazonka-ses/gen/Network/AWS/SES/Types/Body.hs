{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.Body
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.Body
  ( Body (..)
  -- * Smart constructor
  , mkBody
  -- * Lenses
  , bHtml
  , bText
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.Content as Types

-- | Represents the body of the message. You can specify text, HTML, or both. If you use both, then the message should display correctly in the widest variety of email clients.
--
-- /See:/ 'mkBody' smart constructor.
data Body = Body'
  { html :: Core.Maybe Types.Content
    -- ^ The content of the message, in HTML format. Use this for email clients that can process HTML. You can include clickable links, formatted text, and much more in an HTML message.
  , text :: Core.Maybe Types.Content
    -- ^ The content of the message, in text format. Use this for text-based email clients, or clients on high-latency networks (such as mobile devices).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Body' value with any optional fields omitted.
mkBody
    :: Body
mkBody = Body'{html = Core.Nothing, text = Core.Nothing}

-- | The content of the message, in HTML format. Use this for email clients that can process HTML. You can include clickable links, formatted text, and much more in an HTML message.
--
-- /Note:/ Consider using 'html' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bHtml :: Lens.Lens' Body (Core.Maybe Types.Content)
bHtml = Lens.field @"html"
{-# INLINEABLE bHtml #-}
{-# DEPRECATED html "Use generic-lens or generic-optics with 'html' instead"  #-}

-- | The content of the message, in text format. Use this for text-based email clients, or clients on high-latency networks (such as mobile devices).
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bText :: Lens.Lens' Body (Core.Maybe Types.Content)
bText = Lens.field @"text"
{-# INLINEABLE bText #-}
{-# DEPRECATED text "Use generic-lens or generic-optics with 'text' instead"  #-}

instance Core.ToQuery Body where
        toQuery Body{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Html") html Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Text") text
