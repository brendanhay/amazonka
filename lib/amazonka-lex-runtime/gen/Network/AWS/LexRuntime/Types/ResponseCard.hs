{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.ResponseCard
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexRuntime.Types.ResponseCard
  ( ResponseCard (..)
  -- * Smart constructor
  , mkResponseCard
  -- * Lenses
  , rcContentType
  , rcGenericAttachments
  , rcVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexRuntime.Types.ContentType as Types
import qualified Network.AWS.LexRuntime.Types.GenericAttachment as Types
import qualified Network.AWS.Prelude as Core

-- | If you configure a response card when creating your bots, Amazon Lex substitutes the session attributes and slot values that are available, and then returns it. The response card can also come from a Lambda function ( @dialogCodeHook@ and @fulfillmentActivity@ on an intent).
--
-- /See:/ 'mkResponseCard' smart constructor.
data ResponseCard = ResponseCard'
  { contentType :: Core.Maybe Types.ContentType
    -- ^ The content type of the response.
  , genericAttachments :: Core.Maybe [Types.GenericAttachment]
    -- ^ An array of attachment objects representing options.
  , version :: Core.Maybe Core.Text
    -- ^ The version of the response card format.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResponseCard' value with any optional fields omitted.
mkResponseCard
    :: ResponseCard
mkResponseCard
  = ResponseCard'{contentType = Core.Nothing,
                  genericAttachments = Core.Nothing, version = Core.Nothing}

-- | The content type of the response.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcContentType :: Lens.Lens' ResponseCard (Core.Maybe Types.ContentType)
rcContentType = Lens.field @"contentType"
{-# INLINEABLE rcContentType #-}
{-# DEPRECATED contentType "Use generic-lens or generic-optics with 'contentType' instead"  #-}

-- | An array of attachment objects representing options.
--
-- /Note:/ Consider using 'genericAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcGenericAttachments :: Lens.Lens' ResponseCard (Core.Maybe [Types.GenericAttachment])
rcGenericAttachments = Lens.field @"genericAttachments"
{-# INLINEABLE rcGenericAttachments #-}
{-# DEPRECATED genericAttachments "Use generic-lens or generic-optics with 'genericAttachments' instead"  #-}

-- | The version of the response card format.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcVersion :: Lens.Lens' ResponseCard (Core.Maybe Core.Text)
rcVersion = Lens.field @"version"
{-# INLINEABLE rcVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON ResponseCard where
        parseJSON
          = Core.withObject "ResponseCard" Core.$
              \ x ->
                ResponseCard' Core.<$>
                  (x Core..:? "contentType") Core.<*> x Core..:? "genericAttachments"
                    Core.<*> x Core..:? "version"
