{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.TemplateVersionsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.TemplateVersionsResponse
  ( TemplateVersionsResponse (..)
  -- * Smart constructor
  , mkTemplateVersionsResponse
  -- * Lenses
  , tvrItem
  , tvrMessage
  , tvrNextToken
  , tvrRequestID
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.TemplateVersionResponse as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about all the versions of a specific message template.
--
-- /See:/ 'mkTemplateVersionsResponse' smart constructor.
data TemplateVersionsResponse = TemplateVersionsResponse'
  { item :: [Types.TemplateVersionResponse]
    -- ^ An array of responses, one for each version of the message template.
  , message :: Core.Maybe Core.Text
    -- ^ The message that's returned from the API for the request to retrieve information about all the versions of the message template.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
  , requestID :: Core.Maybe Core.Text
    -- ^ The unique identifier for the request to retrieve information about all the versions of the message template.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TemplateVersionsResponse' value with any optional fields omitted.
mkTemplateVersionsResponse
    :: TemplateVersionsResponse
mkTemplateVersionsResponse
  = TemplateVersionsResponse'{item = Core.mempty,
                              message = Core.Nothing, nextToken = Core.Nothing,
                              requestID = Core.Nothing}

-- | An array of responses, one for each version of the message template.
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvrItem :: Lens.Lens' TemplateVersionsResponse [Types.TemplateVersionResponse]
tvrItem = Lens.field @"item"
{-# INLINEABLE tvrItem #-}
{-# DEPRECATED item "Use generic-lens or generic-optics with 'item' instead"  #-}

-- | The message that's returned from the API for the request to retrieve information about all the versions of the message template.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvrMessage :: Lens.Lens' TemplateVersionsResponse (Core.Maybe Core.Text)
tvrMessage = Lens.field @"message"
{-# INLINEABLE tvrMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvrNextToken :: Lens.Lens' TemplateVersionsResponse (Core.Maybe Core.Text)
tvrNextToken = Lens.field @"nextToken"
{-# INLINEABLE tvrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The unique identifier for the request to retrieve information about all the versions of the message template.
--
-- /Note:/ Consider using 'requestID' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvrRequestID :: Lens.Lens' TemplateVersionsResponse (Core.Maybe Core.Text)
tvrRequestID = Lens.field @"requestID"
{-# INLINEABLE tvrRequestID #-}
{-# DEPRECATED requestID "Use generic-lens or generic-optics with 'requestID' instead"  #-}

instance Core.FromJSON TemplateVersionsResponse where
        parseJSON
          = Core.withObject "TemplateVersionsResponse" Core.$
              \ x ->
                TemplateVersionsResponse' Core.<$>
                  (x Core..:? "Item" Core..!= Core.mempty) Core.<*>
                    x Core..:? "Message"
                    Core.<*> x Core..:? "NextToken"
                    Core.<*> x Core..:? "RequestID"
