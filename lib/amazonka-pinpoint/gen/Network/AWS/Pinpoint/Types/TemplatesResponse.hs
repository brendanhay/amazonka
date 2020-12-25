{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.TemplatesResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.TemplatesResponse
  ( TemplatesResponse (..),

    -- * Smart constructor
    mkTemplatesResponse,

    -- * Lenses
    trItem,
    trNextToken,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.TemplateResponse as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about all the message templates that are associated with your Amazon Pinpoint account.
--
-- /See:/ 'mkTemplatesResponse' smart constructor.
data TemplatesResponse = TemplatesResponse'
  { -- | An array of responses, one for each message template that's associated with your Amazon Pinpoint account and meets any filter criteria that you specified in the request.
    item :: [Types.TemplateResponse],
    -- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
    nextToken :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TemplatesResponse' value with any optional fields omitted.
mkTemplatesResponse ::
  TemplatesResponse
mkTemplatesResponse =
  TemplatesResponse' {item = Core.mempty, nextToken = Core.Nothing}

-- | An array of responses, one for each message template that's associated with your Amazon Pinpoint account and meets any filter criteria that you specified in the request.
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trItem :: Lens.Lens' TemplatesResponse [Types.TemplateResponse]
trItem = Lens.field @"item"
{-# DEPRECATED trItem "Use generic-lens or generic-optics with 'item' instead." #-}

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trNextToken :: Lens.Lens' TemplatesResponse (Core.Maybe Core.Text)
trNextToken = Lens.field @"nextToken"
{-# DEPRECATED trNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON TemplatesResponse where
  parseJSON =
    Core.withObject "TemplatesResponse" Core.$
      \x ->
        TemplatesResponse'
          Core.<$> (x Core..:? "Item" Core..!= Core.mempty)
          Core.<*> (x Core..:? "NextToken")
