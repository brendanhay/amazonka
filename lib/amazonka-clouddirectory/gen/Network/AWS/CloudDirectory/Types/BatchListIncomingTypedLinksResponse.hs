{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListIncomingTypedLinksResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListIncomingTypedLinksResponse
  ( BatchListIncomingTypedLinksResponse (..),

    -- * Smart constructor
    mkBatchListIncomingTypedLinksResponse,

    -- * Lenses
    blitlrLinkSpecifiers,
    blitlrNextToken,
  )
where

import qualified Network.AWS.CloudDirectory.Types.NextToken as Types
import qualified Network.AWS.CloudDirectory.Types.TypedLinkSpecifier as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a 'ListIncomingTypedLinks' response operation.
--
-- /See:/ 'mkBatchListIncomingTypedLinksResponse' smart constructor.
data BatchListIncomingTypedLinksResponse = BatchListIncomingTypedLinksResponse'
  { -- | Returns one or more typed link specifiers as output.
    linkSpecifiers :: Core.Maybe [Types.TypedLinkSpecifier],
    -- | The pagination token.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BatchListIncomingTypedLinksResponse' value with any optional fields omitted.
mkBatchListIncomingTypedLinksResponse ::
  BatchListIncomingTypedLinksResponse
mkBatchListIncomingTypedLinksResponse =
  BatchListIncomingTypedLinksResponse'
    { linkSpecifiers =
        Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Returns one or more typed link specifiers as output.
--
-- /Note:/ Consider using 'linkSpecifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blitlrLinkSpecifiers :: Lens.Lens' BatchListIncomingTypedLinksResponse (Core.Maybe [Types.TypedLinkSpecifier])
blitlrLinkSpecifiers = Lens.field @"linkSpecifiers"
{-# DEPRECATED blitlrLinkSpecifiers "Use generic-lens or generic-optics with 'linkSpecifiers' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blitlrNextToken :: Lens.Lens' BatchListIncomingTypedLinksResponse (Core.Maybe Types.NextToken)
blitlrNextToken = Lens.field @"nextToken"
{-# DEPRECATED blitlrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON BatchListIncomingTypedLinksResponse where
  parseJSON =
    Core.withObject "BatchListIncomingTypedLinksResponse" Core.$
      \x ->
        BatchListIncomingTypedLinksResponse'
          Core.<$> (x Core..:? "LinkSpecifiers") Core.<*> (x Core..:? "NextToken")
