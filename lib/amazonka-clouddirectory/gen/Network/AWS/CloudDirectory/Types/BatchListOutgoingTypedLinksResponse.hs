{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListOutgoingTypedLinksResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListOutgoingTypedLinksResponse
  ( BatchListOutgoingTypedLinksResponse (..),

    -- * Smart constructor
    mkBatchListOutgoingTypedLinksResponse,

    -- * Lenses
    blotlrNextToken,
    blotlrTypedLinkSpecifiers,
  )
where

import qualified Network.AWS.CloudDirectory.Types.NextToken as Types
import qualified Network.AWS.CloudDirectory.Types.TypedLinkSpecifier as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a 'ListOutgoingTypedLinks' response operation.
--
-- /See:/ 'mkBatchListOutgoingTypedLinksResponse' smart constructor.
data BatchListOutgoingTypedLinksResponse = BatchListOutgoingTypedLinksResponse'
  { -- | The pagination token.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Returns a typed link specifier as output.
    typedLinkSpecifiers :: Core.Maybe [Types.TypedLinkSpecifier]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BatchListOutgoingTypedLinksResponse' value with any optional fields omitted.
mkBatchListOutgoingTypedLinksResponse ::
  BatchListOutgoingTypedLinksResponse
mkBatchListOutgoingTypedLinksResponse =
  BatchListOutgoingTypedLinksResponse'
    { nextToken = Core.Nothing,
      typedLinkSpecifiers = Core.Nothing
    }

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blotlrNextToken :: Lens.Lens' BatchListOutgoingTypedLinksResponse (Core.Maybe Types.NextToken)
blotlrNextToken = Lens.field @"nextToken"
{-# DEPRECATED blotlrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Returns a typed link specifier as output.
--
-- /Note:/ Consider using 'typedLinkSpecifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blotlrTypedLinkSpecifiers :: Lens.Lens' BatchListOutgoingTypedLinksResponse (Core.Maybe [Types.TypedLinkSpecifier])
blotlrTypedLinkSpecifiers = Lens.field @"typedLinkSpecifiers"
{-# DEPRECATED blotlrTypedLinkSpecifiers "Use generic-lens or generic-optics with 'typedLinkSpecifiers' instead." #-}

instance Core.FromJSON BatchListOutgoingTypedLinksResponse where
  parseJSON =
    Core.withObject "BatchListOutgoingTypedLinksResponse" Core.$
      \x ->
        BatchListOutgoingTypedLinksResponse'
          Core.<$> (x Core..:? "NextToken")
          Core.<*> (x Core..:? "TypedLinkSpecifiers")
