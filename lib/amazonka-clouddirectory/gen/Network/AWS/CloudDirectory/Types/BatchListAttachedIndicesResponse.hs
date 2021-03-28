{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListAttachedIndicesResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchListAttachedIndicesResponse
  ( BatchListAttachedIndicesResponse (..)
  -- * Smart constructor
  , mkBatchListAttachedIndicesResponse
  -- * Lenses
  , blairIndexAttachments
  , blairNextToken
  ) where

import qualified Network.AWS.CloudDirectory.Types.IndexAttachment as Types
import qualified Network.AWS.CloudDirectory.Types.NextToken as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a 'ListAttachedIndices' response operation.
--
-- /See:/ 'mkBatchListAttachedIndicesResponse' smart constructor.
data BatchListAttachedIndicesResponse = BatchListAttachedIndicesResponse'
  { indexAttachments :: Core.Maybe [Types.IndexAttachment]
    -- ^ The indices attached to the specified object.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BatchListAttachedIndicesResponse' value with any optional fields omitted.
mkBatchListAttachedIndicesResponse
    :: BatchListAttachedIndicesResponse
mkBatchListAttachedIndicesResponse
  = BatchListAttachedIndicesResponse'{indexAttachments =
                                        Core.Nothing,
                                      nextToken = Core.Nothing}

-- | The indices attached to the specified object.
--
-- /Note:/ Consider using 'indexAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blairIndexAttachments :: Lens.Lens' BatchListAttachedIndicesResponse (Core.Maybe [Types.IndexAttachment])
blairIndexAttachments = Lens.field @"indexAttachments"
{-# INLINEABLE blairIndexAttachments #-}
{-# DEPRECATED indexAttachments "Use generic-lens or generic-optics with 'indexAttachments' instead"  #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blairNextToken :: Lens.Lens' BatchListAttachedIndicesResponse (Core.Maybe Types.NextToken)
blairNextToken = Lens.field @"nextToken"
{-# INLINEABLE blairNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.FromJSON BatchListAttachedIndicesResponse where
        parseJSON
          = Core.withObject "BatchListAttachedIndicesResponse" Core.$
              \ x ->
                BatchListAttachedIndicesResponse' Core.<$>
                  (x Core..:? "IndexAttachments") Core.<*> x Core..:? "NextToken"
