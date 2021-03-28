{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListIndexResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchListIndexResponse
  ( BatchListIndexResponse (..)
  -- * Smart constructor
  , mkBatchListIndexResponse
  -- * Lenses
  , blirIndexAttachments
  , blirNextToken
  ) where

import qualified Network.AWS.CloudDirectory.Types.IndexAttachment as Types
import qualified Network.AWS.CloudDirectory.Types.NextToken as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a 'ListIndex' response operation.
--
-- /See:/ 'mkBatchListIndexResponse' smart constructor.
data BatchListIndexResponse = BatchListIndexResponse'
  { indexAttachments :: Core.Maybe [Types.IndexAttachment]
    -- ^ The objects and indexed values attached to the index.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BatchListIndexResponse' value with any optional fields omitted.
mkBatchListIndexResponse
    :: BatchListIndexResponse
mkBatchListIndexResponse
  = BatchListIndexResponse'{indexAttachments = Core.Nothing,
                            nextToken = Core.Nothing}

-- | The objects and indexed values attached to the index.
--
-- /Note:/ Consider using 'indexAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blirIndexAttachments :: Lens.Lens' BatchListIndexResponse (Core.Maybe [Types.IndexAttachment])
blirIndexAttachments = Lens.field @"indexAttachments"
{-# INLINEABLE blirIndexAttachments #-}
{-# DEPRECATED indexAttachments "Use generic-lens or generic-optics with 'indexAttachments' instead"  #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blirNextToken :: Lens.Lens' BatchListIndexResponse (Core.Maybe Types.NextToken)
blirNextToken = Lens.field @"nextToken"
{-# INLINEABLE blirNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.FromJSON BatchListIndexResponse where
        parseJSON
          = Core.withObject "BatchListIndexResponse" Core.$
              \ x ->
                BatchListIndexResponse' Core.<$>
                  (x Core..:? "IndexAttachments") Core.<*> x Core..:? "NextToken"
