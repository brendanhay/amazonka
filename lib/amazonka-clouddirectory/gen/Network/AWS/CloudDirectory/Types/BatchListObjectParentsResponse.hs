{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectParentsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchListObjectParentsResponse
  ( BatchListObjectParentsResponse (..)
  -- * Smart constructor
  , mkBatchListObjectParentsResponse
  -- * Lenses
  , bloprNextToken
  , bloprParentLinks
  ) where

import qualified Network.AWS.CloudDirectory.Types.NextToken as Types
import qualified Network.AWS.CloudDirectory.Types.ObjectIdentifierAndLinkNameTuple as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkBatchListObjectParentsResponse' smart constructor.
data BatchListObjectParentsResponse = BatchListObjectParentsResponse'
  { nextToken :: Core.Maybe Types.NextToken
  , parentLinks :: Core.Maybe [Types.ObjectIdentifierAndLinkNameTuple]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchListObjectParentsResponse' value with any optional fields omitted.
mkBatchListObjectParentsResponse
    :: BatchListObjectParentsResponse
mkBatchListObjectParentsResponse
  = BatchListObjectParentsResponse'{nextToken = Core.Nothing,
                                    parentLinks = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bloprNextToken :: Lens.Lens' BatchListObjectParentsResponse (Core.Maybe Types.NextToken)
bloprNextToken = Lens.field @"nextToken"
{-# INLINEABLE bloprNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'parentLinks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bloprParentLinks :: Lens.Lens' BatchListObjectParentsResponse (Core.Maybe [Types.ObjectIdentifierAndLinkNameTuple])
bloprParentLinks = Lens.field @"parentLinks"
{-# INLINEABLE bloprParentLinks #-}
{-# DEPRECATED parentLinks "Use generic-lens or generic-optics with 'parentLinks' instead"  #-}

instance Core.FromJSON BatchListObjectParentsResponse where
        parseJSON
          = Core.withObject "BatchListObjectParentsResponse" Core.$
              \ x ->
                BatchListObjectParentsResponse' Core.<$>
                  (x Core..:? "NextToken") Core.<*> x Core..:? "ParentLinks"
