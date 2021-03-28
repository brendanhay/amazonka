{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectParentPathsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchListObjectParentPathsResponse
  ( BatchListObjectParentPathsResponse (..)
  -- * Smart constructor
  , mkBatchListObjectParentPathsResponse
  -- * Lenses
  , blopprNextToken
  , blopprPathToObjectIdentifiersList
  ) where

import qualified Network.AWS.CloudDirectory.Types.NextToken as Types
import qualified Network.AWS.CloudDirectory.Types.PathToObjectIdentifiers as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a 'ListObjectParentPaths' response operation.
--
-- /See:/ 'mkBatchListObjectParentPathsResponse' smart constructor.
data BatchListObjectParentPathsResponse = BatchListObjectParentPathsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  , pathToObjectIdentifiersList :: Core.Maybe [Types.PathToObjectIdentifiers]
    -- ^ Returns the path to the @ObjectIdentifiers@ that are associated with the directory.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchListObjectParentPathsResponse' value with any optional fields omitted.
mkBatchListObjectParentPathsResponse
    :: BatchListObjectParentPathsResponse
mkBatchListObjectParentPathsResponse
  = BatchListObjectParentPathsResponse'{nextToken = Core.Nothing,
                                        pathToObjectIdentifiersList = Core.Nothing}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blopprNextToken :: Lens.Lens' BatchListObjectParentPathsResponse (Core.Maybe Types.NextToken)
blopprNextToken = Lens.field @"nextToken"
{-# INLINEABLE blopprNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Returns the path to the @ObjectIdentifiers@ that are associated with the directory.
--
-- /Note:/ Consider using 'pathToObjectIdentifiersList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blopprPathToObjectIdentifiersList :: Lens.Lens' BatchListObjectParentPathsResponse (Core.Maybe [Types.PathToObjectIdentifiers])
blopprPathToObjectIdentifiersList = Lens.field @"pathToObjectIdentifiersList"
{-# INLINEABLE blopprPathToObjectIdentifiersList #-}
{-# DEPRECATED pathToObjectIdentifiersList "Use generic-lens or generic-optics with 'pathToObjectIdentifiersList' instead"  #-}

instance Core.FromJSON BatchListObjectParentPathsResponse where
        parseJSON
          = Core.withObject "BatchListObjectParentPathsResponse" Core.$
              \ x ->
                BatchListObjectParentPathsResponse' Core.<$>
                  (x Core..:? "NextToken") Core.<*>
                    x Core..:? "PathToObjectIdentifiersList"
