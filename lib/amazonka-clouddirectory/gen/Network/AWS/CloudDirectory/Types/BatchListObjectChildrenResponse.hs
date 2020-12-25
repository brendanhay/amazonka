{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectChildrenResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListObjectChildrenResponse
  ( BatchListObjectChildrenResponse (..),

    -- * Smart constructor
    mkBatchListObjectChildrenResponse,

    -- * Lenses
    blocrChildren,
    blocrNextToken,
  )
where

import qualified Network.AWS.CloudDirectory.Types.LinkName as Types
import qualified Network.AWS.CloudDirectory.Types.NextToken as Types
import qualified Network.AWS.CloudDirectory.Types.ObjectIdentifier as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a 'ListObjectChildren' response operation.
--
-- /See:/ 'mkBatchListObjectChildrenResponse' smart constructor.
data BatchListObjectChildrenResponse = BatchListObjectChildrenResponse'
  { -- | The children structure, which is a map with the key as the @LinkName@ and @ObjectIdentifier@ as the value.
    children :: Core.Maybe (Core.HashMap Types.LinkName Types.ObjectIdentifier),
    -- | The pagination token.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchListObjectChildrenResponse' value with any optional fields omitted.
mkBatchListObjectChildrenResponse ::
  BatchListObjectChildrenResponse
mkBatchListObjectChildrenResponse =
  BatchListObjectChildrenResponse'
    { children = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The children structure, which is a map with the key as the @LinkName@ and @ObjectIdentifier@ as the value.
--
-- /Note:/ Consider using 'children' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blocrChildren :: Lens.Lens' BatchListObjectChildrenResponse (Core.Maybe (Core.HashMap Types.LinkName Types.ObjectIdentifier))
blocrChildren = Lens.field @"children"
{-# DEPRECATED blocrChildren "Use generic-lens or generic-optics with 'children' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blocrNextToken :: Lens.Lens' BatchListObjectChildrenResponse (Core.Maybe Types.NextToken)
blocrNextToken = Lens.field @"nextToken"
{-# DEPRECATED blocrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON BatchListObjectChildrenResponse where
  parseJSON =
    Core.withObject "BatchListObjectChildrenResponse" Core.$
      \x ->
        BatchListObjectChildrenResponse'
          Core.<$> (x Core..:? "Children") Core.<*> (x Core..:? "NextToken")
