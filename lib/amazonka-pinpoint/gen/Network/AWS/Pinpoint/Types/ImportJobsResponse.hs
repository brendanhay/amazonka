{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ImportJobsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.ImportJobsResponse
  ( ImportJobsResponse (..)
  -- * Smart constructor
  , mkImportJobsResponse
  -- * Lenses
  , ijrItem
  , ijrNextToken
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.ImportJobResponse as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about the status and settings of all the import jobs that are associated with an application or segment. An import job is a job that imports endpoint definitions from one or more files.
--
-- /See:/ 'mkImportJobsResponse' smart constructor.
data ImportJobsResponse = ImportJobsResponse'
  { item :: [Types.ImportJobResponse]
    -- ^ An array of responses, one for each import job that's associated with the application (Import Jobs resource) or segment (Segment Import Jobs resource).
  , nextToken :: Core.Maybe Core.Text
    -- ^ The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportJobsResponse' value with any optional fields omitted.
mkImportJobsResponse
    :: ImportJobsResponse
mkImportJobsResponse
  = ImportJobsResponse'{item = Core.mempty, nextToken = Core.Nothing}

-- | An array of responses, one for each import job that's associated with the application (Import Jobs resource) or segment (Segment Import Jobs resource).
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrItem :: Lens.Lens' ImportJobsResponse [Types.ImportJobResponse]
ijrItem = Lens.field @"item"
{-# INLINEABLE ijrItem #-}
{-# DEPRECATED item "Use generic-lens or generic-optics with 'item' instead"  #-}

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrNextToken :: Lens.Lens' ImportJobsResponse (Core.Maybe Core.Text)
ijrNextToken = Lens.field @"nextToken"
{-# INLINEABLE ijrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.FromJSON ImportJobsResponse where
        parseJSON
          = Core.withObject "ImportJobsResponse" Core.$
              \ x ->
                ImportJobsResponse' Core.<$>
                  (x Core..:? "Item" Core..!= Core.mempty) Core.<*>
                    x Core..:? "NextToken"
