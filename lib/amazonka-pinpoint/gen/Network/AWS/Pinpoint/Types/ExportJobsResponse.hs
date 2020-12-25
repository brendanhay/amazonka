{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ExportJobsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ExportJobsResponse
  ( ExportJobsResponse (..),

    -- * Smart constructor
    mkExportJobsResponse,

    -- * Lenses
    ejrItem,
    ejrNextToken,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.ExportJobResponse as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about all the export jobs that are associated with an application or segment. An export job is a job that exports endpoint definitions to a file.
--
-- /See:/ 'mkExportJobsResponse' smart constructor.
data ExportJobsResponse = ExportJobsResponse'
  { -- | An array of responses, one for each export job that's associated with the application (Export Jobs resource) or segment (Segment Export Jobs resource).
    item :: [Types.ExportJobResponse],
    -- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
    nextToken :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportJobsResponse' value with any optional fields omitted.
mkExportJobsResponse ::
  ExportJobsResponse
mkExportJobsResponse =
  ExportJobsResponse' {item = Core.mempty, nextToken = Core.Nothing}

-- | An array of responses, one for each export job that's associated with the application (Export Jobs resource) or segment (Segment Export Jobs resource).
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrItem :: Lens.Lens' ExportJobsResponse [Types.ExportJobResponse]
ejrItem = Lens.field @"item"
{-# DEPRECATED ejrItem "Use generic-lens or generic-optics with 'item' instead." #-}

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejrNextToken :: Lens.Lens' ExportJobsResponse (Core.Maybe Core.Text)
ejrNextToken = Lens.field @"nextToken"
{-# DEPRECATED ejrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ExportJobsResponse where
  parseJSON =
    Core.withObject "ExportJobsResponse" Core.$
      \x ->
        ExportJobsResponse'
          Core.<$> (x Core..:? "Item" Core..!= Core.mempty)
          Core.<*> (x Core..:? "NextToken")
