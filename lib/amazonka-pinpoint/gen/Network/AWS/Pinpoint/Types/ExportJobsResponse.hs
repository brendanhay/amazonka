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
    ejNextToken,
    ejItem,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.ExportJobResponse
import qualified Network.AWS.Prelude as Lude

-- | Provides information about all the export jobs that are associated with an application or segment. An export job is a job that exports endpoint definitions to a file.
--
-- /See:/ 'mkExportJobsResponse' smart constructor.
data ExportJobsResponse = ExportJobsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    item :: [ExportJobResponse]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportJobsResponse' with the minimum fields required to make a request.
--
-- * 'item' - An array of responses, one for each export job that's associated with the application (Export Jobs resource) or segment (Segment Export Jobs resource).
-- * 'nextToken' - The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
mkExportJobsResponse ::
  ExportJobsResponse
mkExportJobsResponse =
  ExportJobsResponse' {nextToken = Lude.Nothing, item = Lude.mempty}

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejNextToken :: Lens.Lens' ExportJobsResponse (Lude.Maybe Lude.Text)
ejNextToken = Lens.lens (nextToken :: ExportJobsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ExportJobsResponse)
{-# DEPRECATED ejNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of responses, one for each export job that's associated with the application (Export Jobs resource) or segment (Segment Export Jobs resource).
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ejItem :: Lens.Lens' ExportJobsResponse [ExportJobResponse]
ejItem = Lens.lens (item :: ExportJobsResponse -> [ExportJobResponse]) (\s a -> s {item = a} :: ExportJobsResponse)
{-# DEPRECATED ejItem "Use generic-lens or generic-optics with 'item' instead." #-}

instance Lude.FromJSON ExportJobsResponse where
  parseJSON =
    Lude.withObject
      "ExportJobsResponse"
      ( \x ->
          ExportJobsResponse'
            Lude.<$> (x Lude..:? "NextToken")
            Lude.<*> (x Lude..:? "Item" Lude..!= Lude.mempty)
      )
