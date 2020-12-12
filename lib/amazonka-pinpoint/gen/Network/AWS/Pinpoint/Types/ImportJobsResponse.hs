{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ImportJobsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ImportJobsResponse
  ( ImportJobsResponse (..),

    -- * Smart constructor
    mkImportJobsResponse,

    -- * Lenses
    ijNextToken,
    ijItem,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.ImportJobResponse
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the status and settings of all the import jobs that are associated with an application or segment. An import job is a job that imports endpoint definitions from one or more files.
--
-- /See:/ 'mkImportJobsResponse' smart constructor.
data ImportJobsResponse = ImportJobsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    item :: [ImportJobResponse]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportJobsResponse' with the minimum fields required to make a request.
--
-- * 'item' - An array of responses, one for each import job that's associated with the application (Import Jobs resource) or segment (Segment Import Jobs resource).
-- * 'nextToken' - The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
mkImportJobsResponse ::
  ImportJobsResponse
mkImportJobsResponse =
  ImportJobsResponse' {nextToken = Lude.Nothing, item = Lude.mempty}

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijNextToken :: Lens.Lens' ImportJobsResponse (Lude.Maybe Lude.Text)
ijNextToken = Lens.lens (nextToken :: ImportJobsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ImportJobsResponse)
{-# DEPRECATED ijNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of responses, one for each import job that's associated with the application (Import Jobs resource) or segment (Segment Import Jobs resource).
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijItem :: Lens.Lens' ImportJobsResponse [ImportJobResponse]
ijItem = Lens.lens (item :: ImportJobsResponse -> [ImportJobResponse]) (\s a -> s {item = a} :: ImportJobsResponse)
{-# DEPRECATED ijItem "Use generic-lens or generic-optics with 'item' instead." #-}

instance Lude.FromJSON ImportJobsResponse where
  parseJSON =
    Lude.withObject
      "ImportJobsResponse"
      ( \x ->
          ImportJobsResponse'
            Lude.<$> (x Lude..:? "NextToken")
            Lude.<*> (x Lude..:? "Item" Lude..!= Lude.mempty)
      )
