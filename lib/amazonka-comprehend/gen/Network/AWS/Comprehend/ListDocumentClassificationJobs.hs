{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.ListDocumentClassificationJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the documentation classification jobs that you have submitted.
--
-- This operation returns paginated results.
module Network.AWS.Comprehend.ListDocumentClassificationJobs
  ( -- * Creating a request
    ListDocumentClassificationJobs (..),
    mkListDocumentClassificationJobs,

    -- ** Request lenses
    ldcjNextToken,
    ldcjFilter,
    ldcjMaxResults,

    -- * Destructuring the response
    ListDocumentClassificationJobsResponse (..),
    mkListDocumentClassificationJobsResponse,

    -- ** Response lenses
    ldcjrsNextToken,
    ldcjrsDocumentClassificationJobPropertiesList,
    ldcjrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListDocumentClassificationJobs' smart constructor.
data ListDocumentClassificationJobs = ListDocumentClassificationJobs'
  { nextToken ::
      Lude.Maybe Lude.Text,
    filter ::
      Lude.Maybe
        DocumentClassificationJobFilter,
    maxResults ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDocumentClassificationJobs' with the minimum fields required to make a request.
--
-- * 'filter' - Filters the jobs that are returned. You can filter jobs on their names, status, or the date and time that they were submitted. You can only set one filter at a time.
-- * 'maxResults' - The maximum number of results to return in each page. The default is 100.
-- * 'nextToken' - Identifies the next page of results to return.
mkListDocumentClassificationJobs ::
  ListDocumentClassificationJobs
mkListDocumentClassificationJobs =
  ListDocumentClassificationJobs'
    { nextToken = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcjNextToken :: Lens.Lens' ListDocumentClassificationJobs (Lude.Maybe Lude.Text)
ldcjNextToken = Lens.lens (nextToken :: ListDocumentClassificationJobs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDocumentClassificationJobs)
{-# DEPRECATED ldcjNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Filters the jobs that are returned. You can filter jobs on their names, status, or the date and time that they were submitted. You can only set one filter at a time.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcjFilter :: Lens.Lens' ListDocumentClassificationJobs (Lude.Maybe DocumentClassificationJobFilter)
ldcjFilter = Lens.lens (filter :: ListDocumentClassificationJobs -> Lude.Maybe DocumentClassificationJobFilter) (\s a -> s {filter = a} :: ListDocumentClassificationJobs)
{-# DEPRECATED ldcjFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of results to return in each page. The default is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcjMaxResults :: Lens.Lens' ListDocumentClassificationJobs (Lude.Maybe Lude.Natural)
ldcjMaxResults = Lens.lens (maxResults :: ListDocumentClassificationJobs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListDocumentClassificationJobs)
{-# DEPRECATED ldcjMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListDocumentClassificationJobs where
  page rq rs
    | Page.stop (rs Lens.^. ldcjrsNextToken) = Lude.Nothing
    | Page.stop
        (rs Lens.^. ldcjrsDocumentClassificationJobPropertiesList) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldcjNextToken Lens..~ rs Lens.^. ldcjrsNextToken

instance Lude.AWSRequest ListDocumentClassificationJobs where
  type
    Rs ListDocumentClassificationJobs =
      ListDocumentClassificationJobsResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDocumentClassificationJobsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> ( x Lude..?> "DocumentClassificationJobPropertiesList"
                         Lude..!@ Lude.mempty
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDocumentClassificationJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Comprehend_20171127.ListDocumentClassificationJobs" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListDocumentClassificationJobs where
  toJSON ListDocumentClassificationJobs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Filter" Lude..=) Lude.<$> filter,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListDocumentClassificationJobs where
  toPath = Lude.const "/"

instance Lude.ToQuery ListDocumentClassificationJobs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListDocumentClassificationJobsResponse' smart constructor.
data ListDocumentClassificationJobsResponse = ListDocumentClassificationJobsResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    documentClassificationJobPropertiesList ::
      Lude.Maybe
        [DocumentClassificationJobProperties],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDocumentClassificationJobsResponse' with the minimum fields required to make a request.
--
-- * 'documentClassificationJobPropertiesList' - A list containing the properties of each job returned.
-- * 'nextToken' - Identifies the next page of results to return.
-- * 'responseStatus' - The response status code.
mkListDocumentClassificationJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDocumentClassificationJobsResponse
mkListDocumentClassificationJobsResponse pResponseStatus_ =
  ListDocumentClassificationJobsResponse'
    { nextToken = Lude.Nothing,
      documentClassificationJobPropertiesList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcjrsNextToken :: Lens.Lens' ListDocumentClassificationJobsResponse (Lude.Maybe Lude.Text)
ldcjrsNextToken = Lens.lens (nextToken :: ListDocumentClassificationJobsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDocumentClassificationJobsResponse)
{-# DEPRECATED ldcjrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list containing the properties of each job returned.
--
-- /Note:/ Consider using 'documentClassificationJobPropertiesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcjrsDocumentClassificationJobPropertiesList :: Lens.Lens' ListDocumentClassificationJobsResponse (Lude.Maybe [DocumentClassificationJobProperties])
ldcjrsDocumentClassificationJobPropertiesList = Lens.lens (documentClassificationJobPropertiesList :: ListDocumentClassificationJobsResponse -> Lude.Maybe [DocumentClassificationJobProperties]) (\s a -> s {documentClassificationJobPropertiesList = a} :: ListDocumentClassificationJobsResponse)
{-# DEPRECATED ldcjrsDocumentClassificationJobPropertiesList "Use generic-lens or generic-optics with 'documentClassificationJobPropertiesList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcjrsResponseStatus :: Lens.Lens' ListDocumentClassificationJobsResponse Lude.Int
ldcjrsResponseStatus = Lens.lens (responseStatus :: ListDocumentClassificationJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDocumentClassificationJobsResponse)
{-# DEPRECATED ldcjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
