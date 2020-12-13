{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.ListTextTranslationJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the batch translation jobs that you have submitted.
module Network.AWS.Translate.ListTextTranslationJobs
  ( -- * Creating a request
    ListTextTranslationJobs (..),
    mkListTextTranslationJobs,

    -- ** Request lenses
    lttjNextToken,
    lttjFilter,
    lttjMaxResults,

    -- * Destructuring the response
    ListTextTranslationJobsResponse (..),
    mkListTextTranslationJobsResponse,

    -- ** Response lenses
    lttjrsTextTranslationJobPropertiesList,
    lttjrsNextToken,
    lttjrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Translate.Types

-- | /See:/ 'mkListTextTranslationJobs' smart constructor.
data ListTextTranslationJobs = ListTextTranslationJobs'
  { -- | The token to request the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The parameters that specify which batch translation jobs to retrieve. Filters include job name, job status, and submission time. You can only set one filter at a time.
    filter :: Lude.Maybe TextTranslationJobFilter,
    -- | The maximum number of results to return in each page. The default value is 100.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTextTranslationJobs' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to request the next page of results.
-- * 'filter' - The parameters that specify which batch translation jobs to retrieve. Filters include job name, job status, and submission time. You can only set one filter at a time.
-- * 'maxResults' - The maximum number of results to return in each page. The default value is 100.
mkListTextTranslationJobs ::
  ListTextTranslationJobs
mkListTextTranslationJobs =
  ListTextTranslationJobs'
    { nextToken = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lttjNextToken :: Lens.Lens' ListTextTranslationJobs (Lude.Maybe Lude.Text)
lttjNextToken = Lens.lens (nextToken :: ListTextTranslationJobs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTextTranslationJobs)
{-# DEPRECATED lttjNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The parameters that specify which batch translation jobs to retrieve. Filters include job name, job status, and submission time. You can only set one filter at a time.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lttjFilter :: Lens.Lens' ListTextTranslationJobs (Lude.Maybe TextTranslationJobFilter)
lttjFilter = Lens.lens (filter :: ListTextTranslationJobs -> Lude.Maybe TextTranslationJobFilter) (\s a -> s {filter = a} :: ListTextTranslationJobs)
{-# DEPRECATED lttjFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of results to return in each page. The default value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lttjMaxResults :: Lens.Lens' ListTextTranslationJobs (Lude.Maybe Lude.Natural)
lttjMaxResults = Lens.lens (maxResults :: ListTextTranslationJobs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListTextTranslationJobs)
{-# DEPRECATED lttjMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListTextTranslationJobs where
  type Rs ListTextTranslationJobs = ListTextTranslationJobsResponse
  request = Req.postJSON translateService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTextTranslationJobsResponse'
            Lude.<$> ( x Lude..?> "TextTranslationJobPropertiesList"
                         Lude..!@ Lude.mempty
                     )
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTextTranslationJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSShineFrontendService_20170701.ListTextTranslationJobs" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTextTranslationJobs where
  toJSON ListTextTranslationJobs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Filter" Lude..=) Lude.<$> filter,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListTextTranslationJobs where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTextTranslationJobs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTextTranslationJobsResponse' smart constructor.
data ListTextTranslationJobsResponse = ListTextTranslationJobsResponse'
  { -- | A list containing the properties of each job that is returned.
    textTranslationJobPropertiesList :: Lude.Maybe [TextTranslationJobProperties],
    -- | The token to use to retreive the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTextTranslationJobsResponse' with the minimum fields required to make a request.
--
-- * 'textTranslationJobPropertiesList' - A list containing the properties of each job that is returned.
-- * 'nextToken' - The token to use to retreive the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkListTextTranslationJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTextTranslationJobsResponse
mkListTextTranslationJobsResponse pResponseStatus_ =
  ListTextTranslationJobsResponse'
    { textTranslationJobPropertiesList =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list containing the properties of each job that is returned.
--
-- /Note:/ Consider using 'textTranslationJobPropertiesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lttjrsTextTranslationJobPropertiesList :: Lens.Lens' ListTextTranslationJobsResponse (Lude.Maybe [TextTranslationJobProperties])
lttjrsTextTranslationJobPropertiesList = Lens.lens (textTranslationJobPropertiesList :: ListTextTranslationJobsResponse -> Lude.Maybe [TextTranslationJobProperties]) (\s a -> s {textTranslationJobPropertiesList = a} :: ListTextTranslationJobsResponse)
{-# DEPRECATED lttjrsTextTranslationJobPropertiesList "Use generic-lens or generic-optics with 'textTranslationJobPropertiesList' instead." #-}

-- | The token to use to retreive the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lttjrsNextToken :: Lens.Lens' ListTextTranslationJobsResponse (Lude.Maybe Lude.Text)
lttjrsNextToken = Lens.lens (nextToken :: ListTextTranslationJobsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTextTranslationJobsResponse)
{-# DEPRECATED lttjrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lttjrsResponseStatus :: Lens.Lens' ListTextTranslationJobsResponse Lude.Int
lttjrsResponseStatus = Lens.lens (responseStatus :: ListTextTranslationJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTextTranslationJobsResponse)
{-# DEPRECATED lttjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
