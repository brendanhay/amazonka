{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.ListPiiEntitiesDetectionJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the PII entity detection jobs that you have submitted.
module Network.AWS.Comprehend.ListPiiEntitiesDetectionJobs
  ( -- * Creating a request
    ListPiiEntitiesDetectionJobs (..),
    mkListPiiEntitiesDetectionJobs,

    -- ** Request lenses
    lpedjNextToken,
    lpedjFilter,
    lpedjMaxResults,

    -- * Destructuring the response
    ListPiiEntitiesDetectionJobsResponse (..),
    mkListPiiEntitiesDetectionJobsResponse,

    -- ** Response lenses
    lpedjrsNextToken,
    lpedjrsPiiEntitiesDetectionJobPropertiesList,
    lpedjrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListPiiEntitiesDetectionJobs' smart constructor.
data ListPiiEntitiesDetectionJobs = ListPiiEntitiesDetectionJobs'
  { -- | Identifies the next page of results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
    filter :: Lude.Maybe PiiEntitiesDetectionJobFilter,
    -- | The maximum number of results to return in each page.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPiiEntitiesDetectionJobs' with the minimum fields required to make a request.
--
-- * 'nextToken' - Identifies the next page of results to return.
-- * 'filter' - Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
-- * 'maxResults' - The maximum number of results to return in each page.
mkListPiiEntitiesDetectionJobs ::
  ListPiiEntitiesDetectionJobs
mkListPiiEntitiesDetectionJobs =
  ListPiiEntitiesDetectionJobs'
    { nextToken = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpedjNextToken :: Lens.Lens' ListPiiEntitiesDetectionJobs (Lude.Maybe Lude.Text)
lpedjNextToken = Lens.lens (nextToken :: ListPiiEntitiesDetectionJobs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPiiEntitiesDetectionJobs)
{-# DEPRECATED lpedjNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpedjFilter :: Lens.Lens' ListPiiEntitiesDetectionJobs (Lude.Maybe PiiEntitiesDetectionJobFilter)
lpedjFilter = Lens.lens (filter :: ListPiiEntitiesDetectionJobs -> Lude.Maybe PiiEntitiesDetectionJobFilter) (\s a -> s {filter = a} :: ListPiiEntitiesDetectionJobs)
{-# DEPRECATED lpedjFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of results to return in each page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpedjMaxResults :: Lens.Lens' ListPiiEntitiesDetectionJobs (Lude.Maybe Lude.Natural)
lpedjMaxResults = Lens.lens (maxResults :: ListPiiEntitiesDetectionJobs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListPiiEntitiesDetectionJobs)
{-# DEPRECATED lpedjMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListPiiEntitiesDetectionJobs where
  type
    Rs ListPiiEntitiesDetectionJobs =
      ListPiiEntitiesDetectionJobsResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListPiiEntitiesDetectionJobsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> ( x Lude..?> "PiiEntitiesDetectionJobPropertiesList"
                         Lude..!@ Lude.mempty
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPiiEntitiesDetectionJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Comprehend_20171127.ListPiiEntitiesDetectionJobs" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListPiiEntitiesDetectionJobs where
  toJSON ListPiiEntitiesDetectionJobs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Filter" Lude..=) Lude.<$> filter,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListPiiEntitiesDetectionJobs where
  toPath = Lude.const "/"

instance Lude.ToQuery ListPiiEntitiesDetectionJobs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListPiiEntitiesDetectionJobsResponse' smart constructor.
data ListPiiEntitiesDetectionJobsResponse = ListPiiEntitiesDetectionJobsResponse'
  { -- | Identifies the next page of results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list containing the properties of each job that is returned.
    piiEntitiesDetectionJobPropertiesList :: Lude.Maybe [PiiEntitiesDetectionJobProperties],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPiiEntitiesDetectionJobsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Identifies the next page of results to return.
-- * 'piiEntitiesDetectionJobPropertiesList' - A list containing the properties of each job that is returned.
-- * 'responseStatus' - The response status code.
mkListPiiEntitiesDetectionJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPiiEntitiesDetectionJobsResponse
mkListPiiEntitiesDetectionJobsResponse pResponseStatus_ =
  ListPiiEntitiesDetectionJobsResponse'
    { nextToken = Lude.Nothing,
      piiEntitiesDetectionJobPropertiesList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpedjrsNextToken :: Lens.Lens' ListPiiEntitiesDetectionJobsResponse (Lude.Maybe Lude.Text)
lpedjrsNextToken = Lens.lens (nextToken :: ListPiiEntitiesDetectionJobsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPiiEntitiesDetectionJobsResponse)
{-# DEPRECATED lpedjrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list containing the properties of each job that is returned.
--
-- /Note:/ Consider using 'piiEntitiesDetectionJobPropertiesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpedjrsPiiEntitiesDetectionJobPropertiesList :: Lens.Lens' ListPiiEntitiesDetectionJobsResponse (Lude.Maybe [PiiEntitiesDetectionJobProperties])
lpedjrsPiiEntitiesDetectionJobPropertiesList = Lens.lens (piiEntitiesDetectionJobPropertiesList :: ListPiiEntitiesDetectionJobsResponse -> Lude.Maybe [PiiEntitiesDetectionJobProperties]) (\s a -> s {piiEntitiesDetectionJobPropertiesList = a} :: ListPiiEntitiesDetectionJobsResponse)
{-# DEPRECATED lpedjrsPiiEntitiesDetectionJobPropertiesList "Use generic-lens or generic-optics with 'piiEntitiesDetectionJobPropertiesList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpedjrsResponseStatus :: Lens.Lens' ListPiiEntitiesDetectionJobsResponse Lude.Int
lpedjrsResponseStatus = Lens.lens (responseStatus :: ListPiiEntitiesDetectionJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPiiEntitiesDetectionJobsResponse)
{-# DEPRECATED lpedjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
