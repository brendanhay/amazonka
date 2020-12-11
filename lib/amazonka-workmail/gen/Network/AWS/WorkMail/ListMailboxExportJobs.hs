{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.ListMailboxExportJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the mailbox export jobs started for the specified organization within the last seven days.
module Network.AWS.WorkMail.ListMailboxExportJobs
  ( -- * Creating a request
    ListMailboxExportJobs (..),
    mkListMailboxExportJobs,

    -- ** Request lenses
    lmejNextToken,
    lmejMaxResults,
    lmejOrganizationId,

    -- * Destructuring the response
    ListMailboxExportJobsResponse (..),
    mkListMailboxExportJobsResponse,

    -- ** Response lenses
    lmejrsNextToken,
    lmejrsJobs,
    lmejrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkListMailboxExportJobs' smart constructor.
data ListMailboxExportJobs = ListMailboxExportJobs'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    organizationId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListMailboxExportJobs' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to return in a single call.
-- * 'nextToken' - The token to use to retrieve the next page of results.
-- * 'organizationId' - The organization ID.
mkListMailboxExportJobs ::
  -- | 'organizationId'
  Lude.Text ->
  ListMailboxExportJobs
mkListMailboxExportJobs pOrganizationId_ =
  ListMailboxExportJobs'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      organizationId = pOrganizationId_
    }

-- | The token to use to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmejNextToken :: Lens.Lens' ListMailboxExportJobs (Lude.Maybe Lude.Text)
lmejNextToken = Lens.lens (nextToken :: ListMailboxExportJobs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListMailboxExportJobs)
{-# DEPRECATED lmejNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return in a single call.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmejMaxResults :: Lens.Lens' ListMailboxExportJobs (Lude.Maybe Lude.Natural)
lmejMaxResults = Lens.lens (maxResults :: ListMailboxExportJobs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListMailboxExportJobs)
{-# DEPRECATED lmejMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The organization ID.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmejOrganizationId :: Lens.Lens' ListMailboxExportJobs Lude.Text
lmejOrganizationId = Lens.lens (organizationId :: ListMailboxExportJobs -> Lude.Text) (\s a -> s {organizationId = a} :: ListMailboxExportJobs)
{-# DEPRECATED lmejOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Lude.AWSRequest ListMailboxExportJobs where
  type Rs ListMailboxExportJobs = ListMailboxExportJobsResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListMailboxExportJobsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Jobs" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListMailboxExportJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.ListMailboxExportJobs" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListMailboxExportJobs where
  toJSON ListMailboxExportJobs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("OrganizationId" Lude..= organizationId)
          ]
      )

instance Lude.ToPath ListMailboxExportJobs where
  toPath = Lude.const "/"

instance Lude.ToQuery ListMailboxExportJobs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListMailboxExportJobsResponse' smart constructor.
data ListMailboxExportJobsResponse = ListMailboxExportJobsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    jobs ::
      Lude.Maybe [MailboxExportJob],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListMailboxExportJobsResponse' with the minimum fields required to make a request.
--
-- * 'jobs' - The mailbox export job details.
-- * 'nextToken' - The token to use to retrieve the next page of results.
-- * 'responseStatus' - The response status code.
mkListMailboxExportJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListMailboxExportJobsResponse
mkListMailboxExportJobsResponse pResponseStatus_ =
  ListMailboxExportJobsResponse'
    { nextToken = Lude.Nothing,
      jobs = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmejrsNextToken :: Lens.Lens' ListMailboxExportJobsResponse (Lude.Maybe Lude.Text)
lmejrsNextToken = Lens.lens (nextToken :: ListMailboxExportJobsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListMailboxExportJobsResponse)
{-# DEPRECATED lmejrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The mailbox export job details.
--
-- /Note:/ Consider using 'jobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmejrsJobs :: Lens.Lens' ListMailboxExportJobsResponse (Lude.Maybe [MailboxExportJob])
lmejrsJobs = Lens.lens (jobs :: ListMailboxExportJobsResponse -> Lude.Maybe [MailboxExportJob]) (\s a -> s {jobs = a} :: ListMailboxExportJobsResponse)
{-# DEPRECATED lmejrsJobs "Use generic-lens or generic-optics with 'jobs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmejrsResponseStatus :: Lens.Lens' ListMailboxExportJobsResponse Lude.Int
lmejrsResponseStatus = Lens.lens (responseStatus :: ListMailboxExportJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListMailboxExportJobsResponse)
{-# DEPRECATED lmejrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
