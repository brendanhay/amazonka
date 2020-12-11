{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListAuditFindings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the findings (results) of a Device Defender audit or of the audits performed during a specified time period. (Findings are retained for 90 days.)
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListAuditFindings
  ( -- * Creating a request
    ListAuditFindings (..),
    mkListAuditFindings,

    -- ** Request lenses
    lafStartTime,
    lafTaskId,
    lafCheckName,
    lafListSuppressedFindings,
    lafNextToken,
    lafEndTime,
    lafMaxResults,
    lafResourceIdentifier,

    -- * Destructuring the response
    ListAuditFindingsResponse (..),
    mkListAuditFindingsResponse,

    -- ** Response lenses
    lafrsNextToken,
    lafrsFindings,
    lafrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListAuditFindings' smart constructor.
data ListAuditFindings = ListAuditFindings'
  { startTime ::
      Lude.Maybe Lude.Timestamp,
    taskId :: Lude.Maybe Lude.Text,
    checkName :: Lude.Maybe Lude.Text,
    listSuppressedFindings :: Lude.Maybe Lude.Bool,
    nextToken :: Lude.Maybe Lude.Text,
    endTime :: Lude.Maybe Lude.Timestamp,
    maxResults :: Lude.Maybe Lude.Natural,
    resourceIdentifier :: Lude.Maybe ResourceIdentifier
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAuditFindings' with the minimum fields required to make a request.
--
-- * 'checkName' - A filter to limit results to the findings for the specified audit check.
-- * 'endTime' - A filter to limit results to those found before the specified time. You must specify either the startTime and endTime or the taskId, but not both.
-- * 'listSuppressedFindings' - Boolean flag indicating whether only the suppressed findings or the unsuppressed findings should be listed. If this parameter isn't provided, the response will list both suppressed and unsuppressed findings.
-- * 'maxResults' - The maximum number of results to return at one time. The default is 25.
-- * 'nextToken' - The token for the next set of results.
-- * 'resourceIdentifier' - Information identifying the noncompliant resource.
-- * 'startTime' - A filter to limit results to those found after the specified time. You must specify either the startTime and endTime or the taskId, but not both.
-- * 'taskId' - A filter to limit results to the audit with the specified ID. You must specify either the taskId or the startTime and endTime, but not both.
mkListAuditFindings ::
  ListAuditFindings
mkListAuditFindings =
  ListAuditFindings'
    { startTime = Lude.Nothing,
      taskId = Lude.Nothing,
      checkName = Lude.Nothing,
      listSuppressedFindings = Lude.Nothing,
      nextToken = Lude.Nothing,
      endTime = Lude.Nothing,
      maxResults = Lude.Nothing,
      resourceIdentifier = Lude.Nothing
    }

-- | A filter to limit results to those found after the specified time. You must specify either the startTime and endTime or the taskId, but not both.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafStartTime :: Lens.Lens' ListAuditFindings (Lude.Maybe Lude.Timestamp)
lafStartTime = Lens.lens (startTime :: ListAuditFindings -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: ListAuditFindings)
{-# DEPRECATED lafStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | A filter to limit results to the audit with the specified ID. You must specify either the taskId or the startTime and endTime, but not both.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafTaskId :: Lens.Lens' ListAuditFindings (Lude.Maybe Lude.Text)
lafTaskId = Lens.lens (taskId :: ListAuditFindings -> Lude.Maybe Lude.Text) (\s a -> s {taskId = a} :: ListAuditFindings)
{-# DEPRECATED lafTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

-- | A filter to limit results to the findings for the specified audit check.
--
-- /Note:/ Consider using 'checkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafCheckName :: Lens.Lens' ListAuditFindings (Lude.Maybe Lude.Text)
lafCheckName = Lens.lens (checkName :: ListAuditFindings -> Lude.Maybe Lude.Text) (\s a -> s {checkName = a} :: ListAuditFindings)
{-# DEPRECATED lafCheckName "Use generic-lens or generic-optics with 'checkName' instead." #-}

-- | Boolean flag indicating whether only the suppressed findings or the unsuppressed findings should be listed. If this parameter isn't provided, the response will list both suppressed and unsuppressed findings.
--
-- /Note:/ Consider using 'listSuppressedFindings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafListSuppressedFindings :: Lens.Lens' ListAuditFindings (Lude.Maybe Lude.Bool)
lafListSuppressedFindings = Lens.lens (listSuppressedFindings :: ListAuditFindings -> Lude.Maybe Lude.Bool) (\s a -> s {listSuppressedFindings = a} :: ListAuditFindings)
{-# DEPRECATED lafListSuppressedFindings "Use generic-lens or generic-optics with 'listSuppressedFindings' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafNextToken :: Lens.Lens' ListAuditFindings (Lude.Maybe Lude.Text)
lafNextToken = Lens.lens (nextToken :: ListAuditFindings -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAuditFindings)
{-# DEPRECATED lafNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A filter to limit results to those found before the specified time. You must specify either the startTime and endTime or the taskId, but not both.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafEndTime :: Lens.Lens' ListAuditFindings (Lude.Maybe Lude.Timestamp)
lafEndTime = Lens.lens (endTime :: ListAuditFindings -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: ListAuditFindings)
{-# DEPRECATED lafEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The maximum number of results to return at one time. The default is 25.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafMaxResults :: Lens.Lens' ListAuditFindings (Lude.Maybe Lude.Natural)
lafMaxResults = Lens.lens (maxResults :: ListAuditFindings -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListAuditFindings)
{-# DEPRECATED lafMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Information identifying the noncompliant resource.
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafResourceIdentifier :: Lens.Lens' ListAuditFindings (Lude.Maybe ResourceIdentifier)
lafResourceIdentifier = Lens.lens (resourceIdentifier :: ListAuditFindings -> Lude.Maybe ResourceIdentifier) (\s a -> s {resourceIdentifier = a} :: ListAuditFindings)
{-# DEPRECATED lafResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}

instance Page.AWSPager ListAuditFindings where
  page rq rs
    | Page.stop (rs Lens.^. lafrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lafrsFindings) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lafNextToken Lens..~ rs Lens.^. lafrsNextToken

instance Lude.AWSRequest ListAuditFindings where
  type Rs ListAuditFindings = ListAuditFindingsResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAuditFindingsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "findings" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAuditFindings where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON ListAuditFindings where
  toJSON ListAuditFindings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("startTime" Lude..=) Lude.<$> startTime,
            ("taskId" Lude..=) Lude.<$> taskId,
            ("checkName" Lude..=) Lude.<$> checkName,
            ("listSuppressedFindings" Lude..=) Lude.<$> listSuppressedFindings,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("endTime" Lude..=) Lude.<$> endTime,
            ("maxResults" Lude..=) Lude.<$> maxResults,
            ("resourceIdentifier" Lude..=) Lude.<$> resourceIdentifier
          ]
      )

instance Lude.ToPath ListAuditFindings where
  toPath = Lude.const "/audit/findings"

instance Lude.ToQuery ListAuditFindings where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAuditFindingsResponse' smart constructor.
data ListAuditFindingsResponse = ListAuditFindingsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    findings :: Lude.Maybe [AuditFinding],
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

-- | Creates a value of 'ListAuditFindingsResponse' with the minimum fields required to make a request.
--
-- * 'findings' - The findings (results) of the audit.
-- * 'nextToken' - A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
-- * 'responseStatus' - The response status code.
mkListAuditFindingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAuditFindingsResponse
mkListAuditFindingsResponse pResponseStatus_ =
  ListAuditFindingsResponse'
    { nextToken = Lude.Nothing,
      findings = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafrsNextToken :: Lens.Lens' ListAuditFindingsResponse (Lude.Maybe Lude.Text)
lafrsNextToken = Lens.lens (nextToken :: ListAuditFindingsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAuditFindingsResponse)
{-# DEPRECATED lafrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The findings (results) of the audit.
--
-- /Note:/ Consider using 'findings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafrsFindings :: Lens.Lens' ListAuditFindingsResponse (Lude.Maybe [AuditFinding])
lafrsFindings = Lens.lens (findings :: ListAuditFindingsResponse -> Lude.Maybe [AuditFinding]) (\s a -> s {findings = a} :: ListAuditFindingsResponse)
{-# DEPRECATED lafrsFindings "Use generic-lens or generic-optics with 'findings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafrsResponseStatus :: Lens.Lens' ListAuditFindingsResponse Lude.Int
lafrsResponseStatus = Lens.lens (responseStatus :: ListAuditFindingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAuditFindingsResponse)
{-# DEPRECATED lafrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
