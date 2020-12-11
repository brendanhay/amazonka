{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.BatchGetReports
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of reports.
module Network.AWS.CodeBuild.BatchGetReports
  ( -- * Creating a request
    BatchGetReports (..),
    mkBatchGetReports,

    -- ** Request lenses
    bgrReportARNs,

    -- * Destructuring the response
    BatchGetReportsResponse (..),
    mkBatchGetReportsResponse,

    -- ** Response lenses
    bgrrsReports,
    bgrrsReportsNotFound,
    bgrrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchGetReports' smart constructor.
newtype BatchGetReports = BatchGetReports'
  { reportARNs ::
      Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetReports' with the minimum fields required to make a request.
--
-- * 'reportARNs' - An array of ARNs that identify the @Report@ objects to return.
mkBatchGetReports ::
  -- | 'reportARNs'
  Lude.NonEmpty Lude.Text ->
  BatchGetReports
mkBatchGetReports pReportARNs_ =
  BatchGetReports' {reportARNs = pReportARNs_}

-- | An array of ARNs that identify the @Report@ objects to return.
--
-- /Note:/ Consider using 'reportARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgrReportARNs :: Lens.Lens' BatchGetReports (Lude.NonEmpty Lude.Text)
bgrReportARNs = Lens.lens (reportARNs :: BatchGetReports -> Lude.NonEmpty Lude.Text) (\s a -> s {reportARNs = a} :: BatchGetReports)
{-# DEPRECATED bgrReportARNs "Use generic-lens or generic-optics with 'reportARNs' instead." #-}

instance Lude.AWSRequest BatchGetReports where
  type Rs BatchGetReports = BatchGetReportsResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchGetReportsResponse'
            Lude.<$> (x Lude..?> "reports")
            Lude.<*> (x Lude..?> "reportsNotFound")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchGetReports where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.BatchGetReports" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchGetReports where
  toJSON BatchGetReports' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("reportArns" Lude..= reportARNs)])

instance Lude.ToPath BatchGetReports where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchGetReports where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchGetReportsResponse' smart constructor.
data BatchGetReportsResponse = BatchGetReportsResponse'
  { reports ::
      Lude.Maybe (Lude.NonEmpty Report),
    reportsNotFound ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
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

-- | Creates a value of 'BatchGetReportsResponse' with the minimum fields required to make a request.
--
-- * 'reports' - The array of @Report@ objects returned by @BatchGetReports@ .
-- * 'reportsNotFound' - An array of ARNs passed to @BatchGetReportGroups@ that are not associated with a @Report@ .
-- * 'responseStatus' - The response status code.
mkBatchGetReportsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchGetReportsResponse
mkBatchGetReportsResponse pResponseStatus_ =
  BatchGetReportsResponse'
    { reports = Lude.Nothing,
      reportsNotFound = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The array of @Report@ objects returned by @BatchGetReports@ .
--
-- /Note:/ Consider using 'reports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgrrsReports :: Lens.Lens' BatchGetReportsResponse (Lude.Maybe (Lude.NonEmpty Report))
bgrrsReports = Lens.lens (reports :: BatchGetReportsResponse -> Lude.Maybe (Lude.NonEmpty Report)) (\s a -> s {reports = a} :: BatchGetReportsResponse)
{-# DEPRECATED bgrrsReports "Use generic-lens or generic-optics with 'reports' instead." #-}

-- | An array of ARNs passed to @BatchGetReportGroups@ that are not associated with a @Report@ .
--
-- /Note:/ Consider using 'reportsNotFound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgrrsReportsNotFound :: Lens.Lens' BatchGetReportsResponse (Lude.Maybe (Lude.NonEmpty Lude.Text))
bgrrsReportsNotFound = Lens.lens (reportsNotFound :: BatchGetReportsResponse -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {reportsNotFound = a} :: BatchGetReportsResponse)
{-# DEPRECATED bgrrsReportsNotFound "Use generic-lens or generic-optics with 'reportsNotFound' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgrrsResponseStatus :: Lens.Lens' BatchGetReportsResponse Lude.Int
bgrrsResponseStatus = Lens.lens (responseStatus :: BatchGetReportsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchGetReportsResponse)
{-# DEPRECATED bgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
