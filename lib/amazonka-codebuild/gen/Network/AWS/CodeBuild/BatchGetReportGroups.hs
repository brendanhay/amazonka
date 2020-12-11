{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.BatchGetReportGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of report groups.
module Network.AWS.CodeBuild.BatchGetReportGroups
  ( -- * Creating a request
    BatchGetReportGroups (..),
    mkBatchGetReportGroups,

    -- ** Request lenses
    bgrgReportGroupARNs,

    -- * Destructuring the response
    BatchGetReportGroupsResponse (..),
    mkBatchGetReportGroupsResponse,

    -- ** Response lenses
    bgrgrsReportGroups,
    bgrgrsReportGroupsNotFound,
    bgrgrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchGetReportGroups' smart constructor.
newtype BatchGetReportGroups = BatchGetReportGroups'
  { reportGroupARNs ::
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

-- | Creates a value of 'BatchGetReportGroups' with the minimum fields required to make a request.
--
-- * 'reportGroupARNs' - An array of report group ARNs that identify the report groups to return.
mkBatchGetReportGroups ::
  -- | 'reportGroupARNs'
  Lude.NonEmpty Lude.Text ->
  BatchGetReportGroups
mkBatchGetReportGroups pReportGroupARNs_ =
  BatchGetReportGroups' {reportGroupARNs = pReportGroupARNs_}

-- | An array of report group ARNs that identify the report groups to return.
--
-- /Note:/ Consider using 'reportGroupARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgrgReportGroupARNs :: Lens.Lens' BatchGetReportGroups (Lude.NonEmpty Lude.Text)
bgrgReportGroupARNs = Lens.lens (reportGroupARNs :: BatchGetReportGroups -> Lude.NonEmpty Lude.Text) (\s a -> s {reportGroupARNs = a} :: BatchGetReportGroups)
{-# DEPRECATED bgrgReportGroupARNs "Use generic-lens or generic-optics with 'reportGroupARNs' instead." #-}

instance Lude.AWSRequest BatchGetReportGroups where
  type Rs BatchGetReportGroups = BatchGetReportGroupsResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchGetReportGroupsResponse'
            Lude.<$> (x Lude..?> "reportGroups")
            Lude.<*> (x Lude..?> "reportGroupsNotFound")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchGetReportGroups where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.BatchGetReportGroups" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchGetReportGroups where
  toJSON BatchGetReportGroups' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("reportGroupArns" Lude..= reportGroupARNs)]
      )

instance Lude.ToPath BatchGetReportGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchGetReportGroups where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchGetReportGroupsResponse' smart constructor.
data BatchGetReportGroupsResponse = BatchGetReportGroupsResponse'
  { reportGroups ::
      Lude.Maybe
        (Lude.NonEmpty ReportGroup),
    reportGroupsNotFound ::
      Lude.Maybe
        (Lude.NonEmpty Lude.Text),
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

-- | Creates a value of 'BatchGetReportGroupsResponse' with the minimum fields required to make a request.
--
-- * 'reportGroups' - The array of report groups returned by @BatchGetReportGroups@ .
-- * 'reportGroupsNotFound' - An array of ARNs passed to @BatchGetReportGroups@ that are not associated with a @ReportGroup@ .
-- * 'responseStatus' - The response status code.
mkBatchGetReportGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchGetReportGroupsResponse
mkBatchGetReportGroupsResponse pResponseStatus_ =
  BatchGetReportGroupsResponse'
    { reportGroups = Lude.Nothing,
      reportGroupsNotFound = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The array of report groups returned by @BatchGetReportGroups@ .
--
-- /Note:/ Consider using 'reportGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgrgrsReportGroups :: Lens.Lens' BatchGetReportGroupsResponse (Lude.Maybe (Lude.NonEmpty ReportGroup))
bgrgrsReportGroups = Lens.lens (reportGroups :: BatchGetReportGroupsResponse -> Lude.Maybe (Lude.NonEmpty ReportGroup)) (\s a -> s {reportGroups = a} :: BatchGetReportGroupsResponse)
{-# DEPRECATED bgrgrsReportGroups "Use generic-lens or generic-optics with 'reportGroups' instead." #-}

-- | An array of ARNs passed to @BatchGetReportGroups@ that are not associated with a @ReportGroup@ .
--
-- /Note:/ Consider using 'reportGroupsNotFound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgrgrsReportGroupsNotFound :: Lens.Lens' BatchGetReportGroupsResponse (Lude.Maybe (Lude.NonEmpty Lude.Text))
bgrgrsReportGroupsNotFound = Lens.lens (reportGroupsNotFound :: BatchGetReportGroupsResponse -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {reportGroupsNotFound = a} :: BatchGetReportGroupsResponse)
{-# DEPRECATED bgrgrsReportGroupsNotFound "Use generic-lens or generic-optics with 'reportGroupsNotFound' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgrgrsResponseStatus :: Lens.Lens' BatchGetReportGroupsResponse Lude.Int
bgrgrsResponseStatus = Lens.lens (responseStatus :: BatchGetReportGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchGetReportGroupsResponse)
{-# DEPRECATED bgrgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
