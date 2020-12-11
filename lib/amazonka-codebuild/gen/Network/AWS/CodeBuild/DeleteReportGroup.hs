{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.DeleteReportGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a report group. Before you delete a report group, you must delete its reports.
module Network.AWS.CodeBuild.DeleteReportGroup
  ( -- * Creating a request
    DeleteReportGroup (..),
    mkDeleteReportGroup,

    -- ** Request lenses
    drgDeleteReports,
    drgArn,

    -- * Destructuring the response
    DeleteReportGroupResponse (..),
    mkDeleteReportGroupResponse,

    -- ** Response lenses
    drgrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteReportGroup' smart constructor.
data DeleteReportGroup = DeleteReportGroup'
  { deleteReports ::
      Lude.Maybe Lude.Bool,
    arn :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteReportGroup' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the report group to delete.
-- * 'deleteReports' - If @true@ , deletes any reports that belong to a report group before deleting the report group.
--
-- If @false@ , you must delete any reports in the report group. Use <https://docs.aws.amazon.com/codebuild/latest/APIReference/API_ListReportsForReportGroup.html ListReportsForReportGroup> to get the reports in a report group. Use <https://docs.aws.amazon.com/codebuild/latest/APIReference/API_DeleteReport.html DeleteReport> to delete the reports. If you call @DeleteReportGroup@ for a report group that contains one or more reports, an exception is thrown.
mkDeleteReportGroup ::
  -- | 'arn'
  Lude.Text ->
  DeleteReportGroup
mkDeleteReportGroup pArn_ =
  DeleteReportGroup' {deleteReports = Lude.Nothing, arn = pArn_}

-- | If @true@ , deletes any reports that belong to a report group before deleting the report group.
--
-- If @false@ , you must delete any reports in the report group. Use <https://docs.aws.amazon.com/codebuild/latest/APIReference/API_ListReportsForReportGroup.html ListReportsForReportGroup> to get the reports in a report group. Use <https://docs.aws.amazon.com/codebuild/latest/APIReference/API_DeleteReport.html DeleteReport> to delete the reports. If you call @DeleteReportGroup@ for a report group that contains one or more reports, an exception is thrown.
--
-- /Note:/ Consider using 'deleteReports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgDeleteReports :: Lens.Lens' DeleteReportGroup (Lude.Maybe Lude.Bool)
drgDeleteReports = Lens.lens (deleteReports :: DeleteReportGroup -> Lude.Maybe Lude.Bool) (\s a -> s {deleteReports = a} :: DeleteReportGroup)
{-# DEPRECATED drgDeleteReports "Use generic-lens or generic-optics with 'deleteReports' instead." #-}

-- | The ARN of the report group to delete.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgArn :: Lens.Lens' DeleteReportGroup Lude.Text
drgArn = Lens.lens (arn :: DeleteReportGroup -> Lude.Text) (\s a -> s {arn = a} :: DeleteReportGroup)
{-# DEPRECATED drgArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest DeleteReportGroup where
  type Rs DeleteReportGroup = DeleteReportGroupResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteReportGroupResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteReportGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.DeleteReportGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteReportGroup where
  toJSON DeleteReportGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("deleteReports" Lude..=) Lude.<$> deleteReports,
            Lude.Just ("arn" Lude..= arn)
          ]
      )

instance Lude.ToPath DeleteReportGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteReportGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteReportGroupResponse' smart constructor.
newtype DeleteReportGroupResponse = DeleteReportGroupResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteReportGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteReportGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteReportGroupResponse
mkDeleteReportGroupResponse pResponseStatus_ =
  DeleteReportGroupResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgrsResponseStatus :: Lens.Lens' DeleteReportGroupResponse Lude.Int
drgrsResponseStatus = Lens.lens (responseStatus :: DeleteReportGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteReportGroupResponse)
{-# DEPRECATED drgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
