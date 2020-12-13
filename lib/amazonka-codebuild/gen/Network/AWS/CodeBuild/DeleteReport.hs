{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.DeleteReport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a report.
module Network.AWS.CodeBuild.DeleteReport
  ( -- * Creating a request
    DeleteReport (..),
    mkDeleteReport,

    -- ** Request lenses
    drArn,

    -- * Destructuring the response
    DeleteReportResponse (..),
    mkDeleteReportResponse,

    -- ** Response lenses
    drrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteReport' smart constructor.
newtype DeleteReport = DeleteReport'
  { -- | The ARN of the report to delete.
    arn :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteReport' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the report to delete.
mkDeleteReport ::
  -- | 'arn'
  Lude.Text ->
  DeleteReport
mkDeleteReport pArn_ = DeleteReport' {arn = pArn_}

-- | The ARN of the report to delete.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drArn :: Lens.Lens' DeleteReport Lude.Text
drArn = Lens.lens (arn :: DeleteReport -> Lude.Text) (\s a -> s {arn = a} :: DeleteReport)
{-# DEPRECATED drArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest DeleteReport where
  type Rs DeleteReport = DeleteReportResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteReportResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteReport where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.DeleteReport" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteReport where
  toJSON DeleteReport' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("arn" Lude..= arn)])

instance Lude.ToPath DeleteReport where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteReport where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteReportResponse' smart constructor.
newtype DeleteReportResponse = DeleteReportResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteReportResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteReportResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteReportResponse
mkDeleteReportResponse pResponseStatus_ =
  DeleteReportResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsResponseStatus :: Lens.Lens' DeleteReportResponse Lude.Int
drrsResponseStatus = Lens.lens (responseStatus :: DeleteReportResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteReportResponse)
{-# DEPRECATED drrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
