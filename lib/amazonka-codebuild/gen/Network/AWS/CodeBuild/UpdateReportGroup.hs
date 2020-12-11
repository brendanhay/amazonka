{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.UpdateReportGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a report group.
module Network.AWS.CodeBuild.UpdateReportGroup
  ( -- * Creating a request
    UpdateReportGroup (..),
    mkUpdateReportGroup,

    -- ** Request lenses
    urgExportConfig,
    urgTags,
    urgArn,

    -- * Destructuring the response
    UpdateReportGroupResponse (..),
    mkUpdateReportGroupResponse,

    -- ** Response lenses
    urgrsReportGroup,
    urgrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateReportGroup' smart constructor.
data UpdateReportGroup = UpdateReportGroup'
  { exportConfig ::
      Lude.Maybe ReportExportConfig,
    tags :: Lude.Maybe [Tag],
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

-- | Creates a value of 'UpdateReportGroup' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the report group to update.
-- * 'exportConfig' - Used to specify an updated export type. Valid values are:
--
--
--     * @S3@ : The report results are exported to an S3 bucket.
--
--
--     * @NO_EXPORT@ : The report results are not exported.
--
--
-- * 'tags' - An updated list of tag key and value pairs associated with this report group.
--
-- These tags are available for use by AWS services that support AWS CodeBuild report group tags.
mkUpdateReportGroup ::
  -- | 'arn'
  Lude.Text ->
  UpdateReportGroup
mkUpdateReportGroup pArn_ =
  UpdateReportGroup'
    { exportConfig = Lude.Nothing,
      tags = Lude.Nothing,
      arn = pArn_
    }

-- | Used to specify an updated export type. Valid values are:
--
--
--     * @S3@ : The report results are exported to an S3 bucket.
--
--
--     * @NO_EXPORT@ : The report results are not exported.
--
--
--
-- /Note:/ Consider using 'exportConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgExportConfig :: Lens.Lens' UpdateReportGroup (Lude.Maybe ReportExportConfig)
urgExportConfig = Lens.lens (exportConfig :: UpdateReportGroup -> Lude.Maybe ReportExportConfig) (\s a -> s {exportConfig = a} :: UpdateReportGroup)
{-# DEPRECATED urgExportConfig "Use generic-lens or generic-optics with 'exportConfig' instead." #-}

-- | An updated list of tag key and value pairs associated with this report group.
--
-- These tags are available for use by AWS services that support AWS CodeBuild report group tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgTags :: Lens.Lens' UpdateReportGroup (Lude.Maybe [Tag])
urgTags = Lens.lens (tags :: UpdateReportGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: UpdateReportGroup)
{-# DEPRECATED urgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The ARN of the report group to update.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgArn :: Lens.Lens' UpdateReportGroup Lude.Text
urgArn = Lens.lens (arn :: UpdateReportGroup -> Lude.Text) (\s a -> s {arn = a} :: UpdateReportGroup)
{-# DEPRECATED urgArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest UpdateReportGroup where
  type Rs UpdateReportGroup = UpdateReportGroupResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateReportGroupResponse'
            Lude.<$> (x Lude..?> "reportGroup") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateReportGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.UpdateReportGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateReportGroup where
  toJSON UpdateReportGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("exportConfig" Lude..=) Lude.<$> exportConfig,
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("arn" Lude..= arn)
          ]
      )

instance Lude.ToPath UpdateReportGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateReportGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateReportGroupResponse' smart constructor.
data UpdateReportGroupResponse = UpdateReportGroupResponse'
  { reportGroup ::
      Lude.Maybe ReportGroup,
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

-- | Creates a value of 'UpdateReportGroupResponse' with the minimum fields required to make a request.
--
-- * 'reportGroup' - Information about the updated report group.
-- * 'responseStatus' - The response status code.
mkUpdateReportGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateReportGroupResponse
mkUpdateReportGroupResponse pResponseStatus_ =
  UpdateReportGroupResponse'
    { reportGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the updated report group.
--
-- /Note:/ Consider using 'reportGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgrsReportGroup :: Lens.Lens' UpdateReportGroupResponse (Lude.Maybe ReportGroup)
urgrsReportGroup = Lens.lens (reportGroup :: UpdateReportGroupResponse -> Lude.Maybe ReportGroup) (\s a -> s {reportGroup = a} :: UpdateReportGroupResponse)
{-# DEPRECATED urgrsReportGroup "Use generic-lens or generic-optics with 'reportGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgrsResponseStatus :: Lens.Lens' UpdateReportGroupResponse Lude.Int
urgrsResponseStatus = Lens.lens (responseStatus :: UpdateReportGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateReportGroupResponse)
{-# DEPRECATED urgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
