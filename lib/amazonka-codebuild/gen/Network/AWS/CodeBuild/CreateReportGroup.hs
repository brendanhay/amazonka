{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.CreateReportGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a report group. A report group contains a collection of reports.
module Network.AWS.CodeBuild.CreateReportGroup
  ( -- * Creating a request
    CreateReportGroup (..),
    mkCreateReportGroup,

    -- ** Request lenses
    crgTags,
    crgName,
    crgType,
    crgExportConfig,

    -- * Destructuring the response
    CreateReportGroupResponse (..),
    mkCreateReportGroupResponse,

    -- ** Response lenses
    crgrsReportGroup,
    crgrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateReportGroup' smart constructor.
data CreateReportGroup = CreateReportGroup'
  { tags ::
      Lude.Maybe [Tag],
    name :: Lude.Text,
    type' :: ReportType,
    exportConfig :: ReportExportConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateReportGroup' with the minimum fields required to make a request.
--
-- * 'exportConfig' - A @ReportExportConfig@ object that contains information about where the report group test results are exported.
-- * 'name' - The name of the report group.
-- * 'tags' - A list of tag key and value pairs associated with this report group.
--
-- These tags are available for use by AWS services that support AWS CodeBuild report group tags.
-- * 'type'' - The type of report group.
mkCreateReportGroup ::
  -- | 'name'
  Lude.Text ->
  -- | 'type''
  ReportType ->
  -- | 'exportConfig'
  ReportExportConfig ->
  CreateReportGroup
mkCreateReportGroup pName_ pType_ pExportConfig_ =
  CreateReportGroup'
    { tags = Lude.Nothing,
      name = pName_,
      type' = pType_,
      exportConfig = pExportConfig_
    }

-- | A list of tag key and value pairs associated with this report group.
--
-- These tags are available for use by AWS services that support AWS CodeBuild report group tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgTags :: Lens.Lens' CreateReportGroup (Lude.Maybe [Tag])
crgTags = Lens.lens (tags :: CreateReportGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateReportGroup)
{-# DEPRECATED crgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the report group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgName :: Lens.Lens' CreateReportGroup Lude.Text
crgName = Lens.lens (name :: CreateReportGroup -> Lude.Text) (\s a -> s {name = a} :: CreateReportGroup)
{-# DEPRECATED crgName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of report group.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgType :: Lens.Lens' CreateReportGroup ReportType
crgType = Lens.lens (type' :: CreateReportGroup -> ReportType) (\s a -> s {type' = a} :: CreateReportGroup)
{-# DEPRECATED crgType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A @ReportExportConfig@ object that contains information about where the report group test results are exported.
--
-- /Note:/ Consider using 'exportConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgExportConfig :: Lens.Lens' CreateReportGroup ReportExportConfig
crgExportConfig = Lens.lens (exportConfig :: CreateReportGroup -> ReportExportConfig) (\s a -> s {exportConfig = a} :: CreateReportGroup)
{-# DEPRECATED crgExportConfig "Use generic-lens or generic-optics with 'exportConfig' instead." #-}

instance Lude.AWSRequest CreateReportGroup where
  type Rs CreateReportGroup = CreateReportGroupResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateReportGroupResponse'
            Lude.<$> (x Lude..?> "reportGroup") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateReportGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.CreateReportGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateReportGroup where
  toJSON CreateReportGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("name" Lude..= name),
            Lude.Just ("type" Lude..= type'),
            Lude.Just ("exportConfig" Lude..= exportConfig)
          ]
      )

instance Lude.ToPath CreateReportGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateReportGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateReportGroupResponse' smart constructor.
data CreateReportGroupResponse = CreateReportGroupResponse'
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

-- | Creates a value of 'CreateReportGroupResponse' with the minimum fields required to make a request.
--
-- * 'reportGroup' - Information about the report group that was created.
-- * 'responseStatus' - The response status code.
mkCreateReportGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateReportGroupResponse
mkCreateReportGroupResponse pResponseStatus_ =
  CreateReportGroupResponse'
    { reportGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the report group that was created.
--
-- /Note:/ Consider using 'reportGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgrsReportGroup :: Lens.Lens' CreateReportGroupResponse (Lude.Maybe ReportGroup)
crgrsReportGroup = Lens.lens (reportGroup :: CreateReportGroupResponse -> Lude.Maybe ReportGroup) (\s a -> s {reportGroup = a} :: CreateReportGroupResponse)
{-# DEPRECATED crgrsReportGroup "Use generic-lens or generic-optics with 'reportGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgrsResponseStatus :: Lens.Lens' CreateReportGroupResponse Lude.Int
crgrsResponseStatus = Lens.lens (responseStatus :: CreateReportGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateReportGroupResponse)
{-# DEPRECATED crgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
