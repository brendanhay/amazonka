{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.UpdateReportGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a report group.
module Network.AWS.CodeBuild.UpdateReportGroup
  ( -- * Creating a Request
    UpdateReportGroup (..),
    newUpdateReportGroup,

    -- * Request Lenses
    updateReportGroup_exportConfig,
    updateReportGroup_tags,
    updateReportGroup_arn,

    -- * Destructuring the Response
    UpdateReportGroupResponse (..),
    newUpdateReportGroupResponse,

    -- * Response Lenses
    updateReportGroupResponse_reportGroup,
    updateReportGroupResponse_httpStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateReportGroup' smart constructor.
data UpdateReportGroup = UpdateReportGroup'
  { -- | Used to specify an updated export type. Valid values are:
    --
    -- -   @S3@: The report results are exported to an S3 bucket.
    --
    -- -   @NO_EXPORT@: The report results are not exported.
    exportConfig :: Core.Maybe ReportExportConfig,
    -- | An updated list of tag key and value pairs associated with this report
    -- group.
    --
    -- These tags are available for use by AWS services that support AWS
    -- CodeBuild report group tags.
    tags :: Core.Maybe [Tag],
    -- | The ARN of the report group to update.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateReportGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportConfig', 'updateReportGroup_exportConfig' - Used to specify an updated export type. Valid values are:
--
-- -   @S3@: The report results are exported to an S3 bucket.
--
-- -   @NO_EXPORT@: The report results are not exported.
--
-- 'tags', 'updateReportGroup_tags' - An updated list of tag key and value pairs associated with this report
-- group.
--
-- These tags are available for use by AWS services that support AWS
-- CodeBuild report group tags.
--
-- 'arn', 'updateReportGroup_arn' - The ARN of the report group to update.
newUpdateReportGroup ::
  -- | 'arn'
  Core.Text ->
  UpdateReportGroup
newUpdateReportGroup pArn_ =
  UpdateReportGroup'
    { exportConfig = Core.Nothing,
      tags = Core.Nothing,
      arn = pArn_
    }

-- | Used to specify an updated export type. Valid values are:
--
-- -   @S3@: The report results are exported to an S3 bucket.
--
-- -   @NO_EXPORT@: The report results are not exported.
updateReportGroup_exportConfig :: Lens.Lens' UpdateReportGroup (Core.Maybe ReportExportConfig)
updateReportGroup_exportConfig = Lens.lens (\UpdateReportGroup' {exportConfig} -> exportConfig) (\s@UpdateReportGroup' {} a -> s {exportConfig = a} :: UpdateReportGroup)

-- | An updated list of tag key and value pairs associated with this report
-- group.
--
-- These tags are available for use by AWS services that support AWS
-- CodeBuild report group tags.
updateReportGroup_tags :: Lens.Lens' UpdateReportGroup (Core.Maybe [Tag])
updateReportGroup_tags = Lens.lens (\UpdateReportGroup' {tags} -> tags) (\s@UpdateReportGroup' {} a -> s {tags = a} :: UpdateReportGroup) Core.. Lens.mapping Lens._Coerce

-- | The ARN of the report group to update.
updateReportGroup_arn :: Lens.Lens' UpdateReportGroup Core.Text
updateReportGroup_arn = Lens.lens (\UpdateReportGroup' {arn} -> arn) (\s@UpdateReportGroup' {} a -> s {arn = a} :: UpdateReportGroup)

instance Core.AWSRequest UpdateReportGroup where
  type
    AWSResponse UpdateReportGroup =
      UpdateReportGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateReportGroupResponse'
            Core.<$> (x Core..?> "reportGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateReportGroup

instance Core.NFData UpdateReportGroup

instance Core.ToHeaders UpdateReportGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.UpdateReportGroup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateReportGroup where
  toJSON UpdateReportGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("exportConfig" Core..=) Core.<$> exportConfig,
            ("tags" Core..=) Core.<$> tags,
            Core.Just ("arn" Core..= arn)
          ]
      )

instance Core.ToPath UpdateReportGroup where
  toPath = Core.const "/"

instance Core.ToQuery UpdateReportGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateReportGroupResponse' smart constructor.
data UpdateReportGroupResponse = UpdateReportGroupResponse'
  { -- | Information about the updated report group.
    reportGroup :: Core.Maybe ReportGroup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateReportGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportGroup', 'updateReportGroupResponse_reportGroup' - Information about the updated report group.
--
-- 'httpStatus', 'updateReportGroupResponse_httpStatus' - The response's http status code.
newUpdateReportGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateReportGroupResponse
newUpdateReportGroupResponse pHttpStatus_ =
  UpdateReportGroupResponse'
    { reportGroup =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the updated report group.
updateReportGroupResponse_reportGroup :: Lens.Lens' UpdateReportGroupResponse (Core.Maybe ReportGroup)
updateReportGroupResponse_reportGroup = Lens.lens (\UpdateReportGroupResponse' {reportGroup} -> reportGroup) (\s@UpdateReportGroupResponse' {} a -> s {reportGroup = a} :: UpdateReportGroupResponse)

-- | The response's http status code.
updateReportGroupResponse_httpStatus :: Lens.Lens' UpdateReportGroupResponse Core.Int
updateReportGroupResponse_httpStatus = Lens.lens (\UpdateReportGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateReportGroupResponse' {} a -> s {httpStatus = a} :: UpdateReportGroupResponse)

instance Core.NFData UpdateReportGroupResponse
