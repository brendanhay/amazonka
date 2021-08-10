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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateReportGroup' smart constructor.
data UpdateReportGroup = UpdateReportGroup'
  { -- | Used to specify an updated export type. Valid values are:
    --
    -- -   @S3@: The report results are exported to an S3 bucket.
    --
    -- -   @NO_EXPORT@: The report results are not exported.
    exportConfig :: Prelude.Maybe ReportExportConfig,
    -- | An updated list of tag key and value pairs associated with this report
    -- group.
    --
    -- These tags are available for use by AWS services that support AWS
    -- CodeBuild report group tags.
    tags :: Prelude.Maybe [Tag],
    -- | The ARN of the report group to update.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  UpdateReportGroup
newUpdateReportGroup pArn_ =
  UpdateReportGroup'
    { exportConfig = Prelude.Nothing,
      tags = Prelude.Nothing,
      arn = pArn_
    }

-- | Used to specify an updated export type. Valid values are:
--
-- -   @S3@: The report results are exported to an S3 bucket.
--
-- -   @NO_EXPORT@: The report results are not exported.
updateReportGroup_exportConfig :: Lens.Lens' UpdateReportGroup (Prelude.Maybe ReportExportConfig)
updateReportGroup_exportConfig = Lens.lens (\UpdateReportGroup' {exportConfig} -> exportConfig) (\s@UpdateReportGroup' {} a -> s {exportConfig = a} :: UpdateReportGroup)

-- | An updated list of tag key and value pairs associated with this report
-- group.
--
-- These tags are available for use by AWS services that support AWS
-- CodeBuild report group tags.
updateReportGroup_tags :: Lens.Lens' UpdateReportGroup (Prelude.Maybe [Tag])
updateReportGroup_tags = Lens.lens (\UpdateReportGroup' {tags} -> tags) (\s@UpdateReportGroup' {} a -> s {tags = a} :: UpdateReportGroup) Prelude.. Lens.mapping Lens._Coerce

-- | The ARN of the report group to update.
updateReportGroup_arn :: Lens.Lens' UpdateReportGroup Prelude.Text
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
            Prelude.<$> (x Core..?> "reportGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateReportGroup

instance Prelude.NFData UpdateReportGroup

instance Core.ToHeaders UpdateReportGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.UpdateReportGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateReportGroup where
  toJSON UpdateReportGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("exportConfig" Core..=) Prelude.<$> exportConfig,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("arn" Core..= arn)
          ]
      )

instance Core.ToPath UpdateReportGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateReportGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateReportGroupResponse' smart constructor.
data UpdateReportGroupResponse = UpdateReportGroupResponse'
  { -- | Information about the updated report group.
    reportGroup :: Prelude.Maybe ReportGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateReportGroupResponse
newUpdateReportGroupResponse pHttpStatus_ =
  UpdateReportGroupResponse'
    { reportGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the updated report group.
updateReportGroupResponse_reportGroup :: Lens.Lens' UpdateReportGroupResponse (Prelude.Maybe ReportGroup)
updateReportGroupResponse_reportGroup = Lens.lens (\UpdateReportGroupResponse' {reportGroup} -> reportGroup) (\s@UpdateReportGroupResponse' {} a -> s {reportGroup = a} :: UpdateReportGroupResponse)

-- | The response's http status code.
updateReportGroupResponse_httpStatus :: Lens.Lens' UpdateReportGroupResponse Prelude.Int
updateReportGroupResponse_httpStatus = Lens.lens (\UpdateReportGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateReportGroupResponse' {} a -> s {httpStatus = a} :: UpdateReportGroupResponse)

instance Prelude.NFData UpdateReportGroupResponse
