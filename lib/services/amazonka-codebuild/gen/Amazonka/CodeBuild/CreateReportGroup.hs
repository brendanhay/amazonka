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
-- Module      : Amazonka.CodeBuild.CreateReportGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a report group. A report group contains a collection of reports.
module Amazonka.CodeBuild.CreateReportGroup
  ( -- * Creating a Request
    CreateReportGroup (..),
    newCreateReportGroup,

    -- * Request Lenses
    createReportGroup_tags,
    createReportGroup_name,
    createReportGroup_type,
    createReportGroup_exportConfig,

    -- * Destructuring the Response
    CreateReportGroupResponse (..),
    newCreateReportGroupResponse,

    -- * Response Lenses
    createReportGroupResponse_reportGroup,
    createReportGroupResponse_httpStatus,
  )
where

import Amazonka.CodeBuild.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateReportGroup' smart constructor.
data CreateReportGroup = CreateReportGroup'
  { -- | A list of tag key and value pairs associated with this report group.
    --
    -- These tags are available for use by Amazon Web Services services that
    -- support CodeBuild report group tags.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the report group.
    name :: Prelude.Text,
    -- | The type of report group.
    type' :: ReportType,
    -- | A @ReportExportConfig@ object that contains information about where the
    -- report group test results are exported.
    exportConfig :: ReportExportConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateReportGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createReportGroup_tags' - A list of tag key and value pairs associated with this report group.
--
-- These tags are available for use by Amazon Web Services services that
-- support CodeBuild report group tags.
--
-- 'name', 'createReportGroup_name' - The name of the report group.
--
-- 'type'', 'createReportGroup_type' - The type of report group.
--
-- 'exportConfig', 'createReportGroup_exportConfig' - A @ReportExportConfig@ object that contains information about where the
-- report group test results are exported.
newCreateReportGroup ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  ReportType ->
  -- | 'exportConfig'
  ReportExportConfig ->
  CreateReportGroup
newCreateReportGroup pName_ pType_ pExportConfig_ =
  CreateReportGroup'
    { tags = Prelude.Nothing,
      name = pName_,
      type' = pType_,
      exportConfig = pExportConfig_
    }

-- | A list of tag key and value pairs associated with this report group.
--
-- These tags are available for use by Amazon Web Services services that
-- support CodeBuild report group tags.
createReportGroup_tags :: Lens.Lens' CreateReportGroup (Prelude.Maybe [Tag])
createReportGroup_tags = Lens.lens (\CreateReportGroup' {tags} -> tags) (\s@CreateReportGroup' {} a -> s {tags = a} :: CreateReportGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the report group.
createReportGroup_name :: Lens.Lens' CreateReportGroup Prelude.Text
createReportGroup_name = Lens.lens (\CreateReportGroup' {name} -> name) (\s@CreateReportGroup' {} a -> s {name = a} :: CreateReportGroup)

-- | The type of report group.
createReportGroup_type :: Lens.Lens' CreateReportGroup ReportType
createReportGroup_type = Lens.lens (\CreateReportGroup' {type'} -> type') (\s@CreateReportGroup' {} a -> s {type' = a} :: CreateReportGroup)

-- | A @ReportExportConfig@ object that contains information about where the
-- report group test results are exported.
createReportGroup_exportConfig :: Lens.Lens' CreateReportGroup ReportExportConfig
createReportGroup_exportConfig = Lens.lens (\CreateReportGroup' {exportConfig} -> exportConfig) (\s@CreateReportGroup' {} a -> s {exportConfig = a} :: CreateReportGroup)

instance Core.AWSRequest CreateReportGroup where
  type
    AWSResponse CreateReportGroup =
      CreateReportGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateReportGroupResponse'
            Prelude.<$> (x Data..?> "reportGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateReportGroup where
  hashWithSalt _salt CreateReportGroup' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` exportConfig

instance Prelude.NFData CreateReportGroup where
  rnf CreateReportGroup' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf exportConfig

instance Data.ToHeaders CreateReportGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeBuild_20161006.CreateReportGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateReportGroup where
  toJSON CreateReportGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("type" Data..= type'),
            Prelude.Just ("exportConfig" Data..= exportConfig)
          ]
      )

instance Data.ToPath CreateReportGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateReportGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateReportGroupResponse' smart constructor.
data CreateReportGroupResponse = CreateReportGroupResponse'
  { -- | Information about the report group that was created.
    reportGroup :: Prelude.Maybe ReportGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateReportGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportGroup', 'createReportGroupResponse_reportGroup' - Information about the report group that was created.
--
-- 'httpStatus', 'createReportGroupResponse_httpStatus' - The response's http status code.
newCreateReportGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateReportGroupResponse
newCreateReportGroupResponse pHttpStatus_ =
  CreateReportGroupResponse'
    { reportGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the report group that was created.
createReportGroupResponse_reportGroup :: Lens.Lens' CreateReportGroupResponse (Prelude.Maybe ReportGroup)
createReportGroupResponse_reportGroup = Lens.lens (\CreateReportGroupResponse' {reportGroup} -> reportGroup) (\s@CreateReportGroupResponse' {} a -> s {reportGroup = a} :: CreateReportGroupResponse)

-- | The response's http status code.
createReportGroupResponse_httpStatus :: Lens.Lens' CreateReportGroupResponse Prelude.Int
createReportGroupResponse_httpStatus = Lens.lens (\CreateReportGroupResponse' {httpStatus} -> httpStatus) (\s@CreateReportGroupResponse' {} a -> s {httpStatus = a} :: CreateReportGroupResponse)

instance Prelude.NFData CreateReportGroupResponse where
  rnf CreateReportGroupResponse' {..} =
    Prelude.rnf reportGroup
      `Prelude.seq` Prelude.rnf httpStatus
