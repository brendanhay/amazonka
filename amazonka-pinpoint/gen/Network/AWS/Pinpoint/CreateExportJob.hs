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
-- Module      : Network.AWS.Pinpoint.CreateExportJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an export job for an application.
module Network.AWS.Pinpoint.CreateExportJob
  ( -- * Creating a Request
    CreateExportJob (..),
    newCreateExportJob,

    -- * Request Lenses
    createExportJob_applicationId,
    createExportJob_exportJobRequest,

    -- * Destructuring the Response
    CreateExportJobResponse (..),
    newCreateExportJobResponse,

    -- * Response Lenses
    createExportJobResponse_httpStatus,
    createExportJobResponse_exportJobResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateExportJob' smart constructor.
data CreateExportJob = CreateExportJob'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    exportJobRequest :: ExportJobRequest
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateExportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'createExportJob_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'exportJobRequest', 'createExportJob_exportJobRequest' - Undocumented member.
newCreateExportJob ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'exportJobRequest'
  ExportJobRequest ->
  CreateExportJob
newCreateExportJob pApplicationId_ pExportJobRequest_ =
  CreateExportJob'
    { applicationId = pApplicationId_,
      exportJobRequest = pExportJobRequest_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
createExportJob_applicationId :: Lens.Lens' CreateExportJob Core.Text
createExportJob_applicationId = Lens.lens (\CreateExportJob' {applicationId} -> applicationId) (\s@CreateExportJob' {} a -> s {applicationId = a} :: CreateExportJob)

-- | Undocumented member.
createExportJob_exportJobRequest :: Lens.Lens' CreateExportJob ExportJobRequest
createExportJob_exportJobRequest = Lens.lens (\CreateExportJob' {exportJobRequest} -> exportJobRequest) (\s@CreateExportJob' {} a -> s {exportJobRequest = a} :: CreateExportJob)

instance Core.AWSRequest CreateExportJob where
  type
    AWSResponse CreateExportJob =
      CreateExportJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateExportJobResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable CreateExportJob

instance Core.NFData CreateExportJob

instance Core.ToHeaders CreateExportJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateExportJob where
  toJSON CreateExportJob' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ExportJobRequest" Core..= exportJobRequest)
          ]
      )

instance Core.ToPath CreateExportJob where
  toPath CreateExportJob' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/jobs/export"
      ]

instance Core.ToQuery CreateExportJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateExportJobResponse' smart constructor.
data CreateExportJobResponse = CreateExportJobResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    exportJobResponse :: ExportJobResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateExportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createExportJobResponse_httpStatus' - The response's http status code.
--
-- 'exportJobResponse', 'createExportJobResponse_exportJobResponse' - Undocumented member.
newCreateExportJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'exportJobResponse'
  ExportJobResponse ->
  CreateExportJobResponse
newCreateExportJobResponse
  pHttpStatus_
  pExportJobResponse_ =
    CreateExportJobResponse'
      { httpStatus = pHttpStatus_,
        exportJobResponse = pExportJobResponse_
      }

-- | The response's http status code.
createExportJobResponse_httpStatus :: Lens.Lens' CreateExportJobResponse Core.Int
createExportJobResponse_httpStatus = Lens.lens (\CreateExportJobResponse' {httpStatus} -> httpStatus) (\s@CreateExportJobResponse' {} a -> s {httpStatus = a} :: CreateExportJobResponse)

-- | Undocumented member.
createExportJobResponse_exportJobResponse :: Lens.Lens' CreateExportJobResponse ExportJobResponse
createExportJobResponse_exportJobResponse = Lens.lens (\CreateExportJobResponse' {exportJobResponse} -> exportJobResponse) (\s@CreateExportJobResponse' {} a -> s {exportJobResponse = a} :: CreateExportJobResponse)

instance Core.NFData CreateExportJobResponse
