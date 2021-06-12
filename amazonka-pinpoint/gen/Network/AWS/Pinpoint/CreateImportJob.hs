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
-- Module      : Network.AWS.Pinpoint.CreateImportJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an import job for an application.
module Network.AWS.Pinpoint.CreateImportJob
  ( -- * Creating a Request
    CreateImportJob (..),
    newCreateImportJob,

    -- * Request Lenses
    createImportJob_applicationId,
    createImportJob_importJobRequest,

    -- * Destructuring the Response
    CreateImportJobResponse (..),
    newCreateImportJobResponse,

    -- * Response Lenses
    createImportJobResponse_httpStatus,
    createImportJobResponse_importJobResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateImportJob' smart constructor.
data CreateImportJob = CreateImportJob'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    importJobRequest :: ImportJobRequest
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'createImportJob_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'importJobRequest', 'createImportJob_importJobRequest' - Undocumented member.
newCreateImportJob ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'importJobRequest'
  ImportJobRequest ->
  CreateImportJob
newCreateImportJob pApplicationId_ pImportJobRequest_ =
  CreateImportJob'
    { applicationId = pApplicationId_,
      importJobRequest = pImportJobRequest_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
createImportJob_applicationId :: Lens.Lens' CreateImportJob Core.Text
createImportJob_applicationId = Lens.lens (\CreateImportJob' {applicationId} -> applicationId) (\s@CreateImportJob' {} a -> s {applicationId = a} :: CreateImportJob)

-- | Undocumented member.
createImportJob_importJobRequest :: Lens.Lens' CreateImportJob ImportJobRequest
createImportJob_importJobRequest = Lens.lens (\CreateImportJob' {importJobRequest} -> importJobRequest) (\s@CreateImportJob' {} a -> s {importJobRequest = a} :: CreateImportJob)

instance Core.AWSRequest CreateImportJob where
  type
    AWSResponse CreateImportJob =
      CreateImportJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateImportJobResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable CreateImportJob

instance Core.NFData CreateImportJob

instance Core.ToHeaders CreateImportJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateImportJob where
  toJSON CreateImportJob' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ImportJobRequest" Core..= importJobRequest)
          ]
      )

instance Core.ToPath CreateImportJob where
  toPath CreateImportJob' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/jobs/import"
      ]

instance Core.ToQuery CreateImportJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateImportJobResponse' smart constructor.
data CreateImportJobResponse = CreateImportJobResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    importJobResponse :: ImportJobResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createImportJobResponse_httpStatus' - The response's http status code.
--
-- 'importJobResponse', 'createImportJobResponse_importJobResponse' - Undocumented member.
newCreateImportJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'importJobResponse'
  ImportJobResponse ->
  CreateImportJobResponse
newCreateImportJobResponse
  pHttpStatus_
  pImportJobResponse_ =
    CreateImportJobResponse'
      { httpStatus = pHttpStatus_,
        importJobResponse = pImportJobResponse_
      }

-- | The response's http status code.
createImportJobResponse_httpStatus :: Lens.Lens' CreateImportJobResponse Core.Int
createImportJobResponse_httpStatus = Lens.lens (\CreateImportJobResponse' {httpStatus} -> httpStatus) (\s@CreateImportJobResponse' {} a -> s {httpStatus = a} :: CreateImportJobResponse)

-- | Undocumented member.
createImportJobResponse_importJobResponse :: Lens.Lens' CreateImportJobResponse ImportJobResponse
createImportJobResponse_importJobResponse = Lens.lens (\CreateImportJobResponse' {importJobResponse} -> importJobResponse) (\s@CreateImportJobResponse' {} a -> s {importJobResponse = a} :: CreateImportJobResponse)

instance Core.NFData CreateImportJobResponse
