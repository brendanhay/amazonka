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
-- Module      : Amazonka.Pinpoint.CreateImportJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an import job for an application.
module Amazonka.Pinpoint.CreateImportJob
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateImportJob' smart constructor.
data CreateImportJob = CreateImportJob'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    importJobRequest :: ImportJobRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
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
createImportJob_applicationId :: Lens.Lens' CreateImportJob Prelude.Text
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
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable CreateImportJob where
  hashWithSalt _salt CreateImportJob' {..} =
    _salt `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` importJobRequest

instance Prelude.NFData CreateImportJob where
  rnf CreateImportJob' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf importJobRequest

instance Core.ToHeaders CreateImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateImportJob where
  toJSON CreateImportJob' {..} =
    Core.toJSON importJobRequest

instance Core.ToPath CreateImportJob where
  toPath CreateImportJob' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/jobs/import"
      ]

instance Core.ToQuery CreateImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateImportJobResponse' smart constructor.
data CreateImportJobResponse = CreateImportJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    importJobResponse :: ImportJobResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
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
createImportJobResponse_httpStatus :: Lens.Lens' CreateImportJobResponse Prelude.Int
createImportJobResponse_httpStatus = Lens.lens (\CreateImportJobResponse' {httpStatus} -> httpStatus) (\s@CreateImportJobResponse' {} a -> s {httpStatus = a} :: CreateImportJobResponse)

-- | Undocumented member.
createImportJobResponse_importJobResponse :: Lens.Lens' CreateImportJobResponse ImportJobResponse
createImportJobResponse_importJobResponse = Lens.lens (\CreateImportJobResponse' {importJobResponse} -> importJobResponse) (\s@CreateImportJobResponse' {} a -> s {importJobResponse = a} :: CreateImportJobResponse)

instance Prelude.NFData CreateImportJobResponse where
  rnf CreateImportJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf importJobResponse
