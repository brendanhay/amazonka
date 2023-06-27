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
-- Module      : Amazonka.AmplifyUiBuilder.GetCodegenJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an existing code generation job.
module Amazonka.AmplifyUiBuilder.GetCodegenJob
  ( -- * Creating a Request
    GetCodegenJob (..),
    newGetCodegenJob,

    -- * Request Lenses
    getCodegenJob_appId,
    getCodegenJob_environmentName,
    getCodegenJob_id,

    -- * Destructuring the Response
    GetCodegenJobResponse (..),
    newGetCodegenJobResponse,

    -- * Response Lenses
    getCodegenJobResponse_job,
    getCodegenJobResponse_httpStatus,
  )
where

import Amazonka.AmplifyUiBuilder.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCodegenJob' smart constructor.
data GetCodegenJob = GetCodegenJob'
  { -- | The unique ID of the Amplify app associated with the code generation
    -- job.
    appId :: Prelude.Text,
    -- | The name of the backend environment that is a part of the Amplify app
    -- associated with the code generation job.
    environmentName :: Prelude.Text,
    -- | The unique ID of the code generation job.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCodegenJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'getCodegenJob_appId' - The unique ID of the Amplify app associated with the code generation
-- job.
--
-- 'environmentName', 'getCodegenJob_environmentName' - The name of the backend environment that is a part of the Amplify app
-- associated with the code generation job.
--
-- 'id', 'getCodegenJob_id' - The unique ID of the code generation job.
newGetCodegenJob ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'environmentName'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  GetCodegenJob
newGetCodegenJob pAppId_ pEnvironmentName_ pId_ =
  GetCodegenJob'
    { appId = pAppId_,
      environmentName = pEnvironmentName_,
      id = pId_
    }

-- | The unique ID of the Amplify app associated with the code generation
-- job.
getCodegenJob_appId :: Lens.Lens' GetCodegenJob Prelude.Text
getCodegenJob_appId = Lens.lens (\GetCodegenJob' {appId} -> appId) (\s@GetCodegenJob' {} a -> s {appId = a} :: GetCodegenJob)

-- | The name of the backend environment that is a part of the Amplify app
-- associated with the code generation job.
getCodegenJob_environmentName :: Lens.Lens' GetCodegenJob Prelude.Text
getCodegenJob_environmentName = Lens.lens (\GetCodegenJob' {environmentName} -> environmentName) (\s@GetCodegenJob' {} a -> s {environmentName = a} :: GetCodegenJob)

-- | The unique ID of the code generation job.
getCodegenJob_id :: Lens.Lens' GetCodegenJob Prelude.Text
getCodegenJob_id = Lens.lens (\GetCodegenJob' {id} -> id) (\s@GetCodegenJob' {} a -> s {id = a} :: GetCodegenJob)

instance Core.AWSRequest GetCodegenJob where
  type
    AWSResponse GetCodegenJob =
      GetCodegenJobResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCodegenJobResponse'
            Prelude.<$> (Data.eitherParseJSON x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCodegenJob where
  hashWithSalt _salt GetCodegenJob' {..} =
    _salt
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` id

instance Prelude.NFData GetCodegenJob where
  rnf GetCodegenJob' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders GetCodegenJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetCodegenJob where
  toPath GetCodegenJob' {..} =
    Prelude.mconcat
      [ "/app/",
        Data.toBS appId,
        "/environment/",
        Data.toBS environmentName,
        "/codegen-jobs/",
        Data.toBS id
      ]

instance Data.ToQuery GetCodegenJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCodegenJobResponse' smart constructor.
data GetCodegenJobResponse = GetCodegenJobResponse'
  { -- | The configuration settings for the code generation job.
    job :: Prelude.Maybe CodegenJob,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCodegenJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'job', 'getCodegenJobResponse_job' - The configuration settings for the code generation job.
--
-- 'httpStatus', 'getCodegenJobResponse_httpStatus' - The response's http status code.
newGetCodegenJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCodegenJobResponse
newGetCodegenJobResponse pHttpStatus_ =
  GetCodegenJobResponse'
    { job = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The configuration settings for the code generation job.
getCodegenJobResponse_job :: Lens.Lens' GetCodegenJobResponse (Prelude.Maybe CodegenJob)
getCodegenJobResponse_job = Lens.lens (\GetCodegenJobResponse' {job} -> job) (\s@GetCodegenJobResponse' {} a -> s {job = a} :: GetCodegenJobResponse)

-- | The response's http status code.
getCodegenJobResponse_httpStatus :: Lens.Lens' GetCodegenJobResponse Prelude.Int
getCodegenJobResponse_httpStatus = Lens.lens (\GetCodegenJobResponse' {httpStatus} -> httpStatus) (\s@GetCodegenJobResponse' {} a -> s {httpStatus = a} :: GetCodegenJobResponse)

instance Prelude.NFData GetCodegenJobResponse where
  rnf GetCodegenJobResponse' {..} =
    Prelude.rnf job
      `Prelude.seq` Prelude.rnf httpStatus
