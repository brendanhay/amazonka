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
-- Module      : Amazonka.AmplifyUiBuilder.StartCodegenJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a code generation job for for a specified Amplify app and backend
-- environment.
module Amazonka.AmplifyUiBuilder.StartCodegenJob
  ( -- * Creating a Request
    StartCodegenJob (..),
    newStartCodegenJob,

    -- * Request Lenses
    startCodegenJob_clientToken,
    startCodegenJob_appId,
    startCodegenJob_environmentName,
    startCodegenJob_codegenJobToCreate,

    -- * Destructuring the Response
    StartCodegenJobResponse (..),
    newStartCodegenJobResponse,

    -- * Response Lenses
    startCodegenJobResponse_entity,
    startCodegenJobResponse_httpStatus,
  )
where

import Amazonka.AmplifyUiBuilder.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartCodegenJob' smart constructor.
data StartCodegenJob = StartCodegenJob'
  { -- | The idempotency token used to ensure that the code generation job
    -- request completes only once.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The unique ID for the Amplify app.
    appId :: Prelude.Text,
    -- | The name of the backend environment that is a part of the Amplify app.
    environmentName :: Prelude.Text,
    -- | The code generation job resource configuration.
    codegenJobToCreate :: StartCodegenJobData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartCodegenJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'startCodegenJob_clientToken' - The idempotency token used to ensure that the code generation job
-- request completes only once.
--
-- 'appId', 'startCodegenJob_appId' - The unique ID for the Amplify app.
--
-- 'environmentName', 'startCodegenJob_environmentName' - The name of the backend environment that is a part of the Amplify app.
--
-- 'codegenJobToCreate', 'startCodegenJob_codegenJobToCreate' - The code generation job resource configuration.
newStartCodegenJob ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'environmentName'
  Prelude.Text ->
  -- | 'codegenJobToCreate'
  StartCodegenJobData ->
  StartCodegenJob
newStartCodegenJob
  pAppId_
  pEnvironmentName_
  pCodegenJobToCreate_ =
    StartCodegenJob'
      { clientToken = Prelude.Nothing,
        appId = pAppId_,
        environmentName = pEnvironmentName_,
        codegenJobToCreate = pCodegenJobToCreate_
      }

-- | The idempotency token used to ensure that the code generation job
-- request completes only once.
startCodegenJob_clientToken :: Lens.Lens' StartCodegenJob (Prelude.Maybe Prelude.Text)
startCodegenJob_clientToken = Lens.lens (\StartCodegenJob' {clientToken} -> clientToken) (\s@StartCodegenJob' {} a -> s {clientToken = a} :: StartCodegenJob)

-- | The unique ID for the Amplify app.
startCodegenJob_appId :: Lens.Lens' StartCodegenJob Prelude.Text
startCodegenJob_appId = Lens.lens (\StartCodegenJob' {appId} -> appId) (\s@StartCodegenJob' {} a -> s {appId = a} :: StartCodegenJob)

-- | The name of the backend environment that is a part of the Amplify app.
startCodegenJob_environmentName :: Lens.Lens' StartCodegenJob Prelude.Text
startCodegenJob_environmentName = Lens.lens (\StartCodegenJob' {environmentName} -> environmentName) (\s@StartCodegenJob' {} a -> s {environmentName = a} :: StartCodegenJob)

-- | The code generation job resource configuration.
startCodegenJob_codegenJobToCreate :: Lens.Lens' StartCodegenJob StartCodegenJobData
startCodegenJob_codegenJobToCreate = Lens.lens (\StartCodegenJob' {codegenJobToCreate} -> codegenJobToCreate) (\s@StartCodegenJob' {} a -> s {codegenJobToCreate = a} :: StartCodegenJob)

instance Core.AWSRequest StartCodegenJob where
  type
    AWSResponse StartCodegenJob =
      StartCodegenJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartCodegenJobResponse'
            Prelude.<$> (Data.eitherParseJSON x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartCodegenJob where
  hashWithSalt _salt StartCodegenJob' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` codegenJobToCreate

instance Prelude.NFData StartCodegenJob where
  rnf StartCodegenJob' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf codegenJobToCreate

instance Data.ToHeaders StartCodegenJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartCodegenJob where
  toJSON StartCodegenJob' {..} =
    Data.toJSON codegenJobToCreate

instance Data.ToPath StartCodegenJob where
  toPath StartCodegenJob' {..} =
    Prelude.mconcat
      [ "/app/",
        Data.toBS appId,
        "/environment/",
        Data.toBS environmentName,
        "/codegen-jobs"
      ]

instance Data.ToQuery StartCodegenJob where
  toQuery StartCodegenJob' {..} =
    Prelude.mconcat ["clientToken" Data.=: clientToken]

-- | /See:/ 'newStartCodegenJobResponse' smart constructor.
data StartCodegenJobResponse = StartCodegenJobResponse'
  { -- | The code generation job for a UI component that is associated with an
    -- Amplify app.
    entity :: Prelude.Maybe CodegenJob,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartCodegenJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entity', 'startCodegenJobResponse_entity' - The code generation job for a UI component that is associated with an
-- Amplify app.
--
-- 'httpStatus', 'startCodegenJobResponse_httpStatus' - The response's http status code.
newStartCodegenJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartCodegenJobResponse
newStartCodegenJobResponse pHttpStatus_ =
  StartCodegenJobResponse'
    { entity = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The code generation job for a UI component that is associated with an
-- Amplify app.
startCodegenJobResponse_entity :: Lens.Lens' StartCodegenJobResponse (Prelude.Maybe CodegenJob)
startCodegenJobResponse_entity = Lens.lens (\StartCodegenJobResponse' {entity} -> entity) (\s@StartCodegenJobResponse' {} a -> s {entity = a} :: StartCodegenJobResponse)

-- | The response's http status code.
startCodegenJobResponse_httpStatus :: Lens.Lens' StartCodegenJobResponse Prelude.Int
startCodegenJobResponse_httpStatus = Lens.lens (\StartCodegenJobResponse' {httpStatus} -> httpStatus) (\s@StartCodegenJobResponse' {} a -> s {httpStatus = a} :: StartCodegenJobResponse)

instance Prelude.NFData StartCodegenJobResponse where
  rnf StartCodegenJobResponse' {..} =
    Prelude.rnf entity
      `Prelude.seq` Prelude.rnf httpStatus
