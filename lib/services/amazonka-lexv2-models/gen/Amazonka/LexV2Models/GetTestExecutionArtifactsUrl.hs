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
-- Module      : Amazonka.LexV2Models.GetTestExecutionArtifactsUrl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The pre-signed Amazon S3 URL to download the test execution result
-- artifacts.
module Amazonka.LexV2Models.GetTestExecutionArtifactsUrl
  ( -- * Creating a Request
    GetTestExecutionArtifactsUrl (..),
    newGetTestExecutionArtifactsUrl,

    -- * Request Lenses
    getTestExecutionArtifactsUrl_testExecutionId,

    -- * Destructuring the Response
    GetTestExecutionArtifactsUrlResponse (..),
    newGetTestExecutionArtifactsUrlResponse,

    -- * Response Lenses
    getTestExecutionArtifactsUrlResponse_downloadArtifactsUrl,
    getTestExecutionArtifactsUrlResponse_testExecutionId,
    getTestExecutionArtifactsUrlResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetTestExecutionArtifactsUrl' smart constructor.
data GetTestExecutionArtifactsUrl = GetTestExecutionArtifactsUrl'
  { -- | The unique identifier of the completed test execution.
    testExecutionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTestExecutionArtifactsUrl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'testExecutionId', 'getTestExecutionArtifactsUrl_testExecutionId' - The unique identifier of the completed test execution.
newGetTestExecutionArtifactsUrl ::
  -- | 'testExecutionId'
  Prelude.Text ->
  GetTestExecutionArtifactsUrl
newGetTestExecutionArtifactsUrl pTestExecutionId_ =
  GetTestExecutionArtifactsUrl'
    { testExecutionId =
        pTestExecutionId_
    }

-- | The unique identifier of the completed test execution.
getTestExecutionArtifactsUrl_testExecutionId :: Lens.Lens' GetTestExecutionArtifactsUrl Prelude.Text
getTestExecutionArtifactsUrl_testExecutionId = Lens.lens (\GetTestExecutionArtifactsUrl' {testExecutionId} -> testExecutionId) (\s@GetTestExecutionArtifactsUrl' {} a -> s {testExecutionId = a} :: GetTestExecutionArtifactsUrl)

instance Core.AWSRequest GetTestExecutionArtifactsUrl where
  type
    AWSResponse GetTestExecutionArtifactsUrl =
      GetTestExecutionArtifactsUrlResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTestExecutionArtifactsUrlResponse'
            Prelude.<$> (x Data..?> "downloadArtifactsUrl")
            Prelude.<*> (x Data..?> "testExecutionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetTestExecutionArtifactsUrl
  where
  hashWithSalt _salt GetTestExecutionArtifactsUrl' {..} =
    _salt `Prelude.hashWithSalt` testExecutionId

instance Prelude.NFData GetTestExecutionArtifactsUrl where
  rnf GetTestExecutionArtifactsUrl' {..} =
    Prelude.rnf testExecutionId

instance Data.ToHeaders GetTestExecutionArtifactsUrl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetTestExecutionArtifactsUrl where
  toPath GetTestExecutionArtifactsUrl' {..} =
    Prelude.mconcat
      [ "/testexecutions/",
        Data.toBS testExecutionId,
        "/artifacturl"
      ]

instance Data.ToQuery GetTestExecutionArtifactsUrl where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTestExecutionArtifactsUrlResponse' smart constructor.
data GetTestExecutionArtifactsUrlResponse = GetTestExecutionArtifactsUrlResponse'
  { -- | The pre-signed Amazon S3 URL to download completed test execution.
    downloadArtifactsUrl :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the completed test execution.
    testExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTestExecutionArtifactsUrlResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'downloadArtifactsUrl', 'getTestExecutionArtifactsUrlResponse_downloadArtifactsUrl' - The pre-signed Amazon S3 URL to download completed test execution.
--
-- 'testExecutionId', 'getTestExecutionArtifactsUrlResponse_testExecutionId' - The unique identifier of the completed test execution.
--
-- 'httpStatus', 'getTestExecutionArtifactsUrlResponse_httpStatus' - The response's http status code.
newGetTestExecutionArtifactsUrlResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTestExecutionArtifactsUrlResponse
newGetTestExecutionArtifactsUrlResponse pHttpStatus_ =
  GetTestExecutionArtifactsUrlResponse'
    { downloadArtifactsUrl =
        Prelude.Nothing,
      testExecutionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pre-signed Amazon S3 URL to download completed test execution.
getTestExecutionArtifactsUrlResponse_downloadArtifactsUrl :: Lens.Lens' GetTestExecutionArtifactsUrlResponse (Prelude.Maybe Prelude.Text)
getTestExecutionArtifactsUrlResponse_downloadArtifactsUrl = Lens.lens (\GetTestExecutionArtifactsUrlResponse' {downloadArtifactsUrl} -> downloadArtifactsUrl) (\s@GetTestExecutionArtifactsUrlResponse' {} a -> s {downloadArtifactsUrl = a} :: GetTestExecutionArtifactsUrlResponse)

-- | The unique identifier of the completed test execution.
getTestExecutionArtifactsUrlResponse_testExecutionId :: Lens.Lens' GetTestExecutionArtifactsUrlResponse (Prelude.Maybe Prelude.Text)
getTestExecutionArtifactsUrlResponse_testExecutionId = Lens.lens (\GetTestExecutionArtifactsUrlResponse' {testExecutionId} -> testExecutionId) (\s@GetTestExecutionArtifactsUrlResponse' {} a -> s {testExecutionId = a} :: GetTestExecutionArtifactsUrlResponse)

-- | The response's http status code.
getTestExecutionArtifactsUrlResponse_httpStatus :: Lens.Lens' GetTestExecutionArtifactsUrlResponse Prelude.Int
getTestExecutionArtifactsUrlResponse_httpStatus = Lens.lens (\GetTestExecutionArtifactsUrlResponse' {httpStatus} -> httpStatus) (\s@GetTestExecutionArtifactsUrlResponse' {} a -> s {httpStatus = a} :: GetTestExecutionArtifactsUrlResponse)

instance
  Prelude.NFData
    GetTestExecutionArtifactsUrlResponse
  where
  rnf GetTestExecutionArtifactsUrlResponse' {..} =
    Prelude.rnf downloadArtifactsUrl
      `Prelude.seq` Prelude.rnf testExecutionId
      `Prelude.seq` Prelude.rnf httpStatus
