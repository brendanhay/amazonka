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
-- Module      : Amazonka.AuditManager.GetAssessmentFramework
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a framework from Audit Manager.
module Amazonka.AuditManager.GetAssessmentFramework
  ( -- * Creating a Request
    GetAssessmentFramework (..),
    newGetAssessmentFramework,

    -- * Request Lenses
    getAssessmentFramework_frameworkId,

    -- * Destructuring the Response
    GetAssessmentFrameworkResponse (..),
    newGetAssessmentFrameworkResponse,

    -- * Response Lenses
    getAssessmentFrameworkResponse_framework,
    getAssessmentFrameworkResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAssessmentFramework' smart constructor.
data GetAssessmentFramework = GetAssessmentFramework'
  { -- | The identifier for the framework.
    frameworkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAssessmentFramework' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'frameworkId', 'getAssessmentFramework_frameworkId' - The identifier for the framework.
newGetAssessmentFramework ::
  -- | 'frameworkId'
  Prelude.Text ->
  GetAssessmentFramework
newGetAssessmentFramework pFrameworkId_ =
  GetAssessmentFramework'
    { frameworkId =
        pFrameworkId_
    }

-- | The identifier for the framework.
getAssessmentFramework_frameworkId :: Lens.Lens' GetAssessmentFramework Prelude.Text
getAssessmentFramework_frameworkId = Lens.lens (\GetAssessmentFramework' {frameworkId} -> frameworkId) (\s@GetAssessmentFramework' {} a -> s {frameworkId = a} :: GetAssessmentFramework)

instance Core.AWSRequest GetAssessmentFramework where
  type
    AWSResponse GetAssessmentFramework =
      GetAssessmentFrameworkResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAssessmentFrameworkResponse'
            Prelude.<$> (x Data..?> "framework")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAssessmentFramework where
  hashWithSalt _salt GetAssessmentFramework' {..} =
    _salt `Prelude.hashWithSalt` frameworkId

instance Prelude.NFData GetAssessmentFramework where
  rnf GetAssessmentFramework' {..} =
    Prelude.rnf frameworkId

instance Data.ToHeaders GetAssessmentFramework where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetAssessmentFramework where
  toPath GetAssessmentFramework' {..} =
    Prelude.mconcat
      ["/assessmentFrameworks/", Data.toBS frameworkId]

instance Data.ToQuery GetAssessmentFramework where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAssessmentFrameworkResponse' smart constructor.
data GetAssessmentFrameworkResponse = GetAssessmentFrameworkResponse'
  { -- | The framework that the @GetAssessmentFramework@ API returned.
    framework :: Prelude.Maybe Framework,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAssessmentFrameworkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'framework', 'getAssessmentFrameworkResponse_framework' - The framework that the @GetAssessmentFramework@ API returned.
--
-- 'httpStatus', 'getAssessmentFrameworkResponse_httpStatus' - The response's http status code.
newGetAssessmentFrameworkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAssessmentFrameworkResponse
newGetAssessmentFrameworkResponse pHttpStatus_ =
  GetAssessmentFrameworkResponse'
    { framework =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The framework that the @GetAssessmentFramework@ API returned.
getAssessmentFrameworkResponse_framework :: Lens.Lens' GetAssessmentFrameworkResponse (Prelude.Maybe Framework)
getAssessmentFrameworkResponse_framework = Lens.lens (\GetAssessmentFrameworkResponse' {framework} -> framework) (\s@GetAssessmentFrameworkResponse' {} a -> s {framework = a} :: GetAssessmentFrameworkResponse)

-- | The response's http status code.
getAssessmentFrameworkResponse_httpStatus :: Lens.Lens' GetAssessmentFrameworkResponse Prelude.Int
getAssessmentFrameworkResponse_httpStatus = Lens.lens (\GetAssessmentFrameworkResponse' {httpStatus} -> httpStatus) (\s@GetAssessmentFrameworkResponse' {} a -> s {httpStatus = a} :: GetAssessmentFrameworkResponse)

instance
  Prelude.NFData
    GetAssessmentFrameworkResponse
  where
  rnf GetAssessmentFrameworkResponse' {..} =
    Prelude.rnf framework `Prelude.seq`
      Prelude.rnf httpStatus
