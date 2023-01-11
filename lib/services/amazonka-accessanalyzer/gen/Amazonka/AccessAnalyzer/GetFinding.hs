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
-- Module      : Amazonka.AccessAnalyzer.GetFinding
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified finding.
module Amazonka.AccessAnalyzer.GetFinding
  ( -- * Creating a Request
    GetFinding (..),
    newGetFinding,

    -- * Request Lenses
    getFinding_analyzerArn,
    getFinding_id,

    -- * Destructuring the Response
    GetFindingResponse (..),
    newGetFindingResponse,

    -- * Response Lenses
    getFindingResponse_finding,
    getFindingResponse_httpStatus,
  )
where

import Amazonka.AccessAnalyzer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Retrieves a finding.
--
-- /See:/ 'newGetFinding' smart constructor.
data GetFinding = GetFinding'
  { -- | The
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access-analyzer-getting-started.html#permission-resources ARN of the analyzer>
    -- that generated the finding.
    analyzerArn :: Prelude.Text,
    -- | The ID of the finding to retrieve.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFinding' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analyzerArn', 'getFinding_analyzerArn' - The
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access-analyzer-getting-started.html#permission-resources ARN of the analyzer>
-- that generated the finding.
--
-- 'id', 'getFinding_id' - The ID of the finding to retrieve.
newGetFinding ::
  -- | 'analyzerArn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  GetFinding
newGetFinding pAnalyzerArn_ pId_ =
  GetFinding' {analyzerArn = pAnalyzerArn_, id = pId_}

-- | The
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access-analyzer-getting-started.html#permission-resources ARN of the analyzer>
-- that generated the finding.
getFinding_analyzerArn :: Lens.Lens' GetFinding Prelude.Text
getFinding_analyzerArn = Lens.lens (\GetFinding' {analyzerArn} -> analyzerArn) (\s@GetFinding' {} a -> s {analyzerArn = a} :: GetFinding)

-- | The ID of the finding to retrieve.
getFinding_id :: Lens.Lens' GetFinding Prelude.Text
getFinding_id = Lens.lens (\GetFinding' {id} -> id) (\s@GetFinding' {} a -> s {id = a} :: GetFinding)

instance Core.AWSRequest GetFinding where
  type AWSResponse GetFinding = GetFindingResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFindingResponse'
            Prelude.<$> (x Data..?> "finding")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetFinding where
  hashWithSalt _salt GetFinding' {..} =
    _salt `Prelude.hashWithSalt` analyzerArn
      `Prelude.hashWithSalt` id

instance Prelude.NFData GetFinding where
  rnf GetFinding' {..} =
    Prelude.rnf analyzerArn
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders GetFinding where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetFinding where
  toPath GetFinding' {..} =
    Prelude.mconcat ["/finding/", Data.toBS id]

instance Data.ToQuery GetFinding where
  toQuery GetFinding' {..} =
    Prelude.mconcat ["analyzerArn" Data.=: analyzerArn]

-- | The response to the request.
--
-- /See:/ 'newGetFindingResponse' smart constructor.
data GetFindingResponse = GetFindingResponse'
  { -- | A @finding@ object that contains finding details.
    finding :: Prelude.Maybe Finding,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFindingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'finding', 'getFindingResponse_finding' - A @finding@ object that contains finding details.
--
-- 'httpStatus', 'getFindingResponse_httpStatus' - The response's http status code.
newGetFindingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFindingResponse
newGetFindingResponse pHttpStatus_ =
  GetFindingResponse'
    { finding = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A @finding@ object that contains finding details.
getFindingResponse_finding :: Lens.Lens' GetFindingResponse (Prelude.Maybe Finding)
getFindingResponse_finding = Lens.lens (\GetFindingResponse' {finding} -> finding) (\s@GetFindingResponse' {} a -> s {finding = a} :: GetFindingResponse)

-- | The response's http status code.
getFindingResponse_httpStatus :: Lens.Lens' GetFindingResponse Prelude.Int
getFindingResponse_httpStatus = Lens.lens (\GetFindingResponse' {httpStatus} -> httpStatus) (\s@GetFindingResponse' {} a -> s {httpStatus = a} :: GetFindingResponse)

instance Prelude.NFData GetFindingResponse where
  rnf GetFindingResponse' {..} =
    Prelude.rnf finding
      `Prelude.seq` Prelude.rnf httpStatus
