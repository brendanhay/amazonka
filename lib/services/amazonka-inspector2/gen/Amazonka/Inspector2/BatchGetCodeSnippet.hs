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
-- Module      : Amazonka.Inspector2.BatchGetCodeSnippet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves code snippets from findings that Amazon Inspector detected
-- code vulnerabilities in.
module Amazonka.Inspector2.BatchGetCodeSnippet
  ( -- * Creating a Request
    BatchGetCodeSnippet (..),
    newBatchGetCodeSnippet,

    -- * Request Lenses
    batchGetCodeSnippet_findingArns,

    -- * Destructuring the Response
    BatchGetCodeSnippetResponse (..),
    newBatchGetCodeSnippetResponse,

    -- * Response Lenses
    batchGetCodeSnippetResponse_codeSnippetResults,
    batchGetCodeSnippetResponse_errors,
    batchGetCodeSnippetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetCodeSnippet' smart constructor.
data BatchGetCodeSnippet = BatchGetCodeSnippet'
  { -- | An array of finding ARNs for the findings you want to retrieve code
    -- snippets from.
    findingArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetCodeSnippet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'findingArns', 'batchGetCodeSnippet_findingArns' - An array of finding ARNs for the findings you want to retrieve code
-- snippets from.
newBatchGetCodeSnippet ::
  -- | 'findingArns'
  Prelude.NonEmpty Prelude.Text ->
  BatchGetCodeSnippet
newBatchGetCodeSnippet pFindingArns_ =
  BatchGetCodeSnippet'
    { findingArns =
        Lens.coerced Lens.# pFindingArns_
    }

-- | An array of finding ARNs for the findings you want to retrieve code
-- snippets from.
batchGetCodeSnippet_findingArns :: Lens.Lens' BatchGetCodeSnippet (Prelude.NonEmpty Prelude.Text)
batchGetCodeSnippet_findingArns = Lens.lens (\BatchGetCodeSnippet' {findingArns} -> findingArns) (\s@BatchGetCodeSnippet' {} a -> s {findingArns = a} :: BatchGetCodeSnippet) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetCodeSnippet where
  type
    AWSResponse BatchGetCodeSnippet =
      BatchGetCodeSnippetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetCodeSnippetResponse'
            Prelude.<$> ( x
                            Data..?> "codeSnippetResults"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "errors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetCodeSnippet where
  hashWithSalt _salt BatchGetCodeSnippet' {..} =
    _salt `Prelude.hashWithSalt` findingArns

instance Prelude.NFData BatchGetCodeSnippet where
  rnf BatchGetCodeSnippet' {..} =
    Prelude.rnf findingArns

instance Data.ToHeaders BatchGetCodeSnippet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetCodeSnippet where
  toJSON BatchGetCodeSnippet' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("findingArns" Data..= findingArns)]
      )

instance Data.ToPath BatchGetCodeSnippet where
  toPath = Prelude.const "/codesnippet/batchget"

instance Data.ToQuery BatchGetCodeSnippet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetCodeSnippetResponse' smart constructor.
data BatchGetCodeSnippetResponse = BatchGetCodeSnippetResponse'
  { -- | The retrieved code snippets associated with the provided finding ARNs.
    codeSnippetResults :: Prelude.Maybe [CodeSnippetResult],
    -- | Any errors Amazon Inspector encountered while trying to retrieve the
    -- requested code snippets.
    errors :: Prelude.Maybe [CodeSnippetError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetCodeSnippetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeSnippetResults', 'batchGetCodeSnippetResponse_codeSnippetResults' - The retrieved code snippets associated with the provided finding ARNs.
--
-- 'errors', 'batchGetCodeSnippetResponse_errors' - Any errors Amazon Inspector encountered while trying to retrieve the
-- requested code snippets.
--
-- 'httpStatus', 'batchGetCodeSnippetResponse_httpStatus' - The response's http status code.
newBatchGetCodeSnippetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetCodeSnippetResponse
newBatchGetCodeSnippetResponse pHttpStatus_ =
  BatchGetCodeSnippetResponse'
    { codeSnippetResults =
        Prelude.Nothing,
      errors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The retrieved code snippets associated with the provided finding ARNs.
batchGetCodeSnippetResponse_codeSnippetResults :: Lens.Lens' BatchGetCodeSnippetResponse (Prelude.Maybe [CodeSnippetResult])
batchGetCodeSnippetResponse_codeSnippetResults = Lens.lens (\BatchGetCodeSnippetResponse' {codeSnippetResults} -> codeSnippetResults) (\s@BatchGetCodeSnippetResponse' {} a -> s {codeSnippetResults = a} :: BatchGetCodeSnippetResponse) Prelude.. Lens.mapping Lens.coerced

-- | Any errors Amazon Inspector encountered while trying to retrieve the
-- requested code snippets.
batchGetCodeSnippetResponse_errors :: Lens.Lens' BatchGetCodeSnippetResponse (Prelude.Maybe [CodeSnippetError])
batchGetCodeSnippetResponse_errors = Lens.lens (\BatchGetCodeSnippetResponse' {errors} -> errors) (\s@BatchGetCodeSnippetResponse' {} a -> s {errors = a} :: BatchGetCodeSnippetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetCodeSnippetResponse_httpStatus :: Lens.Lens' BatchGetCodeSnippetResponse Prelude.Int
batchGetCodeSnippetResponse_httpStatus = Lens.lens (\BatchGetCodeSnippetResponse' {httpStatus} -> httpStatus) (\s@BatchGetCodeSnippetResponse' {} a -> s {httpStatus = a} :: BatchGetCodeSnippetResponse)

instance Prelude.NFData BatchGetCodeSnippetResponse where
  rnf BatchGetCodeSnippetResponse' {..} =
    Prelude.rnf codeSnippetResults
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf httpStatus
