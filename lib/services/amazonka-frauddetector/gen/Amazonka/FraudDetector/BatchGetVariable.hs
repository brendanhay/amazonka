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
-- Module      : Amazonka.FraudDetector.BatchGetVariable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a batch of variables.
module Amazonka.FraudDetector.BatchGetVariable
  ( -- * Creating a Request
    BatchGetVariable (..),
    newBatchGetVariable,

    -- * Request Lenses
    batchGetVariable_names,

    -- * Destructuring the Response
    BatchGetVariableResponse (..),
    newBatchGetVariableResponse,

    -- * Response Lenses
    batchGetVariableResponse_errors,
    batchGetVariableResponse_variables,
    batchGetVariableResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetVariable' smart constructor.
data BatchGetVariable = BatchGetVariable'
  { -- | The list of variable names to get.
    names :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetVariable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'names', 'batchGetVariable_names' - The list of variable names to get.
newBatchGetVariable ::
  -- | 'names'
  Prelude.NonEmpty Prelude.Text ->
  BatchGetVariable
newBatchGetVariable pNames_ =
  BatchGetVariable'
    { names =
        Lens.coerced Lens.# pNames_
    }

-- | The list of variable names to get.
batchGetVariable_names :: Lens.Lens' BatchGetVariable (Prelude.NonEmpty Prelude.Text)
batchGetVariable_names = Lens.lens (\BatchGetVariable' {names} -> names) (\s@BatchGetVariable' {} a -> s {names = a} :: BatchGetVariable) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetVariable where
  type
    AWSResponse BatchGetVariable =
      BatchGetVariableResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetVariableResponse'
            Prelude.<$> (x Data..?> "errors" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "variables" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetVariable where
  hashWithSalt _salt BatchGetVariable' {..} =
    _salt `Prelude.hashWithSalt` names

instance Prelude.NFData BatchGetVariable where
  rnf BatchGetVariable' {..} = Prelude.rnf names

instance Data.ToHeaders BatchGetVariable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.BatchGetVariable" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetVariable where
  toJSON BatchGetVariable' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("names" Data..= names)]
      )

instance Data.ToPath BatchGetVariable where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchGetVariable where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetVariableResponse' smart constructor.
data BatchGetVariableResponse = BatchGetVariableResponse'
  { -- | The errors from the request.
    errors :: Prelude.Maybe [BatchGetVariableError],
    -- | The returned variables.
    variables :: Prelude.Maybe [Variable],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetVariableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errors', 'batchGetVariableResponse_errors' - The errors from the request.
--
-- 'variables', 'batchGetVariableResponse_variables' - The returned variables.
--
-- 'httpStatus', 'batchGetVariableResponse_httpStatus' - The response's http status code.
newBatchGetVariableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetVariableResponse
newBatchGetVariableResponse pHttpStatus_ =
  BatchGetVariableResponse'
    { errors = Prelude.Nothing,
      variables = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The errors from the request.
batchGetVariableResponse_errors :: Lens.Lens' BatchGetVariableResponse (Prelude.Maybe [BatchGetVariableError])
batchGetVariableResponse_errors = Lens.lens (\BatchGetVariableResponse' {errors} -> errors) (\s@BatchGetVariableResponse' {} a -> s {errors = a} :: BatchGetVariableResponse) Prelude.. Lens.mapping Lens.coerced

-- | The returned variables.
batchGetVariableResponse_variables :: Lens.Lens' BatchGetVariableResponse (Prelude.Maybe [Variable])
batchGetVariableResponse_variables = Lens.lens (\BatchGetVariableResponse' {variables} -> variables) (\s@BatchGetVariableResponse' {} a -> s {variables = a} :: BatchGetVariableResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetVariableResponse_httpStatus :: Lens.Lens' BatchGetVariableResponse Prelude.Int
batchGetVariableResponse_httpStatus = Lens.lens (\BatchGetVariableResponse' {httpStatus} -> httpStatus) (\s@BatchGetVariableResponse' {} a -> s {httpStatus = a} :: BatchGetVariableResponse)

instance Prelude.NFData BatchGetVariableResponse where
  rnf BatchGetVariableResponse' {..} =
    Prelude.rnf errors
      `Prelude.seq` Prelude.rnf variables
      `Prelude.seq` Prelude.rnf httpStatus
