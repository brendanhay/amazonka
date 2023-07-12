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
-- Module      : Amazonka.Personalize.DescribeSolution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a solution. For more information on solutions, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateSolution.html CreateSolution>.
module Amazonka.Personalize.DescribeSolution
  ( -- * Creating a Request
    DescribeSolution (..),
    newDescribeSolution,

    -- * Request Lenses
    describeSolution_solutionArn,

    -- * Destructuring the Response
    DescribeSolutionResponse (..),
    newDescribeSolutionResponse,

    -- * Response Lenses
    describeSolutionResponse_solution,
    describeSolutionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeSolution' smart constructor.
data DescribeSolution = DescribeSolution'
  { -- | The Amazon Resource Name (ARN) of the solution to describe.
    solutionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSolution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'solutionArn', 'describeSolution_solutionArn' - The Amazon Resource Name (ARN) of the solution to describe.
newDescribeSolution ::
  -- | 'solutionArn'
  Prelude.Text ->
  DescribeSolution
newDescribeSolution pSolutionArn_ =
  DescribeSolution' {solutionArn = pSolutionArn_}

-- | The Amazon Resource Name (ARN) of the solution to describe.
describeSolution_solutionArn :: Lens.Lens' DescribeSolution Prelude.Text
describeSolution_solutionArn = Lens.lens (\DescribeSolution' {solutionArn} -> solutionArn) (\s@DescribeSolution' {} a -> s {solutionArn = a} :: DescribeSolution)

instance Core.AWSRequest DescribeSolution where
  type
    AWSResponse DescribeSolution =
      DescribeSolutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSolutionResponse'
            Prelude.<$> (x Data..?> "solution")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSolution where
  hashWithSalt _salt DescribeSolution' {..} =
    _salt `Prelude.hashWithSalt` solutionArn

instance Prelude.NFData DescribeSolution where
  rnf DescribeSolution' {..} = Prelude.rnf solutionArn

instance Data.ToHeaders DescribeSolution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.DescribeSolution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeSolution where
  toJSON DescribeSolution' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("solutionArn" Data..= solutionArn)]
      )

instance Data.ToPath DescribeSolution where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeSolution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSolutionResponse' smart constructor.
data DescribeSolutionResponse = DescribeSolutionResponse'
  { -- | An object that describes the solution.
    solution :: Prelude.Maybe Solution,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSolutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'solution', 'describeSolutionResponse_solution' - An object that describes the solution.
--
-- 'httpStatus', 'describeSolutionResponse_httpStatus' - The response's http status code.
newDescribeSolutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSolutionResponse
newDescribeSolutionResponse pHttpStatus_ =
  DescribeSolutionResponse'
    { solution =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that describes the solution.
describeSolutionResponse_solution :: Lens.Lens' DescribeSolutionResponse (Prelude.Maybe Solution)
describeSolutionResponse_solution = Lens.lens (\DescribeSolutionResponse' {solution} -> solution) (\s@DescribeSolutionResponse' {} a -> s {solution = a} :: DescribeSolutionResponse)

-- | The response's http status code.
describeSolutionResponse_httpStatus :: Lens.Lens' DescribeSolutionResponse Prelude.Int
describeSolutionResponse_httpStatus = Lens.lens (\DescribeSolutionResponse' {httpStatus} -> httpStatus) (\s@DescribeSolutionResponse' {} a -> s {httpStatus = a} :: DescribeSolutionResponse)

instance Prelude.NFData DescribeSolutionResponse where
  rnf DescribeSolutionResponse' {..} =
    Prelude.rnf solution
      `Prelude.seq` Prelude.rnf httpStatus
