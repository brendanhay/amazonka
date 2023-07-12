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
-- Module      : Amazonka.ApplicationInsights.DescribeProblemObservations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the anomalies or errors associated with the problem.
module Amazonka.ApplicationInsights.DescribeProblemObservations
  ( -- * Creating a Request
    DescribeProblemObservations (..),
    newDescribeProblemObservations,

    -- * Request Lenses
    describeProblemObservations_problemId,

    -- * Destructuring the Response
    DescribeProblemObservationsResponse (..),
    newDescribeProblemObservationsResponse,

    -- * Response Lenses
    describeProblemObservationsResponse_relatedObservations,
    describeProblemObservationsResponse_httpStatus,
  )
where

import Amazonka.ApplicationInsights.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeProblemObservations' smart constructor.
data DescribeProblemObservations = DescribeProblemObservations'
  { -- | The ID of the problem.
    problemId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProblemObservations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'problemId', 'describeProblemObservations_problemId' - The ID of the problem.
newDescribeProblemObservations ::
  -- | 'problemId'
  Prelude.Text ->
  DescribeProblemObservations
newDescribeProblemObservations pProblemId_ =
  DescribeProblemObservations'
    { problemId =
        pProblemId_
    }

-- | The ID of the problem.
describeProblemObservations_problemId :: Lens.Lens' DescribeProblemObservations Prelude.Text
describeProblemObservations_problemId = Lens.lens (\DescribeProblemObservations' {problemId} -> problemId) (\s@DescribeProblemObservations' {} a -> s {problemId = a} :: DescribeProblemObservations)

instance Core.AWSRequest DescribeProblemObservations where
  type
    AWSResponse DescribeProblemObservations =
      DescribeProblemObservationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProblemObservationsResponse'
            Prelude.<$> (x Data..?> "RelatedObservations")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeProblemObservations where
  hashWithSalt _salt DescribeProblemObservations' {..} =
    _salt `Prelude.hashWithSalt` problemId

instance Prelude.NFData DescribeProblemObservations where
  rnf DescribeProblemObservations' {..} =
    Prelude.rnf problemId

instance Data.ToHeaders DescribeProblemObservations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "EC2WindowsBarleyService.DescribeProblemObservations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeProblemObservations where
  toJSON DescribeProblemObservations' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ProblemId" Data..= problemId)]
      )

instance Data.ToPath DescribeProblemObservations where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeProblemObservations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeProblemObservationsResponse' smart constructor.
data DescribeProblemObservationsResponse = DescribeProblemObservationsResponse'
  { -- | Observations related to the problem.
    relatedObservations :: Prelude.Maybe RelatedObservations,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProblemObservationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relatedObservations', 'describeProblemObservationsResponse_relatedObservations' - Observations related to the problem.
--
-- 'httpStatus', 'describeProblemObservationsResponse_httpStatus' - The response's http status code.
newDescribeProblemObservationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeProblemObservationsResponse
newDescribeProblemObservationsResponse pHttpStatus_ =
  DescribeProblemObservationsResponse'
    { relatedObservations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Observations related to the problem.
describeProblemObservationsResponse_relatedObservations :: Lens.Lens' DescribeProblemObservationsResponse (Prelude.Maybe RelatedObservations)
describeProblemObservationsResponse_relatedObservations = Lens.lens (\DescribeProblemObservationsResponse' {relatedObservations} -> relatedObservations) (\s@DescribeProblemObservationsResponse' {} a -> s {relatedObservations = a} :: DescribeProblemObservationsResponse)

-- | The response's http status code.
describeProblemObservationsResponse_httpStatus :: Lens.Lens' DescribeProblemObservationsResponse Prelude.Int
describeProblemObservationsResponse_httpStatus = Lens.lens (\DescribeProblemObservationsResponse' {httpStatus} -> httpStatus) (\s@DescribeProblemObservationsResponse' {} a -> s {httpStatus = a} :: DescribeProblemObservationsResponse)

instance
  Prelude.NFData
    DescribeProblemObservationsResponse
  where
  rnf DescribeProblemObservationsResponse' {..} =
    Prelude.rnf relatedObservations
      `Prelude.seq` Prelude.rnf httpStatus
