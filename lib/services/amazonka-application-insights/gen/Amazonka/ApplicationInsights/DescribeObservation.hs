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
-- Module      : Amazonka.ApplicationInsights.DescribeObservation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an anomaly or error with the application.
module Amazonka.ApplicationInsights.DescribeObservation
  ( -- * Creating a Request
    DescribeObservation (..),
    newDescribeObservation,

    -- * Request Lenses
    describeObservation_observationId,

    -- * Destructuring the Response
    DescribeObservationResponse (..),
    newDescribeObservationResponse,

    -- * Response Lenses
    describeObservationResponse_observation,
    describeObservationResponse_httpStatus,
  )
where

import Amazonka.ApplicationInsights.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeObservation' smart constructor.
data DescribeObservation = DescribeObservation'
  { -- | The ID of the observation.
    observationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeObservation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'observationId', 'describeObservation_observationId' - The ID of the observation.
newDescribeObservation ::
  -- | 'observationId'
  Prelude.Text ->
  DescribeObservation
newDescribeObservation pObservationId_ =
  DescribeObservation'
    { observationId =
        pObservationId_
    }

-- | The ID of the observation.
describeObservation_observationId :: Lens.Lens' DescribeObservation Prelude.Text
describeObservation_observationId = Lens.lens (\DescribeObservation' {observationId} -> observationId) (\s@DescribeObservation' {} a -> s {observationId = a} :: DescribeObservation)

instance Core.AWSRequest DescribeObservation where
  type
    AWSResponse DescribeObservation =
      DescribeObservationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeObservationResponse'
            Prelude.<$> (x Data..?> "Observation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeObservation where
  hashWithSalt _salt DescribeObservation' {..} =
    _salt `Prelude.hashWithSalt` observationId

instance Prelude.NFData DescribeObservation where
  rnf DescribeObservation' {..} =
    Prelude.rnf observationId

instance Data.ToHeaders DescribeObservation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "EC2WindowsBarleyService.DescribeObservation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeObservation where
  toJSON DescribeObservation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ObservationId" Data..= observationId)
          ]
      )

instance Data.ToPath DescribeObservation where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeObservation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeObservationResponse' smart constructor.
data DescribeObservationResponse = DescribeObservationResponse'
  { -- | Information about the observation.
    observation :: Prelude.Maybe Observation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeObservationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'observation', 'describeObservationResponse_observation' - Information about the observation.
--
-- 'httpStatus', 'describeObservationResponse_httpStatus' - The response's http status code.
newDescribeObservationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeObservationResponse
newDescribeObservationResponse pHttpStatus_ =
  DescribeObservationResponse'
    { observation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the observation.
describeObservationResponse_observation :: Lens.Lens' DescribeObservationResponse (Prelude.Maybe Observation)
describeObservationResponse_observation = Lens.lens (\DescribeObservationResponse' {observation} -> observation) (\s@DescribeObservationResponse' {} a -> s {observation = a} :: DescribeObservationResponse)

-- | The response's http status code.
describeObservationResponse_httpStatus :: Lens.Lens' DescribeObservationResponse Prelude.Int
describeObservationResponse_httpStatus = Lens.lens (\DescribeObservationResponse' {httpStatus} -> httpStatus) (\s@DescribeObservationResponse' {} a -> s {httpStatus = a} :: DescribeObservationResponse)

instance Prelude.NFData DescribeObservationResponse where
  rnf DescribeObservationResponse' {..} =
    Prelude.rnf observation
      `Prelude.seq` Prelude.rnf httpStatus
