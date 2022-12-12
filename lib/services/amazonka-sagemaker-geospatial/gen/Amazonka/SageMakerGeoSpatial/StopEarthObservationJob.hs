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
-- Module      : Amazonka.SageMakerGeoSpatial.StopEarthObservationJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to stop an existing earth observation job.
module Amazonka.SageMakerGeoSpatial.StopEarthObservationJob
  ( -- * Creating a Request
    StopEarthObservationJob (..),
    newStopEarthObservationJob,

    -- * Request Lenses
    stopEarthObservationJob_arn,

    -- * Destructuring the Response
    StopEarthObservationJobResponse (..),
    newStopEarthObservationJobResponse,

    -- * Response Lenses
    stopEarthObservationJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMakerGeoSpatial.Types

-- | /See:/ 'newStopEarthObservationJob' smart constructor.
data StopEarthObservationJob = StopEarthObservationJob'
  { -- | The Amazon Resource Name (ARN) of the Earth Observation job being
    -- stopped.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopEarthObservationJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'stopEarthObservationJob_arn' - The Amazon Resource Name (ARN) of the Earth Observation job being
-- stopped.
newStopEarthObservationJob ::
  -- | 'arn'
  Prelude.Text ->
  StopEarthObservationJob
newStopEarthObservationJob pArn_ =
  StopEarthObservationJob' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the Earth Observation job being
-- stopped.
stopEarthObservationJob_arn :: Lens.Lens' StopEarthObservationJob Prelude.Text
stopEarthObservationJob_arn = Lens.lens (\StopEarthObservationJob' {arn} -> arn) (\s@StopEarthObservationJob' {} a -> s {arn = a} :: StopEarthObservationJob)

instance Core.AWSRequest StopEarthObservationJob where
  type
    AWSResponse StopEarthObservationJob =
      StopEarthObservationJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopEarthObservationJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopEarthObservationJob where
  hashWithSalt _salt StopEarthObservationJob' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData StopEarthObservationJob where
  rnf StopEarthObservationJob' {..} = Prelude.rnf arn

instance Data.ToHeaders StopEarthObservationJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopEarthObservationJob where
  toJSON StopEarthObservationJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Arn" Data..= arn)]
      )

instance Data.ToPath StopEarthObservationJob where
  toPath = Prelude.const "/earth-observation-jobs/stop"

instance Data.ToQuery StopEarthObservationJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopEarthObservationJobResponse' smart constructor.
data StopEarthObservationJobResponse = StopEarthObservationJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopEarthObservationJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopEarthObservationJobResponse_httpStatus' - The response's http status code.
newStopEarthObservationJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopEarthObservationJobResponse
newStopEarthObservationJobResponse pHttpStatus_ =
  StopEarthObservationJobResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
stopEarthObservationJobResponse_httpStatus :: Lens.Lens' StopEarthObservationJobResponse Prelude.Int
stopEarthObservationJobResponse_httpStatus = Lens.lens (\StopEarthObservationJobResponse' {httpStatus} -> httpStatus) (\s@StopEarthObservationJobResponse' {} a -> s {httpStatus = a} :: StopEarthObservationJobResponse)

instance
  Prelude.NFData
    StopEarthObservationJobResponse
  where
  rnf StopEarthObservationJobResponse' {..} =
    Prelude.rnf httpStatus
