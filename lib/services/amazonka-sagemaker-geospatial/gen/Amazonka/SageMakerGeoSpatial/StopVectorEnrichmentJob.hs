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
-- Module      : Amazonka.SageMakerGeoSpatial.StopVectorEnrichmentJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the Vector Enrichment job for a given job ARN.
module Amazonka.SageMakerGeoSpatial.StopVectorEnrichmentJob
  ( -- * Creating a Request
    StopVectorEnrichmentJob (..),
    newStopVectorEnrichmentJob,

    -- * Request Lenses
    stopVectorEnrichmentJob_arn,

    -- * Destructuring the Response
    StopVectorEnrichmentJobResponse (..),
    newStopVectorEnrichmentJobResponse,

    -- * Response Lenses
    stopVectorEnrichmentJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMakerGeoSpatial.Types

-- | /See:/ 'newStopVectorEnrichmentJob' smart constructor.
data StopVectorEnrichmentJob = StopVectorEnrichmentJob'
  { -- | The Amazon Resource Name (ARN) of the Vector Enrichment job.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopVectorEnrichmentJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'stopVectorEnrichmentJob_arn' - The Amazon Resource Name (ARN) of the Vector Enrichment job.
newStopVectorEnrichmentJob ::
  -- | 'arn'
  Prelude.Text ->
  StopVectorEnrichmentJob
newStopVectorEnrichmentJob pArn_ =
  StopVectorEnrichmentJob' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the Vector Enrichment job.
stopVectorEnrichmentJob_arn :: Lens.Lens' StopVectorEnrichmentJob Prelude.Text
stopVectorEnrichmentJob_arn = Lens.lens (\StopVectorEnrichmentJob' {arn} -> arn) (\s@StopVectorEnrichmentJob' {} a -> s {arn = a} :: StopVectorEnrichmentJob)

instance Core.AWSRequest StopVectorEnrichmentJob where
  type
    AWSResponse StopVectorEnrichmentJob =
      StopVectorEnrichmentJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopVectorEnrichmentJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopVectorEnrichmentJob where
  hashWithSalt _salt StopVectorEnrichmentJob' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData StopVectorEnrichmentJob where
  rnf StopVectorEnrichmentJob' {..} = Prelude.rnf arn

instance Data.ToHeaders StopVectorEnrichmentJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopVectorEnrichmentJob where
  toJSON StopVectorEnrichmentJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Arn" Data..= arn)]
      )

instance Data.ToPath StopVectorEnrichmentJob where
  toPath = Prelude.const "/vector-enrichment-jobs/stop"

instance Data.ToQuery StopVectorEnrichmentJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopVectorEnrichmentJobResponse' smart constructor.
data StopVectorEnrichmentJobResponse = StopVectorEnrichmentJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopVectorEnrichmentJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopVectorEnrichmentJobResponse_httpStatus' - The response's http status code.
newStopVectorEnrichmentJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopVectorEnrichmentJobResponse
newStopVectorEnrichmentJobResponse pHttpStatus_ =
  StopVectorEnrichmentJobResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
stopVectorEnrichmentJobResponse_httpStatus :: Lens.Lens' StopVectorEnrichmentJobResponse Prelude.Int
stopVectorEnrichmentJobResponse_httpStatus = Lens.lens (\StopVectorEnrichmentJobResponse' {httpStatus} -> httpStatus) (\s@StopVectorEnrichmentJobResponse' {} a -> s {httpStatus = a} :: StopVectorEnrichmentJobResponse)

instance
  Prelude.NFData
    StopVectorEnrichmentJobResponse
  where
  rnf StopVectorEnrichmentJobResponse' {..} =
    Prelude.rnf httpStatus
