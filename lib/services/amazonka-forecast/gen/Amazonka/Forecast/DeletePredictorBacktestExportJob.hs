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
-- Module      : Amazonka.Forecast.DeletePredictorBacktestExportJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a predictor backtest export job.
module Amazonka.Forecast.DeletePredictorBacktestExportJob
  ( -- * Creating a Request
    DeletePredictorBacktestExportJob (..),
    newDeletePredictorBacktestExportJob,

    -- * Request Lenses
    deletePredictorBacktestExportJob_predictorBacktestExportJobArn,

    -- * Destructuring the Response
    DeletePredictorBacktestExportJobResponse (..),
    newDeletePredictorBacktestExportJobResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePredictorBacktestExportJob' smart constructor.
data DeletePredictorBacktestExportJob = DeletePredictorBacktestExportJob'
  { -- | The Amazon Resource Name (ARN) of the predictor backtest export job to
    -- delete.
    predictorBacktestExportJobArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePredictorBacktestExportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'predictorBacktestExportJobArn', 'deletePredictorBacktestExportJob_predictorBacktestExportJobArn' - The Amazon Resource Name (ARN) of the predictor backtest export job to
-- delete.
newDeletePredictorBacktestExportJob ::
  -- | 'predictorBacktestExportJobArn'
  Prelude.Text ->
  DeletePredictorBacktestExportJob
newDeletePredictorBacktestExportJob
  pPredictorBacktestExportJobArn_ =
    DeletePredictorBacktestExportJob'
      { predictorBacktestExportJobArn =
          pPredictorBacktestExportJobArn_
      }

-- | The Amazon Resource Name (ARN) of the predictor backtest export job to
-- delete.
deletePredictorBacktestExportJob_predictorBacktestExportJobArn :: Lens.Lens' DeletePredictorBacktestExportJob Prelude.Text
deletePredictorBacktestExportJob_predictorBacktestExportJobArn = Lens.lens (\DeletePredictorBacktestExportJob' {predictorBacktestExportJobArn} -> predictorBacktestExportJobArn) (\s@DeletePredictorBacktestExportJob' {} a -> s {predictorBacktestExportJobArn = a} :: DeletePredictorBacktestExportJob)

instance
  Core.AWSRequest
    DeletePredictorBacktestExportJob
  where
  type
    AWSResponse DeletePredictorBacktestExportJob =
      DeletePredictorBacktestExportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeletePredictorBacktestExportJobResponse'

instance
  Prelude.Hashable
    DeletePredictorBacktestExportJob
  where
  hashWithSalt
    _salt
    DeletePredictorBacktestExportJob' {..} =
      _salt
        `Prelude.hashWithSalt` predictorBacktestExportJobArn

instance
  Prelude.NFData
    DeletePredictorBacktestExportJob
  where
  rnf DeletePredictorBacktestExportJob' {..} =
    Prelude.rnf predictorBacktestExportJobArn

instance
  Core.ToHeaders
    DeletePredictorBacktestExportJob
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonForecast.DeletePredictorBacktestExportJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeletePredictorBacktestExportJob where
  toJSON DeletePredictorBacktestExportJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "PredictorBacktestExportJobArn"
                  Core..= predictorBacktestExportJobArn
              )
          ]
      )

instance Core.ToPath DeletePredictorBacktestExportJob where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DeletePredictorBacktestExportJob
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePredictorBacktestExportJobResponse' smart constructor.
data DeletePredictorBacktestExportJobResponse = DeletePredictorBacktestExportJobResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePredictorBacktestExportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeletePredictorBacktestExportJobResponse ::
  DeletePredictorBacktestExportJobResponse
newDeletePredictorBacktestExportJobResponse =
  DeletePredictorBacktestExportJobResponse'

instance
  Prelude.NFData
    DeletePredictorBacktestExportJobResponse
  where
  rnf _ = ()
