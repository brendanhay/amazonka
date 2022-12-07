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
-- Module      : Amazonka.Forecast.StopResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a resource.
--
-- The resource undergoes the following states: @CREATE_STOPPING@ and
-- @CREATE_STOPPED@. You cannot resume a resource once it has been stopped.
--
-- This operation can be applied to the following resources (and their
-- corresponding child resources):
--
-- -   Dataset Import Job
--
-- -   Predictor Job
--
-- -   Forecast Job
--
-- -   Forecast Export Job
--
-- -   Predictor Backtest Export Job
--
-- -   Explainability Job
--
-- -   Explainability Export Job
module Amazonka.Forecast.StopResource
  ( -- * Creating a Request
    StopResource (..),
    newStopResource,

    -- * Request Lenses
    stopResource_resourceArn,

    -- * Destructuring the Response
    StopResourceResponse (..),
    newStopResourceResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopResource' smart constructor.
data StopResource = StopResource'
  { -- | The Amazon Resource Name (ARN) that identifies the resource to stop. The
    -- supported ARNs are @DatasetImportJobArn@, @PredictorArn@,
    -- @PredictorBacktestExportJobArn@, @ForecastArn@, @ForecastExportJobArn@,
    -- @ExplainabilityArn@, and @ExplainabilityExportArn@.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'stopResource_resourceArn' - The Amazon Resource Name (ARN) that identifies the resource to stop. The
-- supported ARNs are @DatasetImportJobArn@, @PredictorArn@,
-- @PredictorBacktestExportJobArn@, @ForecastArn@, @ForecastExportJobArn@,
-- @ExplainabilityArn@, and @ExplainabilityExportArn@.
newStopResource ::
  -- | 'resourceArn'
  Prelude.Text ->
  StopResource
newStopResource pResourceArn_ =
  StopResource' {resourceArn = pResourceArn_}

-- | The Amazon Resource Name (ARN) that identifies the resource to stop. The
-- supported ARNs are @DatasetImportJobArn@, @PredictorArn@,
-- @PredictorBacktestExportJobArn@, @ForecastArn@, @ForecastExportJobArn@,
-- @ExplainabilityArn@, and @ExplainabilityExportArn@.
stopResource_resourceArn :: Lens.Lens' StopResource Prelude.Text
stopResource_resourceArn = Lens.lens (\StopResource' {resourceArn} -> resourceArn) (\s@StopResource' {} a -> s {resourceArn = a} :: StopResource)

instance Core.AWSRequest StopResource where
  type AWSResponse StopResource = StopResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull StopResourceResponse'

instance Prelude.Hashable StopResource where
  hashWithSalt _salt StopResource' {..} =
    _salt `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData StopResource where
  rnf StopResource' {..} = Prelude.rnf resourceArn

instance Data.ToHeaders StopResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.StopResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopResource where
  toJSON StopResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ResourceArn" Data..= resourceArn)]
      )

instance Data.ToPath StopResource where
  toPath = Prelude.const "/"

instance Data.ToQuery StopResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopResourceResponse' smart constructor.
data StopResourceResponse = StopResourceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopResourceResponse ::
  StopResourceResponse
newStopResourceResponse = StopResourceResponse'

instance Prelude.NFData StopResourceResponse where
  rnf _ = ()
