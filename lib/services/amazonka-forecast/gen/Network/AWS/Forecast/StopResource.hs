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
-- Module      : Network.AWS.Forecast.StopResource
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Forecast.StopResource
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

import qualified Network.AWS.Core as Core
import Network.AWS.Forecast.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopResource' smart constructor.
data StopResource = StopResource'
  { -- | The Amazon Resource Name (ARN) that identifies the resource to stop. The
    -- supported ARNs are @DatasetImportJobArn@, @PredictorArn@,
    -- @PredictorBacktestExportJobArn@, @ForecastArn@, and
    -- @ForecastExportJobArn@.
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
-- @PredictorBacktestExportJobArn@, @ForecastArn@, and
-- @ForecastExportJobArn@.
newStopResource ::
  -- | 'resourceArn'
  Prelude.Text ->
  StopResource
newStopResource pResourceArn_ =
  StopResource' {resourceArn = pResourceArn_}

-- | The Amazon Resource Name (ARN) that identifies the resource to stop. The
-- supported ARNs are @DatasetImportJobArn@, @PredictorArn@,
-- @PredictorBacktestExportJobArn@, @ForecastArn@, and
-- @ForecastExportJobArn@.
stopResource_resourceArn :: Lens.Lens' StopResource Prelude.Text
stopResource_resourceArn = Lens.lens (\StopResource' {resourceArn} -> resourceArn) (\s@StopResource' {} a -> s {resourceArn = a} :: StopResource)

instance Core.AWSRequest StopResource where
  type AWSResponse StopResource = StopResourceResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull StopResourceResponse'

instance Prelude.Hashable StopResource

instance Prelude.NFData StopResource

instance Core.ToHeaders StopResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonForecast.StopResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StopResource where
  toJSON StopResource' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ResourceArn" Core..= resourceArn)]
      )

instance Core.ToPath StopResource where
  toPath = Prelude.const "/"

instance Core.ToQuery StopResource where
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

instance Prelude.NFData StopResourceResponse
