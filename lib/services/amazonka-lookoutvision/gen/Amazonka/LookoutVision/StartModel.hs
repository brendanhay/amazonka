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
-- Module      : Amazonka.LookoutVision.StartModel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the running of the version of an Amazon Lookout for Vision model.
-- Starting a model takes a while to complete. To check the current state
-- of the model, use DescribeModel.
--
-- A model is ready to use when its status is @HOSTED@.
--
-- Once the model is running, you can detect custom labels in new images by
-- calling DetectAnomalies.
--
-- You are charged for the amount of time that the model is running. To
-- stop a running model, call StopModel.
--
-- This operation requires permissions to perform the
-- @lookoutvision:StartModel@ operation.
module Amazonka.LookoutVision.StartModel
  ( -- * Creating a Request
    StartModel (..),
    newStartModel,

    -- * Request Lenses
    startModel_clientToken,
    startModel_maxInferenceUnits,
    startModel_projectName,
    startModel_modelVersion,
    startModel_minInferenceUnits,

    -- * Destructuring the Response
    StartModelResponse (..),
    newStartModelResponse,

    -- * Response Lenses
    startModelResponse_status,
    startModelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutVision.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartModel' smart constructor.
data StartModel = StartModel'
  { -- | ClientToken is an idempotency token that ensures a call to @StartModel@
    -- completes only once. You choose the value to pass. For example, An issue
    -- might prevent you from getting a response from @StartModel@. In this
    -- case, safely retry your call to @StartModel@ by using the same
    -- @ClientToken@ parameter value.
    --
    -- If you don\'t supply a value for @ClientToken@, the AWS SDK you are
    -- using inserts a value for you. This prevents retries after a network
    -- error from making multiple start requests. You\'ll need to provide your
    -- own value for other use cases.
    --
    -- An error occurs if the other input parameters are not the same as in the
    -- first request. Using a different value for @ClientToken@ is considered a
    -- new call to @StartModel@. An idempotency token is active for 8 hours.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of inference units to use for auto-scaling the model.
    -- If you don\'t specify a value, Amazon Lookout for Vision doesn\'t
    -- auto-scale the model.
    maxInferenceUnits :: Prelude.Maybe Prelude.Natural,
    -- | The name of the project that contains the model that you want to start.
    projectName :: Prelude.Text,
    -- | The version of the model that you want to start.
    modelVersion :: Prelude.Text,
    -- | The minimum number of inference units to use. A single inference unit
    -- represents 1 hour of processing. Use a higher number to increase the TPS
    -- throughput of your model. You are charged for the number of inference
    -- units that you use.
    minInferenceUnits :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'startModel_clientToken' - ClientToken is an idempotency token that ensures a call to @StartModel@
-- completes only once. You choose the value to pass. For example, An issue
-- might prevent you from getting a response from @StartModel@. In this
-- case, safely retry your call to @StartModel@ by using the same
-- @ClientToken@ parameter value.
--
-- If you don\'t supply a value for @ClientToken@, the AWS SDK you are
-- using inserts a value for you. This prevents retries after a network
-- error from making multiple start requests. You\'ll need to provide your
-- own value for other use cases.
--
-- An error occurs if the other input parameters are not the same as in the
-- first request. Using a different value for @ClientToken@ is considered a
-- new call to @StartModel@. An idempotency token is active for 8 hours.
--
-- 'maxInferenceUnits', 'startModel_maxInferenceUnits' - The maximum number of inference units to use for auto-scaling the model.
-- If you don\'t specify a value, Amazon Lookout for Vision doesn\'t
-- auto-scale the model.
--
-- 'projectName', 'startModel_projectName' - The name of the project that contains the model that you want to start.
--
-- 'modelVersion', 'startModel_modelVersion' - The version of the model that you want to start.
--
-- 'minInferenceUnits', 'startModel_minInferenceUnits' - The minimum number of inference units to use. A single inference unit
-- represents 1 hour of processing. Use a higher number to increase the TPS
-- throughput of your model. You are charged for the number of inference
-- units that you use.
newStartModel ::
  -- | 'projectName'
  Prelude.Text ->
  -- | 'modelVersion'
  Prelude.Text ->
  -- | 'minInferenceUnits'
  Prelude.Natural ->
  StartModel
newStartModel
  pProjectName_
  pModelVersion_
  pMinInferenceUnits_ =
    StartModel'
      { clientToken = Prelude.Nothing,
        maxInferenceUnits = Prelude.Nothing,
        projectName = pProjectName_,
        modelVersion = pModelVersion_,
        minInferenceUnits = pMinInferenceUnits_
      }

-- | ClientToken is an idempotency token that ensures a call to @StartModel@
-- completes only once. You choose the value to pass. For example, An issue
-- might prevent you from getting a response from @StartModel@. In this
-- case, safely retry your call to @StartModel@ by using the same
-- @ClientToken@ parameter value.
--
-- If you don\'t supply a value for @ClientToken@, the AWS SDK you are
-- using inserts a value for you. This prevents retries after a network
-- error from making multiple start requests. You\'ll need to provide your
-- own value for other use cases.
--
-- An error occurs if the other input parameters are not the same as in the
-- first request. Using a different value for @ClientToken@ is considered a
-- new call to @StartModel@. An idempotency token is active for 8 hours.
startModel_clientToken :: Lens.Lens' StartModel (Prelude.Maybe Prelude.Text)
startModel_clientToken = Lens.lens (\StartModel' {clientToken} -> clientToken) (\s@StartModel' {} a -> s {clientToken = a} :: StartModel)

-- | The maximum number of inference units to use for auto-scaling the model.
-- If you don\'t specify a value, Amazon Lookout for Vision doesn\'t
-- auto-scale the model.
startModel_maxInferenceUnits :: Lens.Lens' StartModel (Prelude.Maybe Prelude.Natural)
startModel_maxInferenceUnits = Lens.lens (\StartModel' {maxInferenceUnits} -> maxInferenceUnits) (\s@StartModel' {} a -> s {maxInferenceUnits = a} :: StartModel)

-- | The name of the project that contains the model that you want to start.
startModel_projectName :: Lens.Lens' StartModel Prelude.Text
startModel_projectName = Lens.lens (\StartModel' {projectName} -> projectName) (\s@StartModel' {} a -> s {projectName = a} :: StartModel)

-- | The version of the model that you want to start.
startModel_modelVersion :: Lens.Lens' StartModel Prelude.Text
startModel_modelVersion = Lens.lens (\StartModel' {modelVersion} -> modelVersion) (\s@StartModel' {} a -> s {modelVersion = a} :: StartModel)

-- | The minimum number of inference units to use. A single inference unit
-- represents 1 hour of processing. Use a higher number to increase the TPS
-- throughput of your model. You are charged for the number of inference
-- units that you use.
startModel_minInferenceUnits :: Lens.Lens' StartModel Prelude.Natural
startModel_minInferenceUnits = Lens.lens (\StartModel' {minInferenceUnits} -> minInferenceUnits) (\s@StartModel' {} a -> s {minInferenceUnits = a} :: StartModel)

instance Core.AWSRequest StartModel where
  type AWSResponse StartModel = StartModelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartModelResponse'
            Prelude.<$> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartModel where
  hashWithSalt _salt StartModel' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` maxInferenceUnits
      `Prelude.hashWithSalt` projectName
      `Prelude.hashWithSalt` modelVersion
      `Prelude.hashWithSalt` minInferenceUnits

instance Prelude.NFData StartModel where
  rnf StartModel' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf maxInferenceUnits `Prelude.seq`
        Prelude.rnf projectName `Prelude.seq`
          Prelude.rnf modelVersion `Prelude.seq`
            Prelude.rnf minInferenceUnits

instance Data.ToHeaders StartModel where
  toHeaders StartModel' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Data.=# clientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON StartModel where
  toJSON StartModel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxInferenceUnits" Data..=)
              Prelude.<$> maxInferenceUnits,
            Prelude.Just
              ("MinInferenceUnits" Data..= minInferenceUnits)
          ]
      )

instance Data.ToPath StartModel where
  toPath StartModel' {..} =
    Prelude.mconcat
      [ "/2020-11-20/projects/",
        Data.toBS projectName,
        "/models/",
        Data.toBS modelVersion,
        "/start"
      ]

instance Data.ToQuery StartModel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartModelResponse' smart constructor.
data StartModelResponse = StartModelResponse'
  { -- | The current running status of the model.
    status :: Prelude.Maybe ModelHostingStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'startModelResponse_status' - The current running status of the model.
--
-- 'httpStatus', 'startModelResponse_httpStatus' - The response's http status code.
newStartModelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartModelResponse
newStartModelResponse pHttpStatus_ =
  StartModelResponse'
    { status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current running status of the model.
startModelResponse_status :: Lens.Lens' StartModelResponse (Prelude.Maybe ModelHostingStatus)
startModelResponse_status = Lens.lens (\StartModelResponse' {status} -> status) (\s@StartModelResponse' {} a -> s {status = a} :: StartModelResponse)

-- | The response's http status code.
startModelResponse_httpStatus :: Lens.Lens' StartModelResponse Prelude.Int
startModelResponse_httpStatus = Lens.lens (\StartModelResponse' {httpStatus} -> httpStatus) (\s@StartModelResponse' {} a -> s {httpStatus = a} :: StartModelResponse)

instance Prelude.NFData StartModelResponse where
  rnf StartModelResponse' {..} =
    Prelude.rnf status `Prelude.seq`
      Prelude.rnf httpStatus
