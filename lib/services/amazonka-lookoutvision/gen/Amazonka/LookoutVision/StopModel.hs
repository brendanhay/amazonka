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
-- Module      : Amazonka.LookoutVision.StopModel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the hosting of a running model. The operation might take a while
-- to complete. To check the current status, call DescribeModel.
--
-- After the model hosting stops, the @Status@ of the model is @TRAINED@.
--
-- This operation requires permissions to perform the
-- @lookoutvision:StopModel@ operation.
module Amazonka.LookoutVision.StopModel
  ( -- * Creating a Request
    StopModel (..),
    newStopModel,

    -- * Request Lenses
    stopModel_clientToken,
    stopModel_projectName,
    stopModel_modelVersion,

    -- * Destructuring the Response
    StopModelResponse (..),
    newStopModelResponse,

    -- * Response Lenses
    stopModelResponse_status,
    stopModelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LookoutVision.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopModel' smart constructor.
data StopModel = StopModel'
  { -- | ClientToken is an idempotency token that ensures a call to @StopModel@
    -- completes only once. You choose the value to pass. For example, An issue
    -- might prevent you from getting a response from @StopModel@. In this
    -- case, safely retry your call to @StopModel@ by using the same
    -- @ClientToken@ parameter value.
    --
    -- If you don\'t supply a value for @ClientToken@, the AWS SDK you are
    -- using inserts a value for you. This prevents retries after a network
    -- error from making multiple stop requests. You\'ll need to provide your
    -- own value for other use cases.
    --
    -- An error occurs if the other input parameters are not the same as in the
    -- first request. Using a different value for @ClientToken@ is considered a
    -- new call to @StopModel@. An idempotency token is active for 8 hours.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the project that contains the model that you want to stop.
    projectName :: Prelude.Text,
    -- | The version of the model that you want to stop.
    modelVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'stopModel_clientToken' - ClientToken is an idempotency token that ensures a call to @StopModel@
-- completes only once. You choose the value to pass. For example, An issue
-- might prevent you from getting a response from @StopModel@. In this
-- case, safely retry your call to @StopModel@ by using the same
-- @ClientToken@ parameter value.
--
-- If you don\'t supply a value for @ClientToken@, the AWS SDK you are
-- using inserts a value for you. This prevents retries after a network
-- error from making multiple stop requests. You\'ll need to provide your
-- own value for other use cases.
--
-- An error occurs if the other input parameters are not the same as in the
-- first request. Using a different value for @ClientToken@ is considered a
-- new call to @StopModel@. An idempotency token is active for 8 hours.
--
-- 'projectName', 'stopModel_projectName' - The name of the project that contains the model that you want to stop.
--
-- 'modelVersion', 'stopModel_modelVersion' - The version of the model that you want to stop.
newStopModel ::
  -- | 'projectName'
  Prelude.Text ->
  -- | 'modelVersion'
  Prelude.Text ->
  StopModel
newStopModel pProjectName_ pModelVersion_ =
  StopModel'
    { clientToken = Prelude.Nothing,
      projectName = pProjectName_,
      modelVersion = pModelVersion_
    }

-- | ClientToken is an idempotency token that ensures a call to @StopModel@
-- completes only once. You choose the value to pass. For example, An issue
-- might prevent you from getting a response from @StopModel@. In this
-- case, safely retry your call to @StopModel@ by using the same
-- @ClientToken@ parameter value.
--
-- If you don\'t supply a value for @ClientToken@, the AWS SDK you are
-- using inserts a value for you. This prevents retries after a network
-- error from making multiple stop requests. You\'ll need to provide your
-- own value for other use cases.
--
-- An error occurs if the other input parameters are not the same as in the
-- first request. Using a different value for @ClientToken@ is considered a
-- new call to @StopModel@. An idempotency token is active for 8 hours.
stopModel_clientToken :: Lens.Lens' StopModel (Prelude.Maybe Prelude.Text)
stopModel_clientToken = Lens.lens (\StopModel' {clientToken} -> clientToken) (\s@StopModel' {} a -> s {clientToken = a} :: StopModel)

-- | The name of the project that contains the model that you want to stop.
stopModel_projectName :: Lens.Lens' StopModel Prelude.Text
stopModel_projectName = Lens.lens (\StopModel' {projectName} -> projectName) (\s@StopModel' {} a -> s {projectName = a} :: StopModel)

-- | The version of the model that you want to stop.
stopModel_modelVersion :: Lens.Lens' StopModel Prelude.Text
stopModel_modelVersion = Lens.lens (\StopModel' {modelVersion} -> modelVersion) (\s@StopModel' {} a -> s {modelVersion = a} :: StopModel)

instance Core.AWSRequest StopModel where
  type AWSResponse StopModel = StopModelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopModelResponse'
            Prelude.<$> (x Core..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopModel where
  hashWithSalt _salt StopModel' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` projectName
      `Prelude.hashWithSalt` modelVersion

instance Prelude.NFData StopModel where
  rnf StopModel' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf projectName
      `Prelude.seq` Prelude.rnf modelVersion

instance Core.ToHeaders StopModel where
  toHeaders StopModel' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Core.=# clientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToJSON StopModel where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath StopModel where
  toPath StopModel' {..} =
    Prelude.mconcat
      [ "/2020-11-20/projects/",
        Core.toBS projectName,
        "/models/",
        Core.toBS modelVersion,
        "/stop"
      ]

instance Core.ToQuery StopModel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopModelResponse' smart constructor.
data StopModelResponse = StopModelResponse'
  { -- | The status of the model.
    status :: Prelude.Maybe ModelHostingStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'stopModelResponse_status' - The status of the model.
--
-- 'httpStatus', 'stopModelResponse_httpStatus' - The response's http status code.
newStopModelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopModelResponse
newStopModelResponse pHttpStatus_ =
  StopModelResponse'
    { status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the model.
stopModelResponse_status :: Lens.Lens' StopModelResponse (Prelude.Maybe ModelHostingStatus)
stopModelResponse_status = Lens.lens (\StopModelResponse' {status} -> status) (\s@StopModelResponse' {} a -> s {status = a} :: StopModelResponse)

-- | The response's http status code.
stopModelResponse_httpStatus :: Lens.Lens' StopModelResponse Prelude.Int
stopModelResponse_httpStatus = Lens.lens (\StopModelResponse' {httpStatus} -> httpStatus) (\s@StopModelResponse' {} a -> s {httpStatus = a} :: StopModelResponse)

instance Prelude.NFData StopModelResponse where
  rnf StopModelResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
