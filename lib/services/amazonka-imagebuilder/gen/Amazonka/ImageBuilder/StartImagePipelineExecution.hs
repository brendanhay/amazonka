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
-- Module      : Amazonka.ImageBuilder.StartImagePipelineExecution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Manually triggers a pipeline to create an image.
module Amazonka.ImageBuilder.StartImagePipelineExecution
  ( -- * Creating a Request
    StartImagePipelineExecution (..),
    newStartImagePipelineExecution,

    -- * Request Lenses
    startImagePipelineExecution_imagePipelineArn,
    startImagePipelineExecution_clientToken,

    -- * Destructuring the Response
    StartImagePipelineExecutionResponse (..),
    newStartImagePipelineExecutionResponse,

    -- * Response Lenses
    startImagePipelineExecutionResponse_clientToken,
    startImagePipelineExecutionResponse_imageBuildVersionArn,
    startImagePipelineExecutionResponse_requestId,
    startImagePipelineExecutionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartImagePipelineExecution' smart constructor.
data StartImagePipelineExecution = StartImagePipelineExecution'
  { -- | The Amazon Resource Name (ARN) of the image pipeline that you want to
    -- manually invoke.
    imagePipelineArn :: Prelude.Text,
    -- | The idempotency token used to make this request idempotent.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartImagePipelineExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imagePipelineArn', 'startImagePipelineExecution_imagePipelineArn' - The Amazon Resource Name (ARN) of the image pipeline that you want to
-- manually invoke.
--
-- 'clientToken', 'startImagePipelineExecution_clientToken' - The idempotency token used to make this request idempotent.
newStartImagePipelineExecution ::
  -- | 'imagePipelineArn'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  StartImagePipelineExecution
newStartImagePipelineExecution
  pImagePipelineArn_
  pClientToken_ =
    StartImagePipelineExecution'
      { imagePipelineArn =
          pImagePipelineArn_,
        clientToken = pClientToken_
      }

-- | The Amazon Resource Name (ARN) of the image pipeline that you want to
-- manually invoke.
startImagePipelineExecution_imagePipelineArn :: Lens.Lens' StartImagePipelineExecution Prelude.Text
startImagePipelineExecution_imagePipelineArn = Lens.lens (\StartImagePipelineExecution' {imagePipelineArn} -> imagePipelineArn) (\s@StartImagePipelineExecution' {} a -> s {imagePipelineArn = a} :: StartImagePipelineExecution)

-- | The idempotency token used to make this request idempotent.
startImagePipelineExecution_clientToken :: Lens.Lens' StartImagePipelineExecution Prelude.Text
startImagePipelineExecution_clientToken = Lens.lens (\StartImagePipelineExecution' {clientToken} -> clientToken) (\s@StartImagePipelineExecution' {} a -> s {clientToken = a} :: StartImagePipelineExecution)

instance Core.AWSRequest StartImagePipelineExecution where
  type
    AWSResponse StartImagePipelineExecution =
      StartImagePipelineExecutionResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartImagePipelineExecutionResponse'
            Prelude.<$> (x Data..?> "clientToken")
            Prelude.<*> (x Data..?> "imageBuildVersionArn")
            Prelude.<*> (x Data..?> "requestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartImagePipelineExecution where
  hashWithSalt _salt StartImagePipelineExecution' {..} =
    _salt `Prelude.hashWithSalt` imagePipelineArn
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData StartImagePipelineExecution where
  rnf StartImagePipelineExecution' {..} =
    Prelude.rnf imagePipelineArn
      `Prelude.seq` Prelude.rnf clientToken

instance Data.ToHeaders StartImagePipelineExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartImagePipelineExecution where
  toJSON StartImagePipelineExecution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("imagePipelineArn" Data..= imagePipelineArn),
            Prelude.Just ("clientToken" Data..= clientToken)
          ]
      )

instance Data.ToPath StartImagePipelineExecution where
  toPath = Prelude.const "/StartImagePipelineExecution"

instance Data.ToQuery StartImagePipelineExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartImagePipelineExecutionResponse' smart constructor.
data StartImagePipelineExecutionResponse = StartImagePipelineExecutionResponse'
  { -- | The idempotency token used to make this request idempotent.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the image that was created by this
    -- request.
    imageBuildVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartImagePipelineExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'startImagePipelineExecutionResponse_clientToken' - The idempotency token used to make this request idempotent.
--
-- 'imageBuildVersionArn', 'startImagePipelineExecutionResponse_imageBuildVersionArn' - The Amazon Resource Name (ARN) of the image that was created by this
-- request.
--
-- 'requestId', 'startImagePipelineExecutionResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'httpStatus', 'startImagePipelineExecutionResponse_httpStatus' - The response's http status code.
newStartImagePipelineExecutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartImagePipelineExecutionResponse
newStartImagePipelineExecutionResponse pHttpStatus_ =
  StartImagePipelineExecutionResponse'
    { clientToken =
        Prelude.Nothing,
      imageBuildVersionArn = Prelude.Nothing,
      requestId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The idempotency token used to make this request idempotent.
startImagePipelineExecutionResponse_clientToken :: Lens.Lens' StartImagePipelineExecutionResponse (Prelude.Maybe Prelude.Text)
startImagePipelineExecutionResponse_clientToken = Lens.lens (\StartImagePipelineExecutionResponse' {clientToken} -> clientToken) (\s@StartImagePipelineExecutionResponse' {} a -> s {clientToken = a} :: StartImagePipelineExecutionResponse)

-- | The Amazon Resource Name (ARN) of the image that was created by this
-- request.
startImagePipelineExecutionResponse_imageBuildVersionArn :: Lens.Lens' StartImagePipelineExecutionResponse (Prelude.Maybe Prelude.Text)
startImagePipelineExecutionResponse_imageBuildVersionArn = Lens.lens (\StartImagePipelineExecutionResponse' {imageBuildVersionArn} -> imageBuildVersionArn) (\s@StartImagePipelineExecutionResponse' {} a -> s {imageBuildVersionArn = a} :: StartImagePipelineExecutionResponse)

-- | The request ID that uniquely identifies this request.
startImagePipelineExecutionResponse_requestId :: Lens.Lens' StartImagePipelineExecutionResponse (Prelude.Maybe Prelude.Text)
startImagePipelineExecutionResponse_requestId = Lens.lens (\StartImagePipelineExecutionResponse' {requestId} -> requestId) (\s@StartImagePipelineExecutionResponse' {} a -> s {requestId = a} :: StartImagePipelineExecutionResponse)

-- | The response's http status code.
startImagePipelineExecutionResponse_httpStatus :: Lens.Lens' StartImagePipelineExecutionResponse Prelude.Int
startImagePipelineExecutionResponse_httpStatus = Lens.lens (\StartImagePipelineExecutionResponse' {httpStatus} -> httpStatus) (\s@StartImagePipelineExecutionResponse' {} a -> s {httpStatus = a} :: StartImagePipelineExecutionResponse)

instance
  Prelude.NFData
    StartImagePipelineExecutionResponse
  where
  rnf StartImagePipelineExecutionResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf imageBuildVersionArn
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf httpStatus
