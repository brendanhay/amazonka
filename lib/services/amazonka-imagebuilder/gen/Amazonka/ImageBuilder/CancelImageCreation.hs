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
-- Module      : Amazonka.ImageBuilder.CancelImageCreation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- CancelImageCreation cancels the creation of Image. This operation can
-- only be used on images in a non-terminal state.
module Amazonka.ImageBuilder.CancelImageCreation
  ( -- * Creating a Request
    CancelImageCreation (..),
    newCancelImageCreation,

    -- * Request Lenses
    cancelImageCreation_imageBuildVersionArn,
    cancelImageCreation_clientToken,

    -- * Destructuring the Response
    CancelImageCreationResponse (..),
    newCancelImageCreationResponse,

    -- * Response Lenses
    cancelImageCreationResponse_clientToken,
    cancelImageCreationResponse_requestId,
    cancelImageCreationResponse_imageBuildVersionArn,
    cancelImageCreationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelImageCreation' smart constructor.
data CancelImageCreation = CancelImageCreation'
  { -- | The Amazon Resource Name (ARN) of the image whose creation you want to
    -- cancel.
    imageBuildVersionArn :: Prelude.Text,
    -- | Unique, case-sensitive identifier you provide to ensure idempotency of
    -- the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring idempotency>
    -- in the /Amazon EC2 API Reference/.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelImageCreation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageBuildVersionArn', 'cancelImageCreation_imageBuildVersionArn' - The Amazon Resource Name (ARN) of the image whose creation you want to
-- cancel.
--
-- 'clientToken', 'cancelImageCreation_clientToken' - Unique, case-sensitive identifier you provide to ensure idempotency of
-- the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring idempotency>
-- in the /Amazon EC2 API Reference/.
newCancelImageCreation ::
  -- | 'imageBuildVersionArn'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  CancelImageCreation
newCancelImageCreation
  pImageBuildVersionArn_
  pClientToken_ =
    CancelImageCreation'
      { imageBuildVersionArn =
          pImageBuildVersionArn_,
        clientToken = pClientToken_
      }

-- | The Amazon Resource Name (ARN) of the image whose creation you want to
-- cancel.
cancelImageCreation_imageBuildVersionArn :: Lens.Lens' CancelImageCreation Prelude.Text
cancelImageCreation_imageBuildVersionArn = Lens.lens (\CancelImageCreation' {imageBuildVersionArn} -> imageBuildVersionArn) (\s@CancelImageCreation' {} a -> s {imageBuildVersionArn = a} :: CancelImageCreation)

-- | Unique, case-sensitive identifier you provide to ensure idempotency of
-- the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring idempotency>
-- in the /Amazon EC2 API Reference/.
cancelImageCreation_clientToken :: Lens.Lens' CancelImageCreation Prelude.Text
cancelImageCreation_clientToken = Lens.lens (\CancelImageCreation' {clientToken} -> clientToken) (\s@CancelImageCreation' {} a -> s {clientToken = a} :: CancelImageCreation)

instance Core.AWSRequest CancelImageCreation where
  type
    AWSResponse CancelImageCreation =
      CancelImageCreationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelImageCreationResponse'
            Prelude.<$> (x Core..?> "clientToken")
            Prelude.<*> (x Core..?> "requestId")
            Prelude.<*> (x Core..?> "imageBuildVersionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelImageCreation where
  hashWithSalt _salt CancelImageCreation' {..} =
    _salt `Prelude.hashWithSalt` imageBuildVersionArn
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData CancelImageCreation where
  rnf CancelImageCreation' {..} =
    Prelude.rnf imageBuildVersionArn
      `Prelude.seq` Prelude.rnf clientToken

instance Core.ToHeaders CancelImageCreation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CancelImageCreation where
  toJSON CancelImageCreation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "imageBuildVersionArn"
                  Core..= imageBuildVersionArn
              ),
            Prelude.Just ("clientToken" Core..= clientToken)
          ]
      )

instance Core.ToPath CancelImageCreation where
  toPath = Prelude.const "/CancelImageCreation"

instance Core.ToQuery CancelImageCreation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelImageCreationResponse' smart constructor.
data CancelImageCreationResponse = CancelImageCreationResponse'
  { -- | The idempotency token that was used for this request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the image whose creation has been
    -- cancelled.
    imageBuildVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelImageCreationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'cancelImageCreationResponse_clientToken' - The idempotency token that was used for this request.
--
-- 'requestId', 'cancelImageCreationResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'imageBuildVersionArn', 'cancelImageCreationResponse_imageBuildVersionArn' - The Amazon Resource Name (ARN) of the image whose creation has been
-- cancelled.
--
-- 'httpStatus', 'cancelImageCreationResponse_httpStatus' - The response's http status code.
newCancelImageCreationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelImageCreationResponse
newCancelImageCreationResponse pHttpStatus_ =
  CancelImageCreationResponse'
    { clientToken =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      imageBuildVersionArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The idempotency token that was used for this request.
cancelImageCreationResponse_clientToken :: Lens.Lens' CancelImageCreationResponse (Prelude.Maybe Prelude.Text)
cancelImageCreationResponse_clientToken = Lens.lens (\CancelImageCreationResponse' {clientToken} -> clientToken) (\s@CancelImageCreationResponse' {} a -> s {clientToken = a} :: CancelImageCreationResponse)

-- | The request ID that uniquely identifies this request.
cancelImageCreationResponse_requestId :: Lens.Lens' CancelImageCreationResponse (Prelude.Maybe Prelude.Text)
cancelImageCreationResponse_requestId = Lens.lens (\CancelImageCreationResponse' {requestId} -> requestId) (\s@CancelImageCreationResponse' {} a -> s {requestId = a} :: CancelImageCreationResponse)

-- | The Amazon Resource Name (ARN) of the image whose creation has been
-- cancelled.
cancelImageCreationResponse_imageBuildVersionArn :: Lens.Lens' CancelImageCreationResponse (Prelude.Maybe Prelude.Text)
cancelImageCreationResponse_imageBuildVersionArn = Lens.lens (\CancelImageCreationResponse' {imageBuildVersionArn} -> imageBuildVersionArn) (\s@CancelImageCreationResponse' {} a -> s {imageBuildVersionArn = a} :: CancelImageCreationResponse)

-- | The response's http status code.
cancelImageCreationResponse_httpStatus :: Lens.Lens' CancelImageCreationResponse Prelude.Int
cancelImageCreationResponse_httpStatus = Lens.lens (\CancelImageCreationResponse' {httpStatus} -> httpStatus) (\s@CancelImageCreationResponse' {} a -> s {httpStatus = a} :: CancelImageCreationResponse)

instance Prelude.NFData CancelImageCreationResponse where
  rnf CancelImageCreationResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf imageBuildVersionArn
      `Prelude.seq` Prelude.rnf httpStatus
