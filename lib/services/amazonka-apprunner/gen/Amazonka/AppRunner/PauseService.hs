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
-- Module      : Amazonka.AppRunner.PauseService
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Pause an active App Runner service. App Runner reduces compute capacity
-- for the service to zero and loses state (for example, ephemeral storage
-- is removed).
--
-- This is an asynchronous operation. On a successful call, you can use the
-- returned @OperationId@ and the ListOperations call to track the
-- operation\'s progress.
module Amazonka.AppRunner.PauseService
  ( -- * Creating a Request
    PauseService (..),
    newPauseService,

    -- * Request Lenses
    pauseService_serviceArn,

    -- * Destructuring the Response
    PauseServiceResponse (..),
    newPauseServiceResponse,

    -- * Response Lenses
    pauseServiceResponse_operationId,
    pauseServiceResponse_httpStatus,
    pauseServiceResponse_service,
  )
where

import Amazonka.AppRunner.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPauseService' smart constructor.
data PauseService = PauseService'
  { -- | The Amazon Resource Name (ARN) of the App Runner service that you want
    -- to pause.
    serviceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PauseService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceArn', 'pauseService_serviceArn' - The Amazon Resource Name (ARN) of the App Runner service that you want
-- to pause.
newPauseService ::
  -- | 'serviceArn'
  Prelude.Text ->
  PauseService
newPauseService pServiceArn_ =
  PauseService' {serviceArn = pServiceArn_}

-- | The Amazon Resource Name (ARN) of the App Runner service that you want
-- to pause.
pauseService_serviceArn :: Lens.Lens' PauseService Prelude.Text
pauseService_serviceArn = Lens.lens (\PauseService' {serviceArn} -> serviceArn) (\s@PauseService' {} a -> s {serviceArn = a} :: PauseService)

instance Core.AWSRequest PauseService where
  type AWSResponse PauseService = PauseServiceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PauseServiceResponse'
            Prelude.<$> (x Core..?> "OperationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "Service")
      )

instance Prelude.Hashable PauseService where
  hashWithSalt _salt PauseService' {..} =
    _salt `Prelude.hashWithSalt` serviceArn

instance Prelude.NFData PauseService where
  rnf PauseService' {..} = Prelude.rnf serviceArn

instance Core.ToHeaders PauseService where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AppRunner.PauseService" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PauseService where
  toJSON PauseService' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ServiceArn" Core..= serviceArn)]
      )

instance Core.ToPath PauseService where
  toPath = Prelude.const "/"

instance Core.ToQuery PauseService where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPauseServiceResponse' smart constructor.
data PauseServiceResponse = PauseServiceResponse'
  { -- | The unique ID of the asynchronous operation that this request started.
    -- You can use it combined with the ListOperations call to track the
    -- operation\'s progress.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A description of the App Runner service that this request just paused.
    service :: Service
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PauseServiceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'pauseServiceResponse_operationId' - The unique ID of the asynchronous operation that this request started.
-- You can use it combined with the ListOperations call to track the
-- operation\'s progress.
--
-- 'httpStatus', 'pauseServiceResponse_httpStatus' - The response's http status code.
--
-- 'service', 'pauseServiceResponse_service' - A description of the App Runner service that this request just paused.
newPauseServiceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'service'
  Service ->
  PauseServiceResponse
newPauseServiceResponse pHttpStatus_ pService_ =
  PauseServiceResponse'
    { operationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      service = pService_
    }

-- | The unique ID of the asynchronous operation that this request started.
-- You can use it combined with the ListOperations call to track the
-- operation\'s progress.
pauseServiceResponse_operationId :: Lens.Lens' PauseServiceResponse (Prelude.Maybe Prelude.Text)
pauseServiceResponse_operationId = Lens.lens (\PauseServiceResponse' {operationId} -> operationId) (\s@PauseServiceResponse' {} a -> s {operationId = a} :: PauseServiceResponse)

-- | The response's http status code.
pauseServiceResponse_httpStatus :: Lens.Lens' PauseServiceResponse Prelude.Int
pauseServiceResponse_httpStatus = Lens.lens (\PauseServiceResponse' {httpStatus} -> httpStatus) (\s@PauseServiceResponse' {} a -> s {httpStatus = a} :: PauseServiceResponse)

-- | A description of the App Runner service that this request just paused.
pauseServiceResponse_service :: Lens.Lens' PauseServiceResponse Service
pauseServiceResponse_service = Lens.lens (\PauseServiceResponse' {service} -> service) (\s@PauseServiceResponse' {} a -> s {service = a} :: PauseServiceResponse)

instance Prelude.NFData PauseServiceResponse where
  rnf PauseServiceResponse' {..} =
    Prelude.rnf operationId
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf service
