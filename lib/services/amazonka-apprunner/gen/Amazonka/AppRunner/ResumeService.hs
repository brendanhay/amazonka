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
-- Module      : Amazonka.AppRunner.ResumeService
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resume an active App Runner service. App Runner provisions compute
-- capacity for the service.
--
-- This is an asynchronous operation. On a successful call, you can use the
-- returned @OperationId@ and the ListOperations call to track the
-- operation\'s progress.
module Amazonka.AppRunner.ResumeService
  ( -- * Creating a Request
    ResumeService (..),
    newResumeService,

    -- * Request Lenses
    resumeService_serviceArn,

    -- * Destructuring the Response
    ResumeServiceResponse (..),
    newResumeServiceResponse,

    -- * Response Lenses
    resumeServiceResponse_operationId,
    resumeServiceResponse_httpStatus,
    resumeServiceResponse_service,
  )
where

import Amazonka.AppRunner.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newResumeService' smart constructor.
data ResumeService = ResumeService'
  { -- | The Amazon Resource Name (ARN) of the App Runner service that you want
    -- to resume.
    serviceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResumeService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceArn', 'resumeService_serviceArn' - The Amazon Resource Name (ARN) of the App Runner service that you want
-- to resume.
newResumeService ::
  -- | 'serviceArn'
  Prelude.Text ->
  ResumeService
newResumeService pServiceArn_ =
  ResumeService' {serviceArn = pServiceArn_}

-- | The Amazon Resource Name (ARN) of the App Runner service that you want
-- to resume.
resumeService_serviceArn :: Lens.Lens' ResumeService Prelude.Text
resumeService_serviceArn = Lens.lens (\ResumeService' {serviceArn} -> serviceArn) (\s@ResumeService' {} a -> s {serviceArn = a} :: ResumeService)

instance Core.AWSRequest ResumeService where
  type
    AWSResponse ResumeService =
      ResumeServiceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ResumeServiceResponse'
            Prelude.<$> (x Data..?> "OperationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Service")
      )

instance Prelude.Hashable ResumeService where
  hashWithSalt _salt ResumeService' {..} =
    _salt `Prelude.hashWithSalt` serviceArn

instance Prelude.NFData ResumeService where
  rnf ResumeService' {..} = Prelude.rnf serviceArn

instance Data.ToHeaders ResumeService where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AppRunner.ResumeService" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ResumeService where
  toJSON ResumeService' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ServiceArn" Data..= serviceArn)]
      )

instance Data.ToPath ResumeService where
  toPath = Prelude.const "/"

instance Data.ToQuery ResumeService where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newResumeServiceResponse' smart constructor.
data ResumeServiceResponse = ResumeServiceResponse'
  { -- | The unique ID of the asynchronous operation that this request started.
    -- You can use it combined with the ListOperations call to track the
    -- operation\'s progress.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A description of the App Runner service that this request just resumed.
    service :: Service
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResumeServiceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'resumeServiceResponse_operationId' - The unique ID of the asynchronous operation that this request started.
-- You can use it combined with the ListOperations call to track the
-- operation\'s progress.
--
-- 'httpStatus', 'resumeServiceResponse_httpStatus' - The response's http status code.
--
-- 'service', 'resumeServiceResponse_service' - A description of the App Runner service that this request just resumed.
newResumeServiceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'service'
  Service ->
  ResumeServiceResponse
newResumeServiceResponse pHttpStatus_ pService_ =
  ResumeServiceResponse'
    { operationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      service = pService_
    }

-- | The unique ID of the asynchronous operation that this request started.
-- You can use it combined with the ListOperations call to track the
-- operation\'s progress.
resumeServiceResponse_operationId :: Lens.Lens' ResumeServiceResponse (Prelude.Maybe Prelude.Text)
resumeServiceResponse_operationId = Lens.lens (\ResumeServiceResponse' {operationId} -> operationId) (\s@ResumeServiceResponse' {} a -> s {operationId = a} :: ResumeServiceResponse)

-- | The response's http status code.
resumeServiceResponse_httpStatus :: Lens.Lens' ResumeServiceResponse Prelude.Int
resumeServiceResponse_httpStatus = Lens.lens (\ResumeServiceResponse' {httpStatus} -> httpStatus) (\s@ResumeServiceResponse' {} a -> s {httpStatus = a} :: ResumeServiceResponse)

-- | A description of the App Runner service that this request just resumed.
resumeServiceResponse_service :: Lens.Lens' ResumeServiceResponse Service
resumeServiceResponse_service = Lens.lens (\ResumeServiceResponse' {service} -> service) (\s@ResumeServiceResponse' {} a -> s {service = a} :: ResumeServiceResponse)

instance Prelude.NFData ResumeServiceResponse where
  rnf ResumeServiceResponse' {..} =
    Prelude.rnf operationId
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf service
