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
-- Module      : Amazonka.AppRunner.StartDeployment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiate a manual deployment of the latest commit in a source code
-- repository or the latest image in a source image repository to an App
-- Runner service.
--
-- For a source code repository, App Runner retrieves the commit and builds
-- a Docker image. For a source image repository, App Runner retrieves the
-- latest Docker image. In both cases, App Runner then deploys the new
-- image to your service and starts a new container instance.
--
-- This is an asynchronous operation. On a successful call, you can use the
-- returned @OperationId@ and the ListOperations call to track the
-- operation\'s progress.
module Amazonka.AppRunner.StartDeployment
  ( -- * Creating a Request
    StartDeployment (..),
    newStartDeployment,

    -- * Request Lenses
    startDeployment_serviceArn,

    -- * Destructuring the Response
    StartDeploymentResponse (..),
    newStartDeploymentResponse,

    -- * Response Lenses
    startDeploymentResponse_httpStatus,
    startDeploymentResponse_operationId,
  )
where

import Amazonka.AppRunner.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartDeployment' smart constructor.
data StartDeployment = StartDeployment'
  { -- | The Amazon Resource Name (ARN) of the App Runner service that you want
    -- to manually deploy to.
    serviceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceArn', 'startDeployment_serviceArn' - The Amazon Resource Name (ARN) of the App Runner service that you want
-- to manually deploy to.
newStartDeployment ::
  -- | 'serviceArn'
  Prelude.Text ->
  StartDeployment
newStartDeployment pServiceArn_ =
  StartDeployment' {serviceArn = pServiceArn_}

-- | The Amazon Resource Name (ARN) of the App Runner service that you want
-- to manually deploy to.
startDeployment_serviceArn :: Lens.Lens' StartDeployment Prelude.Text
startDeployment_serviceArn = Lens.lens (\StartDeployment' {serviceArn} -> serviceArn) (\s@StartDeployment' {} a -> s {serviceArn = a} :: StartDeployment)

instance Core.AWSRequest StartDeployment where
  type
    AWSResponse StartDeployment =
      StartDeploymentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartDeploymentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "OperationId")
      )

instance Prelude.Hashable StartDeployment where
  hashWithSalt _salt StartDeployment' {..} =
    _salt `Prelude.hashWithSalt` serviceArn

instance Prelude.NFData StartDeployment where
  rnf StartDeployment' {..} = Prelude.rnf serviceArn

instance Data.ToHeaders StartDeployment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AppRunner.StartDeployment" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartDeployment where
  toJSON StartDeployment' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ServiceArn" Data..= serviceArn)]
      )

instance Data.ToPath StartDeployment where
  toPath = Prelude.const "/"

instance Data.ToQuery StartDeployment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartDeploymentResponse' smart constructor.
data StartDeploymentResponse = StartDeploymentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique ID of the asynchronous operation that this request started.
    -- You can use it combined with the ListOperations call to track the
    -- operation\'s progress.
    operationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartDeploymentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startDeploymentResponse_httpStatus' - The response's http status code.
--
-- 'operationId', 'startDeploymentResponse_operationId' - The unique ID of the asynchronous operation that this request started.
-- You can use it combined with the ListOperations call to track the
-- operation\'s progress.
newStartDeploymentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'operationId'
  Prelude.Text ->
  StartDeploymentResponse
newStartDeploymentResponse pHttpStatus_ pOperationId_ =
  StartDeploymentResponse'
    { httpStatus = pHttpStatus_,
      operationId = pOperationId_
    }

-- | The response's http status code.
startDeploymentResponse_httpStatus :: Lens.Lens' StartDeploymentResponse Prelude.Int
startDeploymentResponse_httpStatus = Lens.lens (\StartDeploymentResponse' {httpStatus} -> httpStatus) (\s@StartDeploymentResponse' {} a -> s {httpStatus = a} :: StartDeploymentResponse)

-- | The unique ID of the asynchronous operation that this request started.
-- You can use it combined with the ListOperations call to track the
-- operation\'s progress.
startDeploymentResponse_operationId :: Lens.Lens' StartDeploymentResponse Prelude.Text
startDeploymentResponse_operationId = Lens.lens (\StartDeploymentResponse' {operationId} -> operationId) (\s@StartDeploymentResponse' {} a -> s {operationId = a} :: StartDeploymentResponse)

instance Prelude.NFData StartDeploymentResponse where
  rnf StartDeploymentResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf operationId
