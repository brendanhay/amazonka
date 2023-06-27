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
-- Module      : Amazonka.Comprehend.StartFlywheelIteration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start the flywheel iteration.This operation uses any new datasets to
-- train a new model version. For more information about flywheels, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/flywheels-about.html Flywheel overview>
-- in the /Amazon Comprehend Developer Guide/.
module Amazonka.Comprehend.StartFlywheelIteration
  ( -- * Creating a Request
    StartFlywheelIteration (..),
    newStartFlywheelIteration,

    -- * Request Lenses
    startFlywheelIteration_clientRequestToken,
    startFlywheelIteration_flywheelArn,

    -- * Destructuring the Response
    StartFlywheelIterationResponse (..),
    newStartFlywheelIterationResponse,

    -- * Response Lenses
    startFlywheelIterationResponse_flywheelArn,
    startFlywheelIterationResponse_flywheelIterationId,
    startFlywheelIterationResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartFlywheelIteration' smart constructor.
data StartFlywheelIteration = StartFlywheelIteration'
  { -- | A unique identifier for the request. If you don\'t set the client
    -- request token, Amazon Comprehend generates one.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the flywheel.
    flywheelArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartFlywheelIteration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'startFlywheelIteration_clientRequestToken' - A unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend generates one.
--
-- 'flywheelArn', 'startFlywheelIteration_flywheelArn' - The ARN of the flywheel.
newStartFlywheelIteration ::
  -- | 'flywheelArn'
  Prelude.Text ->
  StartFlywheelIteration
newStartFlywheelIteration pFlywheelArn_ =
  StartFlywheelIteration'
    { clientRequestToken =
        Prelude.Nothing,
      flywheelArn = pFlywheelArn_
    }

-- | A unique identifier for the request. If you don\'t set the client
-- request token, Amazon Comprehend generates one.
startFlywheelIteration_clientRequestToken :: Lens.Lens' StartFlywheelIteration (Prelude.Maybe Prelude.Text)
startFlywheelIteration_clientRequestToken = Lens.lens (\StartFlywheelIteration' {clientRequestToken} -> clientRequestToken) (\s@StartFlywheelIteration' {} a -> s {clientRequestToken = a} :: StartFlywheelIteration)

-- | The ARN of the flywheel.
startFlywheelIteration_flywheelArn :: Lens.Lens' StartFlywheelIteration Prelude.Text
startFlywheelIteration_flywheelArn = Lens.lens (\StartFlywheelIteration' {flywheelArn} -> flywheelArn) (\s@StartFlywheelIteration' {} a -> s {flywheelArn = a} :: StartFlywheelIteration)

instance Core.AWSRequest StartFlywheelIteration where
  type
    AWSResponse StartFlywheelIteration =
      StartFlywheelIterationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartFlywheelIterationResponse'
            Prelude.<$> (x Data..?> "FlywheelArn")
            Prelude.<*> (x Data..?> "FlywheelIterationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartFlywheelIteration where
  hashWithSalt _salt StartFlywheelIteration' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` flywheelArn

instance Prelude.NFData StartFlywheelIteration where
  rnf StartFlywheelIteration' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf flywheelArn

instance Data.ToHeaders StartFlywheelIteration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.StartFlywheelIteration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartFlywheelIteration where
  toJSON StartFlywheelIteration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("FlywheelArn" Data..= flywheelArn)
          ]
      )

instance Data.ToPath StartFlywheelIteration where
  toPath = Prelude.const "/"

instance Data.ToQuery StartFlywheelIteration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartFlywheelIterationResponse' smart constructor.
data StartFlywheelIterationResponse = StartFlywheelIterationResponse'
  { flywheelArn :: Prelude.Maybe Prelude.Text,
    flywheelIterationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartFlywheelIterationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flywheelArn', 'startFlywheelIterationResponse_flywheelArn' -
--
-- 'flywheelIterationId', 'startFlywheelIterationResponse_flywheelIterationId' -
--
-- 'httpStatus', 'startFlywheelIterationResponse_httpStatus' - The response's http status code.
newStartFlywheelIterationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartFlywheelIterationResponse
newStartFlywheelIterationResponse pHttpStatus_ =
  StartFlywheelIterationResponse'
    { flywheelArn =
        Prelude.Nothing,
      flywheelIterationId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

startFlywheelIterationResponse_flywheelArn :: Lens.Lens' StartFlywheelIterationResponse (Prelude.Maybe Prelude.Text)
startFlywheelIterationResponse_flywheelArn = Lens.lens (\StartFlywheelIterationResponse' {flywheelArn} -> flywheelArn) (\s@StartFlywheelIterationResponse' {} a -> s {flywheelArn = a} :: StartFlywheelIterationResponse)

startFlywheelIterationResponse_flywheelIterationId :: Lens.Lens' StartFlywheelIterationResponse (Prelude.Maybe Prelude.Text)
startFlywheelIterationResponse_flywheelIterationId = Lens.lens (\StartFlywheelIterationResponse' {flywheelIterationId} -> flywheelIterationId) (\s@StartFlywheelIterationResponse' {} a -> s {flywheelIterationId = a} :: StartFlywheelIterationResponse)

-- | The response's http status code.
startFlywheelIterationResponse_httpStatus :: Lens.Lens' StartFlywheelIterationResponse Prelude.Int
startFlywheelIterationResponse_httpStatus = Lens.lens (\StartFlywheelIterationResponse' {httpStatus} -> httpStatus) (\s@StartFlywheelIterationResponse' {} a -> s {httpStatus = a} :: StartFlywheelIterationResponse)

instance
  Prelude.NFData
    StartFlywheelIterationResponse
  where
  rnf StartFlywheelIterationResponse' {..} =
    Prelude.rnf flywheelArn
      `Prelude.seq` Prelude.rnf flywheelIterationId
      `Prelude.seq` Prelude.rnf httpStatus
