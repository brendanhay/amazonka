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
-- Module      : Amazonka.Comprehend.StopTrainingEntityRecognizer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an entity recognizer training job while in progress.
--
-- If the training job state is @TRAINING@, the job is marked for
-- termination and put into the @STOP_REQUESTED@ state. If the training job
-- completes before it can be stopped, it is put into the @TRAINED@;
-- otherwise the training job is stopped and putted into the @STOPPED@
-- state and the service sends back an HTTP 200 response with an empty HTTP
-- body.
module Amazonka.Comprehend.StopTrainingEntityRecognizer
  ( -- * Creating a Request
    StopTrainingEntityRecognizer (..),
    newStopTrainingEntityRecognizer,

    -- * Request Lenses
    stopTrainingEntityRecognizer_entityRecognizerArn,

    -- * Destructuring the Response
    StopTrainingEntityRecognizerResponse (..),
    newStopTrainingEntityRecognizerResponse,

    -- * Response Lenses
    stopTrainingEntityRecognizerResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopTrainingEntityRecognizer' smart constructor.
data StopTrainingEntityRecognizer = StopTrainingEntityRecognizer'
  { -- | The Amazon Resource Name (ARN) that identifies the entity recognizer
    -- currently being trained.
    entityRecognizerArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopTrainingEntityRecognizer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entityRecognizerArn', 'stopTrainingEntityRecognizer_entityRecognizerArn' - The Amazon Resource Name (ARN) that identifies the entity recognizer
-- currently being trained.
newStopTrainingEntityRecognizer ::
  -- | 'entityRecognizerArn'
  Prelude.Text ->
  StopTrainingEntityRecognizer
newStopTrainingEntityRecognizer pEntityRecognizerArn_ =
  StopTrainingEntityRecognizer'
    { entityRecognizerArn =
        pEntityRecognizerArn_
    }

-- | The Amazon Resource Name (ARN) that identifies the entity recognizer
-- currently being trained.
stopTrainingEntityRecognizer_entityRecognizerArn :: Lens.Lens' StopTrainingEntityRecognizer Prelude.Text
stopTrainingEntityRecognizer_entityRecognizerArn = Lens.lens (\StopTrainingEntityRecognizer' {entityRecognizerArn} -> entityRecognizerArn) (\s@StopTrainingEntityRecognizer' {} a -> s {entityRecognizerArn = a} :: StopTrainingEntityRecognizer)

instance Core.AWSRequest StopTrainingEntityRecognizer where
  type
    AWSResponse StopTrainingEntityRecognizer =
      StopTrainingEntityRecognizerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopTrainingEntityRecognizerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StopTrainingEntityRecognizer
  where
  hashWithSalt _salt StopTrainingEntityRecognizer' {..} =
    _salt `Prelude.hashWithSalt` entityRecognizerArn

instance Prelude.NFData StopTrainingEntityRecognizer where
  rnf StopTrainingEntityRecognizer' {..} =
    Prelude.rnf entityRecognizerArn

instance Data.ToHeaders StopTrainingEntityRecognizer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.StopTrainingEntityRecognizer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopTrainingEntityRecognizer where
  toJSON StopTrainingEntityRecognizer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("EntityRecognizerArn" Data..= entityRecognizerArn)
          ]
      )

instance Data.ToPath StopTrainingEntityRecognizer where
  toPath = Prelude.const "/"

instance Data.ToQuery StopTrainingEntityRecognizer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopTrainingEntityRecognizerResponse' smart constructor.
data StopTrainingEntityRecognizerResponse = StopTrainingEntityRecognizerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopTrainingEntityRecognizerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopTrainingEntityRecognizerResponse_httpStatus' - The response's http status code.
newStopTrainingEntityRecognizerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopTrainingEntityRecognizerResponse
newStopTrainingEntityRecognizerResponse pHttpStatus_ =
  StopTrainingEntityRecognizerResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
stopTrainingEntityRecognizerResponse_httpStatus :: Lens.Lens' StopTrainingEntityRecognizerResponse Prelude.Int
stopTrainingEntityRecognizerResponse_httpStatus = Lens.lens (\StopTrainingEntityRecognizerResponse' {httpStatus} -> httpStatus) (\s@StopTrainingEntityRecognizerResponse' {} a -> s {httpStatus = a} :: StopTrainingEntityRecognizerResponse)

instance
  Prelude.NFData
    StopTrainingEntityRecognizerResponse
  where
  rnf StopTrainingEntityRecognizerResponse' {..} =
    Prelude.rnf httpStatus
