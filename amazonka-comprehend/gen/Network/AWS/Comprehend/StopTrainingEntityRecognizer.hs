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
-- Module      : Network.AWS.Comprehend.StopTrainingEntityRecognizer
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Comprehend.StopTrainingEntityRecognizer
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

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopTrainingEntityRecognizer' smart constructor.
data StopTrainingEntityRecognizer = StopTrainingEntityRecognizer'
  { -- | The Amazon Resource Name (ARN) that identifies the entity recognizer
    -- currently being trained.
    entityRecognizerArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  StopTrainingEntityRecognizer
newStopTrainingEntityRecognizer pEntityRecognizerArn_ =
  StopTrainingEntityRecognizer'
    { entityRecognizerArn =
        pEntityRecognizerArn_
    }

-- | The Amazon Resource Name (ARN) that identifies the entity recognizer
-- currently being trained.
stopTrainingEntityRecognizer_entityRecognizerArn :: Lens.Lens' StopTrainingEntityRecognizer Core.Text
stopTrainingEntityRecognizer_entityRecognizerArn = Lens.lens (\StopTrainingEntityRecognizer' {entityRecognizerArn} -> entityRecognizerArn) (\s@StopTrainingEntityRecognizer' {} a -> s {entityRecognizerArn = a} :: StopTrainingEntityRecognizer)

instance Core.AWSRequest StopTrainingEntityRecognizer where
  type
    AWSResponse StopTrainingEntityRecognizer =
      StopTrainingEntityRecognizerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopTrainingEntityRecognizerResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopTrainingEntityRecognizer

instance Core.NFData StopTrainingEntityRecognizer

instance Core.ToHeaders StopTrainingEntityRecognizer where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.StopTrainingEntityRecognizer" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopTrainingEntityRecognizer where
  toJSON StopTrainingEntityRecognizer' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("EntityRecognizerArn" Core..= entityRecognizerArn)
          ]
      )

instance Core.ToPath StopTrainingEntityRecognizer where
  toPath = Core.const "/"

instance Core.ToQuery StopTrainingEntityRecognizer where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStopTrainingEntityRecognizerResponse' smart constructor.
data StopTrainingEntityRecognizerResponse = StopTrainingEntityRecognizerResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  StopTrainingEntityRecognizerResponse
newStopTrainingEntityRecognizerResponse pHttpStatus_ =
  StopTrainingEntityRecognizerResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
stopTrainingEntityRecognizerResponse_httpStatus :: Lens.Lens' StopTrainingEntityRecognizerResponse Core.Int
stopTrainingEntityRecognizerResponse_httpStatus = Lens.lens (\StopTrainingEntityRecognizerResponse' {httpStatus} -> httpStatus) (\s@StopTrainingEntityRecognizerResponse' {} a -> s {httpStatus = a} :: StopTrainingEntityRecognizerResponse)

instance
  Core.NFData
    StopTrainingEntityRecognizerResponse
