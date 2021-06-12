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
-- Module      : Network.AWS.Comprehend.StopTrainingDocumentClassifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a document classifier training job while in progress.
--
-- If the training job state is @TRAINING@, the job is marked for
-- termination and put into the @STOP_REQUESTED@ state. If the training job
-- completes before it can be stopped, it is put into the @TRAINED@;
-- otherwise the training job is stopped and put into the @STOPPED@ state
-- and the service sends back an HTTP 200 response with an empty HTTP body.
module Network.AWS.Comprehend.StopTrainingDocumentClassifier
  ( -- * Creating a Request
    StopTrainingDocumentClassifier (..),
    newStopTrainingDocumentClassifier,

    -- * Request Lenses
    stopTrainingDocumentClassifier_documentClassifierArn,

    -- * Destructuring the Response
    StopTrainingDocumentClassifierResponse (..),
    newStopTrainingDocumentClassifierResponse,

    -- * Response Lenses
    stopTrainingDocumentClassifierResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopTrainingDocumentClassifier' smart constructor.
data StopTrainingDocumentClassifier = StopTrainingDocumentClassifier'
  { -- | The Amazon Resource Name (ARN) that identifies the document classifier
    -- currently being trained.
    documentClassifierArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopTrainingDocumentClassifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentClassifierArn', 'stopTrainingDocumentClassifier_documentClassifierArn' - The Amazon Resource Name (ARN) that identifies the document classifier
-- currently being trained.
newStopTrainingDocumentClassifier ::
  -- | 'documentClassifierArn'
  Core.Text ->
  StopTrainingDocumentClassifier
newStopTrainingDocumentClassifier
  pDocumentClassifierArn_ =
    StopTrainingDocumentClassifier'
      { documentClassifierArn =
          pDocumentClassifierArn_
      }

-- | The Amazon Resource Name (ARN) that identifies the document classifier
-- currently being trained.
stopTrainingDocumentClassifier_documentClassifierArn :: Lens.Lens' StopTrainingDocumentClassifier Core.Text
stopTrainingDocumentClassifier_documentClassifierArn = Lens.lens (\StopTrainingDocumentClassifier' {documentClassifierArn} -> documentClassifierArn) (\s@StopTrainingDocumentClassifier' {} a -> s {documentClassifierArn = a} :: StopTrainingDocumentClassifier)

instance
  Core.AWSRequest
    StopTrainingDocumentClassifier
  where
  type
    AWSResponse StopTrainingDocumentClassifier =
      StopTrainingDocumentClassifierResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopTrainingDocumentClassifierResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopTrainingDocumentClassifier

instance Core.NFData StopTrainingDocumentClassifier

instance
  Core.ToHeaders
    StopTrainingDocumentClassifier
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.StopTrainingDocumentClassifier" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopTrainingDocumentClassifier where
  toJSON StopTrainingDocumentClassifier' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "DocumentClassifierArn"
                  Core..= documentClassifierArn
              )
          ]
      )

instance Core.ToPath StopTrainingDocumentClassifier where
  toPath = Core.const "/"

instance Core.ToQuery StopTrainingDocumentClassifier where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStopTrainingDocumentClassifierResponse' smart constructor.
data StopTrainingDocumentClassifierResponse = StopTrainingDocumentClassifierResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopTrainingDocumentClassifierResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopTrainingDocumentClassifierResponse_httpStatus' - The response's http status code.
newStopTrainingDocumentClassifierResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StopTrainingDocumentClassifierResponse
newStopTrainingDocumentClassifierResponse
  pHttpStatus_ =
    StopTrainingDocumentClassifierResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
stopTrainingDocumentClassifierResponse_httpStatus :: Lens.Lens' StopTrainingDocumentClassifierResponse Core.Int
stopTrainingDocumentClassifierResponse_httpStatus = Lens.lens (\StopTrainingDocumentClassifierResponse' {httpStatus} -> httpStatus) (\s@StopTrainingDocumentClassifierResponse' {} a -> s {httpStatus = a} :: StopTrainingDocumentClassifierResponse)

instance
  Core.NFData
    StopTrainingDocumentClassifierResponse
