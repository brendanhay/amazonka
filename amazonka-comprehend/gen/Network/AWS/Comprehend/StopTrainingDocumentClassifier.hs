{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopTrainingDocumentClassifier' smart constructor.
data StopTrainingDocumentClassifier = StopTrainingDocumentClassifier'
  { -- | The Amazon Resource Name (ARN) that identifies the document classifier
    -- currently being trained.
    documentClassifierArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  StopTrainingDocumentClassifier
newStopTrainingDocumentClassifier
  pDocumentClassifierArn_ =
    StopTrainingDocumentClassifier'
      { documentClassifierArn =
          pDocumentClassifierArn_
      }

-- | The Amazon Resource Name (ARN) that identifies the document classifier
-- currently being trained.
stopTrainingDocumentClassifier_documentClassifierArn :: Lens.Lens' StopTrainingDocumentClassifier Prelude.Text
stopTrainingDocumentClassifier_documentClassifierArn = Lens.lens (\StopTrainingDocumentClassifier' {documentClassifierArn} -> documentClassifierArn) (\s@StopTrainingDocumentClassifier' {} a -> s {documentClassifierArn = a} :: StopTrainingDocumentClassifier)

instance
  Prelude.AWSRequest
    StopTrainingDocumentClassifier
  where
  type
    Rs StopTrainingDocumentClassifier =
      StopTrainingDocumentClassifierResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopTrainingDocumentClassifierResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StopTrainingDocumentClassifier

instance
  Prelude.NFData
    StopTrainingDocumentClassifier

instance
  Prelude.ToHeaders
    StopTrainingDocumentClassifier
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Comprehend_20171127.StopTrainingDocumentClassifier" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    StopTrainingDocumentClassifier
  where
  toJSON StopTrainingDocumentClassifier' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "DocumentClassifierArn"
                  Prelude..= documentClassifierArn
              )
          ]
      )

instance
  Prelude.ToPath
    StopTrainingDocumentClassifier
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    StopTrainingDocumentClassifier
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopTrainingDocumentClassifierResponse' smart constructor.
data StopTrainingDocumentClassifierResponse = StopTrainingDocumentClassifierResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  StopTrainingDocumentClassifierResponse
newStopTrainingDocumentClassifierResponse
  pHttpStatus_ =
    StopTrainingDocumentClassifierResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
stopTrainingDocumentClassifierResponse_httpStatus :: Lens.Lens' StopTrainingDocumentClassifierResponse Prelude.Int
stopTrainingDocumentClassifierResponse_httpStatus = Lens.lens (\StopTrainingDocumentClassifierResponse' {httpStatus} -> httpStatus) (\s@StopTrainingDocumentClassifierResponse' {} a -> s {httpStatus = a} :: StopTrainingDocumentClassifierResponse)

instance
  Prelude.NFData
    StopTrainingDocumentClassifierResponse
