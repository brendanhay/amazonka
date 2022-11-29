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
-- Module      : Amazonka.Textract.StartDocumentTextDetection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the asynchronous detection of text in a document. Amazon Textract
-- can detect lines of text and the words that make up a line of text.
--
-- @StartDocumentTextDetection@ can analyze text in documents that are in
-- JPEG, PNG, TIFF, and PDF format. The documents are stored in an Amazon
-- S3 bucket. Use DocumentLocation to specify the bucket name and file name
-- of the document.
--
-- @StartTextDetection@ returns a job identifier (@JobId@) that you use to
-- get the results of the operation. When text detection is finished,
-- Amazon Textract publishes a completion status to the Amazon Simple
-- Notification Service (Amazon SNS) topic that you specify in
-- @NotificationChannel@. To get the results of the text detection
-- operation, first check that the status value published to the Amazon SNS
-- topic is @SUCCEEDED@. If so, call GetDocumentTextDetection, and pass the
-- job identifier (@JobId@) from the initial call to
-- @StartDocumentTextDetection@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/textract/latest/dg/how-it-works-detecting.html Document Text Detection>.
module Amazonka.Textract.StartDocumentTextDetection
  ( -- * Creating a Request
    StartDocumentTextDetection (..),
    newStartDocumentTextDetection,

    -- * Request Lenses
    startDocumentTextDetection_clientRequestToken,
    startDocumentTextDetection_kmsKeyId,
    startDocumentTextDetection_jobTag,
    startDocumentTextDetection_outputConfig,
    startDocumentTextDetection_notificationChannel,
    startDocumentTextDetection_documentLocation,

    -- * Destructuring the Response
    StartDocumentTextDetectionResponse (..),
    newStartDocumentTextDetectionResponse,

    -- * Response Lenses
    startDocumentTextDetectionResponse_jobId,
    startDocumentTextDetectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Textract.Types

-- | /See:/ 'newStartDocumentTextDetection' smart constructor.
data StartDocumentTextDetection = StartDocumentTextDetection'
  { -- | The idempotent token that\'s used to identify the start request. If you
    -- use the same token with multiple @StartDocumentTextDetection@ requests,
    -- the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the
    -- same job from being accidentally started more than once. For more
    -- information, see
    -- <https://docs.aws.amazon.com/textract/latest/dg/api-async.html Calling Amazon Textract Asynchronous Operations>.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The KMS key used to encrypt the inference results. This can be in either
    -- Key ID or Key Alias format. When a KMS key is provided, the KMS key will
    -- be used for server-side encryption of the objects in the customer
    -- bucket. When this parameter is not enabled, the result will be encrypted
    -- server side,using SSE-S3.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | An identifier that you specify that\'s included in the completion
    -- notification published to the Amazon SNS topic. For example, you can use
    -- @JobTag@ to identify the type of document that the completion
    -- notification corresponds to (such as a tax form or a receipt).
    jobTag :: Prelude.Maybe Prelude.Text,
    -- | Sets if the output will go to a customer defined bucket. By default
    -- Amazon Textract will save the results internally to be accessed with the
    -- GetDocumentTextDetection operation.
    outputConfig :: Prelude.Maybe OutputConfig,
    -- | The Amazon SNS topic ARN that you want Amazon Textract to publish the
    -- completion status of the operation to.
    notificationChannel :: Prelude.Maybe NotificationChannel,
    -- | The location of the document to be processed.
    documentLocation :: DocumentLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartDocumentTextDetection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'startDocumentTextDetection_clientRequestToken' - The idempotent token that\'s used to identify the start request. If you
-- use the same token with multiple @StartDocumentTextDetection@ requests,
-- the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the
-- same job from being accidentally started more than once. For more
-- information, see
-- <https://docs.aws.amazon.com/textract/latest/dg/api-async.html Calling Amazon Textract Asynchronous Operations>.
--
-- 'kmsKeyId', 'startDocumentTextDetection_kmsKeyId' - The KMS key used to encrypt the inference results. This can be in either
-- Key ID or Key Alias format. When a KMS key is provided, the KMS key will
-- be used for server-side encryption of the objects in the customer
-- bucket. When this parameter is not enabled, the result will be encrypted
-- server side,using SSE-S3.
--
-- 'jobTag', 'startDocumentTextDetection_jobTag' - An identifier that you specify that\'s included in the completion
-- notification published to the Amazon SNS topic. For example, you can use
-- @JobTag@ to identify the type of document that the completion
-- notification corresponds to (such as a tax form or a receipt).
--
-- 'outputConfig', 'startDocumentTextDetection_outputConfig' - Sets if the output will go to a customer defined bucket. By default
-- Amazon Textract will save the results internally to be accessed with the
-- GetDocumentTextDetection operation.
--
-- 'notificationChannel', 'startDocumentTextDetection_notificationChannel' - The Amazon SNS topic ARN that you want Amazon Textract to publish the
-- completion status of the operation to.
--
-- 'documentLocation', 'startDocumentTextDetection_documentLocation' - The location of the document to be processed.
newStartDocumentTextDetection ::
  -- | 'documentLocation'
  DocumentLocation ->
  StartDocumentTextDetection
newStartDocumentTextDetection pDocumentLocation_ =
  StartDocumentTextDetection'
    { clientRequestToken =
        Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      jobTag = Prelude.Nothing,
      outputConfig = Prelude.Nothing,
      notificationChannel = Prelude.Nothing,
      documentLocation = pDocumentLocation_
    }

-- | The idempotent token that\'s used to identify the start request. If you
-- use the same token with multiple @StartDocumentTextDetection@ requests,
-- the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the
-- same job from being accidentally started more than once. For more
-- information, see
-- <https://docs.aws.amazon.com/textract/latest/dg/api-async.html Calling Amazon Textract Asynchronous Operations>.
startDocumentTextDetection_clientRequestToken :: Lens.Lens' StartDocumentTextDetection (Prelude.Maybe Prelude.Text)
startDocumentTextDetection_clientRequestToken = Lens.lens (\StartDocumentTextDetection' {clientRequestToken} -> clientRequestToken) (\s@StartDocumentTextDetection' {} a -> s {clientRequestToken = a} :: StartDocumentTextDetection)

-- | The KMS key used to encrypt the inference results. This can be in either
-- Key ID or Key Alias format. When a KMS key is provided, the KMS key will
-- be used for server-side encryption of the objects in the customer
-- bucket. When this parameter is not enabled, the result will be encrypted
-- server side,using SSE-S3.
startDocumentTextDetection_kmsKeyId :: Lens.Lens' StartDocumentTextDetection (Prelude.Maybe Prelude.Text)
startDocumentTextDetection_kmsKeyId = Lens.lens (\StartDocumentTextDetection' {kmsKeyId} -> kmsKeyId) (\s@StartDocumentTextDetection' {} a -> s {kmsKeyId = a} :: StartDocumentTextDetection)

-- | An identifier that you specify that\'s included in the completion
-- notification published to the Amazon SNS topic. For example, you can use
-- @JobTag@ to identify the type of document that the completion
-- notification corresponds to (such as a tax form or a receipt).
startDocumentTextDetection_jobTag :: Lens.Lens' StartDocumentTextDetection (Prelude.Maybe Prelude.Text)
startDocumentTextDetection_jobTag = Lens.lens (\StartDocumentTextDetection' {jobTag} -> jobTag) (\s@StartDocumentTextDetection' {} a -> s {jobTag = a} :: StartDocumentTextDetection)

-- | Sets if the output will go to a customer defined bucket. By default
-- Amazon Textract will save the results internally to be accessed with the
-- GetDocumentTextDetection operation.
startDocumentTextDetection_outputConfig :: Lens.Lens' StartDocumentTextDetection (Prelude.Maybe OutputConfig)
startDocumentTextDetection_outputConfig = Lens.lens (\StartDocumentTextDetection' {outputConfig} -> outputConfig) (\s@StartDocumentTextDetection' {} a -> s {outputConfig = a} :: StartDocumentTextDetection)

-- | The Amazon SNS topic ARN that you want Amazon Textract to publish the
-- completion status of the operation to.
startDocumentTextDetection_notificationChannel :: Lens.Lens' StartDocumentTextDetection (Prelude.Maybe NotificationChannel)
startDocumentTextDetection_notificationChannel = Lens.lens (\StartDocumentTextDetection' {notificationChannel} -> notificationChannel) (\s@StartDocumentTextDetection' {} a -> s {notificationChannel = a} :: StartDocumentTextDetection)

-- | The location of the document to be processed.
startDocumentTextDetection_documentLocation :: Lens.Lens' StartDocumentTextDetection DocumentLocation
startDocumentTextDetection_documentLocation = Lens.lens (\StartDocumentTextDetection' {documentLocation} -> documentLocation) (\s@StartDocumentTextDetection' {} a -> s {documentLocation = a} :: StartDocumentTextDetection)

instance Core.AWSRequest StartDocumentTextDetection where
  type
    AWSResponse StartDocumentTextDetection =
      StartDocumentTextDetectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartDocumentTextDetectionResponse'
            Prelude.<$> (x Core..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartDocumentTextDetection where
  hashWithSalt _salt StartDocumentTextDetection' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` jobTag
      `Prelude.hashWithSalt` outputConfig
      `Prelude.hashWithSalt` notificationChannel
      `Prelude.hashWithSalt` documentLocation

instance Prelude.NFData StartDocumentTextDetection where
  rnf StartDocumentTextDetection' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf jobTag
      `Prelude.seq` Prelude.rnf outputConfig
      `Prelude.seq` Prelude.rnf notificationChannel
      `Prelude.seq` Prelude.rnf documentLocation

instance Core.ToHeaders StartDocumentTextDetection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Textract.StartDocumentTextDetection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartDocumentTextDetection where
  toJSON StartDocumentTextDetection' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            ("KMSKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("JobTag" Core..=) Prelude.<$> jobTag,
            ("OutputConfig" Core..=) Prelude.<$> outputConfig,
            ("NotificationChannel" Core..=)
              Prelude.<$> notificationChannel,
            Prelude.Just
              ("DocumentLocation" Core..= documentLocation)
          ]
      )

instance Core.ToPath StartDocumentTextDetection where
  toPath = Prelude.const "/"

instance Core.ToQuery StartDocumentTextDetection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartDocumentTextDetectionResponse' smart constructor.
data StartDocumentTextDetectionResponse = StartDocumentTextDetectionResponse'
  { -- | The identifier of the text detection job for the document. Use @JobId@
    -- to identify the job in a subsequent call to @GetDocumentTextDetection@.
    -- A @JobId@ value is only valid for 7 days.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartDocumentTextDetectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'startDocumentTextDetectionResponse_jobId' - The identifier of the text detection job for the document. Use @JobId@
-- to identify the job in a subsequent call to @GetDocumentTextDetection@.
-- A @JobId@ value is only valid for 7 days.
--
-- 'httpStatus', 'startDocumentTextDetectionResponse_httpStatus' - The response's http status code.
newStartDocumentTextDetectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartDocumentTextDetectionResponse
newStartDocumentTextDetectionResponse pHttpStatus_ =
  StartDocumentTextDetectionResponse'
    { jobId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the text detection job for the document. Use @JobId@
-- to identify the job in a subsequent call to @GetDocumentTextDetection@.
-- A @JobId@ value is only valid for 7 days.
startDocumentTextDetectionResponse_jobId :: Lens.Lens' StartDocumentTextDetectionResponse (Prelude.Maybe Prelude.Text)
startDocumentTextDetectionResponse_jobId = Lens.lens (\StartDocumentTextDetectionResponse' {jobId} -> jobId) (\s@StartDocumentTextDetectionResponse' {} a -> s {jobId = a} :: StartDocumentTextDetectionResponse)

-- | The response's http status code.
startDocumentTextDetectionResponse_httpStatus :: Lens.Lens' StartDocumentTextDetectionResponse Prelude.Int
startDocumentTextDetectionResponse_httpStatus = Lens.lens (\StartDocumentTextDetectionResponse' {httpStatus} -> httpStatus) (\s@StartDocumentTextDetectionResponse' {} a -> s {httpStatus = a} :: StartDocumentTextDetectionResponse)

instance
  Prelude.NFData
    StartDocumentTextDetectionResponse
  where
  rnf StartDocumentTextDetectionResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf httpStatus
