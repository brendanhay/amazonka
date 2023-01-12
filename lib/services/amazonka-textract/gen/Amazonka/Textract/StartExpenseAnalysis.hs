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
-- Module      : Amazonka.Textract.StartExpenseAnalysis
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the asynchronous analysis of invoices or receipts for data like
-- contact information, items purchased, and vendor names.
--
-- @StartExpenseAnalysis@ can analyze text in documents that are in JPEG,
-- PNG, and PDF format. The documents must be stored in an Amazon S3
-- bucket. Use the DocumentLocation parameter to specify the name of your
-- S3 bucket and the name of the document in that bucket.
--
-- @StartExpenseAnalysis@ returns a job identifier (@JobId@) that you will
-- provide to @GetExpenseAnalysis@ to retrieve the results of the
-- operation. When the analysis of the input invoices\/receipts is
-- finished, Amazon Textract publishes a completion status to the Amazon
-- Simple Notification Service (Amazon SNS) topic that you provide to the
-- @NotificationChannel@. To obtain the results of the invoice and receipt
-- analysis operation, ensure that the status value published to the Amazon
-- SNS topic is @SUCCEEDED@. If so, call GetExpenseAnalysis, and pass the
-- job identifier (@JobId@) that was returned by your call to
-- @StartExpenseAnalysis@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/textract/latest/dg/invoice-receipts.html Analyzing Invoices and Receipts>.
module Amazonka.Textract.StartExpenseAnalysis
  ( -- * Creating a Request
    StartExpenseAnalysis (..),
    newStartExpenseAnalysis,

    -- * Request Lenses
    startExpenseAnalysis_clientRequestToken,
    startExpenseAnalysis_jobTag,
    startExpenseAnalysis_kmsKeyId,
    startExpenseAnalysis_notificationChannel,
    startExpenseAnalysis_outputConfig,
    startExpenseAnalysis_documentLocation,

    -- * Destructuring the Response
    StartExpenseAnalysisResponse (..),
    newStartExpenseAnalysisResponse,

    -- * Response Lenses
    startExpenseAnalysisResponse_jobId,
    startExpenseAnalysisResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Textract.Types

-- | /See:/ 'newStartExpenseAnalysis' smart constructor.
data StartExpenseAnalysis = StartExpenseAnalysis'
  { -- | The idempotent token that\'s used to identify the start request. If you
    -- use the same token with multiple @StartDocumentTextDetection@ requests,
    -- the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the
    -- same job from being accidentally started more than once. For more
    -- information, see
    -- <https://docs.aws.amazon.com/textract/latest/dg/api-async.html Calling Amazon Textract Asynchronous Operations>
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | An identifier you specify that\'s included in the completion
    -- notification published to the Amazon SNS topic. For example, you can use
    -- @JobTag@ to identify the type of document that the completion
    -- notification corresponds to (such as a tax form or a receipt).
    jobTag :: Prelude.Maybe Prelude.Text,
    -- | The KMS key used to encrypt the inference results. This can be in either
    -- Key ID or Key Alias format. When a KMS key is provided, the KMS key will
    -- be used for server-side encryption of the objects in the customer
    -- bucket. When this parameter is not enabled, the result will be encrypted
    -- server side,using SSE-S3.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon SNS topic ARN that you want Amazon Textract to publish the
    -- completion status of the operation to.
    notificationChannel :: Prelude.Maybe NotificationChannel,
    -- | Sets if the output will go to a customer defined bucket. By default,
    -- Amazon Textract will save the results internally to be accessed by the
    -- @GetExpenseAnalysis@ operation.
    outputConfig :: Prelude.Maybe OutputConfig,
    -- | The location of the document to be processed.
    documentLocation :: DocumentLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartExpenseAnalysis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'startExpenseAnalysis_clientRequestToken' - The idempotent token that\'s used to identify the start request. If you
-- use the same token with multiple @StartDocumentTextDetection@ requests,
-- the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the
-- same job from being accidentally started more than once. For more
-- information, see
-- <https://docs.aws.amazon.com/textract/latest/dg/api-async.html Calling Amazon Textract Asynchronous Operations>
--
-- 'jobTag', 'startExpenseAnalysis_jobTag' - An identifier you specify that\'s included in the completion
-- notification published to the Amazon SNS topic. For example, you can use
-- @JobTag@ to identify the type of document that the completion
-- notification corresponds to (such as a tax form or a receipt).
--
-- 'kmsKeyId', 'startExpenseAnalysis_kmsKeyId' - The KMS key used to encrypt the inference results. This can be in either
-- Key ID or Key Alias format. When a KMS key is provided, the KMS key will
-- be used for server-side encryption of the objects in the customer
-- bucket. When this parameter is not enabled, the result will be encrypted
-- server side,using SSE-S3.
--
-- 'notificationChannel', 'startExpenseAnalysis_notificationChannel' - The Amazon SNS topic ARN that you want Amazon Textract to publish the
-- completion status of the operation to.
--
-- 'outputConfig', 'startExpenseAnalysis_outputConfig' - Sets if the output will go to a customer defined bucket. By default,
-- Amazon Textract will save the results internally to be accessed by the
-- @GetExpenseAnalysis@ operation.
--
-- 'documentLocation', 'startExpenseAnalysis_documentLocation' - The location of the document to be processed.
newStartExpenseAnalysis ::
  -- | 'documentLocation'
  DocumentLocation ->
  StartExpenseAnalysis
newStartExpenseAnalysis pDocumentLocation_ =
  StartExpenseAnalysis'
    { clientRequestToken =
        Prelude.Nothing,
      jobTag = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      notificationChannel = Prelude.Nothing,
      outputConfig = Prelude.Nothing,
      documentLocation = pDocumentLocation_
    }

-- | The idempotent token that\'s used to identify the start request. If you
-- use the same token with multiple @StartDocumentTextDetection@ requests,
-- the same @JobId@ is returned. Use @ClientRequestToken@ to prevent the
-- same job from being accidentally started more than once. For more
-- information, see
-- <https://docs.aws.amazon.com/textract/latest/dg/api-async.html Calling Amazon Textract Asynchronous Operations>
startExpenseAnalysis_clientRequestToken :: Lens.Lens' StartExpenseAnalysis (Prelude.Maybe Prelude.Text)
startExpenseAnalysis_clientRequestToken = Lens.lens (\StartExpenseAnalysis' {clientRequestToken} -> clientRequestToken) (\s@StartExpenseAnalysis' {} a -> s {clientRequestToken = a} :: StartExpenseAnalysis)

-- | An identifier you specify that\'s included in the completion
-- notification published to the Amazon SNS topic. For example, you can use
-- @JobTag@ to identify the type of document that the completion
-- notification corresponds to (such as a tax form or a receipt).
startExpenseAnalysis_jobTag :: Lens.Lens' StartExpenseAnalysis (Prelude.Maybe Prelude.Text)
startExpenseAnalysis_jobTag = Lens.lens (\StartExpenseAnalysis' {jobTag} -> jobTag) (\s@StartExpenseAnalysis' {} a -> s {jobTag = a} :: StartExpenseAnalysis)

-- | The KMS key used to encrypt the inference results. This can be in either
-- Key ID or Key Alias format. When a KMS key is provided, the KMS key will
-- be used for server-side encryption of the objects in the customer
-- bucket. When this parameter is not enabled, the result will be encrypted
-- server side,using SSE-S3.
startExpenseAnalysis_kmsKeyId :: Lens.Lens' StartExpenseAnalysis (Prelude.Maybe Prelude.Text)
startExpenseAnalysis_kmsKeyId = Lens.lens (\StartExpenseAnalysis' {kmsKeyId} -> kmsKeyId) (\s@StartExpenseAnalysis' {} a -> s {kmsKeyId = a} :: StartExpenseAnalysis)

-- | The Amazon SNS topic ARN that you want Amazon Textract to publish the
-- completion status of the operation to.
startExpenseAnalysis_notificationChannel :: Lens.Lens' StartExpenseAnalysis (Prelude.Maybe NotificationChannel)
startExpenseAnalysis_notificationChannel = Lens.lens (\StartExpenseAnalysis' {notificationChannel} -> notificationChannel) (\s@StartExpenseAnalysis' {} a -> s {notificationChannel = a} :: StartExpenseAnalysis)

-- | Sets if the output will go to a customer defined bucket. By default,
-- Amazon Textract will save the results internally to be accessed by the
-- @GetExpenseAnalysis@ operation.
startExpenseAnalysis_outputConfig :: Lens.Lens' StartExpenseAnalysis (Prelude.Maybe OutputConfig)
startExpenseAnalysis_outputConfig = Lens.lens (\StartExpenseAnalysis' {outputConfig} -> outputConfig) (\s@StartExpenseAnalysis' {} a -> s {outputConfig = a} :: StartExpenseAnalysis)

-- | The location of the document to be processed.
startExpenseAnalysis_documentLocation :: Lens.Lens' StartExpenseAnalysis DocumentLocation
startExpenseAnalysis_documentLocation = Lens.lens (\StartExpenseAnalysis' {documentLocation} -> documentLocation) (\s@StartExpenseAnalysis' {} a -> s {documentLocation = a} :: StartExpenseAnalysis)

instance Core.AWSRequest StartExpenseAnalysis where
  type
    AWSResponse StartExpenseAnalysis =
      StartExpenseAnalysisResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartExpenseAnalysisResponse'
            Prelude.<$> (x Data..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartExpenseAnalysis where
  hashWithSalt _salt StartExpenseAnalysis' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` jobTag
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` notificationChannel
      `Prelude.hashWithSalt` outputConfig
      `Prelude.hashWithSalt` documentLocation

instance Prelude.NFData StartExpenseAnalysis where
  rnf StartExpenseAnalysis' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf jobTag
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf notificationChannel
      `Prelude.seq` Prelude.rnf outputConfig
      `Prelude.seq` Prelude.rnf documentLocation

instance Data.ToHeaders StartExpenseAnalysis where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Textract.StartExpenseAnalysis" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartExpenseAnalysis where
  toJSON StartExpenseAnalysis' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("JobTag" Data..=) Prelude.<$> jobTag,
            ("KMSKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("NotificationChannel" Data..=)
              Prelude.<$> notificationChannel,
            ("OutputConfig" Data..=) Prelude.<$> outputConfig,
            Prelude.Just
              ("DocumentLocation" Data..= documentLocation)
          ]
      )

instance Data.ToPath StartExpenseAnalysis where
  toPath = Prelude.const "/"

instance Data.ToQuery StartExpenseAnalysis where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartExpenseAnalysisResponse' smart constructor.
data StartExpenseAnalysisResponse = StartExpenseAnalysisResponse'
  { -- | A unique identifier for the text detection job. The @JobId@ is returned
    -- from @StartExpenseAnalysis@. A @JobId@ value is only valid for 7 days.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartExpenseAnalysisResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'startExpenseAnalysisResponse_jobId' - A unique identifier for the text detection job. The @JobId@ is returned
-- from @StartExpenseAnalysis@. A @JobId@ value is only valid for 7 days.
--
-- 'httpStatus', 'startExpenseAnalysisResponse_httpStatus' - The response's http status code.
newStartExpenseAnalysisResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartExpenseAnalysisResponse
newStartExpenseAnalysisResponse pHttpStatus_ =
  StartExpenseAnalysisResponse'
    { jobId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier for the text detection job. The @JobId@ is returned
-- from @StartExpenseAnalysis@. A @JobId@ value is only valid for 7 days.
startExpenseAnalysisResponse_jobId :: Lens.Lens' StartExpenseAnalysisResponse (Prelude.Maybe Prelude.Text)
startExpenseAnalysisResponse_jobId = Lens.lens (\StartExpenseAnalysisResponse' {jobId} -> jobId) (\s@StartExpenseAnalysisResponse' {} a -> s {jobId = a} :: StartExpenseAnalysisResponse)

-- | The response's http status code.
startExpenseAnalysisResponse_httpStatus :: Lens.Lens' StartExpenseAnalysisResponse Prelude.Int
startExpenseAnalysisResponse_httpStatus = Lens.lens (\StartExpenseAnalysisResponse' {httpStatus} -> httpStatus) (\s@StartExpenseAnalysisResponse' {} a -> s {httpStatus = a} :: StartExpenseAnalysisResponse)

instance Prelude.NFData StartExpenseAnalysisResponse where
  rnf StartExpenseAnalysisResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf httpStatus
