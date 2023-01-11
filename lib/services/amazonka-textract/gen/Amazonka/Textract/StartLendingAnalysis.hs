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
-- Module      : Amazonka.Textract.StartLendingAnalysis
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the classification and analysis of an input document.
-- @StartLendingAnalysis@ initiates the classification and analysis of a
-- packet of lending documents. @StartLendingAnalysis@ operates on a
-- document file located in an Amazon S3 bucket.
--
-- @StartLendingAnalysis@ can analyze text in documents that are in one of
-- the following formats: JPEG, PNG, TIFF, PDF. Use @DocumentLocation@ to
-- specify the bucket name and the file name of the document.
--
-- @StartLendingAnalysis@ returns a job identifier (@JobId@) that you use
-- to get the results of the operation. When the text analysis is finished,
-- Amazon Textract publishes a completion status to the Amazon Simple
-- Notification Service (Amazon SNS) topic that you specify in
-- @NotificationChannel@. To get the results of the text analysis
-- operation, first check that the status value published to the Amazon SNS
-- topic is SUCCEEDED. If the status is SUCCEEDED you can call either
-- @GetLendingAnalysis@ or @GetLendingAnalysisSummary@ and provide the
-- @JobId@ to obtain the results of the analysis.
--
-- If using @OutputConfig@ to specify an Amazon S3 bucket, the output will
-- be contained within the specified prefix in a directory labeled with the
-- job-id. In the directory there are 3 sub-directories:
--
-- -   detailedResponse (contains the GetLendingAnalysis response)
--
-- -   summaryResponse (for the GetLendingAnalysisSummary response)
--
-- -   splitDocuments (documents split across logical boundaries)
module Amazonka.Textract.StartLendingAnalysis
  ( -- * Creating a Request
    StartLendingAnalysis (..),
    newStartLendingAnalysis,

    -- * Request Lenses
    startLendingAnalysis_clientRequestToken,
    startLendingAnalysis_jobTag,
    startLendingAnalysis_kmsKeyId,
    startLendingAnalysis_notificationChannel,
    startLendingAnalysis_outputConfig,
    startLendingAnalysis_documentLocation,

    -- * Destructuring the Response
    StartLendingAnalysisResponse (..),
    newStartLendingAnalysisResponse,

    -- * Response Lenses
    startLendingAnalysisResponse_jobId,
    startLendingAnalysisResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Textract.Types

-- | /See:/ 'newStartLendingAnalysis' smart constructor.
data StartLendingAnalysis = StartLendingAnalysis'
  { -- | The idempotent token that you use to identify the start request. If you
    -- use the same token with multiple @StartLendingAnalysis@ requests, the
    -- same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same
    -- job from being accidentally started more than once. For more
    -- information, see
    -- <https://docs.aws.amazon.com/textract/latest/dg/api-sync.html Calling Amazon Textract Asynchronous Operations>.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | An identifier that you specify to be included in the completion
    -- notification published to the Amazon SNS topic. For example, you can use
    -- @JobTag@ to identify the type of document that the completion
    -- notification corresponds to (such as a tax form or a receipt).
    jobTag :: Prelude.Maybe Prelude.Text,
    -- | The KMS key used to encrypt the inference results. This can be in either
    -- Key ID or Key Alias format. When a KMS key is provided, the KMS key will
    -- be used for server-side encryption of the objects in the customer
    -- bucket. When this parameter is not enabled, the result will be encrypted
    -- server side, using SSE-S3.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    notificationChannel :: Prelude.Maybe NotificationChannel,
    outputConfig :: Prelude.Maybe OutputConfig,
    documentLocation :: DocumentLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartLendingAnalysis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'startLendingAnalysis_clientRequestToken' - The idempotent token that you use to identify the start request. If you
-- use the same token with multiple @StartLendingAnalysis@ requests, the
-- same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same
-- job from being accidentally started more than once. For more
-- information, see
-- <https://docs.aws.amazon.com/textract/latest/dg/api-sync.html Calling Amazon Textract Asynchronous Operations>.
--
-- 'jobTag', 'startLendingAnalysis_jobTag' - An identifier that you specify to be included in the completion
-- notification published to the Amazon SNS topic. For example, you can use
-- @JobTag@ to identify the type of document that the completion
-- notification corresponds to (such as a tax form or a receipt).
--
-- 'kmsKeyId', 'startLendingAnalysis_kmsKeyId' - The KMS key used to encrypt the inference results. This can be in either
-- Key ID or Key Alias format. When a KMS key is provided, the KMS key will
-- be used for server-side encryption of the objects in the customer
-- bucket. When this parameter is not enabled, the result will be encrypted
-- server side, using SSE-S3.
--
-- 'notificationChannel', 'startLendingAnalysis_notificationChannel' - Undocumented member.
--
-- 'outputConfig', 'startLendingAnalysis_outputConfig' - Undocumented member.
--
-- 'documentLocation', 'startLendingAnalysis_documentLocation' - Undocumented member.
newStartLendingAnalysis ::
  -- | 'documentLocation'
  DocumentLocation ->
  StartLendingAnalysis
newStartLendingAnalysis pDocumentLocation_ =
  StartLendingAnalysis'
    { clientRequestToken =
        Prelude.Nothing,
      jobTag = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      notificationChannel = Prelude.Nothing,
      outputConfig = Prelude.Nothing,
      documentLocation = pDocumentLocation_
    }

-- | The idempotent token that you use to identify the start request. If you
-- use the same token with multiple @StartLendingAnalysis@ requests, the
-- same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same
-- job from being accidentally started more than once. For more
-- information, see
-- <https://docs.aws.amazon.com/textract/latest/dg/api-sync.html Calling Amazon Textract Asynchronous Operations>.
startLendingAnalysis_clientRequestToken :: Lens.Lens' StartLendingAnalysis (Prelude.Maybe Prelude.Text)
startLendingAnalysis_clientRequestToken = Lens.lens (\StartLendingAnalysis' {clientRequestToken} -> clientRequestToken) (\s@StartLendingAnalysis' {} a -> s {clientRequestToken = a} :: StartLendingAnalysis)

-- | An identifier that you specify to be included in the completion
-- notification published to the Amazon SNS topic. For example, you can use
-- @JobTag@ to identify the type of document that the completion
-- notification corresponds to (such as a tax form or a receipt).
startLendingAnalysis_jobTag :: Lens.Lens' StartLendingAnalysis (Prelude.Maybe Prelude.Text)
startLendingAnalysis_jobTag = Lens.lens (\StartLendingAnalysis' {jobTag} -> jobTag) (\s@StartLendingAnalysis' {} a -> s {jobTag = a} :: StartLendingAnalysis)

-- | The KMS key used to encrypt the inference results. This can be in either
-- Key ID or Key Alias format. When a KMS key is provided, the KMS key will
-- be used for server-side encryption of the objects in the customer
-- bucket. When this parameter is not enabled, the result will be encrypted
-- server side, using SSE-S3.
startLendingAnalysis_kmsKeyId :: Lens.Lens' StartLendingAnalysis (Prelude.Maybe Prelude.Text)
startLendingAnalysis_kmsKeyId = Lens.lens (\StartLendingAnalysis' {kmsKeyId} -> kmsKeyId) (\s@StartLendingAnalysis' {} a -> s {kmsKeyId = a} :: StartLendingAnalysis)

-- | Undocumented member.
startLendingAnalysis_notificationChannel :: Lens.Lens' StartLendingAnalysis (Prelude.Maybe NotificationChannel)
startLendingAnalysis_notificationChannel = Lens.lens (\StartLendingAnalysis' {notificationChannel} -> notificationChannel) (\s@StartLendingAnalysis' {} a -> s {notificationChannel = a} :: StartLendingAnalysis)

-- | Undocumented member.
startLendingAnalysis_outputConfig :: Lens.Lens' StartLendingAnalysis (Prelude.Maybe OutputConfig)
startLendingAnalysis_outputConfig = Lens.lens (\StartLendingAnalysis' {outputConfig} -> outputConfig) (\s@StartLendingAnalysis' {} a -> s {outputConfig = a} :: StartLendingAnalysis)

-- | Undocumented member.
startLendingAnalysis_documentLocation :: Lens.Lens' StartLendingAnalysis DocumentLocation
startLendingAnalysis_documentLocation = Lens.lens (\StartLendingAnalysis' {documentLocation} -> documentLocation) (\s@StartLendingAnalysis' {} a -> s {documentLocation = a} :: StartLendingAnalysis)

instance Core.AWSRequest StartLendingAnalysis where
  type
    AWSResponse StartLendingAnalysis =
      StartLendingAnalysisResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartLendingAnalysisResponse'
            Prelude.<$> (x Data..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartLendingAnalysis where
  hashWithSalt _salt StartLendingAnalysis' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` jobTag
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` notificationChannel
      `Prelude.hashWithSalt` outputConfig
      `Prelude.hashWithSalt` documentLocation

instance Prelude.NFData StartLendingAnalysis where
  rnf StartLendingAnalysis' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf jobTag
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf notificationChannel
      `Prelude.seq` Prelude.rnf outputConfig
      `Prelude.seq` Prelude.rnf documentLocation

instance Data.ToHeaders StartLendingAnalysis where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Textract.StartLendingAnalysis" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartLendingAnalysis where
  toJSON StartLendingAnalysis' {..} =
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

instance Data.ToPath StartLendingAnalysis where
  toPath = Prelude.const "/"

instance Data.ToQuery StartLendingAnalysis where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartLendingAnalysisResponse' smart constructor.
data StartLendingAnalysisResponse = StartLendingAnalysisResponse'
  { -- | A unique identifier for the lending or text-detection job. The @JobId@
    -- is returned from @StartLendingAnalysis@. A @JobId@ value is only valid
    -- for 7 days.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartLendingAnalysisResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'startLendingAnalysisResponse_jobId' - A unique identifier for the lending or text-detection job. The @JobId@
-- is returned from @StartLendingAnalysis@. A @JobId@ value is only valid
-- for 7 days.
--
-- 'httpStatus', 'startLendingAnalysisResponse_httpStatus' - The response's http status code.
newStartLendingAnalysisResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartLendingAnalysisResponse
newStartLendingAnalysisResponse pHttpStatus_ =
  StartLendingAnalysisResponse'
    { jobId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier for the lending or text-detection job. The @JobId@
-- is returned from @StartLendingAnalysis@. A @JobId@ value is only valid
-- for 7 days.
startLendingAnalysisResponse_jobId :: Lens.Lens' StartLendingAnalysisResponse (Prelude.Maybe Prelude.Text)
startLendingAnalysisResponse_jobId = Lens.lens (\StartLendingAnalysisResponse' {jobId} -> jobId) (\s@StartLendingAnalysisResponse' {} a -> s {jobId = a} :: StartLendingAnalysisResponse)

-- | The response's http status code.
startLendingAnalysisResponse_httpStatus :: Lens.Lens' StartLendingAnalysisResponse Prelude.Int
startLendingAnalysisResponse_httpStatus = Lens.lens (\StartLendingAnalysisResponse' {httpStatus} -> httpStatus) (\s@StartLendingAnalysisResponse' {} a -> s {httpStatus = a} :: StartLendingAnalysisResponse)

instance Prelude.NFData StartLendingAnalysisResponse where
  rnf StartLendingAnalysisResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf httpStatus
