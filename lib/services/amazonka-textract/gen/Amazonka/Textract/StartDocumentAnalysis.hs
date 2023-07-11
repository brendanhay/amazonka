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
-- Module      : Amazonka.Textract.StartDocumentAnalysis
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the asynchronous analysis of an input document for relationships
-- between detected items such as key-value pairs, tables, and selection
-- elements.
--
-- @StartDocumentAnalysis@ can analyze text in documents that are in JPEG,
-- PNG, TIFF, and PDF format. The documents are stored in an Amazon S3
-- bucket. Use DocumentLocation to specify the bucket name and file name of
-- the document.
--
-- @StartDocumentAnalysis@ returns a job identifier (@JobId@) that you use
-- to get the results of the operation. When text analysis is finished,
-- Amazon Textract publishes a completion status to the Amazon Simple
-- Notification Service (Amazon SNS) topic that you specify in
-- @NotificationChannel@. To get the results of the text analysis
-- operation, first check that the status value published to the Amazon SNS
-- topic is @SUCCEEDED@. If so, call GetDocumentAnalysis, and pass the job
-- identifier (@JobId@) from the initial call to @StartDocumentAnalysis@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/textract/latest/dg/how-it-works-analyzing.html Document Text Analysis>.
module Amazonka.Textract.StartDocumentAnalysis
  ( -- * Creating a Request
    StartDocumentAnalysis (..),
    newStartDocumentAnalysis,

    -- * Request Lenses
    startDocumentAnalysis_clientRequestToken,
    startDocumentAnalysis_jobTag,
    startDocumentAnalysis_kmsKeyId,
    startDocumentAnalysis_notificationChannel,
    startDocumentAnalysis_outputConfig,
    startDocumentAnalysis_queriesConfig,
    startDocumentAnalysis_documentLocation,
    startDocumentAnalysis_featureTypes,

    -- * Destructuring the Response
    StartDocumentAnalysisResponse (..),
    newStartDocumentAnalysisResponse,

    -- * Response Lenses
    startDocumentAnalysisResponse_jobId,
    startDocumentAnalysisResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Textract.Types

-- | /See:/ 'newStartDocumentAnalysis' smart constructor.
data StartDocumentAnalysis = StartDocumentAnalysis'
  { -- | The idempotent token that you use to identify the start request. If you
    -- use the same token with multiple @StartDocumentAnalysis@ requests, the
    -- same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same
    -- job from being accidentally started more than once. For more
    -- information, see
    -- <https://docs.aws.amazon.com/textract/latest/dg/api-async.html Calling Amazon Textract Asynchronous Operations>.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | An identifier that you specify that\'s included in the completion
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
    -- GetDocumentAnalysis operation.
    outputConfig :: Prelude.Maybe OutputConfig,
    queriesConfig :: Prelude.Maybe QueriesConfig,
    -- | The location of the document to be processed.
    documentLocation :: DocumentLocation,
    -- | A list of the types of analysis to perform. Add TABLES to the list to
    -- return information about the tables that are detected in the input
    -- document. Add FORMS to return detected form data. To perform both types
    -- of analysis, add TABLES and FORMS to @FeatureTypes@. All lines and words
    -- detected in the document are included in the response (including text
    -- that isn\'t related to the value of @FeatureTypes@).
    featureTypes :: [FeatureType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartDocumentAnalysis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'startDocumentAnalysis_clientRequestToken' - The idempotent token that you use to identify the start request. If you
-- use the same token with multiple @StartDocumentAnalysis@ requests, the
-- same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same
-- job from being accidentally started more than once. For more
-- information, see
-- <https://docs.aws.amazon.com/textract/latest/dg/api-async.html Calling Amazon Textract Asynchronous Operations>.
--
-- 'jobTag', 'startDocumentAnalysis_jobTag' - An identifier that you specify that\'s included in the completion
-- notification published to the Amazon SNS topic. For example, you can use
-- @JobTag@ to identify the type of document that the completion
-- notification corresponds to (such as a tax form or a receipt).
--
-- 'kmsKeyId', 'startDocumentAnalysis_kmsKeyId' - The KMS key used to encrypt the inference results. This can be in either
-- Key ID or Key Alias format. When a KMS key is provided, the KMS key will
-- be used for server-side encryption of the objects in the customer
-- bucket. When this parameter is not enabled, the result will be encrypted
-- server side,using SSE-S3.
--
-- 'notificationChannel', 'startDocumentAnalysis_notificationChannel' - The Amazon SNS topic ARN that you want Amazon Textract to publish the
-- completion status of the operation to.
--
-- 'outputConfig', 'startDocumentAnalysis_outputConfig' - Sets if the output will go to a customer defined bucket. By default,
-- Amazon Textract will save the results internally to be accessed by the
-- GetDocumentAnalysis operation.
--
-- 'queriesConfig', 'startDocumentAnalysis_queriesConfig' - Undocumented member.
--
-- 'documentLocation', 'startDocumentAnalysis_documentLocation' - The location of the document to be processed.
--
-- 'featureTypes', 'startDocumentAnalysis_featureTypes' - A list of the types of analysis to perform. Add TABLES to the list to
-- return information about the tables that are detected in the input
-- document. Add FORMS to return detected form data. To perform both types
-- of analysis, add TABLES and FORMS to @FeatureTypes@. All lines and words
-- detected in the document are included in the response (including text
-- that isn\'t related to the value of @FeatureTypes@).
newStartDocumentAnalysis ::
  -- | 'documentLocation'
  DocumentLocation ->
  StartDocumentAnalysis
newStartDocumentAnalysis pDocumentLocation_ =
  StartDocumentAnalysis'
    { clientRequestToken =
        Prelude.Nothing,
      jobTag = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      notificationChannel = Prelude.Nothing,
      outputConfig = Prelude.Nothing,
      queriesConfig = Prelude.Nothing,
      documentLocation = pDocumentLocation_,
      featureTypes = Prelude.mempty
    }

-- | The idempotent token that you use to identify the start request. If you
-- use the same token with multiple @StartDocumentAnalysis@ requests, the
-- same @JobId@ is returned. Use @ClientRequestToken@ to prevent the same
-- job from being accidentally started more than once. For more
-- information, see
-- <https://docs.aws.amazon.com/textract/latest/dg/api-async.html Calling Amazon Textract Asynchronous Operations>.
startDocumentAnalysis_clientRequestToken :: Lens.Lens' StartDocumentAnalysis (Prelude.Maybe Prelude.Text)
startDocumentAnalysis_clientRequestToken = Lens.lens (\StartDocumentAnalysis' {clientRequestToken} -> clientRequestToken) (\s@StartDocumentAnalysis' {} a -> s {clientRequestToken = a} :: StartDocumentAnalysis)

-- | An identifier that you specify that\'s included in the completion
-- notification published to the Amazon SNS topic. For example, you can use
-- @JobTag@ to identify the type of document that the completion
-- notification corresponds to (such as a tax form or a receipt).
startDocumentAnalysis_jobTag :: Lens.Lens' StartDocumentAnalysis (Prelude.Maybe Prelude.Text)
startDocumentAnalysis_jobTag = Lens.lens (\StartDocumentAnalysis' {jobTag} -> jobTag) (\s@StartDocumentAnalysis' {} a -> s {jobTag = a} :: StartDocumentAnalysis)

-- | The KMS key used to encrypt the inference results. This can be in either
-- Key ID or Key Alias format. When a KMS key is provided, the KMS key will
-- be used for server-side encryption of the objects in the customer
-- bucket. When this parameter is not enabled, the result will be encrypted
-- server side,using SSE-S3.
startDocumentAnalysis_kmsKeyId :: Lens.Lens' StartDocumentAnalysis (Prelude.Maybe Prelude.Text)
startDocumentAnalysis_kmsKeyId = Lens.lens (\StartDocumentAnalysis' {kmsKeyId} -> kmsKeyId) (\s@StartDocumentAnalysis' {} a -> s {kmsKeyId = a} :: StartDocumentAnalysis)

-- | The Amazon SNS topic ARN that you want Amazon Textract to publish the
-- completion status of the operation to.
startDocumentAnalysis_notificationChannel :: Lens.Lens' StartDocumentAnalysis (Prelude.Maybe NotificationChannel)
startDocumentAnalysis_notificationChannel = Lens.lens (\StartDocumentAnalysis' {notificationChannel} -> notificationChannel) (\s@StartDocumentAnalysis' {} a -> s {notificationChannel = a} :: StartDocumentAnalysis)

-- | Sets if the output will go to a customer defined bucket. By default,
-- Amazon Textract will save the results internally to be accessed by the
-- GetDocumentAnalysis operation.
startDocumentAnalysis_outputConfig :: Lens.Lens' StartDocumentAnalysis (Prelude.Maybe OutputConfig)
startDocumentAnalysis_outputConfig = Lens.lens (\StartDocumentAnalysis' {outputConfig} -> outputConfig) (\s@StartDocumentAnalysis' {} a -> s {outputConfig = a} :: StartDocumentAnalysis)

-- | Undocumented member.
startDocumentAnalysis_queriesConfig :: Lens.Lens' StartDocumentAnalysis (Prelude.Maybe QueriesConfig)
startDocumentAnalysis_queriesConfig = Lens.lens (\StartDocumentAnalysis' {queriesConfig} -> queriesConfig) (\s@StartDocumentAnalysis' {} a -> s {queriesConfig = a} :: StartDocumentAnalysis)

-- | The location of the document to be processed.
startDocumentAnalysis_documentLocation :: Lens.Lens' StartDocumentAnalysis DocumentLocation
startDocumentAnalysis_documentLocation = Lens.lens (\StartDocumentAnalysis' {documentLocation} -> documentLocation) (\s@StartDocumentAnalysis' {} a -> s {documentLocation = a} :: StartDocumentAnalysis)

-- | A list of the types of analysis to perform. Add TABLES to the list to
-- return information about the tables that are detected in the input
-- document. Add FORMS to return detected form data. To perform both types
-- of analysis, add TABLES and FORMS to @FeatureTypes@. All lines and words
-- detected in the document are included in the response (including text
-- that isn\'t related to the value of @FeatureTypes@).
startDocumentAnalysis_featureTypes :: Lens.Lens' StartDocumentAnalysis [FeatureType]
startDocumentAnalysis_featureTypes = Lens.lens (\StartDocumentAnalysis' {featureTypes} -> featureTypes) (\s@StartDocumentAnalysis' {} a -> s {featureTypes = a} :: StartDocumentAnalysis) Prelude.. Lens.coerced

instance Core.AWSRequest StartDocumentAnalysis where
  type
    AWSResponse StartDocumentAnalysis =
      StartDocumentAnalysisResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartDocumentAnalysisResponse'
            Prelude.<$> (x Data..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartDocumentAnalysis where
  hashWithSalt _salt StartDocumentAnalysis' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` jobTag
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` notificationChannel
      `Prelude.hashWithSalt` outputConfig
      `Prelude.hashWithSalt` queriesConfig
      `Prelude.hashWithSalt` documentLocation
      `Prelude.hashWithSalt` featureTypes

instance Prelude.NFData StartDocumentAnalysis where
  rnf StartDocumentAnalysis' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf jobTag
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf notificationChannel
      `Prelude.seq` Prelude.rnf outputConfig
      `Prelude.seq` Prelude.rnf queriesConfig
      `Prelude.seq` Prelude.rnf documentLocation
      `Prelude.seq` Prelude.rnf featureTypes

instance Data.ToHeaders StartDocumentAnalysis where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Textract.StartDocumentAnalysis" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartDocumentAnalysis where
  toJSON StartDocumentAnalysis' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("JobTag" Data..=) Prelude.<$> jobTag,
            ("KMSKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("NotificationChannel" Data..=)
              Prelude.<$> notificationChannel,
            ("OutputConfig" Data..=) Prelude.<$> outputConfig,
            ("QueriesConfig" Data..=) Prelude.<$> queriesConfig,
            Prelude.Just
              ("DocumentLocation" Data..= documentLocation),
            Prelude.Just ("FeatureTypes" Data..= featureTypes)
          ]
      )

instance Data.ToPath StartDocumentAnalysis where
  toPath = Prelude.const "/"

instance Data.ToQuery StartDocumentAnalysis where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartDocumentAnalysisResponse' smart constructor.
data StartDocumentAnalysisResponse = StartDocumentAnalysisResponse'
  { -- | The identifier for the document text detection job. Use @JobId@ to
    -- identify the job in a subsequent call to @GetDocumentAnalysis@. A
    -- @JobId@ value is only valid for 7 days.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartDocumentAnalysisResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'startDocumentAnalysisResponse_jobId' - The identifier for the document text detection job. Use @JobId@ to
-- identify the job in a subsequent call to @GetDocumentAnalysis@. A
-- @JobId@ value is only valid for 7 days.
--
-- 'httpStatus', 'startDocumentAnalysisResponse_httpStatus' - The response's http status code.
newStartDocumentAnalysisResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartDocumentAnalysisResponse
newStartDocumentAnalysisResponse pHttpStatus_ =
  StartDocumentAnalysisResponse'
    { jobId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier for the document text detection job. Use @JobId@ to
-- identify the job in a subsequent call to @GetDocumentAnalysis@. A
-- @JobId@ value is only valid for 7 days.
startDocumentAnalysisResponse_jobId :: Lens.Lens' StartDocumentAnalysisResponse (Prelude.Maybe Prelude.Text)
startDocumentAnalysisResponse_jobId = Lens.lens (\StartDocumentAnalysisResponse' {jobId} -> jobId) (\s@StartDocumentAnalysisResponse' {} a -> s {jobId = a} :: StartDocumentAnalysisResponse)

-- | The response's http status code.
startDocumentAnalysisResponse_httpStatus :: Lens.Lens' StartDocumentAnalysisResponse Prelude.Int
startDocumentAnalysisResponse_httpStatus = Lens.lens (\StartDocumentAnalysisResponse' {httpStatus} -> httpStatus) (\s@StartDocumentAnalysisResponse' {} a -> s {httpStatus = a} :: StartDocumentAnalysisResponse)

instance Prelude.NFData StartDocumentAnalysisResponse where
  rnf StartDocumentAnalysisResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf httpStatus
