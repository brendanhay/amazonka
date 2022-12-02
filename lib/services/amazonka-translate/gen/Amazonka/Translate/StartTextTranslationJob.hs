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
-- Module      : Amazonka.Translate.StartTextTranslationJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous batch translation job. Use batch translation jobs
-- to translate large volumes of text across multiple documents at once.
-- For batch translation, the input documents must share the same source
-- language. You can specify one or more target languages. Batch
-- translation translates each input document into each of the target
-- languages. For more information, see
-- <https://docs.aws.amazon.com/translate/latest/dg/async.html Asynchronous batch processing>
--
-- Batch translation jobs can be described with the
-- DescribeTextTranslationJob operation, listed with the
-- ListTextTranslationJobs operation, and stopped with the
-- StopTextTranslationJob operation.
--
-- Amazon Translate does not support batch translation of multiple source
-- languages at once.
module Amazonka.Translate.StartTextTranslationJob
  ( -- * Creating a Request
    StartTextTranslationJob (..),
    newStartTextTranslationJob,

    -- * Request Lenses
    startTextTranslationJob_jobName,
    startTextTranslationJob_terminologyNames,
    startTextTranslationJob_settings,
    startTextTranslationJob_parallelDataNames,
    startTextTranslationJob_inputDataConfig,
    startTextTranslationJob_outputDataConfig,
    startTextTranslationJob_dataAccessRoleArn,
    startTextTranslationJob_sourceLanguageCode,
    startTextTranslationJob_targetLanguageCodes,
    startTextTranslationJob_clientToken,

    -- * Destructuring the Response
    StartTextTranslationJobResponse (..),
    newStartTextTranslationJobResponse,

    -- * Response Lenses
    startTextTranslationJobResponse_jobStatus,
    startTextTranslationJobResponse_jobId,
    startTextTranslationJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Translate.Types

-- | /See:/ 'newStartTextTranslationJob' smart constructor.
data StartTextTranslationJob = StartTextTranslationJob'
  { -- | The name of the batch translation job to be performed.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The name of a custom terminology resource to add to the translation job.
    -- This resource lists examples source terms and the desired translation
    -- for each term.
    --
    -- This parameter accepts only one custom terminology resource.
    --
    -- If you specify multiple target languages for the job, translate uses the
    -- designated terminology for each requested target language that has an
    -- entry for the source term in the terminology file.
    --
    -- For a list of available custom terminology resources, use the
    -- ListTerminologies operation.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/translate/latest/dg/how-custom-terminology.html Custom terminology>.
    terminologyNames :: Prelude.Maybe [Prelude.Text],
    -- | Settings to configure your translation output, including the option to
    -- set the formality level of the output text and the option to mask
    -- profane words and phrases.
    settings :: Prelude.Maybe TranslationSettings,
    -- | The name of a parallel data resource to add to the translation job. This
    -- resource consists of examples that show how you want segments of text to
    -- be translated. If you specify multiple target languages for the job, the
    -- parallel data file must include translations for all the target
    -- languages.
    --
    -- When you add parallel data to a translation job, you create an /Active
    -- Custom Translation/ job.
    --
    -- This parameter accepts only one parallel data resource.
    --
    -- Active Custom Translation jobs are priced at a higher rate than other
    -- jobs that don\'t use parallel data. For more information, see
    -- <http://aws.amazon.com/translate/pricing/ Amazon Translate pricing>.
    --
    -- For a list of available parallel data resources, use the
    -- ListParallelData operation.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/translate/latest/dg/customizing-translations-parallel-data.html Customizing your translations with parallel data>.
    parallelDataNames :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the format and location of the input documents for the
    -- translation job.
    inputDataConfig :: InputDataConfig,
    -- | Specifies the S3 folder to which your job output will be saved.
    outputDataConfig :: OutputDataConfig,
    -- | The Amazon Resource Name (ARN) of an AWS Identity Access and Management
    -- (IAM) role that grants Amazon Translate read access to your input data.
    -- For more information, see
    -- <https://docs.aws.amazon.com/translate/latest/dg/identity-and-access-management.html Identity and access management>
    -- .
    dataAccessRoleArn :: Prelude.Text,
    -- | The language code of the input language. For a list of language codes,
    -- see
    -- <https://docs.aws.amazon.com/translate/latest/dg/what-is-languages.html Supported languages>.
    --
    -- Amazon Translate does not automatically detect a source language during
    -- batch translation jobs.
    sourceLanguageCode :: Prelude.Text,
    -- | The target languages of the translation job. Enter up to 10 language
    -- codes. Each input file is translated into each target language.
    --
    -- Each language code is two or five characters long. For a list of
    -- language codes, see
    -- <https://docs.aws.amazon.com/translate/latest/dg/what-is-languages.html Supported languages>.
    targetLanguageCodes :: Prelude.NonEmpty Prelude.Text,
    -- | A unique identifier for the request. This token is generated for you
    -- when using the Amazon Translate SDK.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartTextTranslationJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobName', 'startTextTranslationJob_jobName' - The name of the batch translation job to be performed.
--
-- 'terminologyNames', 'startTextTranslationJob_terminologyNames' - The name of a custom terminology resource to add to the translation job.
-- This resource lists examples source terms and the desired translation
-- for each term.
--
-- This parameter accepts only one custom terminology resource.
--
-- If you specify multiple target languages for the job, translate uses the
-- designated terminology for each requested target language that has an
-- entry for the source term in the terminology file.
--
-- For a list of available custom terminology resources, use the
-- ListTerminologies operation.
--
-- For more information, see
-- <https://docs.aws.amazon.com/translate/latest/dg/how-custom-terminology.html Custom terminology>.
--
-- 'settings', 'startTextTranslationJob_settings' - Settings to configure your translation output, including the option to
-- set the formality level of the output text and the option to mask
-- profane words and phrases.
--
-- 'parallelDataNames', 'startTextTranslationJob_parallelDataNames' - The name of a parallel data resource to add to the translation job. This
-- resource consists of examples that show how you want segments of text to
-- be translated. If you specify multiple target languages for the job, the
-- parallel data file must include translations for all the target
-- languages.
--
-- When you add parallel data to a translation job, you create an /Active
-- Custom Translation/ job.
--
-- This parameter accepts only one parallel data resource.
--
-- Active Custom Translation jobs are priced at a higher rate than other
-- jobs that don\'t use parallel data. For more information, see
-- <http://aws.amazon.com/translate/pricing/ Amazon Translate pricing>.
--
-- For a list of available parallel data resources, use the
-- ListParallelData operation.
--
-- For more information, see
-- <https://docs.aws.amazon.com/translate/latest/dg/customizing-translations-parallel-data.html Customizing your translations with parallel data>.
--
-- 'inputDataConfig', 'startTextTranslationJob_inputDataConfig' - Specifies the format and location of the input documents for the
-- translation job.
--
-- 'outputDataConfig', 'startTextTranslationJob_outputDataConfig' - Specifies the S3 folder to which your job output will be saved.
--
-- 'dataAccessRoleArn', 'startTextTranslationJob_dataAccessRoleArn' - The Amazon Resource Name (ARN) of an AWS Identity Access and Management
-- (IAM) role that grants Amazon Translate read access to your input data.
-- For more information, see
-- <https://docs.aws.amazon.com/translate/latest/dg/identity-and-access-management.html Identity and access management>
-- .
--
-- 'sourceLanguageCode', 'startTextTranslationJob_sourceLanguageCode' - The language code of the input language. For a list of language codes,
-- see
-- <https://docs.aws.amazon.com/translate/latest/dg/what-is-languages.html Supported languages>.
--
-- Amazon Translate does not automatically detect a source language during
-- batch translation jobs.
--
-- 'targetLanguageCodes', 'startTextTranslationJob_targetLanguageCodes' - The target languages of the translation job. Enter up to 10 language
-- codes. Each input file is translated into each target language.
--
-- Each language code is two or five characters long. For a list of
-- language codes, see
-- <https://docs.aws.amazon.com/translate/latest/dg/what-is-languages.html Supported languages>.
--
-- 'clientToken', 'startTextTranslationJob_clientToken' - A unique identifier for the request. This token is generated for you
-- when using the Amazon Translate SDK.
newStartTextTranslationJob ::
  -- | 'inputDataConfig'
  InputDataConfig ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'dataAccessRoleArn'
  Prelude.Text ->
  -- | 'sourceLanguageCode'
  Prelude.Text ->
  -- | 'targetLanguageCodes'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  StartTextTranslationJob
newStartTextTranslationJob
  pInputDataConfig_
  pOutputDataConfig_
  pDataAccessRoleArn_
  pSourceLanguageCode_
  pTargetLanguageCodes_
  pClientToken_ =
    StartTextTranslationJob'
      { jobName = Prelude.Nothing,
        terminologyNames = Prelude.Nothing,
        settings = Prelude.Nothing,
        parallelDataNames = Prelude.Nothing,
        inputDataConfig = pInputDataConfig_,
        outputDataConfig = pOutputDataConfig_,
        dataAccessRoleArn = pDataAccessRoleArn_,
        sourceLanguageCode = pSourceLanguageCode_,
        targetLanguageCodes =
          Lens.coerced Lens.# pTargetLanguageCodes_,
        clientToken = pClientToken_
      }

-- | The name of the batch translation job to be performed.
startTextTranslationJob_jobName :: Lens.Lens' StartTextTranslationJob (Prelude.Maybe Prelude.Text)
startTextTranslationJob_jobName = Lens.lens (\StartTextTranslationJob' {jobName} -> jobName) (\s@StartTextTranslationJob' {} a -> s {jobName = a} :: StartTextTranslationJob)

-- | The name of a custom terminology resource to add to the translation job.
-- This resource lists examples source terms and the desired translation
-- for each term.
--
-- This parameter accepts only one custom terminology resource.
--
-- If you specify multiple target languages for the job, translate uses the
-- designated terminology for each requested target language that has an
-- entry for the source term in the terminology file.
--
-- For a list of available custom terminology resources, use the
-- ListTerminologies operation.
--
-- For more information, see
-- <https://docs.aws.amazon.com/translate/latest/dg/how-custom-terminology.html Custom terminology>.
startTextTranslationJob_terminologyNames :: Lens.Lens' StartTextTranslationJob (Prelude.Maybe [Prelude.Text])
startTextTranslationJob_terminologyNames = Lens.lens (\StartTextTranslationJob' {terminologyNames} -> terminologyNames) (\s@StartTextTranslationJob' {} a -> s {terminologyNames = a} :: StartTextTranslationJob) Prelude.. Lens.mapping Lens.coerced

-- | Settings to configure your translation output, including the option to
-- set the formality level of the output text and the option to mask
-- profane words and phrases.
startTextTranslationJob_settings :: Lens.Lens' StartTextTranslationJob (Prelude.Maybe TranslationSettings)
startTextTranslationJob_settings = Lens.lens (\StartTextTranslationJob' {settings} -> settings) (\s@StartTextTranslationJob' {} a -> s {settings = a} :: StartTextTranslationJob)

-- | The name of a parallel data resource to add to the translation job. This
-- resource consists of examples that show how you want segments of text to
-- be translated. If you specify multiple target languages for the job, the
-- parallel data file must include translations for all the target
-- languages.
--
-- When you add parallel data to a translation job, you create an /Active
-- Custom Translation/ job.
--
-- This parameter accepts only one parallel data resource.
--
-- Active Custom Translation jobs are priced at a higher rate than other
-- jobs that don\'t use parallel data. For more information, see
-- <http://aws.amazon.com/translate/pricing/ Amazon Translate pricing>.
--
-- For a list of available parallel data resources, use the
-- ListParallelData operation.
--
-- For more information, see
-- <https://docs.aws.amazon.com/translate/latest/dg/customizing-translations-parallel-data.html Customizing your translations with parallel data>.
startTextTranslationJob_parallelDataNames :: Lens.Lens' StartTextTranslationJob (Prelude.Maybe [Prelude.Text])
startTextTranslationJob_parallelDataNames = Lens.lens (\StartTextTranslationJob' {parallelDataNames} -> parallelDataNames) (\s@StartTextTranslationJob' {} a -> s {parallelDataNames = a} :: StartTextTranslationJob) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the format and location of the input documents for the
-- translation job.
startTextTranslationJob_inputDataConfig :: Lens.Lens' StartTextTranslationJob InputDataConfig
startTextTranslationJob_inputDataConfig = Lens.lens (\StartTextTranslationJob' {inputDataConfig} -> inputDataConfig) (\s@StartTextTranslationJob' {} a -> s {inputDataConfig = a} :: StartTextTranslationJob)

-- | Specifies the S3 folder to which your job output will be saved.
startTextTranslationJob_outputDataConfig :: Lens.Lens' StartTextTranslationJob OutputDataConfig
startTextTranslationJob_outputDataConfig = Lens.lens (\StartTextTranslationJob' {outputDataConfig} -> outputDataConfig) (\s@StartTextTranslationJob' {} a -> s {outputDataConfig = a} :: StartTextTranslationJob)

-- | The Amazon Resource Name (ARN) of an AWS Identity Access and Management
-- (IAM) role that grants Amazon Translate read access to your input data.
-- For more information, see
-- <https://docs.aws.amazon.com/translate/latest/dg/identity-and-access-management.html Identity and access management>
-- .
startTextTranslationJob_dataAccessRoleArn :: Lens.Lens' StartTextTranslationJob Prelude.Text
startTextTranslationJob_dataAccessRoleArn = Lens.lens (\StartTextTranslationJob' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@StartTextTranslationJob' {} a -> s {dataAccessRoleArn = a} :: StartTextTranslationJob)

-- | The language code of the input language. For a list of language codes,
-- see
-- <https://docs.aws.amazon.com/translate/latest/dg/what-is-languages.html Supported languages>.
--
-- Amazon Translate does not automatically detect a source language during
-- batch translation jobs.
startTextTranslationJob_sourceLanguageCode :: Lens.Lens' StartTextTranslationJob Prelude.Text
startTextTranslationJob_sourceLanguageCode = Lens.lens (\StartTextTranslationJob' {sourceLanguageCode} -> sourceLanguageCode) (\s@StartTextTranslationJob' {} a -> s {sourceLanguageCode = a} :: StartTextTranslationJob)

-- | The target languages of the translation job. Enter up to 10 language
-- codes. Each input file is translated into each target language.
--
-- Each language code is two or five characters long. For a list of
-- language codes, see
-- <https://docs.aws.amazon.com/translate/latest/dg/what-is-languages.html Supported languages>.
startTextTranslationJob_targetLanguageCodes :: Lens.Lens' StartTextTranslationJob (Prelude.NonEmpty Prelude.Text)
startTextTranslationJob_targetLanguageCodes = Lens.lens (\StartTextTranslationJob' {targetLanguageCodes} -> targetLanguageCodes) (\s@StartTextTranslationJob' {} a -> s {targetLanguageCodes = a} :: StartTextTranslationJob) Prelude.. Lens.coerced

-- | A unique identifier for the request. This token is generated for you
-- when using the Amazon Translate SDK.
startTextTranslationJob_clientToken :: Lens.Lens' StartTextTranslationJob Prelude.Text
startTextTranslationJob_clientToken = Lens.lens (\StartTextTranslationJob' {clientToken} -> clientToken) (\s@StartTextTranslationJob' {} a -> s {clientToken = a} :: StartTextTranslationJob)

instance Core.AWSRequest StartTextTranslationJob where
  type
    AWSResponse StartTextTranslationJob =
      StartTextTranslationJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartTextTranslationJobResponse'
            Prelude.<$> (x Data..?> "JobStatus")
            Prelude.<*> (x Data..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartTextTranslationJob where
  hashWithSalt _salt StartTextTranslationJob' {..} =
    _salt `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` terminologyNames
      `Prelude.hashWithSalt` settings
      `Prelude.hashWithSalt` parallelDataNames
      `Prelude.hashWithSalt` inputDataConfig
      `Prelude.hashWithSalt` outputDataConfig
      `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` sourceLanguageCode
      `Prelude.hashWithSalt` targetLanguageCodes
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData StartTextTranslationJob where
  rnf StartTextTranslationJob' {..} =
    Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf terminologyNames
      `Prelude.seq` Prelude.rnf settings
      `Prelude.seq` Prelude.rnf parallelDataNames
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf sourceLanguageCode
      `Prelude.seq` Prelude.rnf targetLanguageCodes
      `Prelude.seq` Prelude.rnf clientToken

instance Data.ToHeaders StartTextTranslationJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShineFrontendService_20170701.StartTextTranslationJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartTextTranslationJob where
  toJSON StartTextTranslationJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("JobName" Data..=) Prelude.<$> jobName,
            ("TerminologyNames" Data..=)
              Prelude.<$> terminologyNames,
            ("Settings" Data..=) Prelude.<$> settings,
            ("ParallelDataNames" Data..=)
              Prelude.<$> parallelDataNames,
            Prelude.Just
              ("InputDataConfig" Data..= inputDataConfig),
            Prelude.Just
              ("OutputDataConfig" Data..= outputDataConfig),
            Prelude.Just
              ("DataAccessRoleArn" Data..= dataAccessRoleArn),
            Prelude.Just
              ("SourceLanguageCode" Data..= sourceLanguageCode),
            Prelude.Just
              ("TargetLanguageCodes" Data..= targetLanguageCodes),
            Prelude.Just ("ClientToken" Data..= clientToken)
          ]
      )

instance Data.ToPath StartTextTranslationJob where
  toPath = Prelude.const "/"

instance Data.ToQuery StartTextTranslationJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartTextTranslationJobResponse' smart constructor.
data StartTextTranslationJobResponse = StartTextTranslationJobResponse'
  { -- | The status of the job. Possible values include:
    --
    -- -   @SUBMITTED@ - The job has been received and is queued for
    --     processing.
    --
    -- -   @IN_PROGRESS@ - Amazon Translate is processing the job.
    --
    -- -   @COMPLETED@ - The job was successfully completed and the output is
    --     available.
    --
    -- -   @COMPLETED_WITH_ERROR@ - The job was completed with errors. The
    --     errors can be analyzed in the job\'s output.
    --
    -- -   @FAILED@ - The job did not complete. To get details, use the
    --     DescribeTextTranslationJob operation.
    --
    -- -   @STOP_REQUESTED@ - The user who started the job has requested that
    --     it be stopped.
    --
    -- -   @STOPPED@ - The job has been stopped.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The identifier generated for the job. To get the status of a job, use
    -- this ID with the DescribeTextTranslationJob operation.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartTextTranslationJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobStatus', 'startTextTranslationJobResponse_jobStatus' - The status of the job. Possible values include:
--
-- -   @SUBMITTED@ - The job has been received and is queued for
--     processing.
--
-- -   @IN_PROGRESS@ - Amazon Translate is processing the job.
--
-- -   @COMPLETED@ - The job was successfully completed and the output is
--     available.
--
-- -   @COMPLETED_WITH_ERROR@ - The job was completed with errors. The
--     errors can be analyzed in the job\'s output.
--
-- -   @FAILED@ - The job did not complete. To get details, use the
--     DescribeTextTranslationJob operation.
--
-- -   @STOP_REQUESTED@ - The user who started the job has requested that
--     it be stopped.
--
-- -   @STOPPED@ - The job has been stopped.
--
-- 'jobId', 'startTextTranslationJobResponse_jobId' - The identifier generated for the job. To get the status of a job, use
-- this ID with the DescribeTextTranslationJob operation.
--
-- 'httpStatus', 'startTextTranslationJobResponse_httpStatus' - The response's http status code.
newStartTextTranslationJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartTextTranslationJobResponse
newStartTextTranslationJobResponse pHttpStatus_ =
  StartTextTranslationJobResponse'
    { jobStatus =
        Prelude.Nothing,
      jobId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the job. Possible values include:
--
-- -   @SUBMITTED@ - The job has been received and is queued for
--     processing.
--
-- -   @IN_PROGRESS@ - Amazon Translate is processing the job.
--
-- -   @COMPLETED@ - The job was successfully completed and the output is
--     available.
--
-- -   @COMPLETED_WITH_ERROR@ - The job was completed with errors. The
--     errors can be analyzed in the job\'s output.
--
-- -   @FAILED@ - The job did not complete. To get details, use the
--     DescribeTextTranslationJob operation.
--
-- -   @STOP_REQUESTED@ - The user who started the job has requested that
--     it be stopped.
--
-- -   @STOPPED@ - The job has been stopped.
startTextTranslationJobResponse_jobStatus :: Lens.Lens' StartTextTranslationJobResponse (Prelude.Maybe JobStatus)
startTextTranslationJobResponse_jobStatus = Lens.lens (\StartTextTranslationJobResponse' {jobStatus} -> jobStatus) (\s@StartTextTranslationJobResponse' {} a -> s {jobStatus = a} :: StartTextTranslationJobResponse)

-- | The identifier generated for the job. To get the status of a job, use
-- this ID with the DescribeTextTranslationJob operation.
startTextTranslationJobResponse_jobId :: Lens.Lens' StartTextTranslationJobResponse (Prelude.Maybe Prelude.Text)
startTextTranslationJobResponse_jobId = Lens.lens (\StartTextTranslationJobResponse' {jobId} -> jobId) (\s@StartTextTranslationJobResponse' {} a -> s {jobId = a} :: StartTextTranslationJobResponse)

-- | The response's http status code.
startTextTranslationJobResponse_httpStatus :: Lens.Lens' StartTextTranslationJobResponse Prelude.Int
startTextTranslationJobResponse_httpStatus = Lens.lens (\StartTextTranslationJobResponse' {httpStatus} -> httpStatus) (\s@StartTextTranslationJobResponse' {} a -> s {httpStatus = a} :: StartTextTranslationJobResponse)

instance
  Prelude.NFData
    StartTextTranslationJobResponse
  where
  rnf StartTextTranslationJobResponse' {..} =
    Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf httpStatus
