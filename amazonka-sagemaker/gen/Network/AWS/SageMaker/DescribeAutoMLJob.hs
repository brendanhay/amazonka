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
-- Module      : Network.AWS.SageMaker.DescribeAutoMLJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an Amazon SageMaker AutoML job.
module Network.AWS.SageMaker.DescribeAutoMLJob
  ( -- * Creating a Request
    DescribeAutoMLJob (..),
    newDescribeAutoMLJob,

    -- * Request Lenses
    describeAutoMLJob_autoMLJobName,

    -- * Destructuring the Response
    DescribeAutoMLJobResponse (..),
    newDescribeAutoMLJobResponse,

    -- * Response Lenses
    describeAutoMLJobResponse_generateCandidateDefinitionsOnly,
    describeAutoMLJobResponse_modelDeployResult,
    describeAutoMLJobResponse_endTime,
    describeAutoMLJobResponse_resolvedAttributes,
    describeAutoMLJobResponse_autoMLJobArtifacts,
    describeAutoMLJobResponse_partialFailureReasons,
    describeAutoMLJobResponse_failureReason,
    describeAutoMLJobResponse_autoMLJobObjective,
    describeAutoMLJobResponse_modelDeployConfig,
    describeAutoMLJobResponse_autoMLJobConfig,
    describeAutoMLJobResponse_problemType,
    describeAutoMLJobResponse_bestCandidate,
    describeAutoMLJobResponse_httpStatus,
    describeAutoMLJobResponse_autoMLJobName,
    describeAutoMLJobResponse_autoMLJobArn,
    describeAutoMLJobResponse_inputDataConfig,
    describeAutoMLJobResponse_outputDataConfig,
    describeAutoMLJobResponse_roleArn,
    describeAutoMLJobResponse_creationTime,
    describeAutoMLJobResponse_lastModifiedTime,
    describeAutoMLJobResponse_autoMLJobStatus,
    describeAutoMLJobResponse_autoMLJobSecondaryStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeAutoMLJob' smart constructor.
data DescribeAutoMLJob = DescribeAutoMLJob'
  { -- | Requests information about an AutoML job using its unique name.
    autoMLJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAutoMLJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoMLJobName', 'describeAutoMLJob_autoMLJobName' - Requests information about an AutoML job using its unique name.
newDescribeAutoMLJob ::
  -- | 'autoMLJobName'
  Prelude.Text ->
  DescribeAutoMLJob
newDescribeAutoMLJob pAutoMLJobName_ =
  DescribeAutoMLJob' {autoMLJobName = pAutoMLJobName_}

-- | Requests information about an AutoML job using its unique name.
describeAutoMLJob_autoMLJobName :: Lens.Lens' DescribeAutoMLJob Prelude.Text
describeAutoMLJob_autoMLJobName = Lens.lens (\DescribeAutoMLJob' {autoMLJobName} -> autoMLJobName) (\s@DescribeAutoMLJob' {} a -> s {autoMLJobName = a} :: DescribeAutoMLJob)

instance Core.AWSRequest DescribeAutoMLJob where
  type
    AWSResponse DescribeAutoMLJob =
      DescribeAutoMLJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAutoMLJobResponse'
            Prelude.<$> (x Core..?> "GenerateCandidateDefinitionsOnly")
            Prelude.<*> (x Core..?> "ModelDeployResult")
            Prelude.<*> (x Core..?> "EndTime")
            Prelude.<*> (x Core..?> "ResolvedAttributes")
            Prelude.<*> (x Core..?> "AutoMLJobArtifacts")
            Prelude.<*> (x Core..?> "PartialFailureReasons")
            Prelude.<*> (x Core..?> "FailureReason")
            Prelude.<*> (x Core..?> "AutoMLJobObjective")
            Prelude.<*> (x Core..?> "ModelDeployConfig")
            Prelude.<*> (x Core..?> "AutoMLJobConfig")
            Prelude.<*> (x Core..?> "ProblemType")
            Prelude.<*> (x Core..?> "BestCandidate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "AutoMLJobName")
            Prelude.<*> (x Core..:> "AutoMLJobArn")
            Prelude.<*> (x Core..:> "InputDataConfig")
            Prelude.<*> (x Core..:> "OutputDataConfig")
            Prelude.<*> (x Core..:> "RoleArn")
            Prelude.<*> (x Core..:> "CreationTime")
            Prelude.<*> (x Core..:> "LastModifiedTime")
            Prelude.<*> (x Core..:> "AutoMLJobStatus")
            Prelude.<*> (x Core..:> "AutoMLJobSecondaryStatus")
      )

instance Prelude.Hashable DescribeAutoMLJob

instance Prelude.NFData DescribeAutoMLJob

instance Core.ToHeaders DescribeAutoMLJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeAutoMLJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeAutoMLJob where
  toJSON DescribeAutoMLJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AutoMLJobName" Core..= autoMLJobName)
          ]
      )

instance Core.ToPath DescribeAutoMLJob where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeAutoMLJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAutoMLJobResponse' smart constructor.
data DescribeAutoMLJobResponse = DescribeAutoMLJobResponse'
  { -- | Indicates whether the output for an AutoML job generates candidate
    -- definitions only.
    generateCandidateDefinitionsOnly :: Prelude.Maybe Prelude.Bool,
    -- | Provides information about endpoint for the model deployment.
    modelDeployResult :: Prelude.Maybe ModelDeployResult,
    -- | Returns the end time of the AutoML job.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | This contains @ProblemType@, @AutoMLJobObjective@, and
    -- @CompletionCriteria@. If you do not provide these values, they are
    -- auto-inferred. If you do provide them, the values used are the ones you
    -- provide.
    resolvedAttributes :: Prelude.Maybe ResolvedAttributes,
    -- | Returns information on the job\'s artifacts found in
    -- @AutoMLJobArtifacts@.
    autoMLJobArtifacts :: Prelude.Maybe AutoMLJobArtifacts,
    -- | Returns a list of reasons for partial failures within an AutoML job.
    partialFailureReasons :: Prelude.Maybe (Prelude.NonEmpty AutoMLPartialFailureReason),
    -- | Returns the failure reason for an AutoML job, when applicable.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | Returns the job\'s objective.
    autoMLJobObjective :: Prelude.Maybe AutoMLJobObjective,
    -- | Indicates whether the model was deployed automatically to an endpoint
    -- and the name of that endpoint if deployed automatically.
    modelDeployConfig :: Prelude.Maybe ModelDeployConfig,
    -- | Returns the configuration for the AutoML job.
    autoMLJobConfig :: Prelude.Maybe AutoMLJobConfig,
    -- | Returns the job\'s problem type.
    problemType :: Prelude.Maybe ProblemType,
    -- | Returns the job\'s best @AutoMLCandidate@.
    bestCandidate :: Prelude.Maybe AutoMLCandidate,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Returns the name of the AutoML job.
    autoMLJobName :: Prelude.Text,
    -- | Returns the ARN of the AutoML job.
    autoMLJobArn :: Prelude.Text,
    -- | Returns the input data configuration for the AutoML job..
    inputDataConfig :: Prelude.NonEmpty AutoMLChannel,
    -- | Returns the job\'s output data config.
    outputDataConfig :: AutoMLOutputDataConfig,
    -- | The Amazon Resource Name (ARN) of the Amazon Web Services Identity and
    -- Access Management (IAM) role that has read permission to the input data
    -- location and write permission to the output data location in Amazon S3.
    roleArn :: Prelude.Text,
    -- | Returns the creation time of the AutoML job.
    creationTime :: Core.POSIX,
    -- | Returns the job\'s last modified time.
    lastModifiedTime :: Core.POSIX,
    -- | Returns the status of the AutoML job.
    autoMLJobStatus :: AutoMLJobStatus,
    -- | Returns the secondary status of the AutoML job.
    autoMLJobSecondaryStatus :: AutoMLJobSecondaryStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAutoMLJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'generateCandidateDefinitionsOnly', 'describeAutoMLJobResponse_generateCandidateDefinitionsOnly' - Indicates whether the output for an AutoML job generates candidate
-- definitions only.
--
-- 'modelDeployResult', 'describeAutoMLJobResponse_modelDeployResult' - Provides information about endpoint for the model deployment.
--
-- 'endTime', 'describeAutoMLJobResponse_endTime' - Returns the end time of the AutoML job.
--
-- 'resolvedAttributes', 'describeAutoMLJobResponse_resolvedAttributes' - This contains @ProblemType@, @AutoMLJobObjective@, and
-- @CompletionCriteria@. If you do not provide these values, they are
-- auto-inferred. If you do provide them, the values used are the ones you
-- provide.
--
-- 'autoMLJobArtifacts', 'describeAutoMLJobResponse_autoMLJobArtifacts' - Returns information on the job\'s artifacts found in
-- @AutoMLJobArtifacts@.
--
-- 'partialFailureReasons', 'describeAutoMLJobResponse_partialFailureReasons' - Returns a list of reasons for partial failures within an AutoML job.
--
-- 'failureReason', 'describeAutoMLJobResponse_failureReason' - Returns the failure reason for an AutoML job, when applicable.
--
-- 'autoMLJobObjective', 'describeAutoMLJobResponse_autoMLJobObjective' - Returns the job\'s objective.
--
-- 'modelDeployConfig', 'describeAutoMLJobResponse_modelDeployConfig' - Indicates whether the model was deployed automatically to an endpoint
-- and the name of that endpoint if deployed automatically.
--
-- 'autoMLJobConfig', 'describeAutoMLJobResponse_autoMLJobConfig' - Returns the configuration for the AutoML job.
--
-- 'problemType', 'describeAutoMLJobResponse_problemType' - Returns the job\'s problem type.
--
-- 'bestCandidate', 'describeAutoMLJobResponse_bestCandidate' - Returns the job\'s best @AutoMLCandidate@.
--
-- 'httpStatus', 'describeAutoMLJobResponse_httpStatus' - The response's http status code.
--
-- 'autoMLJobName', 'describeAutoMLJobResponse_autoMLJobName' - Returns the name of the AutoML job.
--
-- 'autoMLJobArn', 'describeAutoMLJobResponse_autoMLJobArn' - Returns the ARN of the AutoML job.
--
-- 'inputDataConfig', 'describeAutoMLJobResponse_inputDataConfig' - Returns the input data configuration for the AutoML job..
--
-- 'outputDataConfig', 'describeAutoMLJobResponse_outputDataConfig' - Returns the job\'s output data config.
--
-- 'roleArn', 'describeAutoMLJobResponse_roleArn' - The Amazon Resource Name (ARN) of the Amazon Web Services Identity and
-- Access Management (IAM) role that has read permission to the input data
-- location and write permission to the output data location in Amazon S3.
--
-- 'creationTime', 'describeAutoMLJobResponse_creationTime' - Returns the creation time of the AutoML job.
--
-- 'lastModifiedTime', 'describeAutoMLJobResponse_lastModifiedTime' - Returns the job\'s last modified time.
--
-- 'autoMLJobStatus', 'describeAutoMLJobResponse_autoMLJobStatus' - Returns the status of the AutoML job.
--
-- 'autoMLJobSecondaryStatus', 'describeAutoMLJobResponse_autoMLJobSecondaryStatus' - Returns the secondary status of the AutoML job.
newDescribeAutoMLJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'autoMLJobName'
  Prelude.Text ->
  -- | 'autoMLJobArn'
  Prelude.Text ->
  -- | 'inputDataConfig'
  Prelude.NonEmpty AutoMLChannel ->
  -- | 'outputDataConfig'
  AutoMLOutputDataConfig ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  -- | 'autoMLJobStatus'
  AutoMLJobStatus ->
  -- | 'autoMLJobSecondaryStatus'
  AutoMLJobSecondaryStatus ->
  DescribeAutoMLJobResponse
newDescribeAutoMLJobResponse
  pHttpStatus_
  pAutoMLJobName_
  pAutoMLJobArn_
  pInputDataConfig_
  pOutputDataConfig_
  pRoleArn_
  pCreationTime_
  pLastModifiedTime_
  pAutoMLJobStatus_
  pAutoMLJobSecondaryStatus_ =
    DescribeAutoMLJobResponse'
      { generateCandidateDefinitionsOnly =
          Prelude.Nothing,
        modelDeployResult = Prelude.Nothing,
        endTime = Prelude.Nothing,
        resolvedAttributes = Prelude.Nothing,
        autoMLJobArtifacts = Prelude.Nothing,
        partialFailureReasons = Prelude.Nothing,
        failureReason = Prelude.Nothing,
        autoMLJobObjective = Prelude.Nothing,
        modelDeployConfig = Prelude.Nothing,
        autoMLJobConfig = Prelude.Nothing,
        problemType = Prelude.Nothing,
        bestCandidate = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        autoMLJobName = pAutoMLJobName_,
        autoMLJobArn = pAutoMLJobArn_,
        inputDataConfig =
          Lens._Coerce Lens.# pInputDataConfig_,
        outputDataConfig = pOutputDataConfig_,
        roleArn = pRoleArn_,
        creationTime = Core._Time Lens.# pCreationTime_,
        lastModifiedTime =
          Core._Time Lens.# pLastModifiedTime_,
        autoMLJobStatus = pAutoMLJobStatus_,
        autoMLJobSecondaryStatus =
          pAutoMLJobSecondaryStatus_
      }

-- | Indicates whether the output for an AutoML job generates candidate
-- definitions only.
describeAutoMLJobResponse_generateCandidateDefinitionsOnly :: Lens.Lens' DescribeAutoMLJobResponse (Prelude.Maybe Prelude.Bool)
describeAutoMLJobResponse_generateCandidateDefinitionsOnly = Lens.lens (\DescribeAutoMLJobResponse' {generateCandidateDefinitionsOnly} -> generateCandidateDefinitionsOnly) (\s@DescribeAutoMLJobResponse' {} a -> s {generateCandidateDefinitionsOnly = a} :: DescribeAutoMLJobResponse)

-- | Provides information about endpoint for the model deployment.
describeAutoMLJobResponse_modelDeployResult :: Lens.Lens' DescribeAutoMLJobResponse (Prelude.Maybe ModelDeployResult)
describeAutoMLJobResponse_modelDeployResult = Lens.lens (\DescribeAutoMLJobResponse' {modelDeployResult} -> modelDeployResult) (\s@DescribeAutoMLJobResponse' {} a -> s {modelDeployResult = a} :: DescribeAutoMLJobResponse)

-- | Returns the end time of the AutoML job.
describeAutoMLJobResponse_endTime :: Lens.Lens' DescribeAutoMLJobResponse (Prelude.Maybe Prelude.UTCTime)
describeAutoMLJobResponse_endTime = Lens.lens (\DescribeAutoMLJobResponse' {endTime} -> endTime) (\s@DescribeAutoMLJobResponse' {} a -> s {endTime = a} :: DescribeAutoMLJobResponse) Prelude.. Lens.mapping Core._Time

-- | This contains @ProblemType@, @AutoMLJobObjective@, and
-- @CompletionCriteria@. If you do not provide these values, they are
-- auto-inferred. If you do provide them, the values used are the ones you
-- provide.
describeAutoMLJobResponse_resolvedAttributes :: Lens.Lens' DescribeAutoMLJobResponse (Prelude.Maybe ResolvedAttributes)
describeAutoMLJobResponse_resolvedAttributes = Lens.lens (\DescribeAutoMLJobResponse' {resolvedAttributes} -> resolvedAttributes) (\s@DescribeAutoMLJobResponse' {} a -> s {resolvedAttributes = a} :: DescribeAutoMLJobResponse)

-- | Returns information on the job\'s artifacts found in
-- @AutoMLJobArtifacts@.
describeAutoMLJobResponse_autoMLJobArtifacts :: Lens.Lens' DescribeAutoMLJobResponse (Prelude.Maybe AutoMLJobArtifacts)
describeAutoMLJobResponse_autoMLJobArtifacts = Lens.lens (\DescribeAutoMLJobResponse' {autoMLJobArtifacts} -> autoMLJobArtifacts) (\s@DescribeAutoMLJobResponse' {} a -> s {autoMLJobArtifacts = a} :: DescribeAutoMLJobResponse)

-- | Returns a list of reasons for partial failures within an AutoML job.
describeAutoMLJobResponse_partialFailureReasons :: Lens.Lens' DescribeAutoMLJobResponse (Prelude.Maybe (Prelude.NonEmpty AutoMLPartialFailureReason))
describeAutoMLJobResponse_partialFailureReasons = Lens.lens (\DescribeAutoMLJobResponse' {partialFailureReasons} -> partialFailureReasons) (\s@DescribeAutoMLJobResponse' {} a -> s {partialFailureReasons = a} :: DescribeAutoMLJobResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Returns the failure reason for an AutoML job, when applicable.
describeAutoMLJobResponse_failureReason :: Lens.Lens' DescribeAutoMLJobResponse (Prelude.Maybe Prelude.Text)
describeAutoMLJobResponse_failureReason = Lens.lens (\DescribeAutoMLJobResponse' {failureReason} -> failureReason) (\s@DescribeAutoMLJobResponse' {} a -> s {failureReason = a} :: DescribeAutoMLJobResponse)

-- | Returns the job\'s objective.
describeAutoMLJobResponse_autoMLJobObjective :: Lens.Lens' DescribeAutoMLJobResponse (Prelude.Maybe AutoMLJobObjective)
describeAutoMLJobResponse_autoMLJobObjective = Lens.lens (\DescribeAutoMLJobResponse' {autoMLJobObjective} -> autoMLJobObjective) (\s@DescribeAutoMLJobResponse' {} a -> s {autoMLJobObjective = a} :: DescribeAutoMLJobResponse)

-- | Indicates whether the model was deployed automatically to an endpoint
-- and the name of that endpoint if deployed automatically.
describeAutoMLJobResponse_modelDeployConfig :: Lens.Lens' DescribeAutoMLJobResponse (Prelude.Maybe ModelDeployConfig)
describeAutoMLJobResponse_modelDeployConfig = Lens.lens (\DescribeAutoMLJobResponse' {modelDeployConfig} -> modelDeployConfig) (\s@DescribeAutoMLJobResponse' {} a -> s {modelDeployConfig = a} :: DescribeAutoMLJobResponse)

-- | Returns the configuration for the AutoML job.
describeAutoMLJobResponse_autoMLJobConfig :: Lens.Lens' DescribeAutoMLJobResponse (Prelude.Maybe AutoMLJobConfig)
describeAutoMLJobResponse_autoMLJobConfig = Lens.lens (\DescribeAutoMLJobResponse' {autoMLJobConfig} -> autoMLJobConfig) (\s@DescribeAutoMLJobResponse' {} a -> s {autoMLJobConfig = a} :: DescribeAutoMLJobResponse)

-- | Returns the job\'s problem type.
describeAutoMLJobResponse_problemType :: Lens.Lens' DescribeAutoMLJobResponse (Prelude.Maybe ProblemType)
describeAutoMLJobResponse_problemType = Lens.lens (\DescribeAutoMLJobResponse' {problemType} -> problemType) (\s@DescribeAutoMLJobResponse' {} a -> s {problemType = a} :: DescribeAutoMLJobResponse)

-- | Returns the job\'s best @AutoMLCandidate@.
describeAutoMLJobResponse_bestCandidate :: Lens.Lens' DescribeAutoMLJobResponse (Prelude.Maybe AutoMLCandidate)
describeAutoMLJobResponse_bestCandidate = Lens.lens (\DescribeAutoMLJobResponse' {bestCandidate} -> bestCandidate) (\s@DescribeAutoMLJobResponse' {} a -> s {bestCandidate = a} :: DescribeAutoMLJobResponse)

-- | The response's http status code.
describeAutoMLJobResponse_httpStatus :: Lens.Lens' DescribeAutoMLJobResponse Prelude.Int
describeAutoMLJobResponse_httpStatus = Lens.lens (\DescribeAutoMLJobResponse' {httpStatus} -> httpStatus) (\s@DescribeAutoMLJobResponse' {} a -> s {httpStatus = a} :: DescribeAutoMLJobResponse)

-- | Returns the name of the AutoML job.
describeAutoMLJobResponse_autoMLJobName :: Lens.Lens' DescribeAutoMLJobResponse Prelude.Text
describeAutoMLJobResponse_autoMLJobName = Lens.lens (\DescribeAutoMLJobResponse' {autoMLJobName} -> autoMLJobName) (\s@DescribeAutoMLJobResponse' {} a -> s {autoMLJobName = a} :: DescribeAutoMLJobResponse)

-- | Returns the ARN of the AutoML job.
describeAutoMLJobResponse_autoMLJobArn :: Lens.Lens' DescribeAutoMLJobResponse Prelude.Text
describeAutoMLJobResponse_autoMLJobArn = Lens.lens (\DescribeAutoMLJobResponse' {autoMLJobArn} -> autoMLJobArn) (\s@DescribeAutoMLJobResponse' {} a -> s {autoMLJobArn = a} :: DescribeAutoMLJobResponse)

-- | Returns the input data configuration for the AutoML job..
describeAutoMLJobResponse_inputDataConfig :: Lens.Lens' DescribeAutoMLJobResponse (Prelude.NonEmpty AutoMLChannel)
describeAutoMLJobResponse_inputDataConfig = Lens.lens (\DescribeAutoMLJobResponse' {inputDataConfig} -> inputDataConfig) (\s@DescribeAutoMLJobResponse' {} a -> s {inputDataConfig = a} :: DescribeAutoMLJobResponse) Prelude.. Lens._Coerce

-- | Returns the job\'s output data config.
describeAutoMLJobResponse_outputDataConfig :: Lens.Lens' DescribeAutoMLJobResponse AutoMLOutputDataConfig
describeAutoMLJobResponse_outputDataConfig = Lens.lens (\DescribeAutoMLJobResponse' {outputDataConfig} -> outputDataConfig) (\s@DescribeAutoMLJobResponse' {} a -> s {outputDataConfig = a} :: DescribeAutoMLJobResponse)

-- | The Amazon Resource Name (ARN) of the Amazon Web Services Identity and
-- Access Management (IAM) role that has read permission to the input data
-- location and write permission to the output data location in Amazon S3.
describeAutoMLJobResponse_roleArn :: Lens.Lens' DescribeAutoMLJobResponse Prelude.Text
describeAutoMLJobResponse_roleArn = Lens.lens (\DescribeAutoMLJobResponse' {roleArn} -> roleArn) (\s@DescribeAutoMLJobResponse' {} a -> s {roleArn = a} :: DescribeAutoMLJobResponse)

-- | Returns the creation time of the AutoML job.
describeAutoMLJobResponse_creationTime :: Lens.Lens' DescribeAutoMLJobResponse Prelude.UTCTime
describeAutoMLJobResponse_creationTime = Lens.lens (\DescribeAutoMLJobResponse' {creationTime} -> creationTime) (\s@DescribeAutoMLJobResponse' {} a -> s {creationTime = a} :: DescribeAutoMLJobResponse) Prelude.. Core._Time

-- | Returns the job\'s last modified time.
describeAutoMLJobResponse_lastModifiedTime :: Lens.Lens' DescribeAutoMLJobResponse Prelude.UTCTime
describeAutoMLJobResponse_lastModifiedTime = Lens.lens (\DescribeAutoMLJobResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeAutoMLJobResponse' {} a -> s {lastModifiedTime = a} :: DescribeAutoMLJobResponse) Prelude.. Core._Time

-- | Returns the status of the AutoML job.
describeAutoMLJobResponse_autoMLJobStatus :: Lens.Lens' DescribeAutoMLJobResponse AutoMLJobStatus
describeAutoMLJobResponse_autoMLJobStatus = Lens.lens (\DescribeAutoMLJobResponse' {autoMLJobStatus} -> autoMLJobStatus) (\s@DescribeAutoMLJobResponse' {} a -> s {autoMLJobStatus = a} :: DescribeAutoMLJobResponse)

-- | Returns the secondary status of the AutoML job.
describeAutoMLJobResponse_autoMLJobSecondaryStatus :: Lens.Lens' DescribeAutoMLJobResponse AutoMLJobSecondaryStatus
describeAutoMLJobResponse_autoMLJobSecondaryStatus = Lens.lens (\DescribeAutoMLJobResponse' {autoMLJobSecondaryStatus} -> autoMLJobSecondaryStatus) (\s@DescribeAutoMLJobResponse' {} a -> s {autoMLJobSecondaryStatus = a} :: DescribeAutoMLJobResponse)

instance Prelude.NFData DescribeAutoMLJobResponse
