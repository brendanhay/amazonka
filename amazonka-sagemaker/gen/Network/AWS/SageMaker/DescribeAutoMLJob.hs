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
-- Returns information about an Amazon SageMaker job.
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
    describeAutoMLJobResponse_endTime,
    describeAutoMLJobResponse_resolvedAttributes,
    describeAutoMLJobResponse_autoMLJobArtifacts,
    describeAutoMLJobResponse_failureReason,
    describeAutoMLJobResponse_autoMLJobObjective,
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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeAutoMLJob' smart constructor.
data DescribeAutoMLJob = DescribeAutoMLJob'
  { -- | Request information about a job using that job\'s unique name.
    autoMLJobName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAutoMLJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoMLJobName', 'describeAutoMLJob_autoMLJobName' - Request information about a job using that job\'s unique name.
newDescribeAutoMLJob ::
  -- | 'autoMLJobName'
  Core.Text ->
  DescribeAutoMLJob
newDescribeAutoMLJob pAutoMLJobName_ =
  DescribeAutoMLJob' {autoMLJobName = pAutoMLJobName_}

-- | Request information about a job using that job\'s unique name.
describeAutoMLJob_autoMLJobName :: Lens.Lens' DescribeAutoMLJob Core.Text
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
            Core.<$> (x Core..?> "GenerateCandidateDefinitionsOnly")
            Core.<*> (x Core..?> "EndTime")
            Core.<*> (x Core..?> "ResolvedAttributes")
            Core.<*> (x Core..?> "AutoMLJobArtifacts")
            Core.<*> (x Core..?> "FailureReason")
            Core.<*> (x Core..?> "AutoMLJobObjective")
            Core.<*> (x Core..?> "AutoMLJobConfig")
            Core.<*> (x Core..?> "ProblemType")
            Core.<*> (x Core..?> "BestCandidate")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "AutoMLJobName")
            Core.<*> (x Core..:> "AutoMLJobArn")
            Core.<*> (x Core..:> "InputDataConfig")
            Core.<*> (x Core..:> "OutputDataConfig")
            Core.<*> (x Core..:> "RoleArn")
            Core.<*> (x Core..:> "CreationTime")
            Core.<*> (x Core..:> "LastModifiedTime")
            Core.<*> (x Core..:> "AutoMLJobStatus")
            Core.<*> (x Core..:> "AutoMLJobSecondaryStatus")
      )

instance Core.Hashable DescribeAutoMLJob

instance Core.NFData DescribeAutoMLJob

instance Core.ToHeaders DescribeAutoMLJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DescribeAutoMLJob" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeAutoMLJob where
  toJSON DescribeAutoMLJob' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("AutoMLJobName" Core..= autoMLJobName)]
      )

instance Core.ToPath DescribeAutoMLJob where
  toPath = Core.const "/"

instance Core.ToQuery DescribeAutoMLJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeAutoMLJobResponse' smart constructor.
data DescribeAutoMLJobResponse = DescribeAutoMLJobResponse'
  { -- | Returns the job\'s output from GenerateCandidateDefinitionsOnly.
    generateCandidateDefinitionsOnly :: Core.Maybe Core.Bool,
    -- | Returns the job\'s end time.
    endTime :: Core.Maybe Core.POSIX,
    -- | This contains ProblemType, AutoMLJobObjective and CompletionCriteria.
    -- They\'re auto-inferred values, if not provided by you. If you do provide
    -- them, then they\'ll be the same as provided.
    resolvedAttributes :: Core.Maybe ResolvedAttributes,
    -- | Returns information on the job\'s artifacts found in AutoMLJobArtifacts.
    autoMLJobArtifacts :: Core.Maybe AutoMLJobArtifacts,
    -- | Returns the job\'s FailureReason.
    failureReason :: Core.Maybe Core.Text,
    -- | Returns the job\'s objective.
    autoMLJobObjective :: Core.Maybe AutoMLJobObjective,
    -- | Returns the job\'s config.
    autoMLJobConfig :: Core.Maybe AutoMLJobConfig,
    -- | Returns the job\'s problem type.
    problemType :: Core.Maybe ProblemType,
    -- | Returns the job\'s BestCandidate.
    bestCandidate :: Core.Maybe AutoMLCandidate,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Returns the name of a job.
    autoMLJobName :: Core.Text,
    -- | Returns the job\'s ARN.
    autoMLJobArn :: Core.Text,
    -- | Returns the job\'s input data config.
    inputDataConfig :: Core.NonEmpty AutoMLChannel,
    -- | Returns the job\'s output data config.
    outputDataConfig :: AutoMLOutputDataConfig,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
    -- (IAM) role that has read permission to the input data location and write
    -- permission to the output data location in Amazon S3.
    roleArn :: Core.Text,
    -- | Returns the job\'s creation time.
    creationTime :: Core.POSIX,
    -- | Returns the job\'s last modified time.
    lastModifiedTime :: Core.POSIX,
    -- | Returns the job\'s AutoMLJobStatus.
    autoMLJobStatus :: AutoMLJobStatus,
    -- | Returns the job\'s AutoMLJobSecondaryStatus.
    autoMLJobSecondaryStatus :: AutoMLJobSecondaryStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAutoMLJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'generateCandidateDefinitionsOnly', 'describeAutoMLJobResponse_generateCandidateDefinitionsOnly' - Returns the job\'s output from GenerateCandidateDefinitionsOnly.
--
-- 'endTime', 'describeAutoMLJobResponse_endTime' - Returns the job\'s end time.
--
-- 'resolvedAttributes', 'describeAutoMLJobResponse_resolvedAttributes' - This contains ProblemType, AutoMLJobObjective and CompletionCriteria.
-- They\'re auto-inferred values, if not provided by you. If you do provide
-- them, then they\'ll be the same as provided.
--
-- 'autoMLJobArtifacts', 'describeAutoMLJobResponse_autoMLJobArtifacts' - Returns information on the job\'s artifacts found in AutoMLJobArtifacts.
--
-- 'failureReason', 'describeAutoMLJobResponse_failureReason' - Returns the job\'s FailureReason.
--
-- 'autoMLJobObjective', 'describeAutoMLJobResponse_autoMLJobObjective' - Returns the job\'s objective.
--
-- 'autoMLJobConfig', 'describeAutoMLJobResponse_autoMLJobConfig' - Returns the job\'s config.
--
-- 'problemType', 'describeAutoMLJobResponse_problemType' - Returns the job\'s problem type.
--
-- 'bestCandidate', 'describeAutoMLJobResponse_bestCandidate' - Returns the job\'s BestCandidate.
--
-- 'httpStatus', 'describeAutoMLJobResponse_httpStatus' - The response's http status code.
--
-- 'autoMLJobName', 'describeAutoMLJobResponse_autoMLJobName' - Returns the name of a job.
--
-- 'autoMLJobArn', 'describeAutoMLJobResponse_autoMLJobArn' - Returns the job\'s ARN.
--
-- 'inputDataConfig', 'describeAutoMLJobResponse_inputDataConfig' - Returns the job\'s input data config.
--
-- 'outputDataConfig', 'describeAutoMLJobResponse_outputDataConfig' - Returns the job\'s output data config.
--
-- 'roleArn', 'describeAutoMLJobResponse_roleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that has read permission to the input data location and write
-- permission to the output data location in Amazon S3.
--
-- 'creationTime', 'describeAutoMLJobResponse_creationTime' - Returns the job\'s creation time.
--
-- 'lastModifiedTime', 'describeAutoMLJobResponse_lastModifiedTime' - Returns the job\'s last modified time.
--
-- 'autoMLJobStatus', 'describeAutoMLJobResponse_autoMLJobStatus' - Returns the job\'s AutoMLJobStatus.
--
-- 'autoMLJobSecondaryStatus', 'describeAutoMLJobResponse_autoMLJobSecondaryStatus' - Returns the job\'s AutoMLJobSecondaryStatus.
newDescribeAutoMLJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'autoMLJobName'
  Core.Text ->
  -- | 'autoMLJobArn'
  Core.Text ->
  -- | 'inputDataConfig'
  Core.NonEmpty AutoMLChannel ->
  -- | 'outputDataConfig'
  AutoMLOutputDataConfig ->
  -- | 'roleArn'
  Core.Text ->
  -- | 'creationTime'
  Core.UTCTime ->
  -- | 'lastModifiedTime'
  Core.UTCTime ->
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
          Core.Nothing,
        endTime = Core.Nothing,
        resolvedAttributes = Core.Nothing,
        autoMLJobArtifacts = Core.Nothing,
        failureReason = Core.Nothing,
        autoMLJobObjective = Core.Nothing,
        autoMLJobConfig = Core.Nothing,
        problemType = Core.Nothing,
        bestCandidate = Core.Nothing,
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

-- | Returns the job\'s output from GenerateCandidateDefinitionsOnly.
describeAutoMLJobResponse_generateCandidateDefinitionsOnly :: Lens.Lens' DescribeAutoMLJobResponse (Core.Maybe Core.Bool)
describeAutoMLJobResponse_generateCandidateDefinitionsOnly = Lens.lens (\DescribeAutoMLJobResponse' {generateCandidateDefinitionsOnly} -> generateCandidateDefinitionsOnly) (\s@DescribeAutoMLJobResponse' {} a -> s {generateCandidateDefinitionsOnly = a} :: DescribeAutoMLJobResponse)

-- | Returns the job\'s end time.
describeAutoMLJobResponse_endTime :: Lens.Lens' DescribeAutoMLJobResponse (Core.Maybe Core.UTCTime)
describeAutoMLJobResponse_endTime = Lens.lens (\DescribeAutoMLJobResponse' {endTime} -> endTime) (\s@DescribeAutoMLJobResponse' {} a -> s {endTime = a} :: DescribeAutoMLJobResponse) Core.. Lens.mapping Core._Time

-- | This contains ProblemType, AutoMLJobObjective and CompletionCriteria.
-- They\'re auto-inferred values, if not provided by you. If you do provide
-- them, then they\'ll be the same as provided.
describeAutoMLJobResponse_resolvedAttributes :: Lens.Lens' DescribeAutoMLJobResponse (Core.Maybe ResolvedAttributes)
describeAutoMLJobResponse_resolvedAttributes = Lens.lens (\DescribeAutoMLJobResponse' {resolvedAttributes} -> resolvedAttributes) (\s@DescribeAutoMLJobResponse' {} a -> s {resolvedAttributes = a} :: DescribeAutoMLJobResponse)

-- | Returns information on the job\'s artifacts found in AutoMLJobArtifacts.
describeAutoMLJobResponse_autoMLJobArtifacts :: Lens.Lens' DescribeAutoMLJobResponse (Core.Maybe AutoMLJobArtifacts)
describeAutoMLJobResponse_autoMLJobArtifacts = Lens.lens (\DescribeAutoMLJobResponse' {autoMLJobArtifacts} -> autoMLJobArtifacts) (\s@DescribeAutoMLJobResponse' {} a -> s {autoMLJobArtifacts = a} :: DescribeAutoMLJobResponse)

-- | Returns the job\'s FailureReason.
describeAutoMLJobResponse_failureReason :: Lens.Lens' DescribeAutoMLJobResponse (Core.Maybe Core.Text)
describeAutoMLJobResponse_failureReason = Lens.lens (\DescribeAutoMLJobResponse' {failureReason} -> failureReason) (\s@DescribeAutoMLJobResponse' {} a -> s {failureReason = a} :: DescribeAutoMLJobResponse)

-- | Returns the job\'s objective.
describeAutoMLJobResponse_autoMLJobObjective :: Lens.Lens' DescribeAutoMLJobResponse (Core.Maybe AutoMLJobObjective)
describeAutoMLJobResponse_autoMLJobObjective = Lens.lens (\DescribeAutoMLJobResponse' {autoMLJobObjective} -> autoMLJobObjective) (\s@DescribeAutoMLJobResponse' {} a -> s {autoMLJobObjective = a} :: DescribeAutoMLJobResponse)

-- | Returns the job\'s config.
describeAutoMLJobResponse_autoMLJobConfig :: Lens.Lens' DescribeAutoMLJobResponse (Core.Maybe AutoMLJobConfig)
describeAutoMLJobResponse_autoMLJobConfig = Lens.lens (\DescribeAutoMLJobResponse' {autoMLJobConfig} -> autoMLJobConfig) (\s@DescribeAutoMLJobResponse' {} a -> s {autoMLJobConfig = a} :: DescribeAutoMLJobResponse)

-- | Returns the job\'s problem type.
describeAutoMLJobResponse_problemType :: Lens.Lens' DescribeAutoMLJobResponse (Core.Maybe ProblemType)
describeAutoMLJobResponse_problemType = Lens.lens (\DescribeAutoMLJobResponse' {problemType} -> problemType) (\s@DescribeAutoMLJobResponse' {} a -> s {problemType = a} :: DescribeAutoMLJobResponse)

-- | Returns the job\'s BestCandidate.
describeAutoMLJobResponse_bestCandidate :: Lens.Lens' DescribeAutoMLJobResponse (Core.Maybe AutoMLCandidate)
describeAutoMLJobResponse_bestCandidate = Lens.lens (\DescribeAutoMLJobResponse' {bestCandidate} -> bestCandidate) (\s@DescribeAutoMLJobResponse' {} a -> s {bestCandidate = a} :: DescribeAutoMLJobResponse)

-- | The response's http status code.
describeAutoMLJobResponse_httpStatus :: Lens.Lens' DescribeAutoMLJobResponse Core.Int
describeAutoMLJobResponse_httpStatus = Lens.lens (\DescribeAutoMLJobResponse' {httpStatus} -> httpStatus) (\s@DescribeAutoMLJobResponse' {} a -> s {httpStatus = a} :: DescribeAutoMLJobResponse)

-- | Returns the name of a job.
describeAutoMLJobResponse_autoMLJobName :: Lens.Lens' DescribeAutoMLJobResponse Core.Text
describeAutoMLJobResponse_autoMLJobName = Lens.lens (\DescribeAutoMLJobResponse' {autoMLJobName} -> autoMLJobName) (\s@DescribeAutoMLJobResponse' {} a -> s {autoMLJobName = a} :: DescribeAutoMLJobResponse)

-- | Returns the job\'s ARN.
describeAutoMLJobResponse_autoMLJobArn :: Lens.Lens' DescribeAutoMLJobResponse Core.Text
describeAutoMLJobResponse_autoMLJobArn = Lens.lens (\DescribeAutoMLJobResponse' {autoMLJobArn} -> autoMLJobArn) (\s@DescribeAutoMLJobResponse' {} a -> s {autoMLJobArn = a} :: DescribeAutoMLJobResponse)

-- | Returns the job\'s input data config.
describeAutoMLJobResponse_inputDataConfig :: Lens.Lens' DescribeAutoMLJobResponse (Core.NonEmpty AutoMLChannel)
describeAutoMLJobResponse_inputDataConfig = Lens.lens (\DescribeAutoMLJobResponse' {inputDataConfig} -> inputDataConfig) (\s@DescribeAutoMLJobResponse' {} a -> s {inputDataConfig = a} :: DescribeAutoMLJobResponse) Core.. Lens._Coerce

-- | Returns the job\'s output data config.
describeAutoMLJobResponse_outputDataConfig :: Lens.Lens' DescribeAutoMLJobResponse AutoMLOutputDataConfig
describeAutoMLJobResponse_outputDataConfig = Lens.lens (\DescribeAutoMLJobResponse' {outputDataConfig} -> outputDataConfig) (\s@DescribeAutoMLJobResponse' {} a -> s {outputDataConfig = a} :: DescribeAutoMLJobResponse)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that has read permission to the input data location and write
-- permission to the output data location in Amazon S3.
describeAutoMLJobResponse_roleArn :: Lens.Lens' DescribeAutoMLJobResponse Core.Text
describeAutoMLJobResponse_roleArn = Lens.lens (\DescribeAutoMLJobResponse' {roleArn} -> roleArn) (\s@DescribeAutoMLJobResponse' {} a -> s {roleArn = a} :: DescribeAutoMLJobResponse)

-- | Returns the job\'s creation time.
describeAutoMLJobResponse_creationTime :: Lens.Lens' DescribeAutoMLJobResponse Core.UTCTime
describeAutoMLJobResponse_creationTime = Lens.lens (\DescribeAutoMLJobResponse' {creationTime} -> creationTime) (\s@DescribeAutoMLJobResponse' {} a -> s {creationTime = a} :: DescribeAutoMLJobResponse) Core.. Core._Time

-- | Returns the job\'s last modified time.
describeAutoMLJobResponse_lastModifiedTime :: Lens.Lens' DescribeAutoMLJobResponse Core.UTCTime
describeAutoMLJobResponse_lastModifiedTime = Lens.lens (\DescribeAutoMLJobResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeAutoMLJobResponse' {} a -> s {lastModifiedTime = a} :: DescribeAutoMLJobResponse) Core.. Core._Time

-- | Returns the job\'s AutoMLJobStatus.
describeAutoMLJobResponse_autoMLJobStatus :: Lens.Lens' DescribeAutoMLJobResponse AutoMLJobStatus
describeAutoMLJobResponse_autoMLJobStatus = Lens.lens (\DescribeAutoMLJobResponse' {autoMLJobStatus} -> autoMLJobStatus) (\s@DescribeAutoMLJobResponse' {} a -> s {autoMLJobStatus = a} :: DescribeAutoMLJobResponse)

-- | Returns the job\'s AutoMLJobSecondaryStatus.
describeAutoMLJobResponse_autoMLJobSecondaryStatus :: Lens.Lens' DescribeAutoMLJobResponse AutoMLJobSecondaryStatus
describeAutoMLJobResponse_autoMLJobSecondaryStatus = Lens.lens (\DescribeAutoMLJobResponse' {autoMLJobSecondaryStatus} -> autoMLJobSecondaryStatus) (\s@DescribeAutoMLJobResponse' {} a -> s {autoMLJobSecondaryStatus = a} :: DescribeAutoMLJobResponse)

instance Core.NFData DescribeAutoMLJobResponse
