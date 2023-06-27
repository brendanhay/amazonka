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
-- Module      : Amazonka.SageMaker.DescribeAutoMLJobV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an AutoML job created by calling
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateAutoMLJobV2.html CreateAutoMLJobV2>
-- or
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateAutoMLJob.html CreateAutoMLJob>.
module Amazonka.SageMaker.DescribeAutoMLJobV2
  ( -- * Creating a Request
    DescribeAutoMLJobV2 (..),
    newDescribeAutoMLJobV2,

    -- * Request Lenses
    describeAutoMLJobV2_autoMLJobName,

    -- * Destructuring the Response
    DescribeAutoMLJobV2Response (..),
    newDescribeAutoMLJobV2Response,

    -- * Response Lenses
    describeAutoMLJobV2Response_autoMLJobArtifacts,
    describeAutoMLJobV2Response_autoMLJobObjective,
    describeAutoMLJobV2Response_autoMLProblemTypeConfig,
    describeAutoMLJobV2Response_autoMLProblemTypeConfigName,
    describeAutoMLJobV2Response_bestCandidate,
    describeAutoMLJobV2Response_dataSplitConfig,
    describeAutoMLJobV2Response_endTime,
    describeAutoMLJobV2Response_failureReason,
    describeAutoMLJobV2Response_modelDeployConfig,
    describeAutoMLJobV2Response_modelDeployResult,
    describeAutoMLJobV2Response_partialFailureReasons,
    describeAutoMLJobV2Response_resolvedAttributes,
    describeAutoMLJobV2Response_securityConfig,
    describeAutoMLJobV2Response_httpStatus,
    describeAutoMLJobV2Response_autoMLJobName,
    describeAutoMLJobV2Response_autoMLJobArn,
    describeAutoMLJobV2Response_autoMLJobInputDataConfig,
    describeAutoMLJobV2Response_outputDataConfig,
    describeAutoMLJobV2Response_roleArn,
    describeAutoMLJobV2Response_creationTime,
    describeAutoMLJobV2Response_lastModifiedTime,
    describeAutoMLJobV2Response_autoMLJobStatus,
    describeAutoMLJobV2Response_autoMLJobSecondaryStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeAutoMLJobV2' smart constructor.
data DescribeAutoMLJobV2 = DescribeAutoMLJobV2'
  { -- | Requests information about an AutoML job V2 using its unique name.
    autoMLJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAutoMLJobV2' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoMLJobName', 'describeAutoMLJobV2_autoMLJobName' - Requests information about an AutoML job V2 using its unique name.
newDescribeAutoMLJobV2 ::
  -- | 'autoMLJobName'
  Prelude.Text ->
  DescribeAutoMLJobV2
newDescribeAutoMLJobV2 pAutoMLJobName_ =
  DescribeAutoMLJobV2'
    { autoMLJobName =
        pAutoMLJobName_
    }

-- | Requests information about an AutoML job V2 using its unique name.
describeAutoMLJobV2_autoMLJobName :: Lens.Lens' DescribeAutoMLJobV2 Prelude.Text
describeAutoMLJobV2_autoMLJobName = Lens.lens (\DescribeAutoMLJobV2' {autoMLJobName} -> autoMLJobName) (\s@DescribeAutoMLJobV2' {} a -> s {autoMLJobName = a} :: DescribeAutoMLJobV2)

instance Core.AWSRequest DescribeAutoMLJobV2 where
  type
    AWSResponse DescribeAutoMLJobV2 =
      DescribeAutoMLJobV2Response
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAutoMLJobV2Response'
            Prelude.<$> (x Data..?> "AutoMLJobArtifacts")
            Prelude.<*> (x Data..?> "AutoMLJobObjective")
            Prelude.<*> (x Data..?> "AutoMLProblemTypeConfig")
            Prelude.<*> (x Data..?> "AutoMLProblemTypeConfigName")
            Prelude.<*> (x Data..?> "BestCandidate")
            Prelude.<*> (x Data..?> "DataSplitConfig")
            Prelude.<*> (x Data..?> "EndTime")
            Prelude.<*> (x Data..?> "FailureReason")
            Prelude.<*> (x Data..?> "ModelDeployConfig")
            Prelude.<*> (x Data..?> "ModelDeployResult")
            Prelude.<*> (x Data..?> "PartialFailureReasons")
            Prelude.<*> (x Data..?> "ResolvedAttributes")
            Prelude.<*> (x Data..?> "SecurityConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "AutoMLJobName")
            Prelude.<*> (x Data..:> "AutoMLJobArn")
            Prelude.<*> (x Data..:> "AutoMLJobInputDataConfig")
            Prelude.<*> (x Data..:> "OutputDataConfig")
            Prelude.<*> (x Data..:> "RoleArn")
            Prelude.<*> (x Data..:> "CreationTime")
            Prelude.<*> (x Data..:> "LastModifiedTime")
            Prelude.<*> (x Data..:> "AutoMLJobStatus")
            Prelude.<*> (x Data..:> "AutoMLJobSecondaryStatus")
      )

instance Prelude.Hashable DescribeAutoMLJobV2 where
  hashWithSalt _salt DescribeAutoMLJobV2' {..} =
    _salt `Prelude.hashWithSalt` autoMLJobName

instance Prelude.NFData DescribeAutoMLJobV2 where
  rnf DescribeAutoMLJobV2' {..} =
    Prelude.rnf autoMLJobName

instance Data.ToHeaders DescribeAutoMLJobV2 where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DescribeAutoMLJobV2" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeAutoMLJobV2 where
  toJSON DescribeAutoMLJobV2' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AutoMLJobName" Data..= autoMLJobName)
          ]
      )

instance Data.ToPath DescribeAutoMLJobV2 where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAutoMLJobV2 where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAutoMLJobV2Response' smart constructor.
data DescribeAutoMLJobV2Response = DescribeAutoMLJobV2Response'
  { autoMLJobArtifacts :: Prelude.Maybe AutoMLJobArtifacts,
    -- | Returns the job\'s objective.
    autoMLJobObjective :: Prelude.Maybe AutoMLJobObjective,
    -- | Returns the configuration settings of the problem type set for the
    -- AutoML job V2.
    autoMLProblemTypeConfig :: Prelude.Maybe AutoMLProblemTypeConfig,
    -- | Returns the name of the problem type configuration set for the AutoML
    -- job V2.
    autoMLProblemTypeConfigName :: Prelude.Maybe AutoMLProblemTypeConfigName,
    -- | Information about the candidate produced by an AutoML training job V2,
    -- including its status, steps, and other properties.
    bestCandidate :: Prelude.Maybe AutoMLCandidate,
    -- | Returns the configuration settings of how the data are split into train
    -- and validation datasets.
    dataSplitConfig :: Prelude.Maybe AutoMLDataSplitConfig,
    -- | Returns the end time of the AutoML job V2.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | Returns the reason for the failure of the AutoML job V2, when
    -- applicable.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the model was deployed automatically to an endpoint
    -- and the name of that endpoint if deployed automatically.
    modelDeployConfig :: Prelude.Maybe ModelDeployConfig,
    -- | Provides information about endpoint for the model deployment.
    modelDeployResult :: Prelude.Maybe ModelDeployResult,
    -- | Returns a list of reasons for partial failures within an AutoML job V2.
    partialFailureReasons :: Prelude.Maybe (Prelude.NonEmpty AutoMLPartialFailureReason),
    -- | Returns the resolved attributes used by the AutoML job V2.
    resolvedAttributes :: Prelude.Maybe AutoMLResolvedAttributes,
    -- | Returns the security configuration for traffic encryption or Amazon VPC
    -- settings.
    securityConfig :: Prelude.Maybe AutoMLSecurityConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Returns the name of the AutoML job V2.
    autoMLJobName :: Prelude.Text,
    -- | Returns the Amazon Resource Name (ARN) of the AutoML job V2.
    autoMLJobArn :: Prelude.Text,
    -- | Returns an array of channel objects describing the input data and their
    -- location.
    autoMLJobInputDataConfig :: Prelude.NonEmpty AutoMLJobChannel,
    -- | Returns the job\'s output data config.
    outputDataConfig :: AutoMLOutputDataConfig,
    -- | The ARN of the Identity and Access Management role that has read
    -- permission to the input data location and write permission to the output
    -- data location in Amazon S3.
    roleArn :: Prelude.Text,
    -- | Returns the creation time of the AutoML job V2.
    creationTime :: Data.POSIX,
    -- | Returns the job\'s last modified time.
    lastModifiedTime :: Data.POSIX,
    -- | Returns the status of the AutoML job V2.
    autoMLJobStatus :: AutoMLJobStatus,
    -- | Returns the secondary status of the AutoML job V2.
    autoMLJobSecondaryStatus :: AutoMLJobSecondaryStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAutoMLJobV2Response' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoMLJobArtifacts', 'describeAutoMLJobV2Response_autoMLJobArtifacts' - Undocumented member.
--
-- 'autoMLJobObjective', 'describeAutoMLJobV2Response_autoMLJobObjective' - Returns the job\'s objective.
--
-- 'autoMLProblemTypeConfig', 'describeAutoMLJobV2Response_autoMLProblemTypeConfig' - Returns the configuration settings of the problem type set for the
-- AutoML job V2.
--
-- 'autoMLProblemTypeConfigName', 'describeAutoMLJobV2Response_autoMLProblemTypeConfigName' - Returns the name of the problem type configuration set for the AutoML
-- job V2.
--
-- 'bestCandidate', 'describeAutoMLJobV2Response_bestCandidate' - Information about the candidate produced by an AutoML training job V2,
-- including its status, steps, and other properties.
--
-- 'dataSplitConfig', 'describeAutoMLJobV2Response_dataSplitConfig' - Returns the configuration settings of how the data are split into train
-- and validation datasets.
--
-- 'endTime', 'describeAutoMLJobV2Response_endTime' - Returns the end time of the AutoML job V2.
--
-- 'failureReason', 'describeAutoMLJobV2Response_failureReason' - Returns the reason for the failure of the AutoML job V2, when
-- applicable.
--
-- 'modelDeployConfig', 'describeAutoMLJobV2Response_modelDeployConfig' - Indicates whether the model was deployed automatically to an endpoint
-- and the name of that endpoint if deployed automatically.
--
-- 'modelDeployResult', 'describeAutoMLJobV2Response_modelDeployResult' - Provides information about endpoint for the model deployment.
--
-- 'partialFailureReasons', 'describeAutoMLJobV2Response_partialFailureReasons' - Returns a list of reasons for partial failures within an AutoML job V2.
--
-- 'resolvedAttributes', 'describeAutoMLJobV2Response_resolvedAttributes' - Returns the resolved attributes used by the AutoML job V2.
--
-- 'securityConfig', 'describeAutoMLJobV2Response_securityConfig' - Returns the security configuration for traffic encryption or Amazon VPC
-- settings.
--
-- 'httpStatus', 'describeAutoMLJobV2Response_httpStatus' - The response's http status code.
--
-- 'autoMLJobName', 'describeAutoMLJobV2Response_autoMLJobName' - Returns the name of the AutoML job V2.
--
-- 'autoMLJobArn', 'describeAutoMLJobV2Response_autoMLJobArn' - Returns the Amazon Resource Name (ARN) of the AutoML job V2.
--
-- 'autoMLJobInputDataConfig', 'describeAutoMLJobV2Response_autoMLJobInputDataConfig' - Returns an array of channel objects describing the input data and their
-- location.
--
-- 'outputDataConfig', 'describeAutoMLJobV2Response_outputDataConfig' - Returns the job\'s output data config.
--
-- 'roleArn', 'describeAutoMLJobV2Response_roleArn' - The ARN of the Identity and Access Management role that has read
-- permission to the input data location and write permission to the output
-- data location in Amazon S3.
--
-- 'creationTime', 'describeAutoMLJobV2Response_creationTime' - Returns the creation time of the AutoML job V2.
--
-- 'lastModifiedTime', 'describeAutoMLJobV2Response_lastModifiedTime' - Returns the job\'s last modified time.
--
-- 'autoMLJobStatus', 'describeAutoMLJobV2Response_autoMLJobStatus' - Returns the status of the AutoML job V2.
--
-- 'autoMLJobSecondaryStatus', 'describeAutoMLJobV2Response_autoMLJobSecondaryStatus' - Returns the secondary status of the AutoML job V2.
newDescribeAutoMLJobV2Response ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'autoMLJobName'
  Prelude.Text ->
  -- | 'autoMLJobArn'
  Prelude.Text ->
  -- | 'autoMLJobInputDataConfig'
  Prelude.NonEmpty AutoMLJobChannel ->
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
  DescribeAutoMLJobV2Response
newDescribeAutoMLJobV2Response
  pHttpStatus_
  pAutoMLJobName_
  pAutoMLJobArn_
  pAutoMLJobInputDataConfig_
  pOutputDataConfig_
  pRoleArn_
  pCreationTime_
  pLastModifiedTime_
  pAutoMLJobStatus_
  pAutoMLJobSecondaryStatus_ =
    DescribeAutoMLJobV2Response'
      { autoMLJobArtifacts =
          Prelude.Nothing,
        autoMLJobObjective = Prelude.Nothing,
        autoMLProblemTypeConfig = Prelude.Nothing,
        autoMLProblemTypeConfigName = Prelude.Nothing,
        bestCandidate = Prelude.Nothing,
        dataSplitConfig = Prelude.Nothing,
        endTime = Prelude.Nothing,
        failureReason = Prelude.Nothing,
        modelDeployConfig = Prelude.Nothing,
        modelDeployResult = Prelude.Nothing,
        partialFailureReasons = Prelude.Nothing,
        resolvedAttributes = Prelude.Nothing,
        securityConfig = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        autoMLJobName = pAutoMLJobName_,
        autoMLJobArn = pAutoMLJobArn_,
        autoMLJobInputDataConfig =
          Lens.coerced
            Lens.# pAutoMLJobInputDataConfig_,
        outputDataConfig = pOutputDataConfig_,
        roleArn = pRoleArn_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        lastModifiedTime =
          Data._Time Lens.# pLastModifiedTime_,
        autoMLJobStatus = pAutoMLJobStatus_,
        autoMLJobSecondaryStatus =
          pAutoMLJobSecondaryStatus_
      }

-- | Undocumented member.
describeAutoMLJobV2Response_autoMLJobArtifacts :: Lens.Lens' DescribeAutoMLJobV2Response (Prelude.Maybe AutoMLJobArtifacts)
describeAutoMLJobV2Response_autoMLJobArtifacts = Lens.lens (\DescribeAutoMLJobV2Response' {autoMLJobArtifacts} -> autoMLJobArtifacts) (\s@DescribeAutoMLJobV2Response' {} a -> s {autoMLJobArtifacts = a} :: DescribeAutoMLJobV2Response)

-- | Returns the job\'s objective.
describeAutoMLJobV2Response_autoMLJobObjective :: Lens.Lens' DescribeAutoMLJobV2Response (Prelude.Maybe AutoMLJobObjective)
describeAutoMLJobV2Response_autoMLJobObjective = Lens.lens (\DescribeAutoMLJobV2Response' {autoMLJobObjective} -> autoMLJobObjective) (\s@DescribeAutoMLJobV2Response' {} a -> s {autoMLJobObjective = a} :: DescribeAutoMLJobV2Response)

-- | Returns the configuration settings of the problem type set for the
-- AutoML job V2.
describeAutoMLJobV2Response_autoMLProblemTypeConfig :: Lens.Lens' DescribeAutoMLJobV2Response (Prelude.Maybe AutoMLProblemTypeConfig)
describeAutoMLJobV2Response_autoMLProblemTypeConfig = Lens.lens (\DescribeAutoMLJobV2Response' {autoMLProblemTypeConfig} -> autoMLProblemTypeConfig) (\s@DescribeAutoMLJobV2Response' {} a -> s {autoMLProblemTypeConfig = a} :: DescribeAutoMLJobV2Response)

-- | Returns the name of the problem type configuration set for the AutoML
-- job V2.
describeAutoMLJobV2Response_autoMLProblemTypeConfigName :: Lens.Lens' DescribeAutoMLJobV2Response (Prelude.Maybe AutoMLProblemTypeConfigName)
describeAutoMLJobV2Response_autoMLProblemTypeConfigName = Lens.lens (\DescribeAutoMLJobV2Response' {autoMLProblemTypeConfigName} -> autoMLProblemTypeConfigName) (\s@DescribeAutoMLJobV2Response' {} a -> s {autoMLProblemTypeConfigName = a} :: DescribeAutoMLJobV2Response)

-- | Information about the candidate produced by an AutoML training job V2,
-- including its status, steps, and other properties.
describeAutoMLJobV2Response_bestCandidate :: Lens.Lens' DescribeAutoMLJobV2Response (Prelude.Maybe AutoMLCandidate)
describeAutoMLJobV2Response_bestCandidate = Lens.lens (\DescribeAutoMLJobV2Response' {bestCandidate} -> bestCandidate) (\s@DescribeAutoMLJobV2Response' {} a -> s {bestCandidate = a} :: DescribeAutoMLJobV2Response)

-- | Returns the configuration settings of how the data are split into train
-- and validation datasets.
describeAutoMLJobV2Response_dataSplitConfig :: Lens.Lens' DescribeAutoMLJobV2Response (Prelude.Maybe AutoMLDataSplitConfig)
describeAutoMLJobV2Response_dataSplitConfig = Lens.lens (\DescribeAutoMLJobV2Response' {dataSplitConfig} -> dataSplitConfig) (\s@DescribeAutoMLJobV2Response' {} a -> s {dataSplitConfig = a} :: DescribeAutoMLJobV2Response)

-- | Returns the end time of the AutoML job V2.
describeAutoMLJobV2Response_endTime :: Lens.Lens' DescribeAutoMLJobV2Response (Prelude.Maybe Prelude.UTCTime)
describeAutoMLJobV2Response_endTime = Lens.lens (\DescribeAutoMLJobV2Response' {endTime} -> endTime) (\s@DescribeAutoMLJobV2Response' {} a -> s {endTime = a} :: DescribeAutoMLJobV2Response) Prelude.. Lens.mapping Data._Time

-- | Returns the reason for the failure of the AutoML job V2, when
-- applicable.
describeAutoMLJobV2Response_failureReason :: Lens.Lens' DescribeAutoMLJobV2Response (Prelude.Maybe Prelude.Text)
describeAutoMLJobV2Response_failureReason = Lens.lens (\DescribeAutoMLJobV2Response' {failureReason} -> failureReason) (\s@DescribeAutoMLJobV2Response' {} a -> s {failureReason = a} :: DescribeAutoMLJobV2Response)

-- | Indicates whether the model was deployed automatically to an endpoint
-- and the name of that endpoint if deployed automatically.
describeAutoMLJobV2Response_modelDeployConfig :: Lens.Lens' DescribeAutoMLJobV2Response (Prelude.Maybe ModelDeployConfig)
describeAutoMLJobV2Response_modelDeployConfig = Lens.lens (\DescribeAutoMLJobV2Response' {modelDeployConfig} -> modelDeployConfig) (\s@DescribeAutoMLJobV2Response' {} a -> s {modelDeployConfig = a} :: DescribeAutoMLJobV2Response)

-- | Provides information about endpoint for the model deployment.
describeAutoMLJobV2Response_modelDeployResult :: Lens.Lens' DescribeAutoMLJobV2Response (Prelude.Maybe ModelDeployResult)
describeAutoMLJobV2Response_modelDeployResult = Lens.lens (\DescribeAutoMLJobV2Response' {modelDeployResult} -> modelDeployResult) (\s@DescribeAutoMLJobV2Response' {} a -> s {modelDeployResult = a} :: DescribeAutoMLJobV2Response)

-- | Returns a list of reasons for partial failures within an AutoML job V2.
describeAutoMLJobV2Response_partialFailureReasons :: Lens.Lens' DescribeAutoMLJobV2Response (Prelude.Maybe (Prelude.NonEmpty AutoMLPartialFailureReason))
describeAutoMLJobV2Response_partialFailureReasons = Lens.lens (\DescribeAutoMLJobV2Response' {partialFailureReasons} -> partialFailureReasons) (\s@DescribeAutoMLJobV2Response' {} a -> s {partialFailureReasons = a} :: DescribeAutoMLJobV2Response) Prelude.. Lens.mapping Lens.coerced

-- | Returns the resolved attributes used by the AutoML job V2.
describeAutoMLJobV2Response_resolvedAttributes :: Lens.Lens' DescribeAutoMLJobV2Response (Prelude.Maybe AutoMLResolvedAttributes)
describeAutoMLJobV2Response_resolvedAttributes = Lens.lens (\DescribeAutoMLJobV2Response' {resolvedAttributes} -> resolvedAttributes) (\s@DescribeAutoMLJobV2Response' {} a -> s {resolvedAttributes = a} :: DescribeAutoMLJobV2Response)

-- | Returns the security configuration for traffic encryption or Amazon VPC
-- settings.
describeAutoMLJobV2Response_securityConfig :: Lens.Lens' DescribeAutoMLJobV2Response (Prelude.Maybe AutoMLSecurityConfig)
describeAutoMLJobV2Response_securityConfig = Lens.lens (\DescribeAutoMLJobV2Response' {securityConfig} -> securityConfig) (\s@DescribeAutoMLJobV2Response' {} a -> s {securityConfig = a} :: DescribeAutoMLJobV2Response)

-- | The response's http status code.
describeAutoMLJobV2Response_httpStatus :: Lens.Lens' DescribeAutoMLJobV2Response Prelude.Int
describeAutoMLJobV2Response_httpStatus = Lens.lens (\DescribeAutoMLJobV2Response' {httpStatus} -> httpStatus) (\s@DescribeAutoMLJobV2Response' {} a -> s {httpStatus = a} :: DescribeAutoMLJobV2Response)

-- | Returns the name of the AutoML job V2.
describeAutoMLJobV2Response_autoMLJobName :: Lens.Lens' DescribeAutoMLJobV2Response Prelude.Text
describeAutoMLJobV2Response_autoMLJobName = Lens.lens (\DescribeAutoMLJobV2Response' {autoMLJobName} -> autoMLJobName) (\s@DescribeAutoMLJobV2Response' {} a -> s {autoMLJobName = a} :: DescribeAutoMLJobV2Response)

-- | Returns the Amazon Resource Name (ARN) of the AutoML job V2.
describeAutoMLJobV2Response_autoMLJobArn :: Lens.Lens' DescribeAutoMLJobV2Response Prelude.Text
describeAutoMLJobV2Response_autoMLJobArn = Lens.lens (\DescribeAutoMLJobV2Response' {autoMLJobArn} -> autoMLJobArn) (\s@DescribeAutoMLJobV2Response' {} a -> s {autoMLJobArn = a} :: DescribeAutoMLJobV2Response)

-- | Returns an array of channel objects describing the input data and their
-- location.
describeAutoMLJobV2Response_autoMLJobInputDataConfig :: Lens.Lens' DescribeAutoMLJobV2Response (Prelude.NonEmpty AutoMLJobChannel)
describeAutoMLJobV2Response_autoMLJobInputDataConfig = Lens.lens (\DescribeAutoMLJobV2Response' {autoMLJobInputDataConfig} -> autoMLJobInputDataConfig) (\s@DescribeAutoMLJobV2Response' {} a -> s {autoMLJobInputDataConfig = a} :: DescribeAutoMLJobV2Response) Prelude.. Lens.coerced

-- | Returns the job\'s output data config.
describeAutoMLJobV2Response_outputDataConfig :: Lens.Lens' DescribeAutoMLJobV2Response AutoMLOutputDataConfig
describeAutoMLJobV2Response_outputDataConfig = Lens.lens (\DescribeAutoMLJobV2Response' {outputDataConfig} -> outputDataConfig) (\s@DescribeAutoMLJobV2Response' {} a -> s {outputDataConfig = a} :: DescribeAutoMLJobV2Response)

-- | The ARN of the Identity and Access Management role that has read
-- permission to the input data location and write permission to the output
-- data location in Amazon S3.
describeAutoMLJobV2Response_roleArn :: Lens.Lens' DescribeAutoMLJobV2Response Prelude.Text
describeAutoMLJobV2Response_roleArn = Lens.lens (\DescribeAutoMLJobV2Response' {roleArn} -> roleArn) (\s@DescribeAutoMLJobV2Response' {} a -> s {roleArn = a} :: DescribeAutoMLJobV2Response)

-- | Returns the creation time of the AutoML job V2.
describeAutoMLJobV2Response_creationTime :: Lens.Lens' DescribeAutoMLJobV2Response Prelude.UTCTime
describeAutoMLJobV2Response_creationTime = Lens.lens (\DescribeAutoMLJobV2Response' {creationTime} -> creationTime) (\s@DescribeAutoMLJobV2Response' {} a -> s {creationTime = a} :: DescribeAutoMLJobV2Response) Prelude.. Data._Time

-- | Returns the job\'s last modified time.
describeAutoMLJobV2Response_lastModifiedTime :: Lens.Lens' DescribeAutoMLJobV2Response Prelude.UTCTime
describeAutoMLJobV2Response_lastModifiedTime = Lens.lens (\DescribeAutoMLJobV2Response' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeAutoMLJobV2Response' {} a -> s {lastModifiedTime = a} :: DescribeAutoMLJobV2Response) Prelude.. Data._Time

-- | Returns the status of the AutoML job V2.
describeAutoMLJobV2Response_autoMLJobStatus :: Lens.Lens' DescribeAutoMLJobV2Response AutoMLJobStatus
describeAutoMLJobV2Response_autoMLJobStatus = Lens.lens (\DescribeAutoMLJobV2Response' {autoMLJobStatus} -> autoMLJobStatus) (\s@DescribeAutoMLJobV2Response' {} a -> s {autoMLJobStatus = a} :: DescribeAutoMLJobV2Response)

-- | Returns the secondary status of the AutoML job V2.
describeAutoMLJobV2Response_autoMLJobSecondaryStatus :: Lens.Lens' DescribeAutoMLJobV2Response AutoMLJobSecondaryStatus
describeAutoMLJobV2Response_autoMLJobSecondaryStatus = Lens.lens (\DescribeAutoMLJobV2Response' {autoMLJobSecondaryStatus} -> autoMLJobSecondaryStatus) (\s@DescribeAutoMLJobV2Response' {} a -> s {autoMLJobSecondaryStatus = a} :: DescribeAutoMLJobV2Response)

instance Prelude.NFData DescribeAutoMLJobV2Response where
  rnf DescribeAutoMLJobV2Response' {..} =
    Prelude.rnf autoMLJobArtifacts
      `Prelude.seq` Prelude.rnf autoMLJobObjective
      `Prelude.seq` Prelude.rnf autoMLProblemTypeConfig
      `Prelude.seq` Prelude.rnf autoMLProblemTypeConfigName
      `Prelude.seq` Prelude.rnf bestCandidate
      `Prelude.seq` Prelude.rnf dataSplitConfig
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf modelDeployConfig
      `Prelude.seq` Prelude.rnf modelDeployResult
      `Prelude.seq` Prelude.rnf partialFailureReasons
      `Prelude.seq` Prelude.rnf resolvedAttributes
      `Prelude.seq` Prelude.rnf securityConfig
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf autoMLJobName
      `Prelude.seq` Prelude.rnf autoMLJobArn
      `Prelude.seq` Prelude.rnf autoMLJobInputDataConfig
      `Prelude.seq` Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf
        autoMLJobStatus
      `Prelude.seq` Prelude.rnf
        autoMLJobSecondaryStatus
