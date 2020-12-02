{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelingJobSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.LabelCounters
import Network.AWS.SageMaker.Types.LabelingJobInputConfig
import Network.AWS.SageMaker.Types.LabelingJobOutput
import Network.AWS.SageMaker.Types.LabelingJobStatus

-- | Provides summary information about a labeling job.
--
--
--
-- /See:/ 'labelingJobSummary' smart constructor.
data LabelingJobSummary = LabelingJobSummary'
  { _ljsFailureReason ::
      !(Maybe Text),
    _ljsAnnotationConsolidationLambdaARN :: !(Maybe Text),
    _ljsInputConfig :: !(Maybe LabelingJobInputConfig),
    _ljsLabelingJobOutput :: !(Maybe LabelingJobOutput),
    _ljsLabelingJobName :: !Text,
    _ljsLabelingJobARN :: !Text,
    _ljsCreationTime :: !POSIX,
    _ljsLastModifiedTime :: !POSIX,
    _ljsLabelingJobStatus :: !LabelingJobStatus,
    _ljsLabelCounters :: !LabelCounters,
    _ljsWorkteamARN :: !Text,
    _ljsPreHumanTaskLambdaARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LabelingJobSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljsFailureReason' - If the @LabelingJobStatus@ field is @Failed@ , this field contains a description of the error.
--
-- * 'ljsAnnotationConsolidationLambdaARN' - The Amazon Resource Name (ARN) of the Lambda function used to consolidate the annotations from individual workers into a label for a data object. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-annotation-consolidation.html Annotation Consolidation> .
--
-- * 'ljsInputConfig' - Input configuration for the labeling job.
--
-- * 'ljsLabelingJobOutput' - The location of the output produced by the labeling job.
--
-- * 'ljsLabelingJobName' - The name of the labeling job.
--
-- * 'ljsLabelingJobARN' - The Amazon Resource Name (ARN) assigned to the labeling job when it was created.
--
-- * 'ljsCreationTime' - The date and time that the job was created (timestamp).
--
-- * 'ljsLastModifiedTime' - The date and time that the job was last modified (timestamp).
--
-- * 'ljsLabelingJobStatus' - The current status of the labeling job.
--
-- * 'ljsLabelCounters' - Counts showing the progress of the labeling job.
--
-- * 'ljsWorkteamARN' - The Amazon Resource Name (ARN) of the work team assigned to the job.
--
-- * 'ljsPreHumanTaskLambdaARN' - The Amazon Resource Name (ARN) of a Lambda function. The function is run before each data object is sent to a worker.
labelingJobSummary ::
  -- | 'ljsLabelingJobName'
  Text ->
  -- | 'ljsLabelingJobARN'
  Text ->
  -- | 'ljsCreationTime'
  UTCTime ->
  -- | 'ljsLastModifiedTime'
  UTCTime ->
  -- | 'ljsLabelingJobStatus'
  LabelingJobStatus ->
  -- | 'ljsLabelCounters'
  LabelCounters ->
  -- | 'ljsWorkteamARN'
  Text ->
  -- | 'ljsPreHumanTaskLambdaARN'
  Text ->
  LabelingJobSummary
labelingJobSummary
  pLabelingJobName_
  pLabelingJobARN_
  pCreationTime_
  pLastModifiedTime_
  pLabelingJobStatus_
  pLabelCounters_
  pWorkteamARN_
  pPreHumanTaskLambdaARN_ =
    LabelingJobSummary'
      { _ljsFailureReason = Nothing,
        _ljsAnnotationConsolidationLambdaARN = Nothing,
        _ljsInputConfig = Nothing,
        _ljsLabelingJobOutput = Nothing,
        _ljsLabelingJobName = pLabelingJobName_,
        _ljsLabelingJobARN = pLabelingJobARN_,
        _ljsCreationTime = _Time # pCreationTime_,
        _ljsLastModifiedTime = _Time # pLastModifiedTime_,
        _ljsLabelingJobStatus = pLabelingJobStatus_,
        _ljsLabelCounters = pLabelCounters_,
        _ljsWorkteamARN = pWorkteamARN_,
        _ljsPreHumanTaskLambdaARN = pPreHumanTaskLambdaARN_
      }

-- | If the @LabelingJobStatus@ field is @Failed@ , this field contains a description of the error.
ljsFailureReason :: Lens' LabelingJobSummary (Maybe Text)
ljsFailureReason = lens _ljsFailureReason (\s a -> s {_ljsFailureReason = a})

-- | The Amazon Resource Name (ARN) of the Lambda function used to consolidate the annotations from individual workers into a label for a data object. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-annotation-consolidation.html Annotation Consolidation> .
ljsAnnotationConsolidationLambdaARN :: Lens' LabelingJobSummary (Maybe Text)
ljsAnnotationConsolidationLambdaARN = lens _ljsAnnotationConsolidationLambdaARN (\s a -> s {_ljsAnnotationConsolidationLambdaARN = a})

-- | Input configuration for the labeling job.
ljsInputConfig :: Lens' LabelingJobSummary (Maybe LabelingJobInputConfig)
ljsInputConfig = lens _ljsInputConfig (\s a -> s {_ljsInputConfig = a})

-- | The location of the output produced by the labeling job.
ljsLabelingJobOutput :: Lens' LabelingJobSummary (Maybe LabelingJobOutput)
ljsLabelingJobOutput = lens _ljsLabelingJobOutput (\s a -> s {_ljsLabelingJobOutput = a})

-- | The name of the labeling job.
ljsLabelingJobName :: Lens' LabelingJobSummary Text
ljsLabelingJobName = lens _ljsLabelingJobName (\s a -> s {_ljsLabelingJobName = a})

-- | The Amazon Resource Name (ARN) assigned to the labeling job when it was created.
ljsLabelingJobARN :: Lens' LabelingJobSummary Text
ljsLabelingJobARN = lens _ljsLabelingJobARN (\s a -> s {_ljsLabelingJobARN = a})

-- | The date and time that the job was created (timestamp).
ljsCreationTime :: Lens' LabelingJobSummary UTCTime
ljsCreationTime = lens _ljsCreationTime (\s a -> s {_ljsCreationTime = a}) . _Time

-- | The date and time that the job was last modified (timestamp).
ljsLastModifiedTime :: Lens' LabelingJobSummary UTCTime
ljsLastModifiedTime = lens _ljsLastModifiedTime (\s a -> s {_ljsLastModifiedTime = a}) . _Time

-- | The current status of the labeling job.
ljsLabelingJobStatus :: Lens' LabelingJobSummary LabelingJobStatus
ljsLabelingJobStatus = lens _ljsLabelingJobStatus (\s a -> s {_ljsLabelingJobStatus = a})

-- | Counts showing the progress of the labeling job.
ljsLabelCounters :: Lens' LabelingJobSummary LabelCounters
ljsLabelCounters = lens _ljsLabelCounters (\s a -> s {_ljsLabelCounters = a})

-- | The Amazon Resource Name (ARN) of the work team assigned to the job.
ljsWorkteamARN :: Lens' LabelingJobSummary Text
ljsWorkteamARN = lens _ljsWorkteamARN (\s a -> s {_ljsWorkteamARN = a})

-- | The Amazon Resource Name (ARN) of a Lambda function. The function is run before each data object is sent to a worker.
ljsPreHumanTaskLambdaARN :: Lens' LabelingJobSummary Text
ljsPreHumanTaskLambdaARN = lens _ljsPreHumanTaskLambdaARN (\s a -> s {_ljsPreHumanTaskLambdaARN = a})

instance FromJSON LabelingJobSummary where
  parseJSON =
    withObject
      "LabelingJobSummary"
      ( \x ->
          LabelingJobSummary'
            <$> (x .:? "FailureReason")
            <*> (x .:? "AnnotationConsolidationLambdaArn")
            <*> (x .:? "InputConfig")
            <*> (x .:? "LabelingJobOutput")
            <*> (x .: "LabelingJobName")
            <*> (x .: "LabelingJobArn")
            <*> (x .: "CreationTime")
            <*> (x .: "LastModifiedTime")
            <*> (x .: "LabelingJobStatus")
            <*> (x .: "LabelCounters")
            <*> (x .: "WorkteamArn")
            <*> (x .: "PreHumanTaskLambdaArn")
      )

instance Hashable LabelingJobSummary

instance NFData LabelingJobSummary
