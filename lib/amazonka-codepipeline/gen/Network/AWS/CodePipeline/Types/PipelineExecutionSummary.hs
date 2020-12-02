{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.PipelineExecutionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.PipelineExecutionSummary where

import Network.AWS.CodePipeline.Types.ExecutionTrigger
import Network.AWS.CodePipeline.Types.PipelineExecutionStatus
import Network.AWS.CodePipeline.Types.SourceRevision
import Network.AWS.CodePipeline.Types.StopExecutionTrigger
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Summary information about a pipeline execution.
--
--
--
-- /See:/ 'pipelineExecutionSummary' smart constructor.
data PipelineExecutionSummary = PipelineExecutionSummary'
  { _pesStatus ::
      !(Maybe PipelineExecutionStatus),
    _pesStartTime :: !(Maybe POSIX),
    _pesStopTrigger ::
      !(Maybe StopExecutionTrigger),
    _pesPipelineExecutionId :: !(Maybe Text),
    _pesSourceRevisions ::
      !(Maybe [SourceRevision]),
    _pesTrigger :: !(Maybe ExecutionTrigger),
    _pesLastUpdateTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PipelineExecutionSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pesStatus' - The status of the pipeline execution.     * InProgress: The pipeline execution is currently running.     * Stopped: The pipeline execution was manually stopped. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/concepts.html#concepts-executions-stopped Stopped Executions> .     * Stopping: The pipeline execution received a request to be manually stopped. Depending on the selected stop mode, the execution is either completing or abandoning in-progress actions. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/concepts.html#concepts-executions-stopped Stopped Executions> .     * Succeeded: The pipeline execution was completed successfully.      * Superseded: While this pipeline execution was waiting for the next stage to be completed, a newer pipeline execution advanced and continued through the pipeline instead. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/concepts.html#concepts-superseded Superseded Executions> .     * Failed: The pipeline execution was not completed successfully.
--
-- * 'pesStartTime' - The date and time when the pipeline execution began, in timestamp format.
--
-- * 'pesStopTrigger' - The interaction that stopped a pipeline execution.
--
-- * 'pesPipelineExecutionId' - The ID of the pipeline execution.
--
-- * 'pesSourceRevisions' - A list of the source artifact revisions that initiated a pipeline execution.
--
-- * 'pesTrigger' - The interaction or event that started a pipeline execution, such as automated change detection or a @StartPipelineExecution@ API call.
--
-- * 'pesLastUpdateTime' - The date and time of the last change to the pipeline execution, in timestamp format.
pipelineExecutionSummary ::
  PipelineExecutionSummary
pipelineExecutionSummary =
  PipelineExecutionSummary'
    { _pesStatus = Nothing,
      _pesStartTime = Nothing,
      _pesStopTrigger = Nothing,
      _pesPipelineExecutionId = Nothing,
      _pesSourceRevisions = Nothing,
      _pesTrigger = Nothing,
      _pesLastUpdateTime = Nothing
    }

-- | The status of the pipeline execution.     * InProgress: The pipeline execution is currently running.     * Stopped: The pipeline execution was manually stopped. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/concepts.html#concepts-executions-stopped Stopped Executions> .     * Stopping: The pipeline execution received a request to be manually stopped. Depending on the selected stop mode, the execution is either completing or abandoning in-progress actions. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/concepts.html#concepts-executions-stopped Stopped Executions> .     * Succeeded: The pipeline execution was completed successfully.      * Superseded: While this pipeline execution was waiting for the next stage to be completed, a newer pipeline execution advanced and continued through the pipeline instead. For more information, see <https://docs.aws.amazon.com/codepipeline/latest/userguide/concepts.html#concepts-superseded Superseded Executions> .     * Failed: The pipeline execution was not completed successfully.
pesStatus :: Lens' PipelineExecutionSummary (Maybe PipelineExecutionStatus)
pesStatus = lens _pesStatus (\s a -> s {_pesStatus = a})

-- | The date and time when the pipeline execution began, in timestamp format.
pesStartTime :: Lens' PipelineExecutionSummary (Maybe UTCTime)
pesStartTime = lens _pesStartTime (\s a -> s {_pesStartTime = a}) . mapping _Time

-- | The interaction that stopped a pipeline execution.
pesStopTrigger :: Lens' PipelineExecutionSummary (Maybe StopExecutionTrigger)
pesStopTrigger = lens _pesStopTrigger (\s a -> s {_pesStopTrigger = a})

-- | The ID of the pipeline execution.
pesPipelineExecutionId :: Lens' PipelineExecutionSummary (Maybe Text)
pesPipelineExecutionId = lens _pesPipelineExecutionId (\s a -> s {_pesPipelineExecutionId = a})

-- | A list of the source artifact revisions that initiated a pipeline execution.
pesSourceRevisions :: Lens' PipelineExecutionSummary [SourceRevision]
pesSourceRevisions = lens _pesSourceRevisions (\s a -> s {_pesSourceRevisions = a}) . _Default . _Coerce

-- | The interaction or event that started a pipeline execution, such as automated change detection or a @StartPipelineExecution@ API call.
pesTrigger :: Lens' PipelineExecutionSummary (Maybe ExecutionTrigger)
pesTrigger = lens _pesTrigger (\s a -> s {_pesTrigger = a})

-- | The date and time of the last change to the pipeline execution, in timestamp format.
pesLastUpdateTime :: Lens' PipelineExecutionSummary (Maybe UTCTime)
pesLastUpdateTime = lens _pesLastUpdateTime (\s a -> s {_pesLastUpdateTime = a}) . mapping _Time

instance FromJSON PipelineExecutionSummary where
  parseJSON =
    withObject
      "PipelineExecutionSummary"
      ( \x ->
          PipelineExecutionSummary'
            <$> (x .:? "status")
            <*> (x .:? "startTime")
            <*> (x .:? "stopTrigger")
            <*> (x .:? "pipelineExecutionId")
            <*> (x .:? "sourceRevisions" .!= mempty)
            <*> (x .:? "trigger")
            <*> (x .:? "lastUpdateTime")
      )

instance Hashable PipelineExecutionSummary

instance NFData PipelineExecutionSummary
