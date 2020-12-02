{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.PipelineContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.PipelineContext where

import Network.AWS.CodePipeline.Types.ActionContext
import Network.AWS.CodePipeline.Types.StageContext
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents information about a pipeline to a job worker.
--
--
--
-- /See:/ 'pipelineContext' smart constructor.
data PipelineContext = PipelineContext'
  { _pcStage ::
      !(Maybe StageContext),
    _pcPipelineName :: !(Maybe Text),
    _pcAction :: !(Maybe ActionContext),
    _pcPipelineARN :: !(Maybe Text),
    _pcPipelineExecutionId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PipelineContext' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcStage' - The stage of the pipeline.
--
-- * 'pcPipelineName' - The name of the pipeline. This is a user-specified value. Pipeline names must be unique across all pipeline names under an Amazon Web Services account.
--
-- * 'pcAction' - The context of an action to a job worker in the stage of a pipeline.
--
-- * 'pcPipelineARN' - The Amazon Resource Name (ARN) of the pipeline.
--
-- * 'pcPipelineExecutionId' - The execution ID of the pipeline.
pipelineContext ::
  PipelineContext
pipelineContext =
  PipelineContext'
    { _pcStage = Nothing,
      _pcPipelineName = Nothing,
      _pcAction = Nothing,
      _pcPipelineARN = Nothing,
      _pcPipelineExecutionId = Nothing
    }

-- | The stage of the pipeline.
pcStage :: Lens' PipelineContext (Maybe StageContext)
pcStage = lens _pcStage (\s a -> s {_pcStage = a})

-- | The name of the pipeline. This is a user-specified value. Pipeline names must be unique across all pipeline names under an Amazon Web Services account.
pcPipelineName :: Lens' PipelineContext (Maybe Text)
pcPipelineName = lens _pcPipelineName (\s a -> s {_pcPipelineName = a})

-- | The context of an action to a job worker in the stage of a pipeline.
pcAction :: Lens' PipelineContext (Maybe ActionContext)
pcAction = lens _pcAction (\s a -> s {_pcAction = a})

-- | The Amazon Resource Name (ARN) of the pipeline.
pcPipelineARN :: Lens' PipelineContext (Maybe Text)
pcPipelineARN = lens _pcPipelineARN (\s a -> s {_pcPipelineARN = a})

-- | The execution ID of the pipeline.
pcPipelineExecutionId :: Lens' PipelineContext (Maybe Text)
pcPipelineExecutionId = lens _pcPipelineExecutionId (\s a -> s {_pcPipelineExecutionId = a})

instance FromJSON PipelineContext where
  parseJSON =
    withObject
      "PipelineContext"
      ( \x ->
          PipelineContext'
            <$> (x .:? "stage")
            <*> (x .:? "pipelineName")
            <*> (x .:? "action")
            <*> (x .:? "pipelineArn")
            <*> (x .:? "pipelineExecutionId")
      )

instance Hashable PipelineContext

instance NFData PipelineContext
