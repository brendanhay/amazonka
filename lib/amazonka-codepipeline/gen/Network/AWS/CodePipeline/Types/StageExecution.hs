{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.StageExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.StageExecution where

import Network.AWS.CodePipeline.Types.StageExecutionStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents information about the run of a stage.
--
--
--
-- /See:/ 'stageExecution' smart constructor.
data StageExecution = StageExecution'
  { _sePipelineExecutionId ::
      !Text,
    _seStatus :: !StageExecutionStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StageExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sePipelineExecutionId' - The ID of the pipeline execution associated with the stage.
--
-- * 'seStatus' - The status of the stage, or for a completed stage, the last status of the stage.
stageExecution ::
  -- | 'sePipelineExecutionId'
  Text ->
  -- | 'seStatus'
  StageExecutionStatus ->
  StageExecution
stageExecution pPipelineExecutionId_ pStatus_ =
  StageExecution'
    { _sePipelineExecutionId = pPipelineExecutionId_,
      _seStatus = pStatus_
    }

-- | The ID of the pipeline execution associated with the stage.
sePipelineExecutionId :: Lens' StageExecution Text
sePipelineExecutionId = lens _sePipelineExecutionId (\s a -> s {_sePipelineExecutionId = a})

-- | The status of the stage, or for a completed stage, the last status of the stage.
seStatus :: Lens' StageExecution StageExecutionStatus
seStatus = lens _seStatus (\s a -> s {_seStatus = a})

instance FromJSON StageExecution where
  parseJSON =
    withObject
      "StageExecution"
      ( \x ->
          StageExecution'
            <$> (x .: "pipelineExecutionId") <*> (x .: "status")
      )

instance Hashable StageExecution

instance NFData StageExecution
