{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionExecutionOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionExecutionOutput where

import Network.AWS.CodePipeline.Types.ActionExecutionResult
import Network.AWS.CodePipeline.Types.ArtifactDetail
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Output details listed for an action execution, such as the action execution result.
--
--
--
-- /See:/ 'actionExecutionOutput' smart constructor.
data ActionExecutionOutput = ActionExecutionOutput'
  { _aeoOutputVariables ::
      !(Maybe (Map Text (Text))),
    _aeoOutputArtifacts ::
      !(Maybe [ArtifactDetail]),
    _aeoExecutionResult ::
      !(Maybe ActionExecutionResult)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActionExecutionOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aeoOutputVariables' - The outputVariables field shows the key-value pairs that were output as part of that execution.
--
-- * 'aeoOutputArtifacts' - Details of output artifacts of the action that correspond to the action execution.
--
-- * 'aeoExecutionResult' - Execution result information listed in the output details for an action execution.
actionExecutionOutput ::
  ActionExecutionOutput
actionExecutionOutput =
  ActionExecutionOutput'
    { _aeoOutputVariables = Nothing,
      _aeoOutputArtifacts = Nothing,
      _aeoExecutionResult = Nothing
    }

-- | The outputVariables field shows the key-value pairs that were output as part of that execution.
aeoOutputVariables :: Lens' ActionExecutionOutput (HashMap Text (Text))
aeoOutputVariables = lens _aeoOutputVariables (\s a -> s {_aeoOutputVariables = a}) . _Default . _Map

-- | Details of output artifacts of the action that correspond to the action execution.
aeoOutputArtifacts :: Lens' ActionExecutionOutput [ArtifactDetail]
aeoOutputArtifacts = lens _aeoOutputArtifacts (\s a -> s {_aeoOutputArtifacts = a}) . _Default . _Coerce

-- | Execution result information listed in the output details for an action execution.
aeoExecutionResult :: Lens' ActionExecutionOutput (Maybe ActionExecutionResult)
aeoExecutionResult = lens _aeoExecutionResult (\s a -> s {_aeoExecutionResult = a})

instance FromJSON ActionExecutionOutput where
  parseJSON =
    withObject
      "ActionExecutionOutput"
      ( \x ->
          ActionExecutionOutput'
            <$> (x .:? "outputVariables" .!= mempty)
            <*> (x .:? "outputArtifacts" .!= mempty)
            <*> (x .:? "executionResult")
      )

instance Hashable ActionExecutionOutput

instance NFData ActionExecutionOutput
