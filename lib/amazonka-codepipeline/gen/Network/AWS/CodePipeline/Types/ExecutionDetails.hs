{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ExecutionDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ExecutionDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The details of the actions taken and results produced on an artifact as it passes through stages in the pipeline.
--
--
--
-- /See:/ 'executionDetails' smart constructor.
data ExecutionDetails = ExecutionDetails'
  { _edSummary ::
      !(Maybe Text),
    _edExternalExecutionId :: !(Maybe Text),
    _edPercentComplete :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExecutionDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edSummary' - The summary of the current status of the actions.
--
-- * 'edExternalExecutionId' - The system-generated unique ID of this action used to identify this job worker in any external systems, such as AWS CodeDeploy.
--
-- * 'edPercentComplete' - The percentage of work completed on the action, represented on a scale of 0 to 100 percent.
executionDetails ::
  ExecutionDetails
executionDetails =
  ExecutionDetails'
    { _edSummary = Nothing,
      _edExternalExecutionId = Nothing,
      _edPercentComplete = Nothing
    }

-- | The summary of the current status of the actions.
edSummary :: Lens' ExecutionDetails (Maybe Text)
edSummary = lens _edSummary (\s a -> s {_edSummary = a})

-- | The system-generated unique ID of this action used to identify this job worker in any external systems, such as AWS CodeDeploy.
edExternalExecutionId :: Lens' ExecutionDetails (Maybe Text)
edExternalExecutionId = lens _edExternalExecutionId (\s a -> s {_edExternalExecutionId = a})

-- | The percentage of work completed on the action, represented on a scale of 0 to 100 percent.
edPercentComplete :: Lens' ExecutionDetails (Maybe Natural)
edPercentComplete = lens _edPercentComplete (\s a -> s {_edPercentComplete = a}) . mapping _Nat

instance Hashable ExecutionDetails

instance NFData ExecutionDetails

instance ToJSON ExecutionDetails where
  toJSON ExecutionDetails' {..} =
    object
      ( catMaybes
          [ ("summary" .=) <$> _edSummary,
            ("externalExecutionId" .=) <$> _edExternalExecutionId,
            ("percentComplete" .=) <$> _edPercentComplete
          ]
      )
