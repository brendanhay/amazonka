{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionExecutionResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionExecutionResult where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Execution result information, such as the external execution ID.
--
--
--
-- /See:/ 'actionExecutionResult' smart constructor.
data ActionExecutionResult = ActionExecutionResult'
  { _aerExternalExecutionURL ::
      !(Maybe Text),
    _aerExternalExecutionId :: !(Maybe Text),
    _aerExternalExecutionSummary :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActionExecutionResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aerExternalExecutionURL' - The deepest external link to the external resource (for example, a repository URL or deployment endpoint) that is used when running the action.
--
-- * 'aerExternalExecutionId' - The action provider's external ID for the action execution.
--
-- * 'aerExternalExecutionSummary' - The action provider's summary for the action execution.
actionExecutionResult ::
  ActionExecutionResult
actionExecutionResult =
  ActionExecutionResult'
    { _aerExternalExecutionURL = Nothing,
      _aerExternalExecutionId = Nothing,
      _aerExternalExecutionSummary = Nothing
    }

-- | The deepest external link to the external resource (for example, a repository URL or deployment endpoint) that is used when running the action.
aerExternalExecutionURL :: Lens' ActionExecutionResult (Maybe Text)
aerExternalExecutionURL = lens _aerExternalExecutionURL (\s a -> s {_aerExternalExecutionURL = a})

-- | The action provider's external ID for the action execution.
aerExternalExecutionId :: Lens' ActionExecutionResult (Maybe Text)
aerExternalExecutionId = lens _aerExternalExecutionId (\s a -> s {_aerExternalExecutionId = a})

-- | The action provider's summary for the action execution.
aerExternalExecutionSummary :: Lens' ActionExecutionResult (Maybe Text)
aerExternalExecutionSummary = lens _aerExternalExecutionSummary (\s a -> s {_aerExternalExecutionSummary = a})

instance FromJSON ActionExecutionResult where
  parseJSON =
    withObject
      "ActionExecutionResult"
      ( \x ->
          ActionExecutionResult'
            <$> (x .:? "externalExecutionUrl")
            <*> (x .:? "externalExecutionId")
            <*> (x .:? "externalExecutionSummary")
      )

instance Hashable ActionExecutionResult

instance NFData ActionExecutionResult
