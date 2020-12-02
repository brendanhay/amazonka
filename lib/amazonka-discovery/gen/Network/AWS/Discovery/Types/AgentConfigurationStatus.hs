{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.AgentConfigurationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.AgentConfigurationStatus where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about agents or connectors that were instructed to start collecting data. Information includes the agent/connector ID, a description of the operation, and whether the agent/connector configuration was updated.
--
--
--
-- /See:/ 'agentConfigurationStatus' smart constructor.
data AgentConfigurationStatus = AgentConfigurationStatus'
  { _acsAgentId ::
      !(Maybe Text),
    _acsOperationSucceeded :: !(Maybe Bool),
    _acsDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AgentConfigurationStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acsAgentId' - The agent/connector ID.
--
-- * 'acsOperationSucceeded' - Information about the status of the @StartDataCollection@ and @StopDataCollection@ operations. The system has recorded the data collection operation. The agent/connector receives this command the next time it polls for a new command.
--
-- * 'acsDescription' - A description of the operation performed.
agentConfigurationStatus ::
  AgentConfigurationStatus
agentConfigurationStatus =
  AgentConfigurationStatus'
    { _acsAgentId = Nothing,
      _acsOperationSucceeded = Nothing,
      _acsDescription = Nothing
    }

-- | The agent/connector ID.
acsAgentId :: Lens' AgentConfigurationStatus (Maybe Text)
acsAgentId = lens _acsAgentId (\s a -> s {_acsAgentId = a})

-- | Information about the status of the @StartDataCollection@ and @StopDataCollection@ operations. The system has recorded the data collection operation. The agent/connector receives this command the next time it polls for a new command.
acsOperationSucceeded :: Lens' AgentConfigurationStatus (Maybe Bool)
acsOperationSucceeded = lens _acsOperationSucceeded (\s a -> s {_acsOperationSucceeded = a})

-- | A description of the operation performed.
acsDescription :: Lens' AgentConfigurationStatus (Maybe Text)
acsDescription = lens _acsDescription (\s a -> s {_acsDescription = a})

instance FromJSON AgentConfigurationStatus where
  parseJSON =
    withObject
      "AgentConfigurationStatus"
      ( \x ->
          AgentConfigurationStatus'
            <$> (x .:? "agentId")
            <*> (x .:? "operationSucceeded")
            <*> (x .:? "description")
      )

instance Hashable AgentConfigurationStatus

instance NFData AgentConfigurationStatus
