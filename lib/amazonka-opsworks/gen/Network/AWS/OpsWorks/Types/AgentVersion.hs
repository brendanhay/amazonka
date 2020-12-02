{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.AgentVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.AgentVersion where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types.StackConfigurationManager
import Network.AWS.Prelude

-- | Describes an agent version.
--
--
--
-- /See:/ 'agentVersion' smart constructor.
data AgentVersion = AgentVersion'
  { _avVersion :: !(Maybe Text),
    _avConfigurationManager :: !(Maybe StackConfigurationManager)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AgentVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avVersion' - The agent version.
--
-- * 'avConfigurationManager' - The configuration manager.
agentVersion ::
  AgentVersion
agentVersion =
  AgentVersion'
    { _avVersion = Nothing,
      _avConfigurationManager = Nothing
    }

-- | The agent version.
avVersion :: Lens' AgentVersion (Maybe Text)
avVersion = lens _avVersion (\s a -> s {_avVersion = a})

-- | The configuration manager.
avConfigurationManager :: Lens' AgentVersion (Maybe StackConfigurationManager)
avConfigurationManager = lens _avConfigurationManager (\s a -> s {_avConfigurationManager = a})

instance FromJSON AgentVersion where
  parseJSON =
    withObject
      "AgentVersion"
      ( \x ->
          AgentVersion'
            <$> (x .:? "Version") <*> (x .:? "ConfigurationManager")
      )

instance Hashable AgentVersion

instance NFData AgentVersion
