{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AgentFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AgentFilter where

import Network.AWS.Inspector.Types.AgentHealth
import Network.AWS.Inspector.Types.AgentHealthCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about an Amazon Inspector agent. This data type is used as a request parameter in the 'ListAssessmentRunAgents' action.
--
--
--
-- /See:/ 'agentFilter' smart constructor.
data AgentFilter = AgentFilter'
  { _afAgentHealths :: ![AgentHealth],
    _afAgentHealthCodes :: ![AgentHealthCode]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AgentFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'afAgentHealths' - The current health state of the agent. Values can be set to __HEALTHY__ or __UNHEALTHY__ .
--
-- * 'afAgentHealthCodes' - The detailed health state of the agent. Values can be set to __IDLE__ , __RUNNING__ , __SHUTDOWN__ , __UNHEALTHY__ , __THROTTLED__ , and __UNKNOWN__ .
agentFilter ::
  AgentFilter
agentFilter =
  AgentFilter'
    { _afAgentHealths = mempty,
      _afAgentHealthCodes = mempty
    }

-- | The current health state of the agent. Values can be set to __HEALTHY__ or __UNHEALTHY__ .
afAgentHealths :: Lens' AgentFilter [AgentHealth]
afAgentHealths = lens _afAgentHealths (\s a -> s {_afAgentHealths = a}) . _Coerce

-- | The detailed health state of the agent. Values can be set to __IDLE__ , __RUNNING__ , __SHUTDOWN__ , __UNHEALTHY__ , __THROTTLED__ , and __UNKNOWN__ .
afAgentHealthCodes :: Lens' AgentFilter [AgentHealthCode]
afAgentHealthCodes = lens _afAgentHealthCodes (\s a -> s {_afAgentHealthCodes = a}) . _Coerce

instance Hashable AgentFilter

instance NFData AgentFilter

instance ToJSON AgentFilter where
  toJSON AgentFilter' {..} =
    object
      ( catMaybes
          [ Just ("agentHealths" .= _afAgentHealths),
            Just ("agentHealthCodes" .= _afAgentHealthCodes)
          ]
      )
