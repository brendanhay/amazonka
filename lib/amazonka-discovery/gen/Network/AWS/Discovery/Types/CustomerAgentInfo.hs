{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.CustomerAgentInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.CustomerAgentInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Inventory data for installed discovery agents.
--
--
--
-- /See:/ 'customerAgentInfo' smart constructor.
data CustomerAgentInfo = CustomerAgentInfo'
  { _caiActiveAgents ::
      !Int,
    _caiHealthyAgents :: !Int,
    _caiBlackListedAgents :: !Int,
    _caiShutdownAgents :: !Int,
    _caiUnhealthyAgents :: !Int,
    _caiTotalAgents :: !Int,
    _caiUnknownAgents :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CustomerAgentInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caiActiveAgents' - Number of active discovery agents.
--
-- * 'caiHealthyAgents' - Number of healthy discovery agents
--
-- * 'caiBlackListedAgents' - Number of blacklisted discovery agents.
--
-- * 'caiShutdownAgents' - Number of discovery agents with status SHUTDOWN.
--
-- * 'caiUnhealthyAgents' - Number of unhealthy discovery agents.
--
-- * 'caiTotalAgents' - Total number of discovery agents.
--
-- * 'caiUnknownAgents' - Number of unknown discovery agents.
customerAgentInfo ::
  -- | 'caiActiveAgents'
  Int ->
  -- | 'caiHealthyAgents'
  Int ->
  -- | 'caiBlackListedAgents'
  Int ->
  -- | 'caiShutdownAgents'
  Int ->
  -- | 'caiUnhealthyAgents'
  Int ->
  -- | 'caiTotalAgents'
  Int ->
  -- | 'caiUnknownAgents'
  Int ->
  CustomerAgentInfo
customerAgentInfo
  pActiveAgents_
  pHealthyAgents_
  pBlackListedAgents_
  pShutdownAgents_
  pUnhealthyAgents_
  pTotalAgents_
  pUnknownAgents_ =
    CustomerAgentInfo'
      { _caiActiveAgents = pActiveAgents_,
        _caiHealthyAgents = pHealthyAgents_,
        _caiBlackListedAgents = pBlackListedAgents_,
        _caiShutdownAgents = pShutdownAgents_,
        _caiUnhealthyAgents = pUnhealthyAgents_,
        _caiTotalAgents = pTotalAgents_,
        _caiUnknownAgents = pUnknownAgents_
      }

-- | Number of active discovery agents.
caiActiveAgents :: Lens' CustomerAgentInfo Int
caiActiveAgents = lens _caiActiveAgents (\s a -> s {_caiActiveAgents = a})

-- | Number of healthy discovery agents
caiHealthyAgents :: Lens' CustomerAgentInfo Int
caiHealthyAgents = lens _caiHealthyAgents (\s a -> s {_caiHealthyAgents = a})

-- | Number of blacklisted discovery agents.
caiBlackListedAgents :: Lens' CustomerAgentInfo Int
caiBlackListedAgents = lens _caiBlackListedAgents (\s a -> s {_caiBlackListedAgents = a})

-- | Number of discovery agents with status SHUTDOWN.
caiShutdownAgents :: Lens' CustomerAgentInfo Int
caiShutdownAgents = lens _caiShutdownAgents (\s a -> s {_caiShutdownAgents = a})

-- | Number of unhealthy discovery agents.
caiUnhealthyAgents :: Lens' CustomerAgentInfo Int
caiUnhealthyAgents = lens _caiUnhealthyAgents (\s a -> s {_caiUnhealthyAgents = a})

-- | Total number of discovery agents.
caiTotalAgents :: Lens' CustomerAgentInfo Int
caiTotalAgents = lens _caiTotalAgents (\s a -> s {_caiTotalAgents = a})

-- | Number of unknown discovery agents.
caiUnknownAgents :: Lens' CustomerAgentInfo Int
caiUnknownAgents = lens _caiUnknownAgents (\s a -> s {_caiUnknownAgents = a})

instance FromJSON CustomerAgentInfo where
  parseJSON =
    withObject
      "CustomerAgentInfo"
      ( \x ->
          CustomerAgentInfo'
            <$> (x .: "activeAgents")
            <*> (x .: "healthyAgents")
            <*> (x .: "blackListedAgents")
            <*> (x .: "shutdownAgents")
            <*> (x .: "unhealthyAgents")
            <*> (x .: "totalAgents")
            <*> (x .: "unknownAgents")
      )

instance Hashable CustomerAgentInfo

instance NFData CustomerAgentInfo
