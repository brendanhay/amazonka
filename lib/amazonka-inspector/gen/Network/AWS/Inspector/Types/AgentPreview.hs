{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AgentPreview
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AgentPreview where

import Network.AWS.Inspector.Types.AgentHealth
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Used as a response element in the 'PreviewAgents' action.
--
--
--
-- /See:/ 'agentPreview' smart constructor.
data AgentPreview = AgentPreview'
  { _apHostname :: !(Maybe Text),
    _apAutoScalingGroup :: !(Maybe Text),
    _apOperatingSystem :: !(Maybe Text),
    _apAgentVersion :: !(Maybe Text),
    _apKernelVersion :: !(Maybe Text),
    _apAgentHealth :: !(Maybe AgentHealth),
    _apIpv4Address :: !(Maybe Text),
    _apAgentId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AgentPreview' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apHostname' - The hostname of the EC2 instance on which the Amazon Inspector Agent is installed.
--
-- * 'apAutoScalingGroup' - The Auto Scaling group for the EC2 instance where the agent is installed.
--
-- * 'apOperatingSystem' - The operating system running on the EC2 instance on which the Amazon Inspector Agent is installed.
--
-- * 'apAgentVersion' - The version of the Amazon Inspector Agent.
--
-- * 'apKernelVersion' - The kernel version of the operating system running on the EC2 instance on which the Amazon Inspector Agent is installed.
--
-- * 'apAgentHealth' - The health status of the Amazon Inspector Agent.
--
-- * 'apIpv4Address' - The IP address of the EC2 instance on which the Amazon Inspector Agent is installed.
--
-- * 'apAgentId' - The ID of the EC2 instance where the agent is installed.
agentPreview ::
  -- | 'apAgentId'
  Text ->
  AgentPreview
agentPreview pAgentId_ =
  AgentPreview'
    { _apHostname = Nothing,
      _apAutoScalingGroup = Nothing,
      _apOperatingSystem = Nothing,
      _apAgentVersion = Nothing,
      _apKernelVersion = Nothing,
      _apAgentHealth = Nothing,
      _apIpv4Address = Nothing,
      _apAgentId = pAgentId_
    }

-- | The hostname of the EC2 instance on which the Amazon Inspector Agent is installed.
apHostname :: Lens' AgentPreview (Maybe Text)
apHostname = lens _apHostname (\s a -> s {_apHostname = a})

-- | The Auto Scaling group for the EC2 instance where the agent is installed.
apAutoScalingGroup :: Lens' AgentPreview (Maybe Text)
apAutoScalingGroup = lens _apAutoScalingGroup (\s a -> s {_apAutoScalingGroup = a})

-- | The operating system running on the EC2 instance on which the Amazon Inspector Agent is installed.
apOperatingSystem :: Lens' AgentPreview (Maybe Text)
apOperatingSystem = lens _apOperatingSystem (\s a -> s {_apOperatingSystem = a})

-- | The version of the Amazon Inspector Agent.
apAgentVersion :: Lens' AgentPreview (Maybe Text)
apAgentVersion = lens _apAgentVersion (\s a -> s {_apAgentVersion = a})

-- | The kernel version of the operating system running on the EC2 instance on which the Amazon Inspector Agent is installed.
apKernelVersion :: Lens' AgentPreview (Maybe Text)
apKernelVersion = lens _apKernelVersion (\s a -> s {_apKernelVersion = a})

-- | The health status of the Amazon Inspector Agent.
apAgentHealth :: Lens' AgentPreview (Maybe AgentHealth)
apAgentHealth = lens _apAgentHealth (\s a -> s {_apAgentHealth = a})

-- | The IP address of the EC2 instance on which the Amazon Inspector Agent is installed.
apIpv4Address :: Lens' AgentPreview (Maybe Text)
apIpv4Address = lens _apIpv4Address (\s a -> s {_apIpv4Address = a})

-- | The ID of the EC2 instance where the agent is installed.
apAgentId :: Lens' AgentPreview Text
apAgentId = lens _apAgentId (\s a -> s {_apAgentId = a})

instance FromJSON AgentPreview where
  parseJSON =
    withObject
      "AgentPreview"
      ( \x ->
          AgentPreview'
            <$> (x .:? "hostname")
            <*> (x .:? "autoScalingGroup")
            <*> (x .:? "operatingSystem")
            <*> (x .:? "agentVersion")
            <*> (x .:? "kernelVersion")
            <*> (x .:? "agentHealth")
            <*> (x .:? "ipv4Address")
            <*> (x .: "agentId")
      )

instance Hashable AgentPreview

instance NFData AgentPreview
