{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentRunAgent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssessmentRunAgent where

import Network.AWS.Inspector.Types.AgentHealth
import Network.AWS.Inspector.Types.AgentHealthCode
import Network.AWS.Inspector.Types.TelemetryMetadata
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about an Amazon Inspector agent. This data type is used as a response element in the 'ListAssessmentRunAgents' action.
--
--
--
-- /See:/ 'assessmentRunAgent' smart constructor.
data AssessmentRunAgent = AssessmentRunAgent'
  { _araAutoScalingGroup ::
      !(Maybe Text),
    _araAgentHealthDetails :: !(Maybe Text),
    _araAgentId :: !Text,
    _araAssessmentRunARN :: !Text,
    _araAgentHealth :: !AgentHealth,
    _araAgentHealthCode :: !AgentHealthCode,
    _araTelemetryMetadata :: ![TelemetryMetadata]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssessmentRunAgent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'araAutoScalingGroup' - The Auto Scaling group of the EC2 instance that is specified by the agent ID.
--
-- * 'araAgentHealthDetails' - The description for the agent health code.
--
-- * 'araAgentId' - The AWS account of the EC2 instance where the agent is installed.
--
-- * 'araAssessmentRunARN' - The ARN of the assessment run that is associated with the agent.
--
-- * 'araAgentHealth' - The current health state of the agent.
--
-- * 'araAgentHealthCode' - The detailed health state of the agent.
--
-- * 'araTelemetryMetadata' - The Amazon Inspector application data metrics that are collected by the agent.
assessmentRunAgent ::
  -- | 'araAgentId'
  Text ->
  -- | 'araAssessmentRunARN'
  Text ->
  -- | 'araAgentHealth'
  AgentHealth ->
  -- | 'araAgentHealthCode'
  AgentHealthCode ->
  AssessmentRunAgent
assessmentRunAgent
  pAgentId_
  pAssessmentRunARN_
  pAgentHealth_
  pAgentHealthCode_ =
    AssessmentRunAgent'
      { _araAutoScalingGroup = Nothing,
        _araAgentHealthDetails = Nothing,
        _araAgentId = pAgentId_,
        _araAssessmentRunARN = pAssessmentRunARN_,
        _araAgentHealth = pAgentHealth_,
        _araAgentHealthCode = pAgentHealthCode_,
        _araTelemetryMetadata = mempty
      }

-- | The Auto Scaling group of the EC2 instance that is specified by the agent ID.
araAutoScalingGroup :: Lens' AssessmentRunAgent (Maybe Text)
araAutoScalingGroup = lens _araAutoScalingGroup (\s a -> s {_araAutoScalingGroup = a})

-- | The description for the agent health code.
araAgentHealthDetails :: Lens' AssessmentRunAgent (Maybe Text)
araAgentHealthDetails = lens _araAgentHealthDetails (\s a -> s {_araAgentHealthDetails = a})

-- | The AWS account of the EC2 instance where the agent is installed.
araAgentId :: Lens' AssessmentRunAgent Text
araAgentId = lens _araAgentId (\s a -> s {_araAgentId = a})

-- | The ARN of the assessment run that is associated with the agent.
araAssessmentRunARN :: Lens' AssessmentRunAgent Text
araAssessmentRunARN = lens _araAssessmentRunARN (\s a -> s {_araAssessmentRunARN = a})

-- | The current health state of the agent.
araAgentHealth :: Lens' AssessmentRunAgent AgentHealth
araAgentHealth = lens _araAgentHealth (\s a -> s {_araAgentHealth = a})

-- | The detailed health state of the agent.
araAgentHealthCode :: Lens' AssessmentRunAgent AgentHealthCode
araAgentHealthCode = lens _araAgentHealthCode (\s a -> s {_araAgentHealthCode = a})

-- | The Amazon Inspector application data metrics that are collected by the agent.
araTelemetryMetadata :: Lens' AssessmentRunAgent [TelemetryMetadata]
araTelemetryMetadata = lens _araTelemetryMetadata (\s a -> s {_araTelemetryMetadata = a}) . _Coerce

instance FromJSON AssessmentRunAgent where
  parseJSON =
    withObject
      "AssessmentRunAgent"
      ( \x ->
          AssessmentRunAgent'
            <$> (x .:? "autoScalingGroup")
            <*> (x .:? "agentHealthDetails")
            <*> (x .: "agentId")
            <*> (x .: "assessmentRunArn")
            <*> (x .: "agentHealth")
            <*> (x .: "agentHealthCode")
            <*> (x .:? "telemetryMetadata" .!= mempty)
      )

instance Hashable AssessmentRunAgent

instance NFData AssessmentRunAgent
