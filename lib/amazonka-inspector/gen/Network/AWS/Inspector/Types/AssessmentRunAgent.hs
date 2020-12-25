{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentRunAgent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssessmentRunAgent
  ( AssessmentRunAgent (..),

    -- * Smart constructor
    mkAssessmentRunAgent,

    -- * Lenses
    araAgentId,
    araAssessmentRunArn,
    araAgentHealth,
    araAgentHealthCode,
    araTelemetryMetadata,
    araAgentHealthDetails,
    araAutoScalingGroup,
  )
where

import qualified Network.AWS.Inspector.Types.AgentHealth as Types
import qualified Network.AWS.Inspector.Types.AgentHealthCode as Types
import qualified Network.AWS.Inspector.Types.AgentHealthDetails as Types
import qualified Network.AWS.Inspector.Types.AgentId as Types
import qualified Network.AWS.Inspector.Types.Arn as Types
import qualified Network.AWS.Inspector.Types.AutoScalingGroup as Types
import qualified Network.AWS.Inspector.Types.TelemetryMetadata as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about an Amazon Inspector agent. This data type is used as a response element in the 'ListAssessmentRunAgents' action.
--
-- /See:/ 'mkAssessmentRunAgent' smart constructor.
data AssessmentRunAgent = AssessmentRunAgent'
  { -- | The AWS account of the EC2 instance where the agent is installed.
    agentId :: Types.AgentId,
    -- | The ARN of the assessment run that is associated with the agent.
    assessmentRunArn :: Types.Arn,
    -- | The current health state of the agent.
    agentHealth :: Types.AgentHealth,
    -- | The detailed health state of the agent.
    agentHealthCode :: Types.AgentHealthCode,
    -- | The Amazon Inspector application data metrics that are collected by the agent.
    telemetryMetadata :: [Types.TelemetryMetadata],
    -- | The description for the agent health code.
    agentHealthDetails :: Core.Maybe Types.AgentHealthDetails,
    -- | The Auto Scaling group of the EC2 instance that is specified by the agent ID.
    autoScalingGroup :: Core.Maybe Types.AutoScalingGroup
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssessmentRunAgent' value with any optional fields omitted.
mkAssessmentRunAgent ::
  -- | 'agentId'
  Types.AgentId ->
  -- | 'assessmentRunArn'
  Types.Arn ->
  -- | 'agentHealth'
  Types.AgentHealth ->
  -- | 'agentHealthCode'
  Types.AgentHealthCode ->
  AssessmentRunAgent
mkAssessmentRunAgent
  agentId
  assessmentRunArn
  agentHealth
  agentHealthCode =
    AssessmentRunAgent'
      { agentId,
        assessmentRunArn,
        agentHealth,
        agentHealthCode,
        telemetryMetadata = Core.mempty,
        agentHealthDetails = Core.Nothing,
        autoScalingGroup = Core.Nothing
      }

-- | The AWS account of the EC2 instance where the agent is installed.
--
-- /Note:/ Consider using 'agentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
araAgentId :: Lens.Lens' AssessmentRunAgent Types.AgentId
araAgentId = Lens.field @"agentId"
{-# DEPRECATED araAgentId "Use generic-lens or generic-optics with 'agentId' instead." #-}

-- | The ARN of the assessment run that is associated with the agent.
--
-- /Note:/ Consider using 'assessmentRunArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
araAssessmentRunArn :: Lens.Lens' AssessmentRunAgent Types.Arn
araAssessmentRunArn = Lens.field @"assessmentRunArn"
{-# DEPRECATED araAssessmentRunArn "Use generic-lens or generic-optics with 'assessmentRunArn' instead." #-}

-- | The current health state of the agent.
--
-- /Note:/ Consider using 'agentHealth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
araAgentHealth :: Lens.Lens' AssessmentRunAgent Types.AgentHealth
araAgentHealth = Lens.field @"agentHealth"
{-# DEPRECATED araAgentHealth "Use generic-lens or generic-optics with 'agentHealth' instead." #-}

-- | The detailed health state of the agent.
--
-- /Note:/ Consider using 'agentHealthCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
araAgentHealthCode :: Lens.Lens' AssessmentRunAgent Types.AgentHealthCode
araAgentHealthCode = Lens.field @"agentHealthCode"
{-# DEPRECATED araAgentHealthCode "Use generic-lens or generic-optics with 'agentHealthCode' instead." #-}

-- | The Amazon Inspector application data metrics that are collected by the agent.
--
-- /Note:/ Consider using 'telemetryMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
araTelemetryMetadata :: Lens.Lens' AssessmentRunAgent [Types.TelemetryMetadata]
araTelemetryMetadata = Lens.field @"telemetryMetadata"
{-# DEPRECATED araTelemetryMetadata "Use generic-lens or generic-optics with 'telemetryMetadata' instead." #-}

-- | The description for the agent health code.
--
-- /Note:/ Consider using 'agentHealthDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
araAgentHealthDetails :: Lens.Lens' AssessmentRunAgent (Core.Maybe Types.AgentHealthDetails)
araAgentHealthDetails = Lens.field @"agentHealthDetails"
{-# DEPRECATED araAgentHealthDetails "Use generic-lens or generic-optics with 'agentHealthDetails' instead." #-}

-- | The Auto Scaling group of the EC2 instance that is specified by the agent ID.
--
-- /Note:/ Consider using 'autoScalingGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
araAutoScalingGroup :: Lens.Lens' AssessmentRunAgent (Core.Maybe Types.AutoScalingGroup)
araAutoScalingGroup = Lens.field @"autoScalingGroup"
{-# DEPRECATED araAutoScalingGroup "Use generic-lens or generic-optics with 'autoScalingGroup' instead." #-}

instance Core.FromJSON AssessmentRunAgent where
  parseJSON =
    Core.withObject "AssessmentRunAgent" Core.$
      \x ->
        AssessmentRunAgent'
          Core.<$> (x Core..: "agentId")
          Core.<*> (x Core..: "assessmentRunArn")
          Core.<*> (x Core..: "agentHealth")
          Core.<*> (x Core..: "agentHealthCode")
          Core.<*> (x Core..:? "telemetryMetadata" Core..!= Core.mempty)
          Core.<*> (x Core..:? "agentHealthDetails")
          Core.<*> (x Core..:? "autoScalingGroup")
