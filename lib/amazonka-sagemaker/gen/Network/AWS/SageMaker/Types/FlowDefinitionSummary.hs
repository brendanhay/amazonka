{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.FlowDefinitionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.FlowDefinitionSummary
  ( FlowDefinitionSummary (..),

    -- * Smart constructor
    mkFlowDefinitionSummary,

    -- * Lenses
    fdsFlowDefinitionName,
    fdsFlowDefinitionArn,
    fdsFlowDefinitionStatus,
    fdsCreationTime,
    fdsFailureReason,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.FailureReason as Types
import qualified Network.AWS.SageMaker.Types.FlowDefinitionArn as Types
import qualified Network.AWS.SageMaker.Types.FlowDefinitionName as Types
import qualified Network.AWS.SageMaker.Types.FlowDefinitionStatus as Types

-- | Contains summary information about the flow definition.
--
-- /See:/ 'mkFlowDefinitionSummary' smart constructor.
data FlowDefinitionSummary = FlowDefinitionSummary'
  { -- | The name of the flow definition.
    flowDefinitionName :: Types.FlowDefinitionName,
    -- | The Amazon Resource Name (ARN) of the flow definition.
    flowDefinitionArn :: Types.FlowDefinitionArn,
    -- | The status of the flow definition. Valid values:
    flowDefinitionStatus :: Types.FlowDefinitionStatus,
    -- | The timestamp when SageMaker created the flow definition.
    creationTime :: Core.NominalDiffTime,
    -- | The reason why the flow definition creation failed. A failure reason is returned only when the flow definition status is @Failed@ .
    failureReason :: Core.Maybe Types.FailureReason
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'FlowDefinitionSummary' value with any optional fields omitted.
mkFlowDefinitionSummary ::
  -- | 'flowDefinitionName'
  Types.FlowDefinitionName ->
  -- | 'flowDefinitionArn'
  Types.FlowDefinitionArn ->
  -- | 'flowDefinitionStatus'
  Types.FlowDefinitionStatus ->
  -- | 'creationTime'
  Core.NominalDiffTime ->
  FlowDefinitionSummary
mkFlowDefinitionSummary
  flowDefinitionName
  flowDefinitionArn
  flowDefinitionStatus
  creationTime =
    FlowDefinitionSummary'
      { flowDefinitionName,
        flowDefinitionArn,
        flowDefinitionStatus,
        creationTime,
        failureReason = Core.Nothing
      }

-- | The name of the flow definition.
--
-- /Note:/ Consider using 'flowDefinitionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdsFlowDefinitionName :: Lens.Lens' FlowDefinitionSummary Types.FlowDefinitionName
fdsFlowDefinitionName = Lens.field @"flowDefinitionName"
{-# DEPRECATED fdsFlowDefinitionName "Use generic-lens or generic-optics with 'flowDefinitionName' instead." #-}

-- | The Amazon Resource Name (ARN) of the flow definition.
--
-- /Note:/ Consider using 'flowDefinitionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdsFlowDefinitionArn :: Lens.Lens' FlowDefinitionSummary Types.FlowDefinitionArn
fdsFlowDefinitionArn = Lens.field @"flowDefinitionArn"
{-# DEPRECATED fdsFlowDefinitionArn "Use generic-lens or generic-optics with 'flowDefinitionArn' instead." #-}

-- | The status of the flow definition. Valid values:
--
-- /Note:/ Consider using 'flowDefinitionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdsFlowDefinitionStatus :: Lens.Lens' FlowDefinitionSummary Types.FlowDefinitionStatus
fdsFlowDefinitionStatus = Lens.field @"flowDefinitionStatus"
{-# DEPRECATED fdsFlowDefinitionStatus "Use generic-lens or generic-optics with 'flowDefinitionStatus' instead." #-}

-- | The timestamp when SageMaker created the flow definition.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdsCreationTime :: Lens.Lens' FlowDefinitionSummary Core.NominalDiffTime
fdsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED fdsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The reason why the flow definition creation failed. A failure reason is returned only when the flow definition status is @Failed@ .
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdsFailureReason :: Lens.Lens' FlowDefinitionSummary (Core.Maybe Types.FailureReason)
fdsFailureReason = Lens.field @"failureReason"
{-# DEPRECATED fdsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

instance Core.FromJSON FlowDefinitionSummary where
  parseJSON =
    Core.withObject "FlowDefinitionSummary" Core.$
      \x ->
        FlowDefinitionSummary'
          Core.<$> (x Core..: "FlowDefinitionName")
          Core.<*> (x Core..: "FlowDefinitionArn")
          Core.<*> (x Core..: "FlowDefinitionStatus")
          Core.<*> (x Core..: "CreationTime")
          Core.<*> (x Core..:? "FailureReason")
