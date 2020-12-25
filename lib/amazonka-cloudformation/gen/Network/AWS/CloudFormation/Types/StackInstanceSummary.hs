{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackInstanceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackInstanceSummary
  ( StackInstanceSummary (..),

    -- * Smart constructor
    mkStackInstanceSummary,

    -- * Lenses
    sisAccount,
    sisDriftStatus,
    sisLastDriftCheckTimestamp,
    sisOrganizationalUnitId,
    sisRegion,
    sisStackId,
    sisStackInstanceStatus,
    sisStackSetId,
    sisStatus,
    sisStatusReason,
  )
where

import qualified Network.AWS.CloudFormation.Types.Account as Types
import qualified Network.AWS.CloudFormation.Types.OrganizationalUnitId as Types
import qualified Network.AWS.CloudFormation.Types.Region as Types
import qualified Network.AWS.CloudFormation.Types.StackDriftStatus as Types
import qualified Network.AWS.CloudFormation.Types.StackId as Types
import qualified Network.AWS.CloudFormation.Types.StackInstanceComprehensiveStatus as Types
import qualified Network.AWS.CloudFormation.Types.StackInstanceStatus as Types
import qualified Network.AWS.CloudFormation.Types.StackSetId as Types
import qualified Network.AWS.CloudFormation.Types.StatusReason as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The structure that contains summary information about a stack instance.
--
-- /See:/ 'mkStackInstanceSummary' smart constructor.
data StackInstanceSummary = StackInstanceSummary'
  { -- | [@Self-managed@ permissions] The name of the AWS account that the stack instance is associated with.
    account :: Core.Maybe Types.Account,
    -- | Status of the stack instance's actual configuration compared to the expected template and parameter configuration of the stack set to which it belongs.
    --
    --
    --     * @DRIFTED@ : The stack differs from the expected template and parameter configuration of the stack set to which it belongs. A stack instance is considered to have drifted if one or more of the resources in the associated stack have drifted.
    --
    --
    --     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the stack instance differs from its expected stack set configuration.
    --
    --
    --     * @IN_SYNC@ : The stack instance's actual configuration matches its expected stack set configuration.
    --
    --
    --     * @UNKNOWN@ : This value is reserved for future use.
    driftStatus :: Core.Maybe Types.StackDriftStatus,
    -- | Most recent time when CloudFormation performed a drift detection operation on the stack instance. This value will be @NULL@ for any stack instance on which drift detection has not yet been performed.
    lastDriftCheckTimestamp :: Core.Maybe Core.UTCTime,
    -- | [@Service-managed@ permissions] The organization root ID or organizational unit (OU) IDs that you specified for <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets> .
    organizationalUnitId :: Core.Maybe Types.OrganizationalUnitId,
    -- | The name of the AWS Region that the stack instance is associated with.
    region :: Core.Maybe Types.Region,
    -- | The ID of the stack instance.
    stackId :: Core.Maybe Types.StackId,
    -- | The detailed status of the stack instance.
    stackInstanceStatus :: Core.Maybe Types.StackInstanceComprehensiveStatus,
    -- | The name or unique ID of the stack set that the stack instance is associated with.
    stackSetId :: Core.Maybe Types.StackSetId,
    -- | The status of the stack instance, in terms of its synchronization with its associated stack set.
    --
    --
    --     * @INOPERABLE@ : A @DeleteStackInstances@ operation has failed and left the stack in an unstable state. Stacks in this state are excluded from further @UpdateStackSet@ operations. You might need to perform a @DeleteStackInstances@ operation, with @RetainStacks@ set to @true@ , to delete the stack instance, and then delete the stack manually.
    --
    --
    --     * @OUTDATED@ : The stack isn't currently up to date with the stack set because:
    --
    --     * The associated stack failed during a @CreateStackSet@ or @UpdateStackSet@ operation.
    --
    --
    --     * The stack was part of a @CreateStackSet@ or @UpdateStackSet@ operation that failed or was stopped before the stack was created or updated.
    --
    --
    --
    --
    --     * @CURRENT@ : The stack is currently up to date with the stack set.
    status :: Core.Maybe Types.StackInstanceStatus,
    -- | The explanation for the specific status code assigned to this stack instance.
    statusReason :: Core.Maybe Types.StatusReason
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StackInstanceSummary' value with any optional fields omitted.
mkStackInstanceSummary ::
  StackInstanceSummary
mkStackInstanceSummary =
  StackInstanceSummary'
    { account = Core.Nothing,
      driftStatus = Core.Nothing,
      lastDriftCheckTimestamp = Core.Nothing,
      organizationalUnitId = Core.Nothing,
      region = Core.Nothing,
      stackId = Core.Nothing,
      stackInstanceStatus = Core.Nothing,
      stackSetId = Core.Nothing,
      status = Core.Nothing,
      statusReason = Core.Nothing
    }

-- | [@Self-managed@ permissions] The name of the AWS account that the stack instance is associated with.
--
-- /Note:/ Consider using 'account' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisAccount :: Lens.Lens' StackInstanceSummary (Core.Maybe Types.Account)
sisAccount = Lens.field @"account"
{-# DEPRECATED sisAccount "Use generic-lens or generic-optics with 'account' instead." #-}

-- | Status of the stack instance's actual configuration compared to the expected template and parameter configuration of the stack set to which it belongs.
--
--
--     * @DRIFTED@ : The stack differs from the expected template and parameter configuration of the stack set to which it belongs. A stack instance is considered to have drifted if one or more of the resources in the associated stack have drifted.
--
--
--     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the stack instance differs from its expected stack set configuration.
--
--
--     * @IN_SYNC@ : The stack instance's actual configuration matches its expected stack set configuration.
--
--
--     * @UNKNOWN@ : This value is reserved for future use.
--
--
--
-- /Note:/ Consider using 'driftStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisDriftStatus :: Lens.Lens' StackInstanceSummary (Core.Maybe Types.StackDriftStatus)
sisDriftStatus = Lens.field @"driftStatus"
{-# DEPRECATED sisDriftStatus "Use generic-lens or generic-optics with 'driftStatus' instead." #-}

-- | Most recent time when CloudFormation performed a drift detection operation on the stack instance. This value will be @NULL@ for any stack instance on which drift detection has not yet been performed.
--
-- /Note:/ Consider using 'lastDriftCheckTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisLastDriftCheckTimestamp :: Lens.Lens' StackInstanceSummary (Core.Maybe Core.UTCTime)
sisLastDriftCheckTimestamp = Lens.field @"lastDriftCheckTimestamp"
{-# DEPRECATED sisLastDriftCheckTimestamp "Use generic-lens or generic-optics with 'lastDriftCheckTimestamp' instead." #-}

-- | [@Service-managed@ permissions] The organization root ID or organizational unit (OU) IDs that you specified for <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets> .
--
-- /Note:/ Consider using 'organizationalUnitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisOrganizationalUnitId :: Lens.Lens' StackInstanceSummary (Core.Maybe Types.OrganizationalUnitId)
sisOrganizationalUnitId = Lens.field @"organizationalUnitId"
{-# DEPRECATED sisOrganizationalUnitId "Use generic-lens or generic-optics with 'organizationalUnitId' instead." #-}

-- | The name of the AWS Region that the stack instance is associated with.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisRegion :: Lens.Lens' StackInstanceSummary (Core.Maybe Types.Region)
sisRegion = Lens.field @"region"
{-# DEPRECATED sisRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The ID of the stack instance.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisStackId :: Lens.Lens' StackInstanceSummary (Core.Maybe Types.StackId)
sisStackId = Lens.field @"stackId"
{-# DEPRECATED sisStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The detailed status of the stack instance.
--
-- /Note:/ Consider using 'stackInstanceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisStackInstanceStatus :: Lens.Lens' StackInstanceSummary (Core.Maybe Types.StackInstanceComprehensiveStatus)
sisStackInstanceStatus = Lens.field @"stackInstanceStatus"
{-# DEPRECATED sisStackInstanceStatus "Use generic-lens or generic-optics with 'stackInstanceStatus' instead." #-}

-- | The name or unique ID of the stack set that the stack instance is associated with.
--
-- /Note:/ Consider using 'stackSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisStackSetId :: Lens.Lens' StackInstanceSummary (Core.Maybe Types.StackSetId)
sisStackSetId = Lens.field @"stackSetId"
{-# DEPRECATED sisStackSetId "Use generic-lens or generic-optics with 'stackSetId' instead." #-}

-- | The status of the stack instance, in terms of its synchronization with its associated stack set.
--
--
--     * @INOPERABLE@ : A @DeleteStackInstances@ operation has failed and left the stack in an unstable state. Stacks in this state are excluded from further @UpdateStackSet@ operations. You might need to perform a @DeleteStackInstances@ operation, with @RetainStacks@ set to @true@ , to delete the stack instance, and then delete the stack manually.
--
--
--     * @OUTDATED@ : The stack isn't currently up to date with the stack set because:
--
--     * The associated stack failed during a @CreateStackSet@ or @UpdateStackSet@ operation.
--
--
--     * The stack was part of a @CreateStackSet@ or @UpdateStackSet@ operation that failed or was stopped before the stack was created or updated.
--
--
--
--
--     * @CURRENT@ : The stack is currently up to date with the stack set.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisStatus :: Lens.Lens' StackInstanceSummary (Core.Maybe Types.StackInstanceStatus)
sisStatus = Lens.field @"status"
{-# DEPRECATED sisStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The explanation for the specific status code assigned to this stack instance.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisStatusReason :: Lens.Lens' StackInstanceSummary (Core.Maybe Types.StatusReason)
sisStatusReason = Lens.field @"statusReason"
{-# DEPRECATED sisStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

instance Core.FromXML StackInstanceSummary where
  parseXML x =
    StackInstanceSummary'
      Core.<$> (x Core..@? "Account")
      Core.<*> (x Core..@? "DriftStatus")
      Core.<*> (x Core..@? "LastDriftCheckTimestamp")
      Core.<*> (x Core..@? "OrganizationalUnitId")
      Core.<*> (x Core..@? "Region")
      Core.<*> (x Core..@? "StackId")
      Core.<*> (x Core..@? "StackInstanceStatus")
      Core.<*> (x Core..@? "StackSetId")
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "StatusReason")
