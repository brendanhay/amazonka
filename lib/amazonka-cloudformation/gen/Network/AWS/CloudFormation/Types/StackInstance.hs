{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.StackInstance
  ( StackInstance (..)
  -- * Smart constructor
  , mkStackInstance
  -- * Lenses
  , siAccount
  , siDriftStatus
  , siLastDriftCheckTimestamp
  , siOrganizationalUnitId
  , siParameterOverrides
  , siRegion
  , siStackId
  , siStackInstanceStatus
  , siStackSetId
  , siStatus
  , siStatusReason
  ) where

import qualified Network.AWS.CloudFormation.Types.Account as Types
import qualified Network.AWS.CloudFormation.Types.OrganizationalUnitId as Types
import qualified Network.AWS.CloudFormation.Types.Parameter as Types
import qualified Network.AWS.CloudFormation.Types.Reason as Types
import qualified Network.AWS.CloudFormation.Types.Region as Types
import qualified Network.AWS.CloudFormation.Types.StackDriftStatus as Types
import qualified Network.AWS.CloudFormation.Types.StackId as Types
import qualified Network.AWS.CloudFormation.Types.StackInstanceComprehensiveStatus as Types
import qualified Network.AWS.CloudFormation.Types.StackInstanceStatus as Types
import qualified Network.AWS.CloudFormation.Types.StackSetId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An AWS CloudFormation stack, in a specific account and Region, that's part of a stack set operation. A stack instance is a reference to an attempted or actual stack in a given account within a given Region. A stack instance can exist without a stack—for example, if the stack couldn't be created for some reason. A stack instance is associated with only one stack set. Each stack instance contains the ID of its associated stack set, as well as the ID of the actual stack and the stack status.
--
-- /See:/ 'mkStackInstance' smart constructor.
data StackInstance = StackInstance'
  { account :: Core.Maybe Types.Account
    -- ^ [@Self-managed@ permissions] The name of the AWS account that the stack instance is associated with.
  , driftStatus :: Core.Maybe Types.StackDriftStatus
    -- ^ Status of the stack instance's actual configuration compared to the expected template and parameter configuration of the stack set to which it belongs. 
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
  , lastDriftCheckTimestamp :: Core.Maybe Core.UTCTime
    -- ^ Most recent time when CloudFormation performed a drift detection operation on the stack instance. This value will be @NULL@ for any stack instance on which drift detection has not yet been performed.
  , organizationalUnitId :: Core.Maybe Types.OrganizationalUnitId
    -- ^ [@Service-managed@ permissions] The organization root ID or organizational unit (OU) IDs that you specified for <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets> .
  , parameterOverrides :: Core.Maybe [Types.Parameter]
    -- ^ A list of parameters from the stack set template whose values have been overridden in this stack instance.
  , region :: Core.Maybe Types.Region
    -- ^ The name of the AWS Region that the stack instance is associated with.
  , stackId :: Core.Maybe Types.StackId
    -- ^ The ID of the stack instance.
  , stackInstanceStatus :: Core.Maybe Types.StackInstanceComprehensiveStatus
    -- ^ The detailed status of the stack instance.
  , stackSetId :: Core.Maybe Types.StackSetId
    -- ^ The name or unique ID of the stack set that the stack instance is associated with.
  , status :: Core.Maybe Types.StackInstanceStatus
    -- ^ The status of the stack instance, in terms of its synchronization with its associated stack set.
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
  , statusReason :: Core.Maybe Types.Reason
    -- ^ The explanation for the specific status code that is assigned to this stack instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StackInstance' value with any optional fields omitted.
mkStackInstance
    :: StackInstance
mkStackInstance
  = StackInstance'{account = Core.Nothing,
                   driftStatus = Core.Nothing, lastDriftCheckTimestamp = Core.Nothing,
                   organizationalUnitId = Core.Nothing,
                   parameterOverrides = Core.Nothing, region = Core.Nothing,
                   stackId = Core.Nothing, stackInstanceStatus = Core.Nothing,
                   stackSetId = Core.Nothing, status = Core.Nothing,
                   statusReason = Core.Nothing}

-- | [@Self-managed@ permissions] The name of the AWS account that the stack instance is associated with.
--
-- /Note:/ Consider using 'account' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siAccount :: Lens.Lens' StackInstance (Core.Maybe Types.Account)
siAccount = Lens.field @"account"
{-# INLINEABLE siAccount #-}
{-# DEPRECATED account "Use generic-lens or generic-optics with 'account' instead"  #-}

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
siDriftStatus :: Lens.Lens' StackInstance (Core.Maybe Types.StackDriftStatus)
siDriftStatus = Lens.field @"driftStatus"
{-# INLINEABLE siDriftStatus #-}
{-# DEPRECATED driftStatus "Use generic-lens or generic-optics with 'driftStatus' instead"  #-}

-- | Most recent time when CloudFormation performed a drift detection operation on the stack instance. This value will be @NULL@ for any stack instance on which drift detection has not yet been performed.
--
-- /Note:/ Consider using 'lastDriftCheckTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siLastDriftCheckTimestamp :: Lens.Lens' StackInstance (Core.Maybe Core.UTCTime)
siLastDriftCheckTimestamp = Lens.field @"lastDriftCheckTimestamp"
{-# INLINEABLE siLastDriftCheckTimestamp #-}
{-# DEPRECATED lastDriftCheckTimestamp "Use generic-lens or generic-optics with 'lastDriftCheckTimestamp' instead"  #-}

-- | [@Service-managed@ permissions] The organization root ID or organizational unit (OU) IDs that you specified for <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets> .
--
-- /Note:/ Consider using 'organizationalUnitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siOrganizationalUnitId :: Lens.Lens' StackInstance (Core.Maybe Types.OrganizationalUnitId)
siOrganizationalUnitId = Lens.field @"organizationalUnitId"
{-# INLINEABLE siOrganizationalUnitId #-}
{-# DEPRECATED organizationalUnitId "Use generic-lens or generic-optics with 'organizationalUnitId' instead"  #-}

-- | A list of parameters from the stack set template whose values have been overridden in this stack instance.
--
-- /Note:/ Consider using 'parameterOverrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siParameterOverrides :: Lens.Lens' StackInstance (Core.Maybe [Types.Parameter])
siParameterOverrides = Lens.field @"parameterOverrides"
{-# INLINEABLE siParameterOverrides #-}
{-# DEPRECATED parameterOverrides "Use generic-lens or generic-optics with 'parameterOverrides' instead"  #-}

-- | The name of the AWS Region that the stack instance is associated with.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siRegion :: Lens.Lens' StackInstance (Core.Maybe Types.Region)
siRegion = Lens.field @"region"
{-# INLINEABLE siRegion #-}
{-# DEPRECATED region "Use generic-lens or generic-optics with 'region' instead"  #-}

-- | The ID of the stack instance.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siStackId :: Lens.Lens' StackInstance (Core.Maybe Types.StackId)
siStackId = Lens.field @"stackId"
{-# INLINEABLE siStackId #-}
{-# DEPRECATED stackId "Use generic-lens or generic-optics with 'stackId' instead"  #-}

-- | The detailed status of the stack instance.
--
-- /Note:/ Consider using 'stackInstanceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siStackInstanceStatus :: Lens.Lens' StackInstance (Core.Maybe Types.StackInstanceComprehensiveStatus)
siStackInstanceStatus = Lens.field @"stackInstanceStatus"
{-# INLINEABLE siStackInstanceStatus #-}
{-# DEPRECATED stackInstanceStatus "Use generic-lens or generic-optics with 'stackInstanceStatus' instead"  #-}

-- | The name or unique ID of the stack set that the stack instance is associated with.
--
-- /Note:/ Consider using 'stackSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siStackSetId :: Lens.Lens' StackInstance (Core.Maybe Types.StackSetId)
siStackSetId = Lens.field @"stackSetId"
{-# INLINEABLE siStackSetId #-}
{-# DEPRECATED stackSetId "Use generic-lens or generic-optics with 'stackSetId' instead"  #-}

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
siStatus :: Lens.Lens' StackInstance (Core.Maybe Types.StackInstanceStatus)
siStatus = Lens.field @"status"
{-# INLINEABLE siStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The explanation for the specific status code that is assigned to this stack instance.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siStatusReason :: Lens.Lens' StackInstance (Core.Maybe Types.Reason)
siStatusReason = Lens.field @"statusReason"
{-# INLINEABLE siStatusReason #-}
{-# DEPRECATED statusReason "Use generic-lens or generic-optics with 'statusReason' instead"  #-}

instance Core.FromXML StackInstance where
        parseXML x
          = StackInstance' Core.<$>
              (x Core..@? "Account") Core.<*> x Core..@? "DriftStatus" Core.<*>
                x Core..@? "LastDriftCheckTimestamp"
                Core.<*> x Core..@? "OrganizationalUnitId"
                Core.<*>
                x Core..@? "ParameterOverrides" Core..<@>
                  Core.parseXMLList "member"
                Core.<*> x Core..@? "Region"
                Core.<*> x Core..@? "StackId"
                Core.<*> x Core..@? "StackInstanceStatus"
                Core.<*> x Core..@? "StackSetId"
                Core.<*> x Core..@? "Status"
                Core.<*> x Core..@? "StatusReason"
