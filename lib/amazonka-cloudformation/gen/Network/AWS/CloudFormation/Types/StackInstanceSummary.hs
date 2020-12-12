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
    sisStatus,
    sisLastDriftCheckTimestamp,
    sisAccount,
    sisDriftStatus,
    sisOrganizationalUnitId,
    sisRegion,
    sisStatusReason,
    sisStackId,
    sisStackInstanceStatus,
    sisStackSetId,
  )
where

import Network.AWS.CloudFormation.Types.StackDriftStatus
import Network.AWS.CloudFormation.Types.StackInstanceComprehensiveStatus
import Network.AWS.CloudFormation.Types.StackInstanceStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The structure that contains summary information about a stack instance.
--
-- /See:/ 'mkStackInstanceSummary' smart constructor.
data StackInstanceSummary = StackInstanceSummary'
  { status ::
      Lude.Maybe StackInstanceStatus,
    lastDriftCheckTimestamp ::
      Lude.Maybe Lude.DateTime,
    account :: Lude.Maybe Lude.Text,
    driftStatus :: Lude.Maybe StackDriftStatus,
    organizationalUnitId :: Lude.Maybe Lude.Text,
    region :: Lude.Maybe Lude.Text,
    statusReason :: Lude.Maybe Lude.Text,
    stackId :: Lude.Maybe Lude.Text,
    stackInstanceStatus ::
      Lude.Maybe StackInstanceComprehensiveStatus,
    stackSetId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StackInstanceSummary' with the minimum fields required to make a request.
--
-- * 'account' - [@Self-managed@ permissions] The name of the AWS account that the stack instance is associated with.
-- * 'driftStatus' - Status of the stack instance's actual configuration compared to the expected template and parameter configuration of the stack set to which it belongs.
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
-- * 'lastDriftCheckTimestamp' - Most recent time when CloudFormation performed a drift detection operation on the stack instance. This value will be @NULL@ for any stack instance on which drift detection has not yet been performed.
-- * 'organizationalUnitId' - [@Service-managed@ permissions] The organization root ID or organizational unit (OU) IDs that you specified for <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets> .
-- * 'region' - The name of the AWS Region that the stack instance is associated with.
-- * 'stackId' - The ID of the stack instance.
-- * 'stackInstanceStatus' - The detailed status of the stack instance.
-- * 'stackSetId' - The name or unique ID of the stack set that the stack instance is associated with.
-- * 'status' - The status of the stack instance, in terms of its synchronization with its associated stack set.
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
-- * 'statusReason' - The explanation for the specific status code assigned to this stack instance.
mkStackInstanceSummary ::
  StackInstanceSummary
mkStackInstanceSummary =
  StackInstanceSummary'
    { status = Lude.Nothing,
      lastDriftCheckTimestamp = Lude.Nothing,
      account = Lude.Nothing,
      driftStatus = Lude.Nothing,
      organizationalUnitId = Lude.Nothing,
      region = Lude.Nothing,
      statusReason = Lude.Nothing,
      stackId = Lude.Nothing,
      stackInstanceStatus = Lude.Nothing,
      stackSetId = Lude.Nothing
    }

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
sisStatus :: Lens.Lens' StackInstanceSummary (Lude.Maybe StackInstanceStatus)
sisStatus = Lens.lens (status :: StackInstanceSummary -> Lude.Maybe StackInstanceStatus) (\s a -> s {status = a} :: StackInstanceSummary)
{-# DEPRECATED sisStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Most recent time when CloudFormation performed a drift detection operation on the stack instance. This value will be @NULL@ for any stack instance on which drift detection has not yet been performed.
--
-- /Note:/ Consider using 'lastDriftCheckTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisLastDriftCheckTimestamp :: Lens.Lens' StackInstanceSummary (Lude.Maybe Lude.DateTime)
sisLastDriftCheckTimestamp = Lens.lens (lastDriftCheckTimestamp :: StackInstanceSummary -> Lude.Maybe Lude.DateTime) (\s a -> s {lastDriftCheckTimestamp = a} :: StackInstanceSummary)
{-# DEPRECATED sisLastDriftCheckTimestamp "Use generic-lens or generic-optics with 'lastDriftCheckTimestamp' instead." #-}

-- | [@Self-managed@ permissions] The name of the AWS account that the stack instance is associated with.
--
-- /Note:/ Consider using 'account' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisAccount :: Lens.Lens' StackInstanceSummary (Lude.Maybe Lude.Text)
sisAccount = Lens.lens (account :: StackInstanceSummary -> Lude.Maybe Lude.Text) (\s a -> s {account = a} :: StackInstanceSummary)
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
sisDriftStatus :: Lens.Lens' StackInstanceSummary (Lude.Maybe StackDriftStatus)
sisDriftStatus = Lens.lens (driftStatus :: StackInstanceSummary -> Lude.Maybe StackDriftStatus) (\s a -> s {driftStatus = a} :: StackInstanceSummary)
{-# DEPRECATED sisDriftStatus "Use generic-lens or generic-optics with 'driftStatus' instead." #-}

-- | [@Service-managed@ permissions] The organization root ID or organizational unit (OU) IDs that you specified for <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets> .
--
-- /Note:/ Consider using 'organizationalUnitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisOrganizationalUnitId :: Lens.Lens' StackInstanceSummary (Lude.Maybe Lude.Text)
sisOrganizationalUnitId = Lens.lens (organizationalUnitId :: StackInstanceSummary -> Lude.Maybe Lude.Text) (\s a -> s {organizationalUnitId = a} :: StackInstanceSummary)
{-# DEPRECATED sisOrganizationalUnitId "Use generic-lens or generic-optics with 'organizationalUnitId' instead." #-}

-- | The name of the AWS Region that the stack instance is associated with.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisRegion :: Lens.Lens' StackInstanceSummary (Lude.Maybe Lude.Text)
sisRegion = Lens.lens (region :: StackInstanceSummary -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: StackInstanceSummary)
{-# DEPRECATED sisRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The explanation for the specific status code assigned to this stack instance.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisStatusReason :: Lens.Lens' StackInstanceSummary (Lude.Maybe Lude.Text)
sisStatusReason = Lens.lens (statusReason :: StackInstanceSummary -> Lude.Maybe Lude.Text) (\s a -> s {statusReason = a} :: StackInstanceSummary)
{-# DEPRECATED sisStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

-- | The ID of the stack instance.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisStackId :: Lens.Lens' StackInstanceSummary (Lude.Maybe Lude.Text)
sisStackId = Lens.lens (stackId :: StackInstanceSummary -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: StackInstanceSummary)
{-# DEPRECATED sisStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The detailed status of the stack instance.
--
-- /Note:/ Consider using 'stackInstanceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisStackInstanceStatus :: Lens.Lens' StackInstanceSummary (Lude.Maybe StackInstanceComprehensiveStatus)
sisStackInstanceStatus = Lens.lens (stackInstanceStatus :: StackInstanceSummary -> Lude.Maybe StackInstanceComprehensiveStatus) (\s a -> s {stackInstanceStatus = a} :: StackInstanceSummary)
{-# DEPRECATED sisStackInstanceStatus "Use generic-lens or generic-optics with 'stackInstanceStatus' instead." #-}

-- | The name or unique ID of the stack set that the stack instance is associated with.
--
-- /Note:/ Consider using 'stackSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sisStackSetId :: Lens.Lens' StackInstanceSummary (Lude.Maybe Lude.Text)
sisStackSetId = Lens.lens (stackSetId :: StackInstanceSummary -> Lude.Maybe Lude.Text) (\s a -> s {stackSetId = a} :: StackInstanceSummary)
{-# DEPRECATED sisStackSetId "Use generic-lens or generic-optics with 'stackSetId' instead." #-}

instance Lude.FromXML StackInstanceSummary where
  parseXML x =
    StackInstanceSummary'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "LastDriftCheckTimestamp")
      Lude.<*> (x Lude..@? "Account")
      Lude.<*> (x Lude..@? "DriftStatus")
      Lude.<*> (x Lude..@? "OrganizationalUnitId")
      Lude.<*> (x Lude..@? "Region")
      Lude.<*> (x Lude..@? "StatusReason")
      Lude.<*> (x Lude..@? "StackId")
      Lude.<*> (x Lude..@? "StackInstanceStatus")
      Lude.<*> (x Lude..@? "StackSetId")
