{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackInstance
  ( StackInstance (..),

    -- * Smart constructor
    mkStackInstance,

    -- * Lenses
    siStatus,
    siLastDriftCheckTimestamp,
    siAccount,
    siDriftStatus,
    siOrganizationalUnitId,
    siRegion,
    siStatusReason,
    siStackId,
    siStackInstanceStatus,
    siParameterOverrides,
    siStackSetId,
  )
where

import Network.AWS.CloudFormation.Types.Parameter
import Network.AWS.CloudFormation.Types.StackDriftStatus
import Network.AWS.CloudFormation.Types.StackInstanceComprehensiveStatus
import Network.AWS.CloudFormation.Types.StackInstanceStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An AWS CloudFormation stack, in a specific account and Region, that's part of a stack set operation. A stack instance is a reference to an attempted or actual stack in a given account within a given Region. A stack instance can exist without a stack—for example, if the stack couldn't be created for some reason. A stack instance is associated with only one stack set. Each stack instance contains the ID of its associated stack set, as well as the ID of the actual stack and the stack status.
--
-- /See:/ 'mkStackInstance' smart constructor.
data StackInstance = StackInstance'
  { -- | The status of the stack instance, in terms of its synchronization with its associated stack set.
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
    status :: Lude.Maybe StackInstanceStatus,
    -- | Most recent time when CloudFormation performed a drift detection operation on the stack instance. This value will be @NULL@ for any stack instance on which drift detection has not yet been performed.
    lastDriftCheckTimestamp :: Lude.Maybe Lude.DateTime,
    -- | [@Self-managed@ permissions] The name of the AWS account that the stack instance is associated with.
    account :: Lude.Maybe Lude.Text,
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
    driftStatus :: Lude.Maybe StackDriftStatus,
    -- | [@Service-managed@ permissions] The organization root ID or organizational unit (OU) IDs that you specified for <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets> .
    organizationalUnitId :: Lude.Maybe Lude.Text,
    -- | The name of the AWS Region that the stack instance is associated with.
    region :: Lude.Maybe Lude.Text,
    -- | The explanation for the specific status code that is assigned to this stack instance.
    statusReason :: Lude.Maybe Lude.Text,
    -- | The ID of the stack instance.
    stackId :: Lude.Maybe Lude.Text,
    -- | The detailed status of the stack instance.
    stackInstanceStatus :: Lude.Maybe StackInstanceComprehensiveStatus,
    -- | A list of parameters from the stack set template whose values have been overridden in this stack instance.
    parameterOverrides :: Lude.Maybe [Parameter],
    -- | The name or unique ID of the stack set that the stack instance is associated with.
    stackSetId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StackInstance' with the minimum fields required to make a request.
--
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
-- * 'lastDriftCheckTimestamp' - Most recent time when CloudFormation performed a drift detection operation on the stack instance. This value will be @NULL@ for any stack instance on which drift detection has not yet been performed.
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
-- * 'organizationalUnitId' - [@Service-managed@ permissions] The organization root ID or organizational unit (OU) IDs that you specified for <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets> .
-- * 'region' - The name of the AWS Region that the stack instance is associated with.
-- * 'statusReason' - The explanation for the specific status code that is assigned to this stack instance.
-- * 'stackId' - The ID of the stack instance.
-- * 'stackInstanceStatus' - The detailed status of the stack instance.
-- * 'parameterOverrides' - A list of parameters from the stack set template whose values have been overridden in this stack instance.
-- * 'stackSetId' - The name or unique ID of the stack set that the stack instance is associated with.
mkStackInstance ::
  StackInstance
mkStackInstance =
  StackInstance'
    { status = Lude.Nothing,
      lastDriftCheckTimestamp = Lude.Nothing,
      account = Lude.Nothing,
      driftStatus = Lude.Nothing,
      organizationalUnitId = Lude.Nothing,
      region = Lude.Nothing,
      statusReason = Lude.Nothing,
      stackId = Lude.Nothing,
      stackInstanceStatus = Lude.Nothing,
      parameterOverrides = Lude.Nothing,
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
siStatus :: Lens.Lens' StackInstance (Lude.Maybe StackInstanceStatus)
siStatus = Lens.lens (status :: StackInstance -> Lude.Maybe StackInstanceStatus) (\s a -> s {status = a} :: StackInstance)
{-# DEPRECATED siStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Most recent time when CloudFormation performed a drift detection operation on the stack instance. This value will be @NULL@ for any stack instance on which drift detection has not yet been performed.
--
-- /Note:/ Consider using 'lastDriftCheckTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siLastDriftCheckTimestamp :: Lens.Lens' StackInstance (Lude.Maybe Lude.DateTime)
siLastDriftCheckTimestamp = Lens.lens (lastDriftCheckTimestamp :: StackInstance -> Lude.Maybe Lude.DateTime) (\s a -> s {lastDriftCheckTimestamp = a} :: StackInstance)
{-# DEPRECATED siLastDriftCheckTimestamp "Use generic-lens or generic-optics with 'lastDriftCheckTimestamp' instead." #-}

-- | [@Self-managed@ permissions] The name of the AWS account that the stack instance is associated with.
--
-- /Note:/ Consider using 'account' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siAccount :: Lens.Lens' StackInstance (Lude.Maybe Lude.Text)
siAccount = Lens.lens (account :: StackInstance -> Lude.Maybe Lude.Text) (\s a -> s {account = a} :: StackInstance)
{-# DEPRECATED siAccount "Use generic-lens or generic-optics with 'account' instead." #-}

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
siDriftStatus :: Lens.Lens' StackInstance (Lude.Maybe StackDriftStatus)
siDriftStatus = Lens.lens (driftStatus :: StackInstance -> Lude.Maybe StackDriftStatus) (\s a -> s {driftStatus = a} :: StackInstance)
{-# DEPRECATED siDriftStatus "Use generic-lens or generic-optics with 'driftStatus' instead." #-}

-- | [@Service-managed@ permissions] The organization root ID or organizational unit (OU) IDs that you specified for <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets> .
--
-- /Note:/ Consider using 'organizationalUnitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siOrganizationalUnitId :: Lens.Lens' StackInstance (Lude.Maybe Lude.Text)
siOrganizationalUnitId = Lens.lens (organizationalUnitId :: StackInstance -> Lude.Maybe Lude.Text) (\s a -> s {organizationalUnitId = a} :: StackInstance)
{-# DEPRECATED siOrganizationalUnitId "Use generic-lens or generic-optics with 'organizationalUnitId' instead." #-}

-- | The name of the AWS Region that the stack instance is associated with.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siRegion :: Lens.Lens' StackInstance (Lude.Maybe Lude.Text)
siRegion = Lens.lens (region :: StackInstance -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: StackInstance)
{-# DEPRECATED siRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The explanation for the specific status code that is assigned to this stack instance.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siStatusReason :: Lens.Lens' StackInstance (Lude.Maybe Lude.Text)
siStatusReason = Lens.lens (statusReason :: StackInstance -> Lude.Maybe Lude.Text) (\s a -> s {statusReason = a} :: StackInstance)
{-# DEPRECATED siStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

-- | The ID of the stack instance.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siStackId :: Lens.Lens' StackInstance (Lude.Maybe Lude.Text)
siStackId = Lens.lens (stackId :: StackInstance -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: StackInstance)
{-# DEPRECATED siStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The detailed status of the stack instance.
--
-- /Note:/ Consider using 'stackInstanceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siStackInstanceStatus :: Lens.Lens' StackInstance (Lude.Maybe StackInstanceComprehensiveStatus)
siStackInstanceStatus = Lens.lens (stackInstanceStatus :: StackInstance -> Lude.Maybe StackInstanceComprehensiveStatus) (\s a -> s {stackInstanceStatus = a} :: StackInstance)
{-# DEPRECATED siStackInstanceStatus "Use generic-lens or generic-optics with 'stackInstanceStatus' instead." #-}

-- | A list of parameters from the stack set template whose values have been overridden in this stack instance.
--
-- /Note:/ Consider using 'parameterOverrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siParameterOverrides :: Lens.Lens' StackInstance (Lude.Maybe [Parameter])
siParameterOverrides = Lens.lens (parameterOverrides :: StackInstance -> Lude.Maybe [Parameter]) (\s a -> s {parameterOverrides = a} :: StackInstance)
{-# DEPRECATED siParameterOverrides "Use generic-lens or generic-optics with 'parameterOverrides' instead." #-}

-- | The name or unique ID of the stack set that the stack instance is associated with.
--
-- /Note:/ Consider using 'stackSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siStackSetId :: Lens.Lens' StackInstance (Lude.Maybe Lude.Text)
siStackSetId = Lens.lens (stackSetId :: StackInstance -> Lude.Maybe Lude.Text) (\s a -> s {stackSetId = a} :: StackInstance)
{-# DEPRECATED siStackSetId "Use generic-lens or generic-optics with 'stackSetId' instead." #-}

instance Lude.FromXML StackInstance where
  parseXML x =
    StackInstance'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "LastDriftCheckTimestamp")
      Lude.<*> (x Lude..@? "Account")
      Lude.<*> (x Lude..@? "DriftStatus")
      Lude.<*> (x Lude..@? "OrganizationalUnitId")
      Lude.<*> (x Lude..@? "Region")
      Lude.<*> (x Lude..@? "StatusReason")
      Lude.<*> (x Lude..@? "StackId")
      Lude.<*> (x Lude..@? "StackInstanceStatus")
      Lude.<*> ( x Lude..@? "ParameterOverrides" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "StackSetId")
