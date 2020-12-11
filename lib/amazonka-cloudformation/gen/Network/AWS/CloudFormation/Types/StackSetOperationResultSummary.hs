-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetOperationResultSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSetOperationResultSummary
  ( StackSetOperationResultSummary (..),

    -- * Smart constructor
    mkStackSetOperationResultSummary,

    -- * Lenses
    ssorsStatus,
    ssorsAccount,
    ssorsAccountGateResult,
    ssorsOrganizationalUnitId,
    ssorsRegion,
    ssorsStatusReason,
  )
where

import Network.AWS.CloudFormation.Types.AccountGateResult
import Network.AWS.CloudFormation.Types.StackSetOperationResultStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The structure that contains information about a specified operation's results for a given account in a given Region.
--
-- /See:/ 'mkStackSetOperationResultSummary' smart constructor.
data StackSetOperationResultSummary = StackSetOperationResultSummary'
  { status ::
      Lude.Maybe
        StackSetOperationResultStatus,
    account ::
      Lude.Maybe Lude.Text,
    accountGateResult ::
      Lude.Maybe AccountGateResult,
    organizationalUnitId ::
      Lude.Maybe Lude.Text,
    region ::
      Lude.Maybe Lude.Text,
    statusReason ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StackSetOperationResultSummary' with the minimum fields required to make a request.
--
-- * 'account' - [@Self-managed@ permissions] The name of the AWS account for this operation result.
-- * 'accountGateResult' - The results of the account gate function AWS CloudFormation invokes, if present, before proceeding with stack set operations in an account
-- * 'organizationalUnitId' - [@Service-managed@ permissions] The organization root ID or organizational unit (OU) IDs that you specified for <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets> .
-- * 'region' - The name of the AWS Region for this operation result.
-- * 'status' - The result status of the stack set operation for the given account in the given Region.
--
--
--     * @CANCELLED@ : The operation in the specified account and Region has been cancelled. This is either because a user has stopped the stack set operation, or because the failure tolerance of the stack set operation has been exceeded.
--
--
--     * @FAILED@ : The operation in the specified account and Region failed.
-- If the stack set operation fails in enough accounts within a Region, the failure tolerance for the stack set operation as a whole might be exceeded.
--
--
--     * @RUNNING@ : The operation in the specified account and Region is currently in progress.
--
--
--     * @PENDING@ : The operation in the specified account and Region has yet to start.
--
--
--     * @SUCCEEDED@ : The operation in the specified account and Region completed successfully.
--
--
-- * 'statusReason' - The reason for the assigned result status.
mkStackSetOperationResultSummary ::
  StackSetOperationResultSummary
mkStackSetOperationResultSummary =
  StackSetOperationResultSummary'
    { status = Lude.Nothing,
      account = Lude.Nothing,
      accountGateResult = Lude.Nothing,
      organizationalUnitId = Lude.Nothing,
      region = Lude.Nothing,
      statusReason = Lude.Nothing
    }

-- | The result status of the stack set operation for the given account in the given Region.
--
--
--     * @CANCELLED@ : The operation in the specified account and Region has been cancelled. This is either because a user has stopped the stack set operation, or because the failure tolerance of the stack set operation has been exceeded.
--
--
--     * @FAILED@ : The operation in the specified account and Region failed.
-- If the stack set operation fails in enough accounts within a Region, the failure tolerance for the stack set operation as a whole might be exceeded.
--
--
--     * @RUNNING@ : The operation in the specified account and Region is currently in progress.
--
--
--     * @PENDING@ : The operation in the specified account and Region has yet to start.
--
--
--     * @SUCCEEDED@ : The operation in the specified account and Region completed successfully.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssorsStatus :: Lens.Lens' StackSetOperationResultSummary (Lude.Maybe StackSetOperationResultStatus)
ssorsStatus = Lens.lens (status :: StackSetOperationResultSummary -> Lude.Maybe StackSetOperationResultStatus) (\s a -> s {status = a} :: StackSetOperationResultSummary)
{-# DEPRECATED ssorsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | [@Self-managed@ permissions] The name of the AWS account for this operation result.
--
-- /Note:/ Consider using 'account' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssorsAccount :: Lens.Lens' StackSetOperationResultSummary (Lude.Maybe Lude.Text)
ssorsAccount = Lens.lens (account :: StackSetOperationResultSummary -> Lude.Maybe Lude.Text) (\s a -> s {account = a} :: StackSetOperationResultSummary)
{-# DEPRECATED ssorsAccount "Use generic-lens or generic-optics with 'account' instead." #-}

-- | The results of the account gate function AWS CloudFormation invokes, if present, before proceeding with stack set operations in an account
--
-- /Note:/ Consider using 'accountGateResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssorsAccountGateResult :: Lens.Lens' StackSetOperationResultSummary (Lude.Maybe AccountGateResult)
ssorsAccountGateResult = Lens.lens (accountGateResult :: StackSetOperationResultSummary -> Lude.Maybe AccountGateResult) (\s a -> s {accountGateResult = a} :: StackSetOperationResultSummary)
{-# DEPRECATED ssorsAccountGateResult "Use generic-lens or generic-optics with 'accountGateResult' instead." #-}

-- | [@Service-managed@ permissions] The organization root ID or organizational unit (OU) IDs that you specified for <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets> .
--
-- /Note:/ Consider using 'organizationalUnitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssorsOrganizationalUnitId :: Lens.Lens' StackSetOperationResultSummary (Lude.Maybe Lude.Text)
ssorsOrganizationalUnitId = Lens.lens (organizationalUnitId :: StackSetOperationResultSummary -> Lude.Maybe Lude.Text) (\s a -> s {organizationalUnitId = a} :: StackSetOperationResultSummary)
{-# DEPRECATED ssorsOrganizationalUnitId "Use generic-lens or generic-optics with 'organizationalUnitId' instead." #-}

-- | The name of the AWS Region for this operation result.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssorsRegion :: Lens.Lens' StackSetOperationResultSummary (Lude.Maybe Lude.Text)
ssorsRegion = Lens.lens (region :: StackSetOperationResultSummary -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: StackSetOperationResultSummary)
{-# DEPRECATED ssorsRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The reason for the assigned result status.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssorsStatusReason :: Lens.Lens' StackSetOperationResultSummary (Lude.Maybe Lude.Text)
ssorsStatusReason = Lens.lens (statusReason :: StackSetOperationResultSummary -> Lude.Maybe Lude.Text) (\s a -> s {statusReason = a} :: StackSetOperationResultSummary)
{-# DEPRECATED ssorsStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

instance Lude.FromXML StackSetOperationResultSummary where
  parseXML x =
    StackSetOperationResultSummary'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "Account")
      Lude.<*> (x Lude..@? "AccountGateResult")
      Lude.<*> (x Lude..@? "OrganizationalUnitId")
      Lude.<*> (x Lude..@? "Region")
      Lude.<*> (x Lude..@? "StatusReason")
