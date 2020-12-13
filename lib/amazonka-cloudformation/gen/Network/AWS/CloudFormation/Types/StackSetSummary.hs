{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSetSummary
  ( StackSetSummary (..),

    -- * Smart constructor
    mkStackSetSummary,

    -- * Lenses
    sssStatus,
    sssLastDriftCheckTimestamp,
    sssAutoDeployment,
    sssDriftStatus,
    sssPermissionModel,
    sssStackSetName,
    sssDescription,
    sssStackSetId,
  )
where

import Network.AWS.CloudFormation.Types.AutoDeployment
import Network.AWS.CloudFormation.Types.PermissionModels
import Network.AWS.CloudFormation.Types.StackDriftStatus
import Network.AWS.CloudFormation.Types.StackSetStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The structures that contain summary information about the specified stack set.
--
-- /See:/ 'mkStackSetSummary' smart constructor.
data StackSetSummary = StackSetSummary'
  { -- | The status of the stack set.
    status :: Lude.Maybe StackSetStatus,
    -- | Most recent time when CloudFormation performed a drift detection operation on the stack set. This value will be @NULL@ for any stack set on which drift detection has not yet been performed.
    lastDriftCheckTimestamp :: Lude.Maybe Lude.DateTime,
    -- | [@Service-managed@ permissions] Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to a target organizational unit (OU).
    autoDeployment :: Lude.Maybe AutoDeployment,
    -- | Status of the stack set's actual configuration compared to its expected template and parameter configuration. A stack set is considered to have drifted if one or more of its stack instances have drifted from their expected template and parameter configuration.
    --
    --
    --     * @DRIFTED@ : One or more of the stack instances belonging to the stack set stack differs from the expected template and parameter configuration. A stack instance is considered to have drifted if one or more of the resources in the associated stack have drifted.
    --
    --
    --     * @NOT_CHECKED@ : AWS CloudFormation has not checked the stack set for drift.
    --
    --
    --     * @IN_SYNC@ : All of the stack instances belonging to the stack set stack match from the expected template and parameter configuration.
    --
    --
    --     * @UNKNOWN@ : This value is reserved for future use.
    driftStatus :: Lude.Maybe StackDriftStatus,
    -- | Describes how the IAM roles required for stack set operations are created.
    --
    --
    --     * With @self-managed@ permissions, you must create the administrator and execution roles required to deploy to target accounts. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions> .
    --
    --
    --     * With @service-managed@ permissions, StackSets automatically creates the IAM roles required to deploy to accounts managed by AWS Organizations. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions> .
    permissionModel :: Lude.Maybe PermissionModels,
    -- | The name of the stack set.
    stackSetName :: Lude.Maybe Lude.Text,
    -- | A description of the stack set that you specify when the stack set is created or updated.
    description :: Lude.Maybe Lude.Text,
    -- | The ID of the stack set.
    stackSetId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StackSetSummary' with the minimum fields required to make a request.
--
-- * 'status' - The status of the stack set.
-- * 'lastDriftCheckTimestamp' - Most recent time when CloudFormation performed a drift detection operation on the stack set. This value will be @NULL@ for any stack set on which drift detection has not yet been performed.
-- * 'autoDeployment' - [@Service-managed@ permissions] Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to a target organizational unit (OU).
-- * 'driftStatus' - Status of the stack set's actual configuration compared to its expected template and parameter configuration. A stack set is considered to have drifted if one or more of its stack instances have drifted from their expected template and parameter configuration.
--
--
--     * @DRIFTED@ : One or more of the stack instances belonging to the stack set stack differs from the expected template and parameter configuration. A stack instance is considered to have drifted if one or more of the resources in the associated stack have drifted.
--
--
--     * @NOT_CHECKED@ : AWS CloudFormation has not checked the stack set for drift.
--
--
--     * @IN_SYNC@ : All of the stack instances belonging to the stack set stack match from the expected template and parameter configuration.
--
--
--     * @UNKNOWN@ : This value is reserved for future use.
--
--
-- * 'permissionModel' - Describes how the IAM roles required for stack set operations are created.
--
--
--     * With @self-managed@ permissions, you must create the administrator and execution roles required to deploy to target accounts. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions> .
--
--
--     * With @service-managed@ permissions, StackSets automatically creates the IAM roles required to deploy to accounts managed by AWS Organizations. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions> .
--
--
-- * 'stackSetName' - The name of the stack set.
-- * 'description' - A description of the stack set that you specify when the stack set is created or updated.
-- * 'stackSetId' - The ID of the stack set.
mkStackSetSummary ::
  StackSetSummary
mkStackSetSummary =
  StackSetSummary'
    { status = Lude.Nothing,
      lastDriftCheckTimestamp = Lude.Nothing,
      autoDeployment = Lude.Nothing,
      driftStatus = Lude.Nothing,
      permissionModel = Lude.Nothing,
      stackSetName = Lude.Nothing,
      description = Lude.Nothing,
      stackSetId = Lude.Nothing
    }

-- | The status of the stack set.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssStatus :: Lens.Lens' StackSetSummary (Lude.Maybe StackSetStatus)
sssStatus = Lens.lens (status :: StackSetSummary -> Lude.Maybe StackSetStatus) (\s a -> s {status = a} :: StackSetSummary)
{-# DEPRECATED sssStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Most recent time when CloudFormation performed a drift detection operation on the stack set. This value will be @NULL@ for any stack set on which drift detection has not yet been performed.
--
-- /Note:/ Consider using 'lastDriftCheckTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssLastDriftCheckTimestamp :: Lens.Lens' StackSetSummary (Lude.Maybe Lude.DateTime)
sssLastDriftCheckTimestamp = Lens.lens (lastDriftCheckTimestamp :: StackSetSummary -> Lude.Maybe Lude.DateTime) (\s a -> s {lastDriftCheckTimestamp = a} :: StackSetSummary)
{-# DEPRECATED sssLastDriftCheckTimestamp "Use generic-lens or generic-optics with 'lastDriftCheckTimestamp' instead." #-}

-- | [@Service-managed@ permissions] Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to a target organizational unit (OU).
--
-- /Note:/ Consider using 'autoDeployment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssAutoDeployment :: Lens.Lens' StackSetSummary (Lude.Maybe AutoDeployment)
sssAutoDeployment = Lens.lens (autoDeployment :: StackSetSummary -> Lude.Maybe AutoDeployment) (\s a -> s {autoDeployment = a} :: StackSetSummary)
{-# DEPRECATED sssAutoDeployment "Use generic-lens or generic-optics with 'autoDeployment' instead." #-}

-- | Status of the stack set's actual configuration compared to its expected template and parameter configuration. A stack set is considered to have drifted if one or more of its stack instances have drifted from their expected template and parameter configuration.
--
--
--     * @DRIFTED@ : One or more of the stack instances belonging to the stack set stack differs from the expected template and parameter configuration. A stack instance is considered to have drifted if one or more of the resources in the associated stack have drifted.
--
--
--     * @NOT_CHECKED@ : AWS CloudFormation has not checked the stack set for drift.
--
--
--     * @IN_SYNC@ : All of the stack instances belonging to the stack set stack match from the expected template and parameter configuration.
--
--
--     * @UNKNOWN@ : This value is reserved for future use.
--
--
--
-- /Note:/ Consider using 'driftStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssDriftStatus :: Lens.Lens' StackSetSummary (Lude.Maybe StackDriftStatus)
sssDriftStatus = Lens.lens (driftStatus :: StackSetSummary -> Lude.Maybe StackDriftStatus) (\s a -> s {driftStatus = a} :: StackSetSummary)
{-# DEPRECATED sssDriftStatus "Use generic-lens or generic-optics with 'driftStatus' instead." #-}

-- | Describes how the IAM roles required for stack set operations are created.
--
--
--     * With @self-managed@ permissions, you must create the administrator and execution roles required to deploy to target accounts. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions> .
--
--
--     * With @service-managed@ permissions, StackSets automatically creates the IAM roles required to deploy to accounts managed by AWS Organizations. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions> .
--
--
--
-- /Note:/ Consider using 'permissionModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssPermissionModel :: Lens.Lens' StackSetSummary (Lude.Maybe PermissionModels)
sssPermissionModel = Lens.lens (permissionModel :: StackSetSummary -> Lude.Maybe PermissionModels) (\s a -> s {permissionModel = a} :: StackSetSummary)
{-# DEPRECATED sssPermissionModel "Use generic-lens or generic-optics with 'permissionModel' instead." #-}

-- | The name of the stack set.
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssStackSetName :: Lens.Lens' StackSetSummary (Lude.Maybe Lude.Text)
sssStackSetName = Lens.lens (stackSetName :: StackSetSummary -> Lude.Maybe Lude.Text) (\s a -> s {stackSetName = a} :: StackSetSummary)
{-# DEPRECATED sssStackSetName "Use generic-lens or generic-optics with 'stackSetName' instead." #-}

-- | A description of the stack set that you specify when the stack set is created or updated.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssDescription :: Lens.Lens' StackSetSummary (Lude.Maybe Lude.Text)
sssDescription = Lens.lens (description :: StackSetSummary -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: StackSetSummary)
{-# DEPRECATED sssDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ID of the stack set.
--
-- /Note:/ Consider using 'stackSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssStackSetId :: Lens.Lens' StackSetSummary (Lude.Maybe Lude.Text)
sssStackSetId = Lens.lens (stackSetId :: StackSetSummary -> Lude.Maybe Lude.Text) (\s a -> s {stackSetId = a} :: StackSetSummary)
{-# DEPRECATED sssStackSetId "Use generic-lens or generic-optics with 'stackSetId' instead." #-}

instance Lude.FromXML StackSetSummary where
  parseXML x =
    StackSetSummary'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "LastDriftCheckTimestamp")
      Lude.<*> (x Lude..@? "AutoDeployment")
      Lude.<*> (x Lude..@? "DriftStatus")
      Lude.<*> (x Lude..@? "PermissionModel")
      Lude.<*> (x Lude..@? "StackSetName")
      Lude.<*> (x Lude..@? "Description")
      Lude.<*> (x Lude..@? "StackSetId")
