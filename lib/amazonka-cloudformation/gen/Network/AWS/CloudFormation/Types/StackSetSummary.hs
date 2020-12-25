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
    sssAutoDeployment,
    sssDescription,
    sssDriftStatus,
    sssLastDriftCheckTimestamp,
    sssPermissionModel,
    sssStackSetId,
    sssStackSetName,
    sssStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types.AutoDeployment as Types
import qualified Network.AWS.CloudFormation.Types.Description as Types
import qualified Network.AWS.CloudFormation.Types.PermissionModels as Types
import qualified Network.AWS.CloudFormation.Types.StackDriftStatus as Types
import qualified Network.AWS.CloudFormation.Types.StackSetId as Types
import qualified Network.AWS.CloudFormation.Types.StackSetName as Types
import qualified Network.AWS.CloudFormation.Types.StackSetStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The structures that contain summary information about the specified stack set.
--
-- /See:/ 'mkStackSetSummary' smart constructor.
data StackSetSummary = StackSetSummary'
  { -- | [@Service-managed@ permissions] Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to a target organizational unit (OU).
    autoDeployment :: Core.Maybe Types.AutoDeployment,
    -- | A description of the stack set that you specify when the stack set is created or updated.
    description :: Core.Maybe Types.Description,
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
    driftStatus :: Core.Maybe Types.StackDriftStatus,
    -- | Most recent time when CloudFormation performed a drift detection operation on the stack set. This value will be @NULL@ for any stack set on which drift detection has not yet been performed.
    lastDriftCheckTimestamp :: Core.Maybe Core.UTCTime,
    -- | Describes how the IAM roles required for stack set operations are created.
    --
    --
    --     * With @self-managed@ permissions, you must create the administrator and execution roles required to deploy to target accounts. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions> .
    --
    --
    --     * With @service-managed@ permissions, StackSets automatically creates the IAM roles required to deploy to accounts managed by AWS Organizations. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions> .
    permissionModel :: Core.Maybe Types.PermissionModels,
    -- | The ID of the stack set.
    stackSetId :: Core.Maybe Types.StackSetId,
    -- | The name of the stack set.
    stackSetName :: Core.Maybe Types.StackSetName,
    -- | The status of the stack set.
    status :: Core.Maybe Types.StackSetStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StackSetSummary' value with any optional fields omitted.
mkStackSetSummary ::
  StackSetSummary
mkStackSetSummary =
  StackSetSummary'
    { autoDeployment = Core.Nothing,
      description = Core.Nothing,
      driftStatus = Core.Nothing,
      lastDriftCheckTimestamp = Core.Nothing,
      permissionModel = Core.Nothing,
      stackSetId = Core.Nothing,
      stackSetName = Core.Nothing,
      status = Core.Nothing
    }

-- | [@Service-managed@ permissions] Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to a target organizational unit (OU).
--
-- /Note:/ Consider using 'autoDeployment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssAutoDeployment :: Lens.Lens' StackSetSummary (Core.Maybe Types.AutoDeployment)
sssAutoDeployment = Lens.field @"autoDeployment"
{-# DEPRECATED sssAutoDeployment "Use generic-lens or generic-optics with 'autoDeployment' instead." #-}

-- | A description of the stack set that you specify when the stack set is created or updated.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssDescription :: Lens.Lens' StackSetSummary (Core.Maybe Types.Description)
sssDescription = Lens.field @"description"
{-# DEPRECATED sssDescription "Use generic-lens or generic-optics with 'description' instead." #-}

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
sssDriftStatus :: Lens.Lens' StackSetSummary (Core.Maybe Types.StackDriftStatus)
sssDriftStatus = Lens.field @"driftStatus"
{-# DEPRECATED sssDriftStatus "Use generic-lens or generic-optics with 'driftStatus' instead." #-}

-- | Most recent time when CloudFormation performed a drift detection operation on the stack set. This value will be @NULL@ for any stack set on which drift detection has not yet been performed.
--
-- /Note:/ Consider using 'lastDriftCheckTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssLastDriftCheckTimestamp :: Lens.Lens' StackSetSummary (Core.Maybe Core.UTCTime)
sssLastDriftCheckTimestamp = Lens.field @"lastDriftCheckTimestamp"
{-# DEPRECATED sssLastDriftCheckTimestamp "Use generic-lens or generic-optics with 'lastDriftCheckTimestamp' instead." #-}

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
sssPermissionModel :: Lens.Lens' StackSetSummary (Core.Maybe Types.PermissionModels)
sssPermissionModel = Lens.field @"permissionModel"
{-# DEPRECATED sssPermissionModel "Use generic-lens or generic-optics with 'permissionModel' instead." #-}

-- | The ID of the stack set.
--
-- /Note:/ Consider using 'stackSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssStackSetId :: Lens.Lens' StackSetSummary (Core.Maybe Types.StackSetId)
sssStackSetId = Lens.field @"stackSetId"
{-# DEPRECATED sssStackSetId "Use generic-lens or generic-optics with 'stackSetId' instead." #-}

-- | The name of the stack set.
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssStackSetName :: Lens.Lens' StackSetSummary (Core.Maybe Types.StackSetName)
sssStackSetName = Lens.field @"stackSetName"
{-# DEPRECATED sssStackSetName "Use generic-lens or generic-optics with 'stackSetName' instead." #-}

-- | The status of the stack set.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssStatus :: Lens.Lens' StackSetSummary (Core.Maybe Types.StackSetStatus)
sssStatus = Lens.field @"status"
{-# DEPRECATED sssStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromXML StackSetSummary where
  parseXML x =
    StackSetSummary'
      Core.<$> (x Core..@? "AutoDeployment")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "DriftStatus")
      Core.<*> (x Core..@? "LastDriftCheckTimestamp")
      Core.<*> (x Core..@? "PermissionModel")
      Core.<*> (x Core..@? "StackSetId")
      Core.<*> (x Core..@? "StackSetName")
      Core.<*> (x Core..@? "Status")
