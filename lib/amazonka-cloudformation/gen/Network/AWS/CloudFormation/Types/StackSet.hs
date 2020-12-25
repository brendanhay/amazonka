{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSet
  ( StackSet (..),

    -- * Smart constructor
    mkStackSet,

    -- * Lenses
    ssAdministrationRoleARN,
    ssAutoDeployment,
    ssCapabilities,
    ssDescription,
    ssExecutionRoleName,
    ssOrganizationalUnitIds,
    ssParameters,
    ssPermissionModel,
    ssStackSetARN,
    ssStackSetDriftDetectionDetails,
    ssStackSetId,
    ssStackSetName,
    ssStatus,
    ssTags,
    ssTemplateBody,
  )
where

import qualified Network.AWS.CloudFormation.Types.AdministrationRoleARN as Types
import qualified Network.AWS.CloudFormation.Types.AutoDeployment as Types
import qualified Network.AWS.CloudFormation.Types.Capability as Types
import qualified Network.AWS.CloudFormation.Types.Description as Types
import qualified Network.AWS.CloudFormation.Types.ExecutionRoleName as Types
import qualified Network.AWS.CloudFormation.Types.OrganizationalUnitId as Types
import qualified Network.AWS.CloudFormation.Types.Parameter as Types
import qualified Network.AWS.CloudFormation.Types.PermissionModels as Types
import qualified Network.AWS.CloudFormation.Types.StackSetARN as Types
import qualified Network.AWS.CloudFormation.Types.StackSetDriftDetectionDetails as Types
import qualified Network.AWS.CloudFormation.Types.StackSetId as Types
import qualified Network.AWS.CloudFormation.Types.StackSetName as Types
import qualified Network.AWS.CloudFormation.Types.StackSetStatus as Types
import qualified Network.AWS.CloudFormation.Types.Tag as Types
import qualified Network.AWS.CloudFormation.Types.TemplateBody as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A structure that contains information about a stack set. A stack set enables you to provision stacks into AWS accounts and across Regions by using a single CloudFormation template. In the stack set, you specify the template to use, as well as any parameters and capabilities that the template requires.
--
-- /See:/ 'mkStackSet' smart constructor.
data StackSet = StackSet'
  { -- | The Amazon Resource Number (ARN) of the IAM role used to create or update the stack set.
    --
    -- Use customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Prerequisites: Granting Permissions for Stack Set Operations> in the /AWS CloudFormation User Guide/ .
    administrationRoleARN :: Core.Maybe Types.AdministrationRoleARN,
    -- | [@Service-managed@ permissions] Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to a target organization or organizational unit (OU).
    autoDeployment :: Core.Maybe Types.AutoDeployment,
    -- | The capabilities that are allowed in the stack set. Some stack set templates might include resources that can affect permissions in your AWS account—for example, by creating new AWS Identity and Access Management (IAM) users. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates.>
    capabilities :: Core.Maybe [Types.Capability],
    -- | A description of the stack set that you specify when the stack set is created or updated.
    description :: Core.Maybe Types.Description,
    -- | The name of the IAM execution role used to create or update the stack set.
    --
    -- Use customized execution roles to control which stack resources users and groups can include in their stack sets.
    executionRoleName :: Core.Maybe Types.ExecutionRoleName,
    -- | [@Service-managed@ permissions] The organization root ID or organizational unit (OU) IDs that you specified for <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets> .
    organizationalUnitIds :: Core.Maybe [Types.OrganizationalUnitId],
    -- | A list of input parameters for a stack set.
    parameters :: Core.Maybe [Types.Parameter],
    -- | Describes how the IAM roles required for stack set operations are created.
    --
    --
    --     * With @self-managed@ permissions, you must create the administrator and execution roles required to deploy to target accounts. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions> .
    --
    --
    --     * With @service-managed@ permissions, StackSets automatically creates the IAM roles required to deploy to accounts managed by AWS Organizations. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions> .
    permissionModel :: Core.Maybe Types.PermissionModels,
    -- | The Amazon Resource Number (ARN) of the stack set.
    stackSetARN :: Core.Maybe Types.StackSetARN,
    -- | Detailed information about the drift status of the stack set.
    --
    -- For stack sets, contains information about the last /completed/ drift operation performed on the stack set. Information about drift operations currently in progress is not included.
    stackSetDriftDetectionDetails :: Core.Maybe Types.StackSetDriftDetectionDetails,
    -- | The ID of the stack set.
    stackSetId :: Core.Maybe Types.StackSetId,
    -- | The name that's associated with the stack set.
    stackSetName :: Core.Maybe Types.StackSetName,
    -- | The status of the stack set.
    status :: Core.Maybe Types.StackSetStatus,
    -- | A list of tags that specify information about the stack set. A maximum number of 50 tags can be specified.
    tags :: Core.Maybe [Types.Tag],
    -- | The structure that contains the body of the template that was used to create or update the stack set.
    templateBody :: Core.Maybe Types.TemplateBody
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StackSet' value with any optional fields omitted.
mkStackSet ::
  StackSet
mkStackSet =
  StackSet'
    { administrationRoleARN = Core.Nothing,
      autoDeployment = Core.Nothing,
      capabilities = Core.Nothing,
      description = Core.Nothing,
      executionRoleName = Core.Nothing,
      organizationalUnitIds = Core.Nothing,
      parameters = Core.Nothing,
      permissionModel = Core.Nothing,
      stackSetARN = Core.Nothing,
      stackSetDriftDetectionDetails = Core.Nothing,
      stackSetId = Core.Nothing,
      stackSetName = Core.Nothing,
      status = Core.Nothing,
      tags = Core.Nothing,
      templateBody = Core.Nothing
    }

-- | The Amazon Resource Number (ARN) of the IAM role used to create or update the stack set.
--
-- Use customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Prerequisites: Granting Permissions for Stack Set Operations> in the /AWS CloudFormation User Guide/ .
--
-- /Note:/ Consider using 'administrationRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssAdministrationRoleARN :: Lens.Lens' StackSet (Core.Maybe Types.AdministrationRoleARN)
ssAdministrationRoleARN = Lens.field @"administrationRoleARN"
{-# DEPRECATED ssAdministrationRoleARN "Use generic-lens or generic-optics with 'administrationRoleARN' instead." #-}

-- | [@Service-managed@ permissions] Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to a target organization or organizational unit (OU).
--
-- /Note:/ Consider using 'autoDeployment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssAutoDeployment :: Lens.Lens' StackSet (Core.Maybe Types.AutoDeployment)
ssAutoDeployment = Lens.field @"autoDeployment"
{-# DEPRECATED ssAutoDeployment "Use generic-lens or generic-optics with 'autoDeployment' instead." #-}

-- | The capabilities that are allowed in the stack set. Some stack set templates might include resources that can affect permissions in your AWS account—for example, by creating new AWS Identity and Access Management (IAM) users. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates.>
--
-- /Note:/ Consider using 'capabilities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssCapabilities :: Lens.Lens' StackSet (Core.Maybe [Types.Capability])
ssCapabilities = Lens.field @"capabilities"
{-# DEPRECATED ssCapabilities "Use generic-lens or generic-optics with 'capabilities' instead." #-}

-- | A description of the stack set that you specify when the stack set is created or updated.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDescription :: Lens.Lens' StackSet (Core.Maybe Types.Description)
ssDescription = Lens.field @"description"
{-# DEPRECATED ssDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the IAM execution role used to create or update the stack set.
--
-- Use customized execution roles to control which stack resources users and groups can include in their stack sets.
--
-- /Note:/ Consider using 'executionRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssExecutionRoleName :: Lens.Lens' StackSet (Core.Maybe Types.ExecutionRoleName)
ssExecutionRoleName = Lens.field @"executionRoleName"
{-# DEPRECATED ssExecutionRoleName "Use generic-lens or generic-optics with 'executionRoleName' instead." #-}

-- | [@Service-managed@ permissions] The organization root ID or organizational unit (OU) IDs that you specified for <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets> .
--
-- /Note:/ Consider using 'organizationalUnitIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssOrganizationalUnitIds :: Lens.Lens' StackSet (Core.Maybe [Types.OrganizationalUnitId])
ssOrganizationalUnitIds = Lens.field @"organizationalUnitIds"
{-# DEPRECATED ssOrganizationalUnitIds "Use generic-lens or generic-optics with 'organizationalUnitIds' instead." #-}

-- | A list of input parameters for a stack set.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssParameters :: Lens.Lens' StackSet (Core.Maybe [Types.Parameter])
ssParameters = Lens.field @"parameters"
{-# DEPRECATED ssParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

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
ssPermissionModel :: Lens.Lens' StackSet (Core.Maybe Types.PermissionModels)
ssPermissionModel = Lens.field @"permissionModel"
{-# DEPRECATED ssPermissionModel "Use generic-lens or generic-optics with 'permissionModel' instead." #-}

-- | The Amazon Resource Number (ARN) of the stack set.
--
-- /Note:/ Consider using 'stackSetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStackSetARN :: Lens.Lens' StackSet (Core.Maybe Types.StackSetARN)
ssStackSetARN = Lens.field @"stackSetARN"
{-# DEPRECATED ssStackSetARN "Use generic-lens or generic-optics with 'stackSetARN' instead." #-}

-- | Detailed information about the drift status of the stack set.
--
-- For stack sets, contains information about the last /completed/ drift operation performed on the stack set. Information about drift operations currently in progress is not included.
--
-- /Note:/ Consider using 'stackSetDriftDetectionDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStackSetDriftDetectionDetails :: Lens.Lens' StackSet (Core.Maybe Types.StackSetDriftDetectionDetails)
ssStackSetDriftDetectionDetails = Lens.field @"stackSetDriftDetectionDetails"
{-# DEPRECATED ssStackSetDriftDetectionDetails "Use generic-lens or generic-optics with 'stackSetDriftDetectionDetails' instead." #-}

-- | The ID of the stack set.
--
-- /Note:/ Consider using 'stackSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStackSetId :: Lens.Lens' StackSet (Core.Maybe Types.StackSetId)
ssStackSetId = Lens.field @"stackSetId"
{-# DEPRECATED ssStackSetId "Use generic-lens or generic-optics with 'stackSetId' instead." #-}

-- | The name that's associated with the stack set.
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStackSetName :: Lens.Lens' StackSet (Core.Maybe Types.StackSetName)
ssStackSetName = Lens.field @"stackSetName"
{-# DEPRECATED ssStackSetName "Use generic-lens or generic-optics with 'stackSetName' instead." #-}

-- | The status of the stack set.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStatus :: Lens.Lens' StackSet (Core.Maybe Types.StackSetStatus)
ssStatus = Lens.field @"status"
{-# DEPRECATED ssStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A list of tags that specify information about the stack set. A maximum number of 50 tags can be specified.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssTags :: Lens.Lens' StackSet (Core.Maybe [Types.Tag])
ssTags = Lens.field @"tags"
{-# DEPRECATED ssTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The structure that contains the body of the template that was used to create or update the stack set.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssTemplateBody :: Lens.Lens' StackSet (Core.Maybe Types.TemplateBody)
ssTemplateBody = Lens.field @"templateBody"
{-# DEPRECATED ssTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

instance Core.FromXML StackSet where
  parseXML x =
    StackSet'
      Core.<$> (x Core..@? "AdministrationRoleARN")
      Core.<*> (x Core..@? "AutoDeployment")
      Core.<*> (x Core..@? "Capabilities" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "ExecutionRoleName")
      Core.<*> ( x Core..@? "OrganizationalUnitIds"
                   Core..<@> Core.parseXMLList "member"
               )
      Core.<*> (x Core..@? "Parameters" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "PermissionModel")
      Core.<*> (x Core..@? "StackSetARN")
      Core.<*> (x Core..@? "StackSetDriftDetectionDetails")
      Core.<*> (x Core..@? "StackSetId")
      Core.<*> (x Core..@? "StackSetName")
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "Tags" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "TemplateBody")
