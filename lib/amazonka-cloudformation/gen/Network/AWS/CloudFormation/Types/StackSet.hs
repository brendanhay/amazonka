{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.StackSet
  ( StackSet (..)
  -- * Smart constructor
  , mkStackSet
  -- * Lenses
  , ssAdministrationRoleARN
  , ssAutoDeployment
  , ssCapabilities
  , ssDescription
  , ssExecutionRoleName
  , ssOrganizationalUnitIds
  , ssParameters
  , ssPermissionModel
  , ssStackSetARN
  , ssStackSetDriftDetectionDetails
  , ssStackSetId
  , ssStackSetName
  , ssStatus
  , ssTags
  , ssTemplateBody
  ) where

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
  { administrationRoleARN :: Core.Maybe Types.AdministrationRoleARN
    -- ^ The Amazon Resource Number (ARN) of the IAM role used to create or update the stack set.
--
-- Use customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Prerequisites: Granting Permissions for Stack Set Operations> in the /AWS CloudFormation User Guide/ .
  , autoDeployment :: Core.Maybe Types.AutoDeployment
    -- ^ [@Service-managed@ permissions] Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to a target organization or organizational unit (OU).
  , capabilities :: Core.Maybe [Types.Capability]
    -- ^ The capabilities that are allowed in the stack set. Some stack set templates might include resources that can affect permissions in your AWS account—for example, by creating new AWS Identity and Access Management (IAM) users. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates.> 
  , description :: Core.Maybe Types.Description
    -- ^ A description of the stack set that you specify when the stack set is created or updated.
  , executionRoleName :: Core.Maybe Types.ExecutionRoleName
    -- ^ The name of the IAM execution role used to create or update the stack set. 
--
-- Use customized execution roles to control which stack resources users and groups can include in their stack sets. 
  , organizationalUnitIds :: Core.Maybe [Types.OrganizationalUnitId]
    -- ^ [@Service-managed@ permissions] The organization root ID or organizational unit (OU) IDs that you specified for <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets> .
  , parameters :: Core.Maybe [Types.Parameter]
    -- ^ A list of input parameters for a stack set.
  , permissionModel :: Core.Maybe Types.PermissionModels
    -- ^ Describes how the IAM roles required for stack set operations are created.
--
--
--     * With @self-managed@ permissions, you must create the administrator and execution roles required to deploy to target accounts. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions> .
--
--
--     * With @service-managed@ permissions, StackSets automatically creates the IAM roles required to deploy to accounts managed by AWS Organizations. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions> .
--
--
  , stackSetARN :: Core.Maybe Types.StackSetARN
    -- ^ The Amazon Resource Number (ARN) of the stack set.
  , stackSetDriftDetectionDetails :: Core.Maybe Types.StackSetDriftDetectionDetails
    -- ^ Detailed information about the drift status of the stack set.
--
-- For stack sets, contains information about the last /completed/ drift operation performed on the stack set. Information about drift operations currently in progress is not included.
  , stackSetId :: Core.Maybe Types.StackSetId
    -- ^ The ID of the stack set.
  , stackSetName :: Core.Maybe Types.StackSetName
    -- ^ The name that's associated with the stack set.
  , status :: Core.Maybe Types.StackSetStatus
    -- ^ The status of the stack set.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of tags that specify information about the stack set. A maximum number of 50 tags can be specified.
  , templateBody :: Core.Maybe Types.TemplateBody
    -- ^ The structure that contains the body of the template that was used to create or update the stack set.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StackSet' value with any optional fields omitted.
mkStackSet
    :: StackSet
mkStackSet
  = StackSet'{administrationRoleARN = Core.Nothing,
              autoDeployment = Core.Nothing, capabilities = Core.Nothing,
              description = Core.Nothing, executionRoleName = Core.Nothing,
              organizationalUnitIds = Core.Nothing, parameters = Core.Nothing,
              permissionModel = Core.Nothing, stackSetARN = Core.Nothing,
              stackSetDriftDetectionDetails = Core.Nothing,
              stackSetId = Core.Nothing, stackSetName = Core.Nothing,
              status = Core.Nothing, tags = Core.Nothing,
              templateBody = Core.Nothing}

-- | The Amazon Resource Number (ARN) of the IAM role used to create or update the stack set.
--
-- Use customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Prerequisites: Granting Permissions for Stack Set Operations> in the /AWS CloudFormation User Guide/ .
--
-- /Note:/ Consider using 'administrationRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssAdministrationRoleARN :: Lens.Lens' StackSet (Core.Maybe Types.AdministrationRoleARN)
ssAdministrationRoleARN = Lens.field @"administrationRoleARN"
{-# INLINEABLE ssAdministrationRoleARN #-}
{-# DEPRECATED administrationRoleARN "Use generic-lens or generic-optics with 'administrationRoleARN' instead"  #-}

-- | [@Service-managed@ permissions] Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to a target organization or organizational unit (OU).
--
-- /Note:/ Consider using 'autoDeployment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssAutoDeployment :: Lens.Lens' StackSet (Core.Maybe Types.AutoDeployment)
ssAutoDeployment = Lens.field @"autoDeployment"
{-# INLINEABLE ssAutoDeployment #-}
{-# DEPRECATED autoDeployment "Use generic-lens or generic-optics with 'autoDeployment' instead"  #-}

-- | The capabilities that are allowed in the stack set. Some stack set templates might include resources that can affect permissions in your AWS account—for example, by creating new AWS Identity and Access Management (IAM) users. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates.> 
--
-- /Note:/ Consider using 'capabilities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssCapabilities :: Lens.Lens' StackSet (Core.Maybe [Types.Capability])
ssCapabilities = Lens.field @"capabilities"
{-# INLINEABLE ssCapabilities #-}
{-# DEPRECATED capabilities "Use generic-lens or generic-optics with 'capabilities' instead"  #-}

-- | A description of the stack set that you specify when the stack set is created or updated.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDescription :: Lens.Lens' StackSet (Core.Maybe Types.Description)
ssDescription = Lens.field @"description"
{-# INLINEABLE ssDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The name of the IAM execution role used to create or update the stack set. 
--
-- Use customized execution roles to control which stack resources users and groups can include in their stack sets. 
--
-- /Note:/ Consider using 'executionRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssExecutionRoleName :: Lens.Lens' StackSet (Core.Maybe Types.ExecutionRoleName)
ssExecutionRoleName = Lens.field @"executionRoleName"
{-# INLINEABLE ssExecutionRoleName #-}
{-# DEPRECATED executionRoleName "Use generic-lens or generic-optics with 'executionRoleName' instead"  #-}

-- | [@Service-managed@ permissions] The organization root ID or organizational unit (OU) IDs that you specified for <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets> .
--
-- /Note:/ Consider using 'organizationalUnitIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssOrganizationalUnitIds :: Lens.Lens' StackSet (Core.Maybe [Types.OrganizationalUnitId])
ssOrganizationalUnitIds = Lens.field @"organizationalUnitIds"
{-# INLINEABLE ssOrganizationalUnitIds #-}
{-# DEPRECATED organizationalUnitIds "Use generic-lens or generic-optics with 'organizationalUnitIds' instead"  #-}

-- | A list of input parameters for a stack set.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssParameters :: Lens.Lens' StackSet (Core.Maybe [Types.Parameter])
ssParameters = Lens.field @"parameters"
{-# INLINEABLE ssParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

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
{-# INLINEABLE ssPermissionModel #-}
{-# DEPRECATED permissionModel "Use generic-lens or generic-optics with 'permissionModel' instead"  #-}

-- | The Amazon Resource Number (ARN) of the stack set.
--
-- /Note:/ Consider using 'stackSetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStackSetARN :: Lens.Lens' StackSet (Core.Maybe Types.StackSetARN)
ssStackSetARN = Lens.field @"stackSetARN"
{-# INLINEABLE ssStackSetARN #-}
{-# DEPRECATED stackSetARN "Use generic-lens or generic-optics with 'stackSetARN' instead"  #-}

-- | Detailed information about the drift status of the stack set.
--
-- For stack sets, contains information about the last /completed/ drift operation performed on the stack set. Information about drift operations currently in progress is not included.
--
-- /Note:/ Consider using 'stackSetDriftDetectionDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStackSetDriftDetectionDetails :: Lens.Lens' StackSet (Core.Maybe Types.StackSetDriftDetectionDetails)
ssStackSetDriftDetectionDetails = Lens.field @"stackSetDriftDetectionDetails"
{-# INLINEABLE ssStackSetDriftDetectionDetails #-}
{-# DEPRECATED stackSetDriftDetectionDetails "Use generic-lens or generic-optics with 'stackSetDriftDetectionDetails' instead"  #-}

-- | The ID of the stack set.
--
-- /Note:/ Consider using 'stackSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStackSetId :: Lens.Lens' StackSet (Core.Maybe Types.StackSetId)
ssStackSetId = Lens.field @"stackSetId"
{-# INLINEABLE ssStackSetId #-}
{-# DEPRECATED stackSetId "Use generic-lens or generic-optics with 'stackSetId' instead"  #-}

-- | The name that's associated with the stack set.
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStackSetName :: Lens.Lens' StackSet (Core.Maybe Types.StackSetName)
ssStackSetName = Lens.field @"stackSetName"
{-# INLINEABLE ssStackSetName #-}
{-# DEPRECATED stackSetName "Use generic-lens or generic-optics with 'stackSetName' instead"  #-}

-- | The status of the stack set.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStatus :: Lens.Lens' StackSet (Core.Maybe Types.StackSetStatus)
ssStatus = Lens.field @"status"
{-# INLINEABLE ssStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | A list of tags that specify information about the stack set. A maximum number of 50 tags can be specified.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssTags :: Lens.Lens' StackSet (Core.Maybe [Types.Tag])
ssTags = Lens.field @"tags"
{-# INLINEABLE ssTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The structure that contains the body of the template that was used to create or update the stack set.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssTemplateBody :: Lens.Lens' StackSet (Core.Maybe Types.TemplateBody)
ssTemplateBody = Lens.field @"templateBody"
{-# INLINEABLE ssTemplateBody #-}
{-# DEPRECATED templateBody "Use generic-lens or generic-optics with 'templateBody' instead"  #-}

instance Core.FromXML StackSet where
        parseXML x
          = StackSet' Core.<$>
              (x Core..@? "AdministrationRoleARN") Core.<*>
                x Core..@? "AutoDeployment"
                Core.<*>
                x Core..@? "Capabilities" Core..<@> Core.parseXMLList "member"
                Core.<*> x Core..@? "Description"
                Core.<*> x Core..@? "ExecutionRoleName"
                Core.<*>
                x Core..@? "OrganizationalUnitIds" Core..<@>
                  Core.parseXMLList "member"
                Core.<*>
                x Core..@? "Parameters" Core..<@> Core.parseXMLList "member"
                Core.<*> x Core..@? "PermissionModel"
                Core.<*> x Core..@? "StackSetARN"
                Core.<*> x Core..@? "StackSetDriftDetectionDetails"
                Core.<*> x Core..@? "StackSetId"
                Core.<*> x Core..@? "StackSetName"
                Core.<*> x Core..@? "Status"
                Core.<*> x Core..@? "Tags" Core..<@> Core.parseXMLList "member"
                Core.<*> x Core..@? "TemplateBody"
