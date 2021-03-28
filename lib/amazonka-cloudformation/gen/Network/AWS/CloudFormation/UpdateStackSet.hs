{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.UpdateStackSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the stack set, and associated stack instances in the specified accounts and Regions.
--
-- Even if the stack set operation created by updating the stack set fails (completely or partially, below or above a specified failure tolerance), the stack set is updated with your changes. Subsequent 'CreateStackInstances' calls on the specified stack set use the updated stack set.
module Network.AWS.CloudFormation.UpdateStackSet
    (
    -- * Creating a request
      UpdateStackSet (..)
    , mkUpdateStackSet
    -- ** Request lenses
    , ussStackSetName
    , ussAccounts
    , ussAdministrationRoleARN
    , ussAutoDeployment
    , ussCapabilities
    , ussDeploymentTargets
    , ussDescription
    , ussExecutionRoleName
    , ussOperationId
    , ussOperationPreferences
    , ussParameters
    , ussPermissionModel
    , ussRegions
    , ussTags
    , ussTemplateBody
    , ussTemplateURL
    , ussUsePreviousTemplate

    -- * Destructuring the response
    , UpdateStackSetResponse (..)
    , mkUpdateStackSetResponse
    -- ** Response lenses
    , ussrrsOperationId
    , ussrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateStackSet' smart constructor.
data UpdateStackSet = UpdateStackSet'
  { stackSetName :: Types.StackSetName
    -- ^ The name or unique ID of the stack set that you want to update.
  , accounts :: Core.Maybe [Types.Account]
    -- ^ [@Self-managed@ permissions] The accounts in which to update associated stack instances. If you specify accounts, you must also specify the Regions in which to update stack set instances.
--
-- To update /all/ the stack instances associated with this stack set, do not specify the @Accounts@ or @Regions@ properties.
-- If the stack set update includes changes to the template (that is, if the @TemplateBody@ or @TemplateURL@ properties are specified), or the @Parameters@ property, AWS CloudFormation marks all stack instances with a status of @OUTDATED@ prior to updating the stack instances in the specified accounts and Regions. If the stack set update does not include changes to the template or parameters, AWS CloudFormation updates the stack instances in the specified accounts and Regions, while leaving all other stack instances with their existing stack instance status. 
  , administrationRoleARN :: Core.Maybe Types.AdministrationRoleARN
    -- ^ The Amazon Resource Number (ARN) of the IAM role to use to update this stack set.
--
-- Specify an IAM role only if you are using customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Granting Permissions for Stack Set Operations> in the /AWS CloudFormation User Guide/ .
-- If you specified a customized administrator role when you created the stack set, you must specify a customized administrator role, even if it is the same customized administrator role used with this stack set previously.
  , autoDeployment :: Core.Maybe Types.AutoDeployment
    -- ^ [@Service-managed@ permissions] Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to a target organization or organizational unit (OU).
--
-- If you specify @AutoDeployment@ , do not specify @DeploymentTargets@ or @Regions@ .
  , capabilities :: Core.Maybe [Types.Capability]
    -- ^ In some cases, you must explicitly acknowledge that your stack template contains certain capabilities in order for AWS CloudFormation to update the stack set and its associated stack instances.
--
--
--     * @CAPABILITY_IAM@ and @CAPABILITY_NAMED_IAM@ 
-- Some stack templates might include resources that can affect permissions in your AWS account; for example, by creating new AWS Identity and Access Management (IAM) users. For those stacks sets, you must explicitly acknowledge this by specifying one of these capabilities.
-- The following IAM resources require you to specify either the @CAPABILITY_IAM@ or @CAPABILITY_NAMED_IAM@ capability.
--
--     * If you have IAM resources, you can specify either capability. 
--
--
--     * If you have IAM resources with custom names, you /must/ specify @CAPABILITY_NAMED_IAM@ . 
--
--
--     * If you don't specify either of these capabilities, AWS CloudFormation returns an @InsufficientCapabilities@ error.
--
--
-- If your stack template contains these resources, we recommend that you review all permissions associated with them and edit their permissions if necessary.
--
--     * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-accesskey.html AWS::IAM::AccessKey> 
--
--
--     * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-group.html AWS::IAM::Group> 
--
--
--     * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-instanceprofile.html AWS::IAM::InstanceProfile> 
--
--
--     * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-policy.html AWS::IAM::Policy> 
--
--
--     * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-role.html AWS::IAM::Role> 
--
--
--     * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-user.html AWS::IAM::User> 
--
--
--     * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-addusertogroup.html AWS::IAM::UserToGroupAddition> 
--
--
-- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates> .
--
--
--     * @CAPABILITY_AUTO_EXPAND@ 
-- Some templates contain macros. If your stack template contains one or more macros, and you choose to update a stack directly from the processed template, without first reviewing the resulting changes in a change set, you must acknowledge this capability. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-macros.html Using AWS CloudFormation Macros to Perform Custom Processing on Templates> .
-- /Important:/ Stack sets do not currently support macros in stack templates. (This includes the <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/create-reusable-transform-function-snippets-and-add-to-your-template-with-aws-include-transform.html AWS::Include> and <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/transform-aws-serverless.html AWS::Serverless> transforms, which are macros hosted by AWS CloudFormation.) Even if you specify this capability, if you include a macro in your template the stack set operation will fail.
--
--
  , deploymentTargets :: Core.Maybe Types.DeploymentTargets
    -- ^ [@Service-managed@ permissions] The AWS Organizations accounts in which to update associated stack instances.
--
-- To update all the stack instances associated with this stack set, do not specify @DeploymentTargets@ or @Regions@ .
-- If the stack set update includes changes to the template (that is, if @TemplateBody@ or @TemplateURL@ is specified), or the @Parameters@ , AWS CloudFormation marks all stack instances with a status of @OUTDATED@ prior to updating the stack instances in the specified accounts and Regions. If the stack set update does not include changes to the template or parameters, AWS CloudFormation updates the stack instances in the specified accounts and Regions, while leaving all other stack instances with their existing stack instance status.
  , description :: Core.Maybe Types.Description
    -- ^ A brief description of updates that you are making.
  , executionRoleName :: Core.Maybe Types.ExecutionRoleName
    -- ^ The name of the IAM execution role to use to update the stack set. If you do not specify an execution role, AWS CloudFormation uses the @AWSCloudFormationStackSetExecutionRole@ role for the stack set operation.
--
-- Specify an IAM role only if you are using customized execution roles to control which stack resources users and groups can include in their stack sets. 
-- If you specify a customized execution role, AWS CloudFormation uses that role to update the stack. If you do not specify a customized execution role, AWS CloudFormation performs the update using the role previously associated with the stack set, so long as you have permissions to perform operations on the stack set.
  , operationId :: Core.Maybe Types.ClientRequestToken
    -- ^ The unique ID for this stack set operation. 
--
-- The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You might retry stack set operation requests to ensure that AWS CloudFormation successfully received them.
-- If you don't specify an operation ID, AWS CloudFormation generates one automatically.
-- Repeating this stack set operation with a new operation ID retries all stack instances whose status is @OUTDATED@ . 
  , operationPreferences :: Core.Maybe Types.StackSetOperationPreferences
    -- ^ Preferences for how AWS CloudFormation performs this stack set operation.
  , parameters :: Core.Maybe [Types.Parameter]
    -- ^ A list of input parameters for the stack set template. 
  , permissionModel :: Core.Maybe Types.PermissionModels
    -- ^ Describes how the IAM roles required for stack set operations are created. You cannot modify @PermissionModel@ if there are stack instances associated with your stack set.
--
--
--     * With @self-managed@ permissions, you must create the administrator and execution roles required to deploy to target accounts. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions> .
--
--
--     * With @service-managed@ permissions, StackSets automatically creates the IAM roles required to deploy to accounts managed by AWS Organizations. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions> .
--
--
  , regions :: Core.Maybe [Types.Region]
    -- ^ The Regions in which to update associated stack instances. If you specify Regions, you must also specify accounts in which to update stack set instances.
--
-- To update /all/ the stack instances associated with this stack set, do not specify the @Accounts@ or @Regions@ properties.
-- If the stack set update includes changes to the template (that is, if the @TemplateBody@ or @TemplateURL@ properties are specified), or the @Parameters@ property, AWS CloudFormation marks all stack instances with a status of @OUTDATED@ prior to updating the stack instances in the specified accounts and Regions. If the stack set update does not include changes to the template or parameters, AWS CloudFormation updates the stack instances in the specified accounts and Regions, while leaving all other stack instances with their existing stack instance status. 
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The key-value pairs to associate with this stack set and the stacks created from it. AWS CloudFormation also propagates these tags to supported resources that are created in the stacks. You can specify a maximum number of 50 tags.
--
-- If you specify tags for this parameter, those tags replace any list of tags that are currently associated with this stack set. This means:
--
--     * If you don't specify this parameter, AWS CloudFormation doesn't modify the stack's tags. 
--
--
--     * If you specify /any/ tags using this parameter, you must specify /all/ the tags that you want associated with this stack set, even tags you've specifed before (for example, when creating the stack set or during a previous update of the stack set.). Any tags that you don't include in the updated list of tags are removed from the stack set, and therefore from the stacks and resources as well. 
--
--
--     * If you specify an empty value, AWS CloudFormation removes all currently associated tags.
--
--
-- If you specify new tags as part of an @UpdateStackSet@ action, AWS CloudFormation checks to see if you have the required IAM permission to tag resources. If you omit tags that are currently associated with the stack set from the list of tags you specify, AWS CloudFormation assumes that you want to remove those tags from the stack set, and checks to see if you have permission to untag resources. If you don't have the necessary permission(s), the entire @UpdateStackSet@ action fails with an @access denied@ error, and the stack set is not updated.
  , templateBody :: Core.Maybe Types.TemplateBody
    -- ^ The structure that contains the template body, with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters: @TemplateBody@ or @TemplateURL@ —or set @UsePreviousTemplate@ to true.
  , templateURL :: Core.Maybe Types.TemplateURL
    -- ^ The location of the file that contains the template body. The URL must point to a template (maximum size: 460,800 bytes) that is located in an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters: @TemplateBody@ or @TemplateURL@ —or set @UsePreviousTemplate@ to true. 
  , usePreviousTemplate :: Core.Maybe Core.Bool
    -- ^ Use the existing template that's associated with the stack set that you're updating.
--
-- Conditional: You must specify only one of the following parameters: @TemplateBody@ or @TemplateURL@ —or set @UsePreviousTemplate@ to true. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateStackSet' value with any optional fields omitted.
mkUpdateStackSet
    :: Types.StackSetName -- ^ 'stackSetName'
    -> UpdateStackSet
mkUpdateStackSet stackSetName
  = UpdateStackSet'{stackSetName, accounts = Core.Nothing,
                    administrationRoleARN = Core.Nothing,
                    autoDeployment = Core.Nothing, capabilities = Core.Nothing,
                    deploymentTargets = Core.Nothing, description = Core.Nothing,
                    executionRoleName = Core.Nothing, operationId = Core.Nothing,
                    operationPreferences = Core.Nothing, parameters = Core.Nothing,
                    permissionModel = Core.Nothing, regions = Core.Nothing,
                    tags = Core.Nothing, templateBody = Core.Nothing,
                    templateURL = Core.Nothing, usePreviousTemplate = Core.Nothing}

-- | The name or unique ID of the stack set that you want to update.
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussStackSetName :: Lens.Lens' UpdateStackSet Types.StackSetName
ussStackSetName = Lens.field @"stackSetName"
{-# INLINEABLE ussStackSetName #-}
{-# DEPRECATED stackSetName "Use generic-lens or generic-optics with 'stackSetName' instead"  #-}

-- | [@Self-managed@ permissions] The accounts in which to update associated stack instances. If you specify accounts, you must also specify the Regions in which to update stack set instances.
--
-- To update /all/ the stack instances associated with this stack set, do not specify the @Accounts@ or @Regions@ properties.
-- If the stack set update includes changes to the template (that is, if the @TemplateBody@ or @TemplateURL@ properties are specified), or the @Parameters@ property, AWS CloudFormation marks all stack instances with a status of @OUTDATED@ prior to updating the stack instances in the specified accounts and Regions. If the stack set update does not include changes to the template or parameters, AWS CloudFormation updates the stack instances in the specified accounts and Regions, while leaving all other stack instances with their existing stack instance status. 
--
-- /Note:/ Consider using 'accounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussAccounts :: Lens.Lens' UpdateStackSet (Core.Maybe [Types.Account])
ussAccounts = Lens.field @"accounts"
{-# INLINEABLE ussAccounts #-}
{-# DEPRECATED accounts "Use generic-lens or generic-optics with 'accounts' instead"  #-}

-- | The Amazon Resource Number (ARN) of the IAM role to use to update this stack set.
--
-- Specify an IAM role only if you are using customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Granting Permissions for Stack Set Operations> in the /AWS CloudFormation User Guide/ .
-- If you specified a customized administrator role when you created the stack set, you must specify a customized administrator role, even if it is the same customized administrator role used with this stack set previously.
--
-- /Note:/ Consider using 'administrationRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussAdministrationRoleARN :: Lens.Lens' UpdateStackSet (Core.Maybe Types.AdministrationRoleARN)
ussAdministrationRoleARN = Lens.field @"administrationRoleARN"
{-# INLINEABLE ussAdministrationRoleARN #-}
{-# DEPRECATED administrationRoleARN "Use generic-lens or generic-optics with 'administrationRoleARN' instead"  #-}

-- | [@Service-managed@ permissions] Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to a target organization or organizational unit (OU).
--
-- If you specify @AutoDeployment@ , do not specify @DeploymentTargets@ or @Regions@ .
--
-- /Note:/ Consider using 'autoDeployment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussAutoDeployment :: Lens.Lens' UpdateStackSet (Core.Maybe Types.AutoDeployment)
ussAutoDeployment = Lens.field @"autoDeployment"
{-# INLINEABLE ussAutoDeployment #-}
{-# DEPRECATED autoDeployment "Use generic-lens or generic-optics with 'autoDeployment' instead"  #-}

-- | In some cases, you must explicitly acknowledge that your stack template contains certain capabilities in order for AWS CloudFormation to update the stack set and its associated stack instances.
--
--
--     * @CAPABILITY_IAM@ and @CAPABILITY_NAMED_IAM@ 
-- Some stack templates might include resources that can affect permissions in your AWS account; for example, by creating new AWS Identity and Access Management (IAM) users. For those stacks sets, you must explicitly acknowledge this by specifying one of these capabilities.
-- The following IAM resources require you to specify either the @CAPABILITY_IAM@ or @CAPABILITY_NAMED_IAM@ capability.
--
--     * If you have IAM resources, you can specify either capability. 
--
--
--     * If you have IAM resources with custom names, you /must/ specify @CAPABILITY_NAMED_IAM@ . 
--
--
--     * If you don't specify either of these capabilities, AWS CloudFormation returns an @InsufficientCapabilities@ error.
--
--
-- If your stack template contains these resources, we recommend that you review all permissions associated with them and edit their permissions if necessary.
--
--     * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-accesskey.html AWS::IAM::AccessKey> 
--
--
--     * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-group.html AWS::IAM::Group> 
--
--
--     * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-instanceprofile.html AWS::IAM::InstanceProfile> 
--
--
--     * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-policy.html AWS::IAM::Policy> 
--
--
--     * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-role.html AWS::IAM::Role> 
--
--
--     * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-user.html AWS::IAM::User> 
--
--
--     * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-addusertogroup.html AWS::IAM::UserToGroupAddition> 
--
--
-- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates> .
--
--
--     * @CAPABILITY_AUTO_EXPAND@ 
-- Some templates contain macros. If your stack template contains one or more macros, and you choose to update a stack directly from the processed template, without first reviewing the resulting changes in a change set, you must acknowledge this capability. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-macros.html Using AWS CloudFormation Macros to Perform Custom Processing on Templates> .
-- /Important:/ Stack sets do not currently support macros in stack templates. (This includes the <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/create-reusable-transform-function-snippets-and-add-to-your-template-with-aws-include-transform.html AWS::Include> and <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/transform-aws-serverless.html AWS::Serverless> transforms, which are macros hosted by AWS CloudFormation.) Even if you specify this capability, if you include a macro in your template the stack set operation will fail.
--
--
--
-- /Note:/ Consider using 'capabilities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussCapabilities :: Lens.Lens' UpdateStackSet (Core.Maybe [Types.Capability])
ussCapabilities = Lens.field @"capabilities"
{-# INLINEABLE ussCapabilities #-}
{-# DEPRECATED capabilities "Use generic-lens or generic-optics with 'capabilities' instead"  #-}

-- | [@Service-managed@ permissions] The AWS Organizations accounts in which to update associated stack instances.
--
-- To update all the stack instances associated with this stack set, do not specify @DeploymentTargets@ or @Regions@ .
-- If the stack set update includes changes to the template (that is, if @TemplateBody@ or @TemplateURL@ is specified), or the @Parameters@ , AWS CloudFormation marks all stack instances with a status of @OUTDATED@ prior to updating the stack instances in the specified accounts and Regions. If the stack set update does not include changes to the template or parameters, AWS CloudFormation updates the stack instances in the specified accounts and Regions, while leaving all other stack instances with their existing stack instance status.
--
-- /Note:/ Consider using 'deploymentTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussDeploymentTargets :: Lens.Lens' UpdateStackSet (Core.Maybe Types.DeploymentTargets)
ussDeploymentTargets = Lens.field @"deploymentTargets"
{-# INLINEABLE ussDeploymentTargets #-}
{-# DEPRECATED deploymentTargets "Use generic-lens or generic-optics with 'deploymentTargets' instead"  #-}

-- | A brief description of updates that you are making.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussDescription :: Lens.Lens' UpdateStackSet (Core.Maybe Types.Description)
ussDescription = Lens.field @"description"
{-# INLINEABLE ussDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The name of the IAM execution role to use to update the stack set. If you do not specify an execution role, AWS CloudFormation uses the @AWSCloudFormationStackSetExecutionRole@ role for the stack set operation.
--
-- Specify an IAM role only if you are using customized execution roles to control which stack resources users and groups can include in their stack sets. 
-- If you specify a customized execution role, AWS CloudFormation uses that role to update the stack. If you do not specify a customized execution role, AWS CloudFormation performs the update using the role previously associated with the stack set, so long as you have permissions to perform operations on the stack set.
--
-- /Note:/ Consider using 'executionRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussExecutionRoleName :: Lens.Lens' UpdateStackSet (Core.Maybe Types.ExecutionRoleName)
ussExecutionRoleName = Lens.field @"executionRoleName"
{-# INLINEABLE ussExecutionRoleName #-}
{-# DEPRECATED executionRoleName "Use generic-lens or generic-optics with 'executionRoleName' instead"  #-}

-- | The unique ID for this stack set operation. 
--
-- The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You might retry stack set operation requests to ensure that AWS CloudFormation successfully received them.
-- If you don't specify an operation ID, AWS CloudFormation generates one automatically.
-- Repeating this stack set operation with a new operation ID retries all stack instances whose status is @OUTDATED@ . 
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussOperationId :: Lens.Lens' UpdateStackSet (Core.Maybe Types.ClientRequestToken)
ussOperationId = Lens.field @"operationId"
{-# INLINEABLE ussOperationId #-}
{-# DEPRECATED operationId "Use generic-lens or generic-optics with 'operationId' instead"  #-}

-- | Preferences for how AWS CloudFormation performs this stack set operation.
--
-- /Note:/ Consider using 'operationPreferences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussOperationPreferences :: Lens.Lens' UpdateStackSet (Core.Maybe Types.StackSetOperationPreferences)
ussOperationPreferences = Lens.field @"operationPreferences"
{-# INLINEABLE ussOperationPreferences #-}
{-# DEPRECATED operationPreferences "Use generic-lens or generic-optics with 'operationPreferences' instead"  #-}

-- | A list of input parameters for the stack set template. 
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussParameters :: Lens.Lens' UpdateStackSet (Core.Maybe [Types.Parameter])
ussParameters = Lens.field @"parameters"
{-# INLINEABLE ussParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | Describes how the IAM roles required for stack set operations are created. You cannot modify @PermissionModel@ if there are stack instances associated with your stack set.
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
ussPermissionModel :: Lens.Lens' UpdateStackSet (Core.Maybe Types.PermissionModels)
ussPermissionModel = Lens.field @"permissionModel"
{-# INLINEABLE ussPermissionModel #-}
{-# DEPRECATED permissionModel "Use generic-lens or generic-optics with 'permissionModel' instead"  #-}

-- | The Regions in which to update associated stack instances. If you specify Regions, you must also specify accounts in which to update stack set instances.
--
-- To update /all/ the stack instances associated with this stack set, do not specify the @Accounts@ or @Regions@ properties.
-- If the stack set update includes changes to the template (that is, if the @TemplateBody@ or @TemplateURL@ properties are specified), or the @Parameters@ property, AWS CloudFormation marks all stack instances with a status of @OUTDATED@ prior to updating the stack instances in the specified accounts and Regions. If the stack set update does not include changes to the template or parameters, AWS CloudFormation updates the stack instances in the specified accounts and Regions, while leaving all other stack instances with their existing stack instance status. 
--
-- /Note:/ Consider using 'regions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussRegions :: Lens.Lens' UpdateStackSet (Core.Maybe [Types.Region])
ussRegions = Lens.field @"regions"
{-# INLINEABLE ussRegions #-}
{-# DEPRECATED regions "Use generic-lens or generic-optics with 'regions' instead"  #-}

-- | The key-value pairs to associate with this stack set and the stacks created from it. AWS CloudFormation also propagates these tags to supported resources that are created in the stacks. You can specify a maximum number of 50 tags.
--
-- If you specify tags for this parameter, those tags replace any list of tags that are currently associated with this stack set. This means:
--
--     * If you don't specify this parameter, AWS CloudFormation doesn't modify the stack's tags. 
--
--
--     * If you specify /any/ tags using this parameter, you must specify /all/ the tags that you want associated with this stack set, even tags you've specifed before (for example, when creating the stack set or during a previous update of the stack set.). Any tags that you don't include in the updated list of tags are removed from the stack set, and therefore from the stacks and resources as well. 
--
--
--     * If you specify an empty value, AWS CloudFormation removes all currently associated tags.
--
--
-- If you specify new tags as part of an @UpdateStackSet@ action, AWS CloudFormation checks to see if you have the required IAM permission to tag resources. If you omit tags that are currently associated with the stack set from the list of tags you specify, AWS CloudFormation assumes that you want to remove those tags from the stack set, and checks to see if you have permission to untag resources. If you don't have the necessary permission(s), the entire @UpdateStackSet@ action fails with an @access denied@ error, and the stack set is not updated.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussTags :: Lens.Lens' UpdateStackSet (Core.Maybe [Types.Tag])
ussTags = Lens.field @"tags"
{-# INLINEABLE ussTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The structure that contains the template body, with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters: @TemplateBody@ or @TemplateURL@ —or set @UsePreviousTemplate@ to true.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussTemplateBody :: Lens.Lens' UpdateStackSet (Core.Maybe Types.TemplateBody)
ussTemplateBody = Lens.field @"templateBody"
{-# INLINEABLE ussTemplateBody #-}
{-# DEPRECATED templateBody "Use generic-lens or generic-optics with 'templateBody' instead"  #-}

-- | The location of the file that contains the template body. The URL must point to a template (maximum size: 460,800 bytes) that is located in an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters: @TemplateBody@ or @TemplateURL@ —or set @UsePreviousTemplate@ to true. 
--
-- /Note:/ Consider using 'templateURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussTemplateURL :: Lens.Lens' UpdateStackSet (Core.Maybe Types.TemplateURL)
ussTemplateURL = Lens.field @"templateURL"
{-# INLINEABLE ussTemplateURL #-}
{-# DEPRECATED templateURL "Use generic-lens or generic-optics with 'templateURL' instead"  #-}

-- | Use the existing template that's associated with the stack set that you're updating.
--
-- Conditional: You must specify only one of the following parameters: @TemplateBody@ or @TemplateURL@ —or set @UsePreviousTemplate@ to true. 
--
-- /Note:/ Consider using 'usePreviousTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussUsePreviousTemplate :: Lens.Lens' UpdateStackSet (Core.Maybe Core.Bool)
ussUsePreviousTemplate = Lens.field @"usePreviousTemplate"
{-# INLINEABLE ussUsePreviousTemplate #-}
{-# DEPRECATED usePreviousTemplate "Use generic-lens or generic-optics with 'usePreviousTemplate' instead"  #-}

instance Core.ToQuery UpdateStackSet where
        toQuery UpdateStackSet{..}
          = Core.toQueryPair "Action" ("UpdateStackSet" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-05-15" :: Core.Text)
              Core.<> Core.toQueryPair "StackSetName" stackSetName
              Core.<>
              Core.toQueryPair "Accounts"
                (Core.maybe Core.mempty (Core.toQueryList "member") accounts)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AdministrationRoleARN")
                administrationRoleARN
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AutoDeployment")
                autoDeployment
              Core.<>
              Core.toQueryPair "Capabilities"
                (Core.maybe Core.mempty (Core.toQueryList "member") capabilities)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DeploymentTargets")
                deploymentTargets
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ExecutionRoleName")
                executionRoleName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "OperationId") operationId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "OperationPreferences")
                operationPreferences
              Core.<>
              Core.toQueryPair "Parameters"
                (Core.maybe Core.mempty (Core.toQueryList "member") parameters)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PermissionModel")
                permissionModel
              Core.<>
              Core.toQueryPair "Regions"
                (Core.maybe Core.mempty (Core.toQueryList "member") regions)
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "member") tags)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TemplateBody")
                templateBody
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TemplateURL") templateURL
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "UsePreviousTemplate")
                usePreviousTemplate

instance Core.ToHeaders UpdateStackSet where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest UpdateStackSet where
        type Rs UpdateStackSet = UpdateStackSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "UpdateStackSetResult"
              (\ s h x ->
                 UpdateStackSetResponse' Core.<$>
                   (x Core..@? "OperationId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateStackSetResponse' smart constructor.
data UpdateStackSetResponse = UpdateStackSetResponse'
  { operationId :: Core.Maybe Types.OperationId
    -- ^ The unique ID for this stack set operation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateStackSetResponse' value with any optional fields omitted.
mkUpdateStackSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateStackSetResponse
mkUpdateStackSetResponse responseStatus
  = UpdateStackSetResponse'{operationId = Core.Nothing,
                            responseStatus}

-- | The unique ID for this stack set operation.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussrrsOperationId :: Lens.Lens' UpdateStackSetResponse (Core.Maybe Types.OperationId)
ussrrsOperationId = Lens.field @"operationId"
{-# INLINEABLE ussrrsOperationId #-}
{-# DEPRECATED operationId "Use generic-lens or generic-optics with 'operationId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussrrsResponseStatus :: Lens.Lens' UpdateStackSetResponse Core.Int
ussrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ussrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
