{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    UpdateStackSet (..),
    mkUpdateStackSet,

    -- ** Request lenses
    ussAdministrationRoleARN,
    ussUsePreviousTemplate,
    ussAccounts,
    ussRegions,
    ussAutoDeployment,
    ussPermissionModel,
    ussParameters,
    ussOperationPreferences,
    ussOperationId,
    ussTemplateBody,
    ussTemplateURL,
    ussDeploymentTargets,
    ussStackSetName,
    ussDescription,
    ussCapabilities,
    ussTags,
    ussExecutionRoleName,

    -- * Destructuring the response
    UpdateStackSetResponse (..),
    mkUpdateStackSetResponse,

    -- ** Response lenses
    ussrsOperationId,
    ussrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateStackSet' smart constructor.
data UpdateStackSet = UpdateStackSet'
  { -- | The Amazon Resource Number (ARN) of the IAM role to use to update this stack set.
    --
    -- Specify an IAM role only if you are using customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Granting Permissions for Stack Set Operations> in the /AWS CloudFormation User Guide/ .
    -- If you specified a customized administrator role when you created the stack set, you must specify a customized administrator role, even if it is the same customized administrator role used with this stack set previously.
    administrationRoleARN :: Lude.Maybe Lude.Text,
    -- | Use the existing template that's associated with the stack set that you're updating.
    --
    -- Conditional: You must specify only one of the following parameters: @TemplateBody@ or @TemplateURL@ —or set @UsePreviousTemplate@ to true.
    usePreviousTemplate :: Lude.Maybe Lude.Bool,
    -- | [@Self-managed@ permissions] The accounts in which to update associated stack instances. If you specify accounts, you must also specify the Regions in which to update stack set instances.
    --
    -- To update /all/ the stack instances associated with this stack set, do not specify the @Accounts@ or @Regions@ properties.
    -- If the stack set update includes changes to the template (that is, if the @TemplateBody@ or @TemplateURL@ properties are specified), or the @Parameters@ property, AWS CloudFormation marks all stack instances with a status of @OUTDATED@ prior to updating the stack instances in the specified accounts and Regions. If the stack set update does not include changes to the template or parameters, AWS CloudFormation updates the stack instances in the specified accounts and Regions, while leaving all other stack instances with their existing stack instance status.
    accounts :: Lude.Maybe [Lude.Text],
    -- | The Regions in which to update associated stack instances. If you specify Regions, you must also specify accounts in which to update stack set instances.
    --
    -- To update /all/ the stack instances associated with this stack set, do not specify the @Accounts@ or @Regions@ properties.
    -- If the stack set update includes changes to the template (that is, if the @TemplateBody@ or @TemplateURL@ properties are specified), or the @Parameters@ property, AWS CloudFormation marks all stack instances with a status of @OUTDATED@ prior to updating the stack instances in the specified accounts and Regions. If the stack set update does not include changes to the template or parameters, AWS CloudFormation updates the stack instances in the specified accounts and Regions, while leaving all other stack instances with their existing stack instance status.
    regions :: Lude.Maybe [Lude.Text],
    -- | [@Service-managed@ permissions] Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to a target organization or organizational unit (OU).
    --
    -- If you specify @AutoDeployment@ , do not specify @DeploymentTargets@ or @Regions@ .
    autoDeployment :: Lude.Maybe AutoDeployment,
    -- | Describes how the IAM roles required for stack set operations are created. You cannot modify @PermissionModel@ if there are stack instances associated with your stack set.
    --
    --
    --     * With @self-managed@ permissions, you must create the administrator and execution roles required to deploy to target accounts. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions> .
    --
    --
    --     * With @service-managed@ permissions, StackSets automatically creates the IAM roles required to deploy to accounts managed by AWS Organizations. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions> .
    permissionModel :: Lude.Maybe PermissionModels,
    -- | A list of input parameters for the stack set template.
    parameters :: Lude.Maybe [Parameter],
    -- | Preferences for how AWS CloudFormation performs this stack set operation.
    operationPreferences :: Lude.Maybe StackSetOperationPreferences,
    -- | The unique ID for this stack set operation.
    --
    -- The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You might retry stack set operation requests to ensure that AWS CloudFormation successfully received them.
    -- If you don't specify an operation ID, AWS CloudFormation generates one automatically.
    -- Repeating this stack set operation with a new operation ID retries all stack instances whose status is @OUTDATED@ .
    operationId :: Lude.Maybe Lude.Text,
    -- | The structure that contains the template body, with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
    --
    -- Conditional: You must specify only one of the following parameters: @TemplateBody@ or @TemplateURL@ —or set @UsePreviousTemplate@ to true.
    templateBody :: Lude.Maybe Lude.Text,
    -- | The location of the file that contains the template body. The URL must point to a template (maximum size: 460,800 bytes) that is located in an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
    --
    -- Conditional: You must specify only one of the following parameters: @TemplateBody@ or @TemplateURL@ —or set @UsePreviousTemplate@ to true.
    templateURL :: Lude.Maybe Lude.Text,
    -- | [@Service-managed@ permissions] The AWS Organizations accounts in which to update associated stack instances.
    --
    -- To update all the stack instances associated with this stack set, do not specify @DeploymentTargets@ or @Regions@ .
    -- If the stack set update includes changes to the template (that is, if @TemplateBody@ or @TemplateURL@ is specified), or the @Parameters@ , AWS CloudFormation marks all stack instances with a status of @OUTDATED@ prior to updating the stack instances in the specified accounts and Regions. If the stack set update does not include changes to the template or parameters, AWS CloudFormation updates the stack instances in the specified accounts and Regions, while leaving all other stack instances with their existing stack instance status.
    deploymentTargets :: Lude.Maybe DeploymentTargets,
    -- | The name or unique ID of the stack set that you want to update.
    stackSetName :: Lude.Text,
    -- | A brief description of updates that you are making.
    description :: Lude.Maybe Lude.Text,
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
    capabilities :: Lude.Maybe [Capability],
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
    tags :: Lude.Maybe [Tag],
    -- | The name of the IAM execution role to use to update the stack set. If you do not specify an execution role, AWS CloudFormation uses the @AWSCloudFormationStackSetExecutionRole@ role for the stack set operation.
    --
    -- Specify an IAM role only if you are using customized execution roles to control which stack resources users and groups can include in their stack sets.
    -- If you specify a customized execution role, AWS CloudFormation uses that role to update the stack. If you do not specify a customized execution role, AWS CloudFormation performs the update using the role previously associated with the stack set, so long as you have permissions to perform operations on the stack set.
    executionRoleName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateStackSet' with the minimum fields required to make a request.
--
-- * 'administrationRoleARN' - The Amazon Resource Number (ARN) of the IAM role to use to update this stack set.
--
-- Specify an IAM role only if you are using customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Granting Permissions for Stack Set Operations> in the /AWS CloudFormation User Guide/ .
-- If you specified a customized administrator role when you created the stack set, you must specify a customized administrator role, even if it is the same customized administrator role used with this stack set previously.
-- * 'usePreviousTemplate' - Use the existing template that's associated with the stack set that you're updating.
--
-- Conditional: You must specify only one of the following parameters: @TemplateBody@ or @TemplateURL@ —or set @UsePreviousTemplate@ to true.
-- * 'accounts' - [@Self-managed@ permissions] The accounts in which to update associated stack instances. If you specify accounts, you must also specify the Regions in which to update stack set instances.
--
-- To update /all/ the stack instances associated with this stack set, do not specify the @Accounts@ or @Regions@ properties.
-- If the stack set update includes changes to the template (that is, if the @TemplateBody@ or @TemplateURL@ properties are specified), or the @Parameters@ property, AWS CloudFormation marks all stack instances with a status of @OUTDATED@ prior to updating the stack instances in the specified accounts and Regions. If the stack set update does not include changes to the template or parameters, AWS CloudFormation updates the stack instances in the specified accounts and Regions, while leaving all other stack instances with their existing stack instance status.
-- * 'regions' - The Regions in which to update associated stack instances. If you specify Regions, you must also specify accounts in which to update stack set instances.
--
-- To update /all/ the stack instances associated with this stack set, do not specify the @Accounts@ or @Regions@ properties.
-- If the stack set update includes changes to the template (that is, if the @TemplateBody@ or @TemplateURL@ properties are specified), or the @Parameters@ property, AWS CloudFormation marks all stack instances with a status of @OUTDATED@ prior to updating the stack instances in the specified accounts and Regions. If the stack set update does not include changes to the template or parameters, AWS CloudFormation updates the stack instances in the specified accounts and Regions, while leaving all other stack instances with their existing stack instance status.
-- * 'autoDeployment' - [@Service-managed@ permissions] Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to a target organization or organizational unit (OU).
--
-- If you specify @AutoDeployment@ , do not specify @DeploymentTargets@ or @Regions@ .
-- * 'permissionModel' - Describes how the IAM roles required for stack set operations are created. You cannot modify @PermissionModel@ if there are stack instances associated with your stack set.
--
--
--     * With @self-managed@ permissions, you must create the administrator and execution roles required to deploy to target accounts. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions> .
--
--
--     * With @service-managed@ permissions, StackSets automatically creates the IAM roles required to deploy to accounts managed by AWS Organizations. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions> .
--
--
-- * 'parameters' - A list of input parameters for the stack set template.
-- * 'operationPreferences' - Preferences for how AWS CloudFormation performs this stack set operation.
-- * 'operationId' - The unique ID for this stack set operation.
--
-- The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You might retry stack set operation requests to ensure that AWS CloudFormation successfully received them.
-- If you don't specify an operation ID, AWS CloudFormation generates one automatically.
-- Repeating this stack set operation with a new operation ID retries all stack instances whose status is @OUTDATED@ .
-- * 'templateBody' - The structure that contains the template body, with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters: @TemplateBody@ or @TemplateURL@ —or set @UsePreviousTemplate@ to true.
-- * 'templateURL' - The location of the file that contains the template body. The URL must point to a template (maximum size: 460,800 bytes) that is located in an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters: @TemplateBody@ or @TemplateURL@ —or set @UsePreviousTemplate@ to true.
-- * 'deploymentTargets' - [@Service-managed@ permissions] The AWS Organizations accounts in which to update associated stack instances.
--
-- To update all the stack instances associated with this stack set, do not specify @DeploymentTargets@ or @Regions@ .
-- If the stack set update includes changes to the template (that is, if @TemplateBody@ or @TemplateURL@ is specified), or the @Parameters@ , AWS CloudFormation marks all stack instances with a status of @OUTDATED@ prior to updating the stack instances in the specified accounts and Regions. If the stack set update does not include changes to the template or parameters, AWS CloudFormation updates the stack instances in the specified accounts and Regions, while leaving all other stack instances with their existing stack instance status.
-- * 'stackSetName' - The name or unique ID of the stack set that you want to update.
-- * 'description' - A brief description of updates that you are making.
-- * 'capabilities' - In some cases, you must explicitly acknowledge that your stack template contains certain capabilities in order for AWS CloudFormation to update the stack set and its associated stack instances.
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
-- * 'tags' - The key-value pairs to associate with this stack set and the stacks created from it. AWS CloudFormation also propagates these tags to supported resources that are created in the stacks. You can specify a maximum number of 50 tags.
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
-- * 'executionRoleName' - The name of the IAM execution role to use to update the stack set. If you do not specify an execution role, AWS CloudFormation uses the @AWSCloudFormationStackSetExecutionRole@ role for the stack set operation.
--
-- Specify an IAM role only if you are using customized execution roles to control which stack resources users and groups can include in their stack sets.
-- If you specify a customized execution role, AWS CloudFormation uses that role to update the stack. If you do not specify a customized execution role, AWS CloudFormation performs the update using the role previously associated with the stack set, so long as you have permissions to perform operations on the stack set.
mkUpdateStackSet ::
  -- | 'stackSetName'
  Lude.Text ->
  UpdateStackSet
mkUpdateStackSet pStackSetName_ =
  UpdateStackSet'
    { administrationRoleARN = Lude.Nothing,
      usePreviousTemplate = Lude.Nothing,
      accounts = Lude.Nothing,
      regions = Lude.Nothing,
      autoDeployment = Lude.Nothing,
      permissionModel = Lude.Nothing,
      parameters = Lude.Nothing,
      operationPreferences = Lude.Nothing,
      operationId = Lude.Nothing,
      templateBody = Lude.Nothing,
      templateURL = Lude.Nothing,
      deploymentTargets = Lude.Nothing,
      stackSetName = pStackSetName_,
      description = Lude.Nothing,
      capabilities = Lude.Nothing,
      tags = Lude.Nothing,
      executionRoleName = Lude.Nothing
    }

-- | The Amazon Resource Number (ARN) of the IAM role to use to update this stack set.
--
-- Specify an IAM role only if you are using customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Granting Permissions for Stack Set Operations> in the /AWS CloudFormation User Guide/ .
-- If you specified a customized administrator role when you created the stack set, you must specify a customized administrator role, even if it is the same customized administrator role used with this stack set previously.
--
-- /Note:/ Consider using 'administrationRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussAdministrationRoleARN :: Lens.Lens' UpdateStackSet (Lude.Maybe Lude.Text)
ussAdministrationRoleARN = Lens.lens (administrationRoleARN :: UpdateStackSet -> Lude.Maybe Lude.Text) (\s a -> s {administrationRoleARN = a} :: UpdateStackSet)
{-# DEPRECATED ussAdministrationRoleARN "Use generic-lens or generic-optics with 'administrationRoleARN' instead." #-}

-- | Use the existing template that's associated with the stack set that you're updating.
--
-- Conditional: You must specify only one of the following parameters: @TemplateBody@ or @TemplateURL@ —or set @UsePreviousTemplate@ to true.
--
-- /Note:/ Consider using 'usePreviousTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussUsePreviousTemplate :: Lens.Lens' UpdateStackSet (Lude.Maybe Lude.Bool)
ussUsePreviousTemplate = Lens.lens (usePreviousTemplate :: UpdateStackSet -> Lude.Maybe Lude.Bool) (\s a -> s {usePreviousTemplate = a} :: UpdateStackSet)
{-# DEPRECATED ussUsePreviousTemplate "Use generic-lens or generic-optics with 'usePreviousTemplate' instead." #-}

-- | [@Self-managed@ permissions] The accounts in which to update associated stack instances. If you specify accounts, you must also specify the Regions in which to update stack set instances.
--
-- To update /all/ the stack instances associated with this stack set, do not specify the @Accounts@ or @Regions@ properties.
-- If the stack set update includes changes to the template (that is, if the @TemplateBody@ or @TemplateURL@ properties are specified), or the @Parameters@ property, AWS CloudFormation marks all stack instances with a status of @OUTDATED@ prior to updating the stack instances in the specified accounts and Regions. If the stack set update does not include changes to the template or parameters, AWS CloudFormation updates the stack instances in the specified accounts and Regions, while leaving all other stack instances with their existing stack instance status.
--
-- /Note:/ Consider using 'accounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussAccounts :: Lens.Lens' UpdateStackSet (Lude.Maybe [Lude.Text])
ussAccounts = Lens.lens (accounts :: UpdateStackSet -> Lude.Maybe [Lude.Text]) (\s a -> s {accounts = a} :: UpdateStackSet)
{-# DEPRECATED ussAccounts "Use generic-lens or generic-optics with 'accounts' instead." #-}

-- | The Regions in which to update associated stack instances. If you specify Regions, you must also specify accounts in which to update stack set instances.
--
-- To update /all/ the stack instances associated with this stack set, do not specify the @Accounts@ or @Regions@ properties.
-- If the stack set update includes changes to the template (that is, if the @TemplateBody@ or @TemplateURL@ properties are specified), or the @Parameters@ property, AWS CloudFormation marks all stack instances with a status of @OUTDATED@ prior to updating the stack instances in the specified accounts and Regions. If the stack set update does not include changes to the template or parameters, AWS CloudFormation updates the stack instances in the specified accounts and Regions, while leaving all other stack instances with their existing stack instance status.
--
-- /Note:/ Consider using 'regions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussRegions :: Lens.Lens' UpdateStackSet (Lude.Maybe [Lude.Text])
ussRegions = Lens.lens (regions :: UpdateStackSet -> Lude.Maybe [Lude.Text]) (\s a -> s {regions = a} :: UpdateStackSet)
{-# DEPRECATED ussRegions "Use generic-lens or generic-optics with 'regions' instead." #-}

-- | [@Service-managed@ permissions] Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to a target organization or organizational unit (OU).
--
-- If you specify @AutoDeployment@ , do not specify @DeploymentTargets@ or @Regions@ .
--
-- /Note:/ Consider using 'autoDeployment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussAutoDeployment :: Lens.Lens' UpdateStackSet (Lude.Maybe AutoDeployment)
ussAutoDeployment = Lens.lens (autoDeployment :: UpdateStackSet -> Lude.Maybe AutoDeployment) (\s a -> s {autoDeployment = a} :: UpdateStackSet)
{-# DEPRECATED ussAutoDeployment "Use generic-lens or generic-optics with 'autoDeployment' instead." #-}

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
ussPermissionModel :: Lens.Lens' UpdateStackSet (Lude.Maybe PermissionModels)
ussPermissionModel = Lens.lens (permissionModel :: UpdateStackSet -> Lude.Maybe PermissionModels) (\s a -> s {permissionModel = a} :: UpdateStackSet)
{-# DEPRECATED ussPermissionModel "Use generic-lens or generic-optics with 'permissionModel' instead." #-}

-- | A list of input parameters for the stack set template.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussParameters :: Lens.Lens' UpdateStackSet (Lude.Maybe [Parameter])
ussParameters = Lens.lens (parameters :: UpdateStackSet -> Lude.Maybe [Parameter]) (\s a -> s {parameters = a} :: UpdateStackSet)
{-# DEPRECATED ussParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | Preferences for how AWS CloudFormation performs this stack set operation.
--
-- /Note:/ Consider using 'operationPreferences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussOperationPreferences :: Lens.Lens' UpdateStackSet (Lude.Maybe StackSetOperationPreferences)
ussOperationPreferences = Lens.lens (operationPreferences :: UpdateStackSet -> Lude.Maybe StackSetOperationPreferences) (\s a -> s {operationPreferences = a} :: UpdateStackSet)
{-# DEPRECATED ussOperationPreferences "Use generic-lens or generic-optics with 'operationPreferences' instead." #-}

-- | The unique ID for this stack set operation.
--
-- The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You might retry stack set operation requests to ensure that AWS CloudFormation successfully received them.
-- If you don't specify an operation ID, AWS CloudFormation generates one automatically.
-- Repeating this stack set operation with a new operation ID retries all stack instances whose status is @OUTDATED@ .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussOperationId :: Lens.Lens' UpdateStackSet (Lude.Maybe Lude.Text)
ussOperationId = Lens.lens (operationId :: UpdateStackSet -> Lude.Maybe Lude.Text) (\s a -> s {operationId = a} :: UpdateStackSet)
{-# DEPRECATED ussOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The structure that contains the template body, with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters: @TemplateBody@ or @TemplateURL@ —or set @UsePreviousTemplate@ to true.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussTemplateBody :: Lens.Lens' UpdateStackSet (Lude.Maybe Lude.Text)
ussTemplateBody = Lens.lens (templateBody :: UpdateStackSet -> Lude.Maybe Lude.Text) (\s a -> s {templateBody = a} :: UpdateStackSet)
{-# DEPRECATED ussTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | The location of the file that contains the template body. The URL must point to a template (maximum size: 460,800 bytes) that is located in an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters: @TemplateBody@ or @TemplateURL@ —or set @UsePreviousTemplate@ to true.
--
-- /Note:/ Consider using 'templateURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussTemplateURL :: Lens.Lens' UpdateStackSet (Lude.Maybe Lude.Text)
ussTemplateURL = Lens.lens (templateURL :: UpdateStackSet -> Lude.Maybe Lude.Text) (\s a -> s {templateURL = a} :: UpdateStackSet)
{-# DEPRECATED ussTemplateURL "Use generic-lens or generic-optics with 'templateURL' instead." #-}

-- | [@Service-managed@ permissions] The AWS Organizations accounts in which to update associated stack instances.
--
-- To update all the stack instances associated with this stack set, do not specify @DeploymentTargets@ or @Regions@ .
-- If the stack set update includes changes to the template (that is, if @TemplateBody@ or @TemplateURL@ is specified), or the @Parameters@ , AWS CloudFormation marks all stack instances with a status of @OUTDATED@ prior to updating the stack instances in the specified accounts and Regions. If the stack set update does not include changes to the template or parameters, AWS CloudFormation updates the stack instances in the specified accounts and Regions, while leaving all other stack instances with their existing stack instance status.
--
-- /Note:/ Consider using 'deploymentTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussDeploymentTargets :: Lens.Lens' UpdateStackSet (Lude.Maybe DeploymentTargets)
ussDeploymentTargets = Lens.lens (deploymentTargets :: UpdateStackSet -> Lude.Maybe DeploymentTargets) (\s a -> s {deploymentTargets = a} :: UpdateStackSet)
{-# DEPRECATED ussDeploymentTargets "Use generic-lens or generic-optics with 'deploymentTargets' instead." #-}

-- | The name or unique ID of the stack set that you want to update.
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussStackSetName :: Lens.Lens' UpdateStackSet Lude.Text
ussStackSetName = Lens.lens (stackSetName :: UpdateStackSet -> Lude.Text) (\s a -> s {stackSetName = a} :: UpdateStackSet)
{-# DEPRECATED ussStackSetName "Use generic-lens or generic-optics with 'stackSetName' instead." #-}

-- | A brief description of updates that you are making.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussDescription :: Lens.Lens' UpdateStackSet (Lude.Maybe Lude.Text)
ussDescription = Lens.lens (description :: UpdateStackSet -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateStackSet)
{-# DEPRECATED ussDescription "Use generic-lens or generic-optics with 'description' instead." #-}

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
ussCapabilities :: Lens.Lens' UpdateStackSet (Lude.Maybe [Capability])
ussCapabilities = Lens.lens (capabilities :: UpdateStackSet -> Lude.Maybe [Capability]) (\s a -> s {capabilities = a} :: UpdateStackSet)
{-# DEPRECATED ussCapabilities "Use generic-lens or generic-optics with 'capabilities' instead." #-}

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
ussTags :: Lens.Lens' UpdateStackSet (Lude.Maybe [Tag])
ussTags = Lens.lens (tags :: UpdateStackSet -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: UpdateStackSet)
{-# DEPRECATED ussTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the IAM execution role to use to update the stack set. If you do not specify an execution role, AWS CloudFormation uses the @AWSCloudFormationStackSetExecutionRole@ role for the stack set operation.
--
-- Specify an IAM role only if you are using customized execution roles to control which stack resources users and groups can include in their stack sets.
-- If you specify a customized execution role, AWS CloudFormation uses that role to update the stack. If you do not specify a customized execution role, AWS CloudFormation performs the update using the role previously associated with the stack set, so long as you have permissions to perform operations on the stack set.
--
-- /Note:/ Consider using 'executionRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussExecutionRoleName :: Lens.Lens' UpdateStackSet (Lude.Maybe Lude.Text)
ussExecutionRoleName = Lens.lens (executionRoleName :: UpdateStackSet -> Lude.Maybe Lude.Text) (\s a -> s {executionRoleName = a} :: UpdateStackSet)
{-# DEPRECATED ussExecutionRoleName "Use generic-lens or generic-optics with 'executionRoleName' instead." #-}

instance Lude.AWSRequest UpdateStackSet where
  type Rs UpdateStackSet = UpdateStackSetResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "UpdateStackSetResult"
      ( \s h x ->
          UpdateStackSetResponse'
            Lude.<$> (x Lude..@? "OperationId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateStackSet where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateStackSet where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateStackSet where
  toQuery UpdateStackSet' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("UpdateStackSet" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "AdministrationRoleARN" Lude.=: administrationRoleARN,
        "UsePreviousTemplate" Lude.=: usePreviousTemplate,
        "Accounts"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> accounts),
        "Regions"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> regions),
        "AutoDeployment" Lude.=: autoDeployment,
        "PermissionModel" Lude.=: permissionModel,
        "Parameters"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> parameters),
        "OperationPreferences" Lude.=: operationPreferences,
        "OperationId" Lude.=: operationId,
        "TemplateBody" Lude.=: templateBody,
        "TemplateURL" Lude.=: templateURL,
        "DeploymentTargets" Lude.=: deploymentTargets,
        "StackSetName" Lude.=: stackSetName,
        "Description" Lude.=: description,
        "Capabilities"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> capabilities),
        "Tags"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> tags),
        "ExecutionRoleName" Lude.=: executionRoleName
      ]

-- | /See:/ 'mkUpdateStackSetResponse' smart constructor.
data UpdateStackSetResponse = UpdateStackSetResponse'
  { -- | The unique ID for this stack set operation.
    operationId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateStackSetResponse' with the minimum fields required to make a request.
--
-- * 'operationId' - The unique ID for this stack set operation.
-- * 'responseStatus' - The response status code.
mkUpdateStackSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateStackSetResponse
mkUpdateStackSetResponse pResponseStatus_ =
  UpdateStackSetResponse'
    { operationId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique ID for this stack set operation.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussrsOperationId :: Lens.Lens' UpdateStackSetResponse (Lude.Maybe Lude.Text)
ussrsOperationId = Lens.lens (operationId :: UpdateStackSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {operationId = a} :: UpdateStackSetResponse)
{-# DEPRECATED ussrsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussrsResponseStatus :: Lens.Lens' UpdateStackSetResponse Lude.Int
ussrsResponseStatus = Lens.lens (responseStatus :: UpdateStackSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateStackSetResponse)
{-# DEPRECATED ussrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
