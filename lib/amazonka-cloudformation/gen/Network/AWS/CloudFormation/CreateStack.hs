{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.CreateStack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a stack as specified in the template. After the call completes successfully, the stack creation starts. You can check the status of the stack via the 'DescribeStacks' API.
module Network.AWS.CloudFormation.CreateStack
  ( -- * Creating a request
    CreateStack (..),
    mkCreateStack,

    -- ** Request lenses
    csStackName,
    csCapabilities,
    csClientRequestToken,
    csDisableRollback,
    csEnableTerminationProtection,
    csNotificationARNs,
    csOnFailure,
    csParameters,
    csResourceTypes,
    csRoleARN,
    csRollbackConfiguration,
    csStackPolicyBody,
    csStackPolicyURL,
    csTags,
    csTemplateBody,
    csTemplateURL,
    csTimeoutInMinutes,

    -- * Destructuring the response
    CreateStackResponse (..),
    mkCreateStackResponse,

    -- ** Response lenses
    csrrsStackId,
    csrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for 'CreateStack' action.
--
-- /See:/ 'mkCreateStack' smart constructor.
data CreateStack = CreateStack'
  { -- | The name that is associated with the stack. The name must be unique in the Region in which you are creating the stack.
    stackName :: Types.StackName,
    -- | In some cases, you must explicitly acknowledge that your stack template contains certain capabilities in order for AWS CloudFormation to create the stack.
    --
    --
    --     * @CAPABILITY_IAM@ and @CAPABILITY_NAMED_IAM@
    -- Some stack templates might include resources that can affect permissions in your AWS account; for example, by creating new AWS Identity and Access Management (IAM) users. For those stacks, you must explicitly acknowledge this by specifying one of these capabilities.
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
    -- Some template contain macros. Macros perform custom processing on templates; this can include simple actions like find-and-replace operations, all the way to extensive transformations of entire templates. Because of this, users typically create a change set from the processed template, so that they can review the changes resulting from the macros before actually creating the stack. If your stack template contains one or more macros, and you choose to create a stack directly from the processed template, without first reviewing the resulting changes in a change set, you must acknowledge this capability. This includes the <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/create-reusable-transform-function-snippets-and-add-to-your-template-with-aws-include-transform.html AWS::Include> and <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/transform-aws-serverless.html AWS::Serverless> transforms, which are macros hosted by AWS CloudFormation.
    -- If you want to create a stack from a stack template that contains macros /and/ nested stacks, you must create the stack directly from the template using this capability.
    -- /Important:/ You should only create stacks directly from a stack template that contains macros if you know what processing the macro performs.
    -- Each macro relies on an underlying Lambda service function for processing stack templates. Be aware that the Lambda function owner can update the function operation without AWS CloudFormation being notified.
    -- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-macros.html Using AWS CloudFormation Macros to Perform Custom Processing on Templates> .
    capabilities :: Core.Maybe [Types.Capability],
    -- | A unique identifier for this @CreateStack@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to create a stack with the same name. You might retry @CreateStack@ requests to ensure that AWS CloudFormation successfully received them.
    --
    -- All events triggered by a given stack operation are assigned the same client request token, which you can use to track operations. For example, if you execute a @CreateStack@ operation with the token @token1@ , then all the @StackEvents@ generated by that operation will have @ClientRequestToken@ set as @token1@ .
    -- In the console, stack operations display the client request token on the Events tab. Stack operations that are initiated from the console use the token format /Console-StackOperation-ID/ , which helps you easily identify the stack operation . For example, if you create a stack using the console, each stack event would be assigned the same token in the following format: @Console-CreateStack-7f59c3cf-00d2-40c7-b2ff-e75db0987002@ .
    clientRequestToken :: Core.Maybe Types.ClientRequestToken,
    -- | Set to @true@ to disable rollback of the stack if stack creation failed. You can specify either @DisableRollback@ or @OnFailure@ , but not both.
    --
    -- Default: @false@
    disableRollback :: Core.Maybe Core.Bool,
    -- | Whether to enable termination protection on the specified stack. If a user attempts to delete a stack with termination protection enabled, the operation fails and the stack remains unchanged. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html Protecting a Stack From Being Deleted> in the /AWS CloudFormation User Guide/ . Termination protection is disabled on stacks by default.
    --
    -- For <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html nested stacks> , termination protection is set on the root stack and cannot be changed directly on the nested stack.
    enableTerminationProtection :: Core.Maybe Core.Bool,
    -- | The Simple Notification Service (SNS) topic ARNs to publish stack related events. You can find your SNS topic ARNs using the SNS console or your Command Line Interface (CLI).
    notificationARNs :: Core.Maybe [Types.NotificationARN],
    -- | Determines what action will be taken if stack creation fails. This must be one of: DO_NOTHING, ROLLBACK, or DELETE. You can specify either @OnFailure@ or @DisableRollback@ , but not both.
    --
    -- Default: @ROLLBACK@
    onFailure :: Core.Maybe Types.OnFailure,
    -- | A list of @Parameter@ structures that specify input parameters for the stack. For more information, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter> data type.
    parameters :: Core.Maybe [Types.Parameter],
    -- | The template resource types that you have permissions to work with for this create stack action, such as @AWS::EC2::Instance@ , @AWS::EC2::*@ , or @Custom::MyCustomInstance@ . Use the following syntax to describe template resource types: @AWS::*@ (for all AWS resource), @Custom::*@ (for all custom resources), @Custom::/logical_ID/ @ (for a specific custom resource), @AWS::/service_name/ ::*@ (for all resources of a particular AWS service), and @AWS::/service_name/ ::/resource_logical_ID/ @ (for a specific AWS resource).
    --
    -- If the list of resource types doesn't include a resource that you're creating, the stack creation fails. By default, AWS CloudFormation grants permissions to all resource types. AWS Identity and Access Management (IAM) uses this parameter for AWS CloudFormation-specific condition keys in IAM policies. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html Controlling Access with AWS Identity and Access Management> .
    resourceTypes :: Core.Maybe [Types.ResourceType],
    -- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that AWS CloudFormation assumes to create the stack. AWS CloudFormation uses the role's credentials to make calls on your behalf. AWS CloudFormation always uses this role for all future operations on the stack. As long as users have permission to operate on the stack, AWS CloudFormation uses this role even if the users don't have permission to pass it. Ensure that the role grants least privilege.
    --
    -- If you don't specify a value, AWS CloudFormation uses the role that was previously associated with the stack. If no role is available, AWS CloudFormation uses a temporary session that is generated from your user credentials.
    roleARN :: Core.Maybe Types.RoleARN,
    -- | The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
    rollbackConfiguration :: Core.Maybe Types.RollbackConfiguration,
    -- | Structure containing the stack policy body. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources> in the /AWS CloudFormation User Guide/ . You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
    stackPolicyBody :: Core.Maybe Types.StackPolicyBody,
    -- | Location of a file containing the stack policy. The URL must point to a policy (maximum size: 16 KB) located in an S3 bucket in the same Region as the stack. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
    stackPolicyURL :: Core.Maybe Types.StackPolicyURL,
    -- | Key-value pairs to associate with this stack. AWS CloudFormation also propagates these tags to the resources created in the stack. A maximum number of 50 tags can be specified.
    tags :: Core.Maybe [Types.Tag],
    -- | Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
    --
    -- Conditional: You must specify either the @TemplateBody@ or the @TemplateURL@ parameter, but not both.
    templateBody :: Core.Maybe Types.TemplateBody,
    -- | Location of file containing the template body. The URL must point to a template (max size: 460,800 bytes) that is located in an Amazon S3 bucket. For more information, go to the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
    --
    -- Conditional: You must specify either the @TemplateBody@ or the @TemplateURL@ parameter, but not both.
    templateURL :: Core.Maybe Types.TemplateURL,
    -- | The amount of time that can pass before the stack status becomes CREATE_FAILED; if @DisableRollback@ is not set or is set to @false@ , the stack will be rolled back.
    timeoutInMinutes :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStack' value with any optional fields omitted.
mkCreateStack ::
  -- | 'stackName'
  Types.StackName ->
  CreateStack
mkCreateStack stackName =
  CreateStack'
    { stackName,
      capabilities = Core.Nothing,
      clientRequestToken = Core.Nothing,
      disableRollback = Core.Nothing,
      enableTerminationProtection = Core.Nothing,
      notificationARNs = Core.Nothing,
      onFailure = Core.Nothing,
      parameters = Core.Nothing,
      resourceTypes = Core.Nothing,
      roleARN = Core.Nothing,
      rollbackConfiguration = Core.Nothing,
      stackPolicyBody = Core.Nothing,
      stackPolicyURL = Core.Nothing,
      tags = Core.Nothing,
      templateBody = Core.Nothing,
      templateURL = Core.Nothing,
      timeoutInMinutes = Core.Nothing
    }

-- | The name that is associated with the stack. The name must be unique in the Region in which you are creating the stack.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStackName :: Lens.Lens' CreateStack Types.StackName
csStackName = Lens.field @"stackName"
{-# DEPRECATED csStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

-- | In some cases, you must explicitly acknowledge that your stack template contains certain capabilities in order for AWS CloudFormation to create the stack.
--
--
--     * @CAPABILITY_IAM@ and @CAPABILITY_NAMED_IAM@
-- Some stack templates might include resources that can affect permissions in your AWS account; for example, by creating new AWS Identity and Access Management (IAM) users. For those stacks, you must explicitly acknowledge this by specifying one of these capabilities.
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
-- Some template contain macros. Macros perform custom processing on templates; this can include simple actions like find-and-replace operations, all the way to extensive transformations of entire templates. Because of this, users typically create a change set from the processed template, so that they can review the changes resulting from the macros before actually creating the stack. If your stack template contains one or more macros, and you choose to create a stack directly from the processed template, without first reviewing the resulting changes in a change set, you must acknowledge this capability. This includes the <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/create-reusable-transform-function-snippets-and-add-to-your-template-with-aws-include-transform.html AWS::Include> and <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/transform-aws-serverless.html AWS::Serverless> transforms, which are macros hosted by AWS CloudFormation.
-- If you want to create a stack from a stack template that contains macros /and/ nested stacks, you must create the stack directly from the template using this capability.
-- /Important:/ You should only create stacks directly from a stack template that contains macros if you know what processing the macro performs.
-- Each macro relies on an underlying Lambda service function for processing stack templates. Be aware that the Lambda function owner can update the function operation without AWS CloudFormation being notified.
-- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-macros.html Using AWS CloudFormation Macros to Perform Custom Processing on Templates> .
--
--
--
-- /Note:/ Consider using 'capabilities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCapabilities :: Lens.Lens' CreateStack (Core.Maybe [Types.Capability])
csCapabilities = Lens.field @"capabilities"
{-# DEPRECATED csCapabilities "Use generic-lens or generic-optics with 'capabilities' instead." #-}

-- | A unique identifier for this @CreateStack@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to create a stack with the same name. You might retry @CreateStack@ requests to ensure that AWS CloudFormation successfully received them.
--
-- All events triggered by a given stack operation are assigned the same client request token, which you can use to track operations. For example, if you execute a @CreateStack@ operation with the token @token1@ , then all the @StackEvents@ generated by that operation will have @ClientRequestToken@ set as @token1@ .
-- In the console, stack operations display the client request token on the Events tab. Stack operations that are initiated from the console use the token format /Console-StackOperation-ID/ , which helps you easily identify the stack operation . For example, if you create a stack using the console, each stack event would be assigned the same token in the following format: @Console-CreateStack-7f59c3cf-00d2-40c7-b2ff-e75db0987002@ .
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csClientRequestToken :: Lens.Lens' CreateStack (Core.Maybe Types.ClientRequestToken)
csClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED csClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | Set to @true@ to disable rollback of the stack if stack creation failed. You can specify either @DisableRollback@ or @OnFailure@ , but not both.
--
-- Default: @false@
--
-- /Note:/ Consider using 'disableRollback' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDisableRollback :: Lens.Lens' CreateStack (Core.Maybe Core.Bool)
csDisableRollback = Lens.field @"disableRollback"
{-# DEPRECATED csDisableRollback "Use generic-lens or generic-optics with 'disableRollback' instead." #-}

-- | Whether to enable termination protection on the specified stack. If a user attempts to delete a stack with termination protection enabled, the operation fails and the stack remains unchanged. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html Protecting a Stack From Being Deleted> in the /AWS CloudFormation User Guide/ . Termination protection is disabled on stacks by default.
--
-- For <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html nested stacks> , termination protection is set on the root stack and cannot be changed directly on the nested stack.
--
-- /Note:/ Consider using 'enableTerminationProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csEnableTerminationProtection :: Lens.Lens' CreateStack (Core.Maybe Core.Bool)
csEnableTerminationProtection = Lens.field @"enableTerminationProtection"
{-# DEPRECATED csEnableTerminationProtection "Use generic-lens or generic-optics with 'enableTerminationProtection' instead." #-}

-- | The Simple Notification Service (SNS) topic ARNs to publish stack related events. You can find your SNS topic ARNs using the SNS console or your Command Line Interface (CLI).
--
-- /Note:/ Consider using 'notificationARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csNotificationARNs :: Lens.Lens' CreateStack (Core.Maybe [Types.NotificationARN])
csNotificationARNs = Lens.field @"notificationARNs"
{-# DEPRECATED csNotificationARNs "Use generic-lens or generic-optics with 'notificationARNs' instead." #-}

-- | Determines what action will be taken if stack creation fails. This must be one of: DO_NOTHING, ROLLBACK, or DELETE. You can specify either @OnFailure@ or @DisableRollback@ , but not both.
--
-- Default: @ROLLBACK@
--
-- /Note:/ Consider using 'onFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csOnFailure :: Lens.Lens' CreateStack (Core.Maybe Types.OnFailure)
csOnFailure = Lens.field @"onFailure"
{-# DEPRECATED csOnFailure "Use generic-lens or generic-optics with 'onFailure' instead." #-}

-- | A list of @Parameter@ structures that specify input parameters for the stack. For more information, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter> data type.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csParameters :: Lens.Lens' CreateStack (Core.Maybe [Types.Parameter])
csParameters = Lens.field @"parameters"
{-# DEPRECATED csParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The template resource types that you have permissions to work with for this create stack action, such as @AWS::EC2::Instance@ , @AWS::EC2::*@ , or @Custom::MyCustomInstance@ . Use the following syntax to describe template resource types: @AWS::*@ (for all AWS resource), @Custom::*@ (for all custom resources), @Custom::/logical_ID/ @ (for a specific custom resource), @AWS::/service_name/ ::*@ (for all resources of a particular AWS service), and @AWS::/service_name/ ::/resource_logical_ID/ @ (for a specific AWS resource).
--
-- If the list of resource types doesn't include a resource that you're creating, the stack creation fails. By default, AWS CloudFormation grants permissions to all resource types. AWS Identity and Access Management (IAM) uses this parameter for AWS CloudFormation-specific condition keys in IAM policies. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html Controlling Access with AWS Identity and Access Management> .
--
-- /Note:/ Consider using 'resourceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csResourceTypes :: Lens.Lens' CreateStack (Core.Maybe [Types.ResourceType])
csResourceTypes = Lens.field @"resourceTypes"
{-# DEPRECATED csResourceTypes "Use generic-lens or generic-optics with 'resourceTypes' instead." #-}

-- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that AWS CloudFormation assumes to create the stack. AWS CloudFormation uses the role's credentials to make calls on your behalf. AWS CloudFormation always uses this role for all future operations on the stack. As long as users have permission to operate on the stack, AWS CloudFormation uses this role even if the users don't have permission to pass it. Ensure that the role grants least privilege.
--
-- If you don't specify a value, AWS CloudFormation uses the role that was previously associated with the stack. If no role is available, AWS CloudFormation uses a temporary session that is generated from your user credentials.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csRoleARN :: Lens.Lens' CreateStack (Core.Maybe Types.RoleARN)
csRoleARN = Lens.field @"roleARN"
{-# DEPRECATED csRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
--
-- /Note:/ Consider using 'rollbackConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csRollbackConfiguration :: Lens.Lens' CreateStack (Core.Maybe Types.RollbackConfiguration)
csRollbackConfiguration = Lens.field @"rollbackConfiguration"
{-# DEPRECATED csRollbackConfiguration "Use generic-lens or generic-optics with 'rollbackConfiguration' instead." #-}

-- | Structure containing the stack policy body. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources> in the /AWS CloudFormation User Guide/ . You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
--
-- /Note:/ Consider using 'stackPolicyBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStackPolicyBody :: Lens.Lens' CreateStack (Core.Maybe Types.StackPolicyBody)
csStackPolicyBody = Lens.field @"stackPolicyBody"
{-# DEPRECATED csStackPolicyBody "Use generic-lens or generic-optics with 'stackPolicyBody' instead." #-}

-- | Location of a file containing the stack policy. The URL must point to a policy (maximum size: 16 KB) located in an S3 bucket in the same Region as the stack. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
--
-- /Note:/ Consider using 'stackPolicyURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStackPolicyURL :: Lens.Lens' CreateStack (Core.Maybe Types.StackPolicyURL)
csStackPolicyURL = Lens.field @"stackPolicyURL"
{-# DEPRECATED csStackPolicyURL "Use generic-lens or generic-optics with 'stackPolicyURL' instead." #-}

-- | Key-value pairs to associate with this stack. AWS CloudFormation also propagates these tags to the resources created in the stack. A maximum number of 50 tags can be specified.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTags :: Lens.Lens' CreateStack (Core.Maybe [Types.Tag])
csTags = Lens.field @"tags"
{-# DEPRECATED csTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify either the @TemplateBody@ or the @TemplateURL@ parameter, but not both.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTemplateBody :: Lens.Lens' CreateStack (Core.Maybe Types.TemplateBody)
csTemplateBody = Lens.field @"templateBody"
{-# DEPRECATED csTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | Location of file containing the template body. The URL must point to a template (max size: 460,800 bytes) that is located in an Amazon S3 bucket. For more information, go to the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify either the @TemplateBody@ or the @TemplateURL@ parameter, but not both.
--
-- /Note:/ Consider using 'templateURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTemplateURL :: Lens.Lens' CreateStack (Core.Maybe Types.TemplateURL)
csTemplateURL = Lens.field @"templateURL"
{-# DEPRECATED csTemplateURL "Use generic-lens or generic-optics with 'templateURL' instead." #-}

-- | The amount of time that can pass before the stack status becomes CREATE_FAILED; if @DisableRollback@ is not set or is set to @false@ , the stack will be rolled back.
--
-- /Note:/ Consider using 'timeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTimeoutInMinutes :: Lens.Lens' CreateStack (Core.Maybe Core.Natural)
csTimeoutInMinutes = Lens.field @"timeoutInMinutes"
{-# DEPRECATED csTimeoutInMinutes "Use generic-lens or generic-optics with 'timeoutInMinutes' instead." #-}

instance Core.AWSRequest CreateStack where
  type Rs CreateStack = CreateStackResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CreateStack")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "StackName" stackName)
                Core.<> ( Core.toQueryValue
                            "Capabilities"
                            (Core.toQueryList "member" Core.<$> capabilities)
                        )
                Core.<> ( Core.toQueryValue "ClientRequestToken"
                            Core.<$> clientRequestToken
                        )
                Core.<> (Core.toQueryValue "DisableRollback" Core.<$> disableRollback)
                Core.<> ( Core.toQueryValue "EnableTerminationProtection"
                            Core.<$> enableTerminationProtection
                        )
                Core.<> ( Core.toQueryValue
                            "NotificationARNs"
                            (Core.toQueryList "member" Core.<$> notificationARNs)
                        )
                Core.<> (Core.toQueryValue "OnFailure" Core.<$> onFailure)
                Core.<> ( Core.toQueryValue
                            "Parameters"
                            (Core.toQueryList "member" Core.<$> parameters)
                        )
                Core.<> ( Core.toQueryValue
                            "ResourceTypes"
                            (Core.toQueryList "member" Core.<$> resourceTypes)
                        )
                Core.<> (Core.toQueryValue "RoleARN" Core.<$> roleARN)
                Core.<> ( Core.toQueryValue "RollbackConfiguration"
                            Core.<$> rollbackConfiguration
                        )
                Core.<> (Core.toQueryValue "StackPolicyBody" Core.<$> stackPolicyBody)
                Core.<> (Core.toQueryValue "StackPolicyURL" Core.<$> stackPolicyURL)
                Core.<> ( Core.toQueryValue
                            "Tags"
                            (Core.toQueryList "member" Core.<$> tags)
                        )
                Core.<> (Core.toQueryValue "TemplateBody" Core.<$> templateBody)
                Core.<> (Core.toQueryValue "TemplateURL" Core.<$> templateURL)
                Core.<> (Core.toQueryValue "TimeoutInMinutes" Core.<$> timeoutInMinutes)
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateStackResult"
      ( \s h x ->
          CreateStackResponse'
            Core.<$> (x Core..@? "StackId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The output for a 'CreateStack' action.
--
-- /See:/ 'mkCreateStackResponse' smart constructor.
data CreateStackResponse = CreateStackResponse'
  { -- | Unique identifier of the stack.
    stackId :: Core.Maybe Types.StackId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStackResponse' value with any optional fields omitted.
mkCreateStackResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateStackResponse
mkCreateStackResponse responseStatus =
  CreateStackResponse' {stackId = Core.Nothing, responseStatus}

-- | Unique identifier of the stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsStackId :: Lens.Lens' CreateStackResponse (Core.Maybe Types.StackId)
csrrsStackId = Lens.field @"stackId"
{-# DEPRECATED csrrsStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsResponseStatus :: Lens.Lens' CreateStackResponse Core.Int
csrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED csrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
