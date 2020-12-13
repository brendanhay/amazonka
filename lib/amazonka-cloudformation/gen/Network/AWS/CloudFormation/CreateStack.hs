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
    csDisableRollback,
    csNotificationARNs,
    csEnableTerminationProtection,
    csStackPolicyBody,
    csParameters,
    csStackPolicyURL,
    csTemplateBody,
    csTemplateURL,
    csClientRequestToken,
    csCapabilities,
    csRollbackConfiguration,
    csOnFailure,
    csResourceTypes,
    csTags,
    csTimeoutInMinutes,
    csRoleARN,
    csStackName,

    -- * Destructuring the response
    CreateStackResponse (..),
    mkCreateStackResponse,

    -- ** Response lenses
    csrsStackId,
    csrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for 'CreateStack' action.
--
-- /See:/ 'mkCreateStack' smart constructor.
data CreateStack = CreateStack'
  { -- | Set to @true@ to disable rollback of the stack if stack creation failed. You can specify either @DisableRollback@ or @OnFailure@ , but not both.
    --
    -- Default: @false@
    disableRollback :: Lude.Maybe Lude.Bool,
    -- | The Simple Notification Service (SNS) topic ARNs to publish stack related events. You can find your SNS topic ARNs using the SNS console or your Command Line Interface (CLI).
    notificationARNs :: Lude.Maybe [Lude.Text],
    -- | Whether to enable termination protection on the specified stack. If a user attempts to delete a stack with termination protection enabled, the operation fails and the stack remains unchanged. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html Protecting a Stack From Being Deleted> in the /AWS CloudFormation User Guide/ . Termination protection is disabled on stacks by default.
    --
    -- For <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html nested stacks> , termination protection is set on the root stack and cannot be changed directly on the nested stack.
    enableTerminationProtection :: Lude.Maybe Lude.Bool,
    -- | Structure containing the stack policy body. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources> in the /AWS CloudFormation User Guide/ . You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
    stackPolicyBody :: Lude.Maybe Lude.Text,
    -- | A list of @Parameter@ structures that specify input parameters for the stack. For more information, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter> data type.
    parameters :: Lude.Maybe [Parameter],
    -- | Location of a file containing the stack policy. The URL must point to a policy (maximum size: 16 KB) located in an S3 bucket in the same Region as the stack. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
    stackPolicyURL :: Lude.Maybe Lude.Text,
    -- | Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
    --
    -- Conditional: You must specify either the @TemplateBody@ or the @TemplateURL@ parameter, but not both.
    templateBody :: Lude.Maybe Lude.Text,
    -- | Location of file containing the template body. The URL must point to a template (max size: 460,800 bytes) that is located in an Amazon S3 bucket. For more information, go to the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
    --
    -- Conditional: You must specify either the @TemplateBody@ or the @TemplateURL@ parameter, but not both.
    templateURL :: Lude.Maybe Lude.Text,
    -- | A unique identifier for this @CreateStack@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to create a stack with the same name. You might retry @CreateStack@ requests to ensure that AWS CloudFormation successfully received them.
    --
    -- All events triggered by a given stack operation are assigned the same client request token, which you can use to track operations. For example, if you execute a @CreateStack@ operation with the token @token1@ , then all the @StackEvents@ generated by that operation will have @ClientRequestToken@ set as @token1@ .
    -- In the console, stack operations display the client request token on the Events tab. Stack operations that are initiated from the console use the token format /Console-StackOperation-ID/ , which helps you easily identify the stack operation . For example, if you create a stack using the console, each stack event would be assigned the same token in the following format: @Console-CreateStack-7f59c3cf-00d2-40c7-b2ff-e75db0987002@ .
    clientRequestToken :: Lude.Maybe Lude.Text,
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
    capabilities :: Lude.Maybe [Capability],
    -- | The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
    rollbackConfiguration :: Lude.Maybe RollbackConfiguration,
    -- | Determines what action will be taken if stack creation fails. This must be one of: DO_NOTHING, ROLLBACK, or DELETE. You can specify either @OnFailure@ or @DisableRollback@ , but not both.
    --
    -- Default: @ROLLBACK@
    onFailure :: Lude.Maybe OnFailure,
    -- | The template resource types that you have permissions to work with for this create stack action, such as @AWS::EC2::Instance@ , @AWS::EC2::*@ , or @Custom::MyCustomInstance@ . Use the following syntax to describe template resource types: @AWS::*@ (for all AWS resource), @Custom::*@ (for all custom resources), @Custom::/logical_ID/ @ (for a specific custom resource), @AWS::/service_name/ ::*@ (for all resources of a particular AWS service), and @AWS::/service_name/ ::/resource_logical_ID/ @ (for a specific AWS resource).
    --
    -- If the list of resource types doesn't include a resource that you're creating, the stack creation fails. By default, AWS CloudFormation grants permissions to all resource types. AWS Identity and Access Management (IAM) uses this parameter for AWS CloudFormation-specific condition keys in IAM policies. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html Controlling Access with AWS Identity and Access Management> .
    resourceTypes :: Lude.Maybe [Lude.Text],
    -- | Key-value pairs to associate with this stack. AWS CloudFormation also propagates these tags to the resources created in the stack. A maximum number of 50 tags can be specified.
    tags :: Lude.Maybe [Tag],
    -- | The amount of time that can pass before the stack status becomes CREATE_FAILED; if @DisableRollback@ is not set or is set to @false@ , the stack will be rolled back.
    timeoutInMinutes :: Lude.Maybe Lude.Natural,
    -- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that AWS CloudFormation assumes to create the stack. AWS CloudFormation uses the role's credentials to make calls on your behalf. AWS CloudFormation always uses this role for all future operations on the stack. As long as users have permission to operate on the stack, AWS CloudFormation uses this role even if the users don't have permission to pass it. Ensure that the role grants least privilege.
    --
    -- If you don't specify a value, AWS CloudFormation uses the role that was previously associated with the stack. If no role is available, AWS CloudFormation uses a temporary session that is generated from your user credentials.
    roleARN :: Lude.Maybe Lude.Text,
    -- | The name that is associated with the stack. The name must be unique in the Region in which you are creating the stack.
    stackName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStack' with the minimum fields required to make a request.
--
-- * 'disableRollback' - Set to @true@ to disable rollback of the stack if stack creation failed. You can specify either @DisableRollback@ or @OnFailure@ , but not both.
--
-- Default: @false@
-- * 'notificationARNs' - The Simple Notification Service (SNS) topic ARNs to publish stack related events. You can find your SNS topic ARNs using the SNS console or your Command Line Interface (CLI).
-- * 'enableTerminationProtection' - Whether to enable termination protection on the specified stack. If a user attempts to delete a stack with termination protection enabled, the operation fails and the stack remains unchanged. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html Protecting a Stack From Being Deleted> in the /AWS CloudFormation User Guide/ . Termination protection is disabled on stacks by default.
--
-- For <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html nested stacks> , termination protection is set on the root stack and cannot be changed directly on the nested stack.
-- * 'stackPolicyBody' - Structure containing the stack policy body. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources> in the /AWS CloudFormation User Guide/ . You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
-- * 'parameters' - A list of @Parameter@ structures that specify input parameters for the stack. For more information, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter> data type.
-- * 'stackPolicyURL' - Location of a file containing the stack policy. The URL must point to a policy (maximum size: 16 KB) located in an S3 bucket in the same Region as the stack. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
-- * 'templateBody' - Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify either the @TemplateBody@ or the @TemplateURL@ parameter, but not both.
-- * 'templateURL' - Location of file containing the template body. The URL must point to a template (max size: 460,800 bytes) that is located in an Amazon S3 bucket. For more information, go to the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify either the @TemplateBody@ or the @TemplateURL@ parameter, but not both.
-- * 'clientRequestToken' - A unique identifier for this @CreateStack@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to create a stack with the same name. You might retry @CreateStack@ requests to ensure that AWS CloudFormation successfully received them.
--
-- All events triggered by a given stack operation are assigned the same client request token, which you can use to track operations. For example, if you execute a @CreateStack@ operation with the token @token1@ , then all the @StackEvents@ generated by that operation will have @ClientRequestToken@ set as @token1@ .
-- In the console, stack operations display the client request token on the Events tab. Stack operations that are initiated from the console use the token format /Console-StackOperation-ID/ , which helps you easily identify the stack operation . For example, if you create a stack using the console, each stack event would be assigned the same token in the following format: @Console-CreateStack-7f59c3cf-00d2-40c7-b2ff-e75db0987002@ .
-- * 'capabilities' - In some cases, you must explicitly acknowledge that your stack template contains certain capabilities in order for AWS CloudFormation to create the stack.
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
-- * 'rollbackConfiguration' - The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
-- * 'onFailure' - Determines what action will be taken if stack creation fails. This must be one of: DO_NOTHING, ROLLBACK, or DELETE. You can specify either @OnFailure@ or @DisableRollback@ , but not both.
--
-- Default: @ROLLBACK@
-- * 'resourceTypes' - The template resource types that you have permissions to work with for this create stack action, such as @AWS::EC2::Instance@ , @AWS::EC2::*@ , or @Custom::MyCustomInstance@ . Use the following syntax to describe template resource types: @AWS::*@ (for all AWS resource), @Custom::*@ (for all custom resources), @Custom::/logical_ID/ @ (for a specific custom resource), @AWS::/service_name/ ::*@ (for all resources of a particular AWS service), and @AWS::/service_name/ ::/resource_logical_ID/ @ (for a specific AWS resource).
--
-- If the list of resource types doesn't include a resource that you're creating, the stack creation fails. By default, AWS CloudFormation grants permissions to all resource types. AWS Identity and Access Management (IAM) uses this parameter for AWS CloudFormation-specific condition keys in IAM policies. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html Controlling Access with AWS Identity and Access Management> .
-- * 'tags' - Key-value pairs to associate with this stack. AWS CloudFormation also propagates these tags to the resources created in the stack. A maximum number of 50 tags can be specified.
-- * 'timeoutInMinutes' - The amount of time that can pass before the stack status becomes CREATE_FAILED; if @DisableRollback@ is not set or is set to @false@ , the stack will be rolled back.
-- * 'roleARN' - The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that AWS CloudFormation assumes to create the stack. AWS CloudFormation uses the role's credentials to make calls on your behalf. AWS CloudFormation always uses this role for all future operations on the stack. As long as users have permission to operate on the stack, AWS CloudFormation uses this role even if the users don't have permission to pass it. Ensure that the role grants least privilege.
--
-- If you don't specify a value, AWS CloudFormation uses the role that was previously associated with the stack. If no role is available, AWS CloudFormation uses a temporary session that is generated from your user credentials.
-- * 'stackName' - The name that is associated with the stack. The name must be unique in the Region in which you are creating the stack.
mkCreateStack ::
  -- | 'stackName'
  Lude.Text ->
  CreateStack
mkCreateStack pStackName_ =
  CreateStack'
    { disableRollback = Lude.Nothing,
      notificationARNs = Lude.Nothing,
      enableTerminationProtection = Lude.Nothing,
      stackPolicyBody = Lude.Nothing,
      parameters = Lude.Nothing,
      stackPolicyURL = Lude.Nothing,
      templateBody = Lude.Nothing,
      templateURL = Lude.Nothing,
      clientRequestToken = Lude.Nothing,
      capabilities = Lude.Nothing,
      rollbackConfiguration = Lude.Nothing,
      onFailure = Lude.Nothing,
      resourceTypes = Lude.Nothing,
      tags = Lude.Nothing,
      timeoutInMinutes = Lude.Nothing,
      roleARN = Lude.Nothing,
      stackName = pStackName_
    }

-- | Set to @true@ to disable rollback of the stack if stack creation failed. You can specify either @DisableRollback@ or @OnFailure@ , but not both.
--
-- Default: @false@
--
-- /Note:/ Consider using 'disableRollback' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDisableRollback :: Lens.Lens' CreateStack (Lude.Maybe Lude.Bool)
csDisableRollback = Lens.lens (disableRollback :: CreateStack -> Lude.Maybe Lude.Bool) (\s a -> s {disableRollback = a} :: CreateStack)
{-# DEPRECATED csDisableRollback "Use generic-lens or generic-optics with 'disableRollback' instead." #-}

-- | The Simple Notification Service (SNS) topic ARNs to publish stack related events. You can find your SNS topic ARNs using the SNS console or your Command Line Interface (CLI).
--
-- /Note:/ Consider using 'notificationARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csNotificationARNs :: Lens.Lens' CreateStack (Lude.Maybe [Lude.Text])
csNotificationARNs = Lens.lens (notificationARNs :: CreateStack -> Lude.Maybe [Lude.Text]) (\s a -> s {notificationARNs = a} :: CreateStack)
{-# DEPRECATED csNotificationARNs "Use generic-lens or generic-optics with 'notificationARNs' instead." #-}

-- | Whether to enable termination protection on the specified stack. If a user attempts to delete a stack with termination protection enabled, the operation fails and the stack remains unchanged. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html Protecting a Stack From Being Deleted> in the /AWS CloudFormation User Guide/ . Termination protection is disabled on stacks by default.
--
-- For <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html nested stacks> , termination protection is set on the root stack and cannot be changed directly on the nested stack.
--
-- /Note:/ Consider using 'enableTerminationProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csEnableTerminationProtection :: Lens.Lens' CreateStack (Lude.Maybe Lude.Bool)
csEnableTerminationProtection = Lens.lens (enableTerminationProtection :: CreateStack -> Lude.Maybe Lude.Bool) (\s a -> s {enableTerminationProtection = a} :: CreateStack)
{-# DEPRECATED csEnableTerminationProtection "Use generic-lens or generic-optics with 'enableTerminationProtection' instead." #-}

-- | Structure containing the stack policy body. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources> in the /AWS CloudFormation User Guide/ . You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
--
-- /Note:/ Consider using 'stackPolicyBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStackPolicyBody :: Lens.Lens' CreateStack (Lude.Maybe Lude.Text)
csStackPolicyBody = Lens.lens (stackPolicyBody :: CreateStack -> Lude.Maybe Lude.Text) (\s a -> s {stackPolicyBody = a} :: CreateStack)
{-# DEPRECATED csStackPolicyBody "Use generic-lens or generic-optics with 'stackPolicyBody' instead." #-}

-- | A list of @Parameter@ structures that specify input parameters for the stack. For more information, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter> data type.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csParameters :: Lens.Lens' CreateStack (Lude.Maybe [Parameter])
csParameters = Lens.lens (parameters :: CreateStack -> Lude.Maybe [Parameter]) (\s a -> s {parameters = a} :: CreateStack)
{-# DEPRECATED csParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | Location of a file containing the stack policy. The URL must point to a policy (maximum size: 16 KB) located in an S3 bucket in the same Region as the stack. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
--
-- /Note:/ Consider using 'stackPolicyURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStackPolicyURL :: Lens.Lens' CreateStack (Lude.Maybe Lude.Text)
csStackPolicyURL = Lens.lens (stackPolicyURL :: CreateStack -> Lude.Maybe Lude.Text) (\s a -> s {stackPolicyURL = a} :: CreateStack)
{-# DEPRECATED csStackPolicyURL "Use generic-lens or generic-optics with 'stackPolicyURL' instead." #-}

-- | Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify either the @TemplateBody@ or the @TemplateURL@ parameter, but not both.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTemplateBody :: Lens.Lens' CreateStack (Lude.Maybe Lude.Text)
csTemplateBody = Lens.lens (templateBody :: CreateStack -> Lude.Maybe Lude.Text) (\s a -> s {templateBody = a} :: CreateStack)
{-# DEPRECATED csTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | Location of file containing the template body. The URL must point to a template (max size: 460,800 bytes) that is located in an Amazon S3 bucket. For more information, go to the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify either the @TemplateBody@ or the @TemplateURL@ parameter, but not both.
--
-- /Note:/ Consider using 'templateURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTemplateURL :: Lens.Lens' CreateStack (Lude.Maybe Lude.Text)
csTemplateURL = Lens.lens (templateURL :: CreateStack -> Lude.Maybe Lude.Text) (\s a -> s {templateURL = a} :: CreateStack)
{-# DEPRECATED csTemplateURL "Use generic-lens or generic-optics with 'templateURL' instead." #-}

-- | A unique identifier for this @CreateStack@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to create a stack with the same name. You might retry @CreateStack@ requests to ensure that AWS CloudFormation successfully received them.
--
-- All events triggered by a given stack operation are assigned the same client request token, which you can use to track operations. For example, if you execute a @CreateStack@ operation with the token @token1@ , then all the @StackEvents@ generated by that operation will have @ClientRequestToken@ set as @token1@ .
-- In the console, stack operations display the client request token on the Events tab. Stack operations that are initiated from the console use the token format /Console-StackOperation-ID/ , which helps you easily identify the stack operation . For example, if you create a stack using the console, each stack event would be assigned the same token in the following format: @Console-CreateStack-7f59c3cf-00d2-40c7-b2ff-e75db0987002@ .
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csClientRequestToken :: Lens.Lens' CreateStack (Lude.Maybe Lude.Text)
csClientRequestToken = Lens.lens (clientRequestToken :: CreateStack -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: CreateStack)
{-# DEPRECATED csClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

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
csCapabilities :: Lens.Lens' CreateStack (Lude.Maybe [Capability])
csCapabilities = Lens.lens (capabilities :: CreateStack -> Lude.Maybe [Capability]) (\s a -> s {capabilities = a} :: CreateStack)
{-# DEPRECATED csCapabilities "Use generic-lens or generic-optics with 'capabilities' instead." #-}

-- | The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
--
-- /Note:/ Consider using 'rollbackConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csRollbackConfiguration :: Lens.Lens' CreateStack (Lude.Maybe RollbackConfiguration)
csRollbackConfiguration = Lens.lens (rollbackConfiguration :: CreateStack -> Lude.Maybe RollbackConfiguration) (\s a -> s {rollbackConfiguration = a} :: CreateStack)
{-# DEPRECATED csRollbackConfiguration "Use generic-lens or generic-optics with 'rollbackConfiguration' instead." #-}

-- | Determines what action will be taken if stack creation fails. This must be one of: DO_NOTHING, ROLLBACK, or DELETE. You can specify either @OnFailure@ or @DisableRollback@ , but not both.
--
-- Default: @ROLLBACK@
--
-- /Note:/ Consider using 'onFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csOnFailure :: Lens.Lens' CreateStack (Lude.Maybe OnFailure)
csOnFailure = Lens.lens (onFailure :: CreateStack -> Lude.Maybe OnFailure) (\s a -> s {onFailure = a} :: CreateStack)
{-# DEPRECATED csOnFailure "Use generic-lens or generic-optics with 'onFailure' instead." #-}

-- | The template resource types that you have permissions to work with for this create stack action, such as @AWS::EC2::Instance@ , @AWS::EC2::*@ , or @Custom::MyCustomInstance@ . Use the following syntax to describe template resource types: @AWS::*@ (for all AWS resource), @Custom::*@ (for all custom resources), @Custom::/logical_ID/ @ (for a specific custom resource), @AWS::/service_name/ ::*@ (for all resources of a particular AWS service), and @AWS::/service_name/ ::/resource_logical_ID/ @ (for a specific AWS resource).
--
-- If the list of resource types doesn't include a resource that you're creating, the stack creation fails. By default, AWS CloudFormation grants permissions to all resource types. AWS Identity and Access Management (IAM) uses this parameter for AWS CloudFormation-specific condition keys in IAM policies. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html Controlling Access with AWS Identity and Access Management> .
--
-- /Note:/ Consider using 'resourceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csResourceTypes :: Lens.Lens' CreateStack (Lude.Maybe [Lude.Text])
csResourceTypes = Lens.lens (resourceTypes :: CreateStack -> Lude.Maybe [Lude.Text]) (\s a -> s {resourceTypes = a} :: CreateStack)
{-# DEPRECATED csResourceTypes "Use generic-lens or generic-optics with 'resourceTypes' instead." #-}

-- | Key-value pairs to associate with this stack. AWS CloudFormation also propagates these tags to the resources created in the stack. A maximum number of 50 tags can be specified.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTags :: Lens.Lens' CreateStack (Lude.Maybe [Tag])
csTags = Lens.lens (tags :: CreateStack -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateStack)
{-# DEPRECATED csTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The amount of time that can pass before the stack status becomes CREATE_FAILED; if @DisableRollback@ is not set or is set to @false@ , the stack will be rolled back.
--
-- /Note:/ Consider using 'timeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTimeoutInMinutes :: Lens.Lens' CreateStack (Lude.Maybe Lude.Natural)
csTimeoutInMinutes = Lens.lens (timeoutInMinutes :: CreateStack -> Lude.Maybe Lude.Natural) (\s a -> s {timeoutInMinutes = a} :: CreateStack)
{-# DEPRECATED csTimeoutInMinutes "Use generic-lens or generic-optics with 'timeoutInMinutes' instead." #-}

-- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that AWS CloudFormation assumes to create the stack. AWS CloudFormation uses the role's credentials to make calls on your behalf. AWS CloudFormation always uses this role for all future operations on the stack. As long as users have permission to operate on the stack, AWS CloudFormation uses this role even if the users don't have permission to pass it. Ensure that the role grants least privilege.
--
-- If you don't specify a value, AWS CloudFormation uses the role that was previously associated with the stack. If no role is available, AWS CloudFormation uses a temporary session that is generated from your user credentials.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csRoleARN :: Lens.Lens' CreateStack (Lude.Maybe Lude.Text)
csRoleARN = Lens.lens (roleARN :: CreateStack -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: CreateStack)
{-# DEPRECATED csRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The name that is associated with the stack. The name must be unique in the Region in which you are creating the stack.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStackName :: Lens.Lens' CreateStack Lude.Text
csStackName = Lens.lens (stackName :: CreateStack -> Lude.Text) (\s a -> s {stackName = a} :: CreateStack)
{-# DEPRECATED csStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Lude.AWSRequest CreateStack where
  type Rs CreateStack = CreateStackResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "CreateStackResult"
      ( \s h x ->
          CreateStackResponse'
            Lude.<$> (x Lude..@? "StackId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateStack where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateStack where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateStack where
  toQuery CreateStack' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateStack" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "DisableRollback" Lude.=: disableRollback,
        "NotificationARNs"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> notificationARNs),
        "EnableTerminationProtection" Lude.=: enableTerminationProtection,
        "StackPolicyBody" Lude.=: stackPolicyBody,
        "Parameters"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> parameters),
        "StackPolicyURL" Lude.=: stackPolicyURL,
        "TemplateBody" Lude.=: templateBody,
        "TemplateURL" Lude.=: templateURL,
        "ClientRequestToken" Lude.=: clientRequestToken,
        "Capabilities"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> capabilities),
        "RollbackConfiguration" Lude.=: rollbackConfiguration,
        "OnFailure" Lude.=: onFailure,
        "ResourceTypes"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> resourceTypes),
        "Tags"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> tags),
        "TimeoutInMinutes" Lude.=: timeoutInMinutes,
        "RoleARN" Lude.=: roleARN,
        "StackName" Lude.=: stackName
      ]

-- | The output for a 'CreateStack' action.
--
-- /See:/ 'mkCreateStackResponse' smart constructor.
data CreateStackResponse = CreateStackResponse'
  { -- | Unique identifier of the stack.
    stackId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStackResponse' with the minimum fields required to make a request.
--
-- * 'stackId' - Unique identifier of the stack.
-- * 'responseStatus' - The response status code.
mkCreateStackResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateStackResponse
mkCreateStackResponse pResponseStatus_ =
  CreateStackResponse'
    { stackId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Unique identifier of the stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsStackId :: Lens.Lens' CreateStackResponse (Lude.Maybe Lude.Text)
csrsStackId = Lens.lens (stackId :: CreateStackResponse -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: CreateStackResponse)
{-# DEPRECATED csrsStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsResponseStatus :: Lens.Lens' CreateStackResponse Lude.Int
csrsResponseStatus = Lens.lens (responseStatus :: CreateStackResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateStackResponse)
{-# DEPRECATED csrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
