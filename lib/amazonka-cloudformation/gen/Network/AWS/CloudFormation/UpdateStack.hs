{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.UpdateStack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a stack as specified in the template. After the call completes successfully, the stack update starts. You can check the status of the stack via the 'DescribeStacks' action.
--
-- To get a copy of the template for an existing stack, you can use the 'GetTemplate' action.
-- For more information about creating an update template, updating a stack, and monitoring the progress of the update, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-updating-stacks.html Updating a Stack> .
module Network.AWS.CloudFormation.UpdateStack
  ( -- * Creating a request
    UpdateStack (..),
    mkUpdateStack,

    -- ** Request lenses
    usUsePreviousTemplate,
    usNotificationARNs,
    usStackPolicyBody,
    usStackPolicyDuringUpdateBody,
    usStackPolicyDuringUpdateURL,
    usParameters,
    usStackPolicyURL,
    usTemplateBody,
    usTemplateURL,
    usClientRequestToken,
    usCapabilities,
    usRollbackConfiguration,
    usResourceTypes,
    usTags,
    usRoleARN,
    usStackName,

    -- * Destructuring the response
    UpdateStackResponse (..),
    mkUpdateStackResponse,

    -- ** Response lenses
    usrsStackId,
    usrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for an 'UpdateStack' action.
--
-- /See:/ 'mkUpdateStack' smart constructor.
data UpdateStack = UpdateStack'
  { usePreviousTemplate ::
      Lude.Maybe Lude.Bool,
    notificationARNs :: Lude.Maybe [Lude.Text],
    stackPolicyBody :: Lude.Maybe Lude.Text,
    stackPolicyDuringUpdateBody :: Lude.Maybe Lude.Text,
    stackPolicyDuringUpdateURL :: Lude.Maybe Lude.Text,
    parameters :: Lude.Maybe [Parameter],
    stackPolicyURL :: Lude.Maybe Lude.Text,
    templateBody :: Lude.Maybe Lude.Text,
    templateURL :: Lude.Maybe Lude.Text,
    clientRequestToken :: Lude.Maybe Lude.Text,
    capabilities :: Lude.Maybe [Capability],
    rollbackConfiguration :: Lude.Maybe RollbackConfiguration,
    resourceTypes :: Lude.Maybe [Lude.Text],
    tags :: Lude.Maybe [Tag],
    roleARN :: Lude.Maybe Lude.Text,
    stackName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateStack' with the minimum fields required to make a request.
--
-- * 'capabilities' - In some cases, you must explicitly acknowledge that your stack template contains certain capabilities in order for AWS CloudFormation to update the stack.
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
-- Some template contain macros. Macros perform custom processing on templates; this can include simple actions like find-and-replace operations, all the way to extensive transformations of entire templates. Because of this, users typically create a change set from the processed template, so that they can review the changes resulting from the macros before actually updating the stack. If your stack template contains one or more macros, and you choose to update a stack directly from the processed template, without first reviewing the resulting changes in a change set, you must acknowledge this capability. This includes the <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/create-reusable-transform-function-snippets-and-add-to-your-template-with-aws-include-transform.html AWS::Include> and <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/transform-aws-serverless.html AWS::Serverless> transforms, which are macros hosted by AWS CloudFormation.
-- If you want to update a stack from a stack template that contains macros /and/ nested stacks, you must update the stack directly from the template using this capability.
-- /Important:/ You should only update stacks directly from a stack template that contains macros if you know what processing the macro performs.
-- Each macro relies on an underlying Lambda service function for processing stack templates. Be aware that the Lambda function owner can update the function operation without AWS CloudFormation being notified.
-- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-macros.html Using AWS CloudFormation Macros to Perform Custom Processing on Templates> .
--
--
-- * 'clientRequestToken' - A unique identifier for this @UpdateStack@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to update a stack with the same name. You might retry @UpdateStack@ requests to ensure that AWS CloudFormation successfully received them.
--
-- All events triggered by a given stack operation are assigned the same client request token, which you can use to track operations. For example, if you execute a @CreateStack@ operation with the token @token1@ , then all the @StackEvents@ generated by that operation will have @ClientRequestToken@ set as @token1@ .
-- In the console, stack operations display the client request token on the Events tab. Stack operations that are initiated from the console use the token format /Console-StackOperation-ID/ , which helps you easily identify the stack operation . For example, if you create a stack using the console, each stack event would be assigned the same token in the following format: @Console-CreateStack-7f59c3cf-00d2-40c7-b2ff-e75db0987002@ .
-- * 'notificationARNs' - Amazon Simple Notification Service topic Amazon Resource Names (ARNs) that AWS CloudFormation associates with the stack. Specify an empty list to remove all notification topics.
-- * 'parameters' - A list of @Parameter@ structures that specify input parameters for the stack. For more information, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter> data type.
-- * 'resourceTypes' - The template resource types that you have permissions to work with for this update stack action, such as @AWS::EC2::Instance@ , @AWS::EC2::*@ , or @Custom::MyCustomInstance@ .
--
-- If the list of resource types doesn't include a resource that you're updating, the stack update fails. By default, AWS CloudFormation grants permissions to all resource types. AWS Identity and Access Management (IAM) uses this parameter for AWS CloudFormation-specific condition keys in IAM policies. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html Controlling Access with AWS Identity and Access Management> .
-- * 'roleARN' - The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that AWS CloudFormation assumes to update the stack. AWS CloudFormation uses the role's credentials to make calls on your behalf. AWS CloudFormation always uses this role for all future operations on the stack. As long as users have permission to operate on the stack, AWS CloudFormation uses this role even if the users don't have permission to pass it. Ensure that the role grants least privilege.
--
-- If you don't specify a value, AWS CloudFormation uses the role that was previously associated with the stack. If no role is available, AWS CloudFormation uses a temporary session that is generated from your user credentials.
-- * 'rollbackConfiguration' - The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
-- * 'stackName' - The name or unique stack ID of the stack to update.
-- * 'stackPolicyBody' - Structure containing a new stack policy body. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
--
-- You might update the stack policy, for example, in order to protect a new resource that you created during a stack update. If you do not specify a stack policy, the current policy that is associated with the stack is unchanged.
-- * 'stackPolicyDuringUpdateBody' - Structure containing the temporary overriding stack policy body. You can specify either the @StackPolicyDuringUpdateBody@ or the @StackPolicyDuringUpdateURL@ parameter, but not both.
--
-- If you want to update protected resources, specify a temporary overriding stack policy during this update. If you do not specify a stack policy, the current policy that is associated with the stack will be used.
-- * 'stackPolicyDuringUpdateURL' - Location of a file containing the temporary overriding stack policy. The URL must point to a policy (max size: 16KB) located in an S3 bucket in the same Region as the stack. You can specify either the @StackPolicyDuringUpdateBody@ or the @StackPolicyDuringUpdateURL@ parameter, but not both.
--
-- If you want to update protected resources, specify a temporary overriding stack policy during this update. If you do not specify a stack policy, the current policy that is associated with the stack will be used.
-- * 'stackPolicyURL' - Location of a file containing the updated stack policy. The URL must point to a policy (max size: 16KB) located in an S3 bucket in the same Region as the stack. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
--
-- You might update the stack policy, for example, in order to protect a new resource that you created during a stack update. If you do not specify a stack policy, the current policy that is associated with the stack is unchanged.
-- * 'tags' - Key-value pairs to associate with this stack. AWS CloudFormation also propagates these tags to supported resources in the stack. You can specify a maximum number of 50 tags.
--
-- If you don't specify this parameter, AWS CloudFormation doesn't modify the stack's tags. If you specify an empty value, AWS CloudFormation removes all associated tags.
-- * 'templateBody' - Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.)
--
-- Conditional: You must specify only one of the following parameters: @TemplateBody@ , @TemplateURL@ , or set the @UsePreviousTemplate@ to @true@ .
-- * 'templateURL' - Location of file containing the template body. The URL must point to a template that is located in an Amazon S3 bucket. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters: @TemplateBody@ , @TemplateURL@ , or set the @UsePreviousTemplate@ to @true@ .
-- * 'usePreviousTemplate' - Reuse the existing template that is associated with the stack that you are updating.
--
-- Conditional: You must specify only one of the following parameters: @TemplateBody@ , @TemplateURL@ , or set the @UsePreviousTemplate@ to @true@ .
mkUpdateStack ::
  -- | 'stackName'
  Lude.Text ->
  UpdateStack
mkUpdateStack pStackName_ =
  UpdateStack'
    { usePreviousTemplate = Lude.Nothing,
      notificationARNs = Lude.Nothing,
      stackPolicyBody = Lude.Nothing,
      stackPolicyDuringUpdateBody = Lude.Nothing,
      stackPolicyDuringUpdateURL = Lude.Nothing,
      parameters = Lude.Nothing,
      stackPolicyURL = Lude.Nothing,
      templateBody = Lude.Nothing,
      templateURL = Lude.Nothing,
      clientRequestToken = Lude.Nothing,
      capabilities = Lude.Nothing,
      rollbackConfiguration = Lude.Nothing,
      resourceTypes = Lude.Nothing,
      tags = Lude.Nothing,
      roleARN = Lude.Nothing,
      stackName = pStackName_
    }

-- | Reuse the existing template that is associated with the stack that you are updating.
--
-- Conditional: You must specify only one of the following parameters: @TemplateBody@ , @TemplateURL@ , or set the @UsePreviousTemplate@ to @true@ .
--
-- /Note:/ Consider using 'usePreviousTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usUsePreviousTemplate :: Lens.Lens' UpdateStack (Lude.Maybe Lude.Bool)
usUsePreviousTemplate = Lens.lens (usePreviousTemplate :: UpdateStack -> Lude.Maybe Lude.Bool) (\s a -> s {usePreviousTemplate = a} :: UpdateStack)
{-# DEPRECATED usUsePreviousTemplate "Use generic-lens or generic-optics with 'usePreviousTemplate' instead." #-}

-- | Amazon Simple Notification Service topic Amazon Resource Names (ARNs) that AWS CloudFormation associates with the stack. Specify an empty list to remove all notification topics.
--
-- /Note:/ Consider using 'notificationARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usNotificationARNs :: Lens.Lens' UpdateStack (Lude.Maybe [Lude.Text])
usNotificationARNs = Lens.lens (notificationARNs :: UpdateStack -> Lude.Maybe [Lude.Text]) (\s a -> s {notificationARNs = a} :: UpdateStack)
{-# DEPRECATED usNotificationARNs "Use generic-lens or generic-optics with 'notificationARNs' instead." #-}

-- | Structure containing a new stack policy body. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
--
-- You might update the stack policy, for example, in order to protect a new resource that you created during a stack update. If you do not specify a stack policy, the current policy that is associated with the stack is unchanged.
--
-- /Note:/ Consider using 'stackPolicyBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usStackPolicyBody :: Lens.Lens' UpdateStack (Lude.Maybe Lude.Text)
usStackPolicyBody = Lens.lens (stackPolicyBody :: UpdateStack -> Lude.Maybe Lude.Text) (\s a -> s {stackPolicyBody = a} :: UpdateStack)
{-# DEPRECATED usStackPolicyBody "Use generic-lens or generic-optics with 'stackPolicyBody' instead." #-}

-- | Structure containing the temporary overriding stack policy body. You can specify either the @StackPolicyDuringUpdateBody@ or the @StackPolicyDuringUpdateURL@ parameter, but not both.
--
-- If you want to update protected resources, specify a temporary overriding stack policy during this update. If you do not specify a stack policy, the current policy that is associated with the stack will be used.
--
-- /Note:/ Consider using 'stackPolicyDuringUpdateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usStackPolicyDuringUpdateBody :: Lens.Lens' UpdateStack (Lude.Maybe Lude.Text)
usStackPolicyDuringUpdateBody = Lens.lens (stackPolicyDuringUpdateBody :: UpdateStack -> Lude.Maybe Lude.Text) (\s a -> s {stackPolicyDuringUpdateBody = a} :: UpdateStack)
{-# DEPRECATED usStackPolicyDuringUpdateBody "Use generic-lens or generic-optics with 'stackPolicyDuringUpdateBody' instead." #-}

-- | Location of a file containing the temporary overriding stack policy. The URL must point to a policy (max size: 16KB) located in an S3 bucket in the same Region as the stack. You can specify either the @StackPolicyDuringUpdateBody@ or the @StackPolicyDuringUpdateURL@ parameter, but not both.
--
-- If you want to update protected resources, specify a temporary overriding stack policy during this update. If you do not specify a stack policy, the current policy that is associated with the stack will be used.
--
-- /Note:/ Consider using 'stackPolicyDuringUpdateURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usStackPolicyDuringUpdateURL :: Lens.Lens' UpdateStack (Lude.Maybe Lude.Text)
usStackPolicyDuringUpdateURL = Lens.lens (stackPolicyDuringUpdateURL :: UpdateStack -> Lude.Maybe Lude.Text) (\s a -> s {stackPolicyDuringUpdateURL = a} :: UpdateStack)
{-# DEPRECATED usStackPolicyDuringUpdateURL "Use generic-lens or generic-optics with 'stackPolicyDuringUpdateURL' instead." #-}

-- | A list of @Parameter@ structures that specify input parameters for the stack. For more information, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter> data type.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usParameters :: Lens.Lens' UpdateStack (Lude.Maybe [Parameter])
usParameters = Lens.lens (parameters :: UpdateStack -> Lude.Maybe [Parameter]) (\s a -> s {parameters = a} :: UpdateStack)
{-# DEPRECATED usParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | Location of a file containing the updated stack policy. The URL must point to a policy (max size: 16KB) located in an S3 bucket in the same Region as the stack. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
--
-- You might update the stack policy, for example, in order to protect a new resource that you created during a stack update. If you do not specify a stack policy, the current policy that is associated with the stack is unchanged.
--
-- /Note:/ Consider using 'stackPolicyURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usStackPolicyURL :: Lens.Lens' UpdateStack (Lude.Maybe Lude.Text)
usStackPolicyURL = Lens.lens (stackPolicyURL :: UpdateStack -> Lude.Maybe Lude.Text) (\s a -> s {stackPolicyURL = a} :: UpdateStack)
{-# DEPRECATED usStackPolicyURL "Use generic-lens or generic-optics with 'stackPolicyURL' instead." #-}

-- | Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.)
--
-- Conditional: You must specify only one of the following parameters: @TemplateBody@ , @TemplateURL@ , or set the @UsePreviousTemplate@ to @true@ .
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usTemplateBody :: Lens.Lens' UpdateStack (Lude.Maybe Lude.Text)
usTemplateBody = Lens.lens (templateBody :: UpdateStack -> Lude.Maybe Lude.Text) (\s a -> s {templateBody = a} :: UpdateStack)
{-# DEPRECATED usTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | Location of file containing the template body. The URL must point to a template that is located in an Amazon S3 bucket. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters: @TemplateBody@ , @TemplateURL@ , or set the @UsePreviousTemplate@ to @true@ .
--
-- /Note:/ Consider using 'templateURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usTemplateURL :: Lens.Lens' UpdateStack (Lude.Maybe Lude.Text)
usTemplateURL = Lens.lens (templateURL :: UpdateStack -> Lude.Maybe Lude.Text) (\s a -> s {templateURL = a} :: UpdateStack)
{-# DEPRECATED usTemplateURL "Use generic-lens or generic-optics with 'templateURL' instead." #-}

-- | A unique identifier for this @UpdateStack@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to update a stack with the same name. You might retry @UpdateStack@ requests to ensure that AWS CloudFormation successfully received them.
--
-- All events triggered by a given stack operation are assigned the same client request token, which you can use to track operations. For example, if you execute a @CreateStack@ operation with the token @token1@ , then all the @StackEvents@ generated by that operation will have @ClientRequestToken@ set as @token1@ .
-- In the console, stack operations display the client request token on the Events tab. Stack operations that are initiated from the console use the token format /Console-StackOperation-ID/ , which helps you easily identify the stack operation . For example, if you create a stack using the console, each stack event would be assigned the same token in the following format: @Console-CreateStack-7f59c3cf-00d2-40c7-b2ff-e75db0987002@ .
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usClientRequestToken :: Lens.Lens' UpdateStack (Lude.Maybe Lude.Text)
usClientRequestToken = Lens.lens (clientRequestToken :: UpdateStack -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: UpdateStack)
{-# DEPRECATED usClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | In some cases, you must explicitly acknowledge that your stack template contains certain capabilities in order for AWS CloudFormation to update the stack.
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
-- Some template contain macros. Macros perform custom processing on templates; this can include simple actions like find-and-replace operations, all the way to extensive transformations of entire templates. Because of this, users typically create a change set from the processed template, so that they can review the changes resulting from the macros before actually updating the stack. If your stack template contains one or more macros, and you choose to update a stack directly from the processed template, without first reviewing the resulting changes in a change set, you must acknowledge this capability. This includes the <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/create-reusable-transform-function-snippets-and-add-to-your-template-with-aws-include-transform.html AWS::Include> and <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/transform-aws-serverless.html AWS::Serverless> transforms, which are macros hosted by AWS CloudFormation.
-- If you want to update a stack from a stack template that contains macros /and/ nested stacks, you must update the stack directly from the template using this capability.
-- /Important:/ You should only update stacks directly from a stack template that contains macros if you know what processing the macro performs.
-- Each macro relies on an underlying Lambda service function for processing stack templates. Be aware that the Lambda function owner can update the function operation without AWS CloudFormation being notified.
-- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-macros.html Using AWS CloudFormation Macros to Perform Custom Processing on Templates> .
--
--
--
-- /Note:/ Consider using 'capabilities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usCapabilities :: Lens.Lens' UpdateStack (Lude.Maybe [Capability])
usCapabilities = Lens.lens (capabilities :: UpdateStack -> Lude.Maybe [Capability]) (\s a -> s {capabilities = a} :: UpdateStack)
{-# DEPRECATED usCapabilities "Use generic-lens or generic-optics with 'capabilities' instead." #-}

-- | The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
--
-- /Note:/ Consider using 'rollbackConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usRollbackConfiguration :: Lens.Lens' UpdateStack (Lude.Maybe RollbackConfiguration)
usRollbackConfiguration = Lens.lens (rollbackConfiguration :: UpdateStack -> Lude.Maybe RollbackConfiguration) (\s a -> s {rollbackConfiguration = a} :: UpdateStack)
{-# DEPRECATED usRollbackConfiguration "Use generic-lens or generic-optics with 'rollbackConfiguration' instead." #-}

-- | The template resource types that you have permissions to work with for this update stack action, such as @AWS::EC2::Instance@ , @AWS::EC2::*@ , or @Custom::MyCustomInstance@ .
--
-- If the list of resource types doesn't include a resource that you're updating, the stack update fails. By default, AWS CloudFormation grants permissions to all resource types. AWS Identity and Access Management (IAM) uses this parameter for AWS CloudFormation-specific condition keys in IAM policies. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html Controlling Access with AWS Identity and Access Management> .
--
-- /Note:/ Consider using 'resourceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usResourceTypes :: Lens.Lens' UpdateStack (Lude.Maybe [Lude.Text])
usResourceTypes = Lens.lens (resourceTypes :: UpdateStack -> Lude.Maybe [Lude.Text]) (\s a -> s {resourceTypes = a} :: UpdateStack)
{-# DEPRECATED usResourceTypes "Use generic-lens or generic-optics with 'resourceTypes' instead." #-}

-- | Key-value pairs to associate with this stack. AWS CloudFormation also propagates these tags to supported resources in the stack. You can specify a maximum number of 50 tags.
--
-- If you don't specify this parameter, AWS CloudFormation doesn't modify the stack's tags. If you specify an empty value, AWS CloudFormation removes all associated tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usTags :: Lens.Lens' UpdateStack (Lude.Maybe [Tag])
usTags = Lens.lens (tags :: UpdateStack -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: UpdateStack)
{-# DEPRECATED usTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that AWS CloudFormation assumes to update the stack. AWS CloudFormation uses the role's credentials to make calls on your behalf. AWS CloudFormation always uses this role for all future operations on the stack. As long as users have permission to operate on the stack, AWS CloudFormation uses this role even if the users don't have permission to pass it. Ensure that the role grants least privilege.
--
-- If you don't specify a value, AWS CloudFormation uses the role that was previously associated with the stack. If no role is available, AWS CloudFormation uses a temporary session that is generated from your user credentials.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usRoleARN :: Lens.Lens' UpdateStack (Lude.Maybe Lude.Text)
usRoleARN = Lens.lens (roleARN :: UpdateStack -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: UpdateStack)
{-# DEPRECATED usRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The name or unique stack ID of the stack to update.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usStackName :: Lens.Lens' UpdateStack Lude.Text
usStackName = Lens.lens (stackName :: UpdateStack -> Lude.Text) (\s a -> s {stackName = a} :: UpdateStack)
{-# DEPRECATED usStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Lude.AWSRequest UpdateStack where
  type Rs UpdateStack = UpdateStackResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "UpdateStackResult"
      ( \s h x ->
          UpdateStackResponse'
            Lude.<$> (x Lude..@? "StackId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateStack where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateStack where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateStack where
  toQuery UpdateStack' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("UpdateStack" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "UsePreviousTemplate" Lude.=: usePreviousTemplate,
        "NotificationARNs"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> notificationARNs),
        "StackPolicyBody" Lude.=: stackPolicyBody,
        "StackPolicyDuringUpdateBody" Lude.=: stackPolicyDuringUpdateBody,
        "StackPolicyDuringUpdateURL" Lude.=: stackPolicyDuringUpdateURL,
        "Parameters"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> parameters),
        "StackPolicyURL" Lude.=: stackPolicyURL,
        "TemplateBody" Lude.=: templateBody,
        "TemplateURL" Lude.=: templateURL,
        "ClientRequestToken" Lude.=: clientRequestToken,
        "Capabilities"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> capabilities),
        "RollbackConfiguration" Lude.=: rollbackConfiguration,
        "ResourceTypes"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> resourceTypes),
        "Tags"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> tags),
        "RoleARN" Lude.=: roleARN,
        "StackName" Lude.=: stackName
      ]

-- | The output for an 'UpdateStack' action.
--
-- /See:/ 'mkUpdateStackResponse' smart constructor.
data UpdateStackResponse = UpdateStackResponse'
  { stackId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateStackResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'stackId' - Unique identifier of the stack.
mkUpdateStackResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateStackResponse
mkUpdateStackResponse pResponseStatus_ =
  UpdateStackResponse'
    { stackId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Unique identifier of the stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrsStackId :: Lens.Lens' UpdateStackResponse (Lude.Maybe Lude.Text)
usrsStackId = Lens.lens (stackId :: UpdateStackResponse -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: UpdateStackResponse)
{-# DEPRECATED usrsStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrsResponseStatus :: Lens.Lens' UpdateStackResponse Lude.Int
usrsResponseStatus = Lens.lens (responseStatus :: UpdateStackResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateStackResponse)
{-# DEPRECATED usrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
