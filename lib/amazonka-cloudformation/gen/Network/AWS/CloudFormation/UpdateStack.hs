{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateStack (..)
    , mkUpdateStack
    -- ** Request lenses
    , usStackName
    , usCapabilities
    , usClientRequestToken
    , usNotificationARNs
    , usParameters
    , usResourceTypes
    , usRoleARN
    , usRollbackConfiguration
    , usStackPolicyBody
    , usStackPolicyDuringUpdateBody
    , usStackPolicyDuringUpdateURL
    , usStackPolicyURL
    , usTags
    , usTemplateBody
    , usTemplateURL
    , usUsePreviousTemplate

    -- * Destructuring the response
    , UpdateStackResponse (..)
    , mkUpdateStackResponse
    -- ** Response lenses
    , usrrsStackId
    , usrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for an 'UpdateStack' action.
--
-- /See:/ 'mkUpdateStack' smart constructor.
data UpdateStack = UpdateStack'
  { stackName :: Types.StackName
    -- ^ The name or unique stack ID of the stack to update.
  , capabilities :: Core.Maybe [Types.Capability]
    -- ^ In some cases, you must explicitly acknowledge that your stack template contains certain capabilities in order for AWS CloudFormation to update the stack.
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
  , clientRequestToken :: Core.Maybe Types.ClientRequestToken
    -- ^ A unique identifier for this @UpdateStack@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to update a stack with the same name. You might retry @UpdateStack@ requests to ensure that AWS CloudFormation successfully received them.
--
-- All events triggered by a given stack operation are assigned the same client request token, which you can use to track operations. For example, if you execute a @CreateStack@ operation with the token @token1@ , then all the @StackEvents@ generated by that operation will have @ClientRequestToken@ set as @token1@ .
-- In the console, stack operations display the client request token on the Events tab. Stack operations that are initiated from the console use the token format /Console-StackOperation-ID/ , which helps you easily identify the stack operation . For example, if you create a stack using the console, each stack event would be assigned the same token in the following format: @Console-CreateStack-7f59c3cf-00d2-40c7-b2ff-e75db0987002@ . 
  , notificationARNs :: Core.Maybe [Types.NotificationARN]
    -- ^ Amazon Simple Notification Service topic Amazon Resource Names (ARNs) that AWS CloudFormation associates with the stack. Specify an empty list to remove all notification topics.
  , parameters :: Core.Maybe [Types.Parameter]
    -- ^ A list of @Parameter@ structures that specify input parameters for the stack. For more information, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter> data type.
  , resourceTypes :: Core.Maybe [Types.ResourceType]
    -- ^ The template resource types that you have permissions to work with for this update stack action, such as @AWS::EC2::Instance@ , @AWS::EC2::*@ , or @Custom::MyCustomInstance@ .
--
-- If the list of resource types doesn't include a resource that you're updating, the stack update fails. By default, AWS CloudFormation grants permissions to all resource types. AWS Identity and Access Management (IAM) uses this parameter for AWS CloudFormation-specific condition keys in IAM policies. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html Controlling Access with AWS Identity and Access Management> .
  , roleARN :: Core.Maybe Types.RoleARN
    -- ^ The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that AWS CloudFormation assumes to update the stack. AWS CloudFormation uses the role's credentials to make calls on your behalf. AWS CloudFormation always uses this role for all future operations on the stack. As long as users have permission to operate on the stack, AWS CloudFormation uses this role even if the users don't have permission to pass it. Ensure that the role grants least privilege.
--
-- If you don't specify a value, AWS CloudFormation uses the role that was previously associated with the stack. If no role is available, AWS CloudFormation uses a temporary session that is generated from your user credentials.
  , rollbackConfiguration :: Core.Maybe Types.RollbackConfiguration
    -- ^ The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
  , stackPolicyBody :: Core.Maybe Types.StackPolicyBody
    -- ^ Structure containing a new stack policy body. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
--
-- You might update the stack policy, for example, in order to protect a new resource that you created during a stack update. If you do not specify a stack policy, the current policy that is associated with the stack is unchanged.
  , stackPolicyDuringUpdateBody :: Core.Maybe Types.StackPolicyDuringUpdateBody
    -- ^ Structure containing the temporary overriding stack policy body. You can specify either the @StackPolicyDuringUpdateBody@ or the @StackPolicyDuringUpdateURL@ parameter, but not both.
--
-- If you want to update protected resources, specify a temporary overriding stack policy during this update. If you do not specify a stack policy, the current policy that is associated with the stack will be used.
  , stackPolicyDuringUpdateURL :: Core.Maybe Types.StackPolicyDuringUpdateURL
    -- ^ Location of a file containing the temporary overriding stack policy. The URL must point to a policy (max size: 16KB) located in an S3 bucket in the same Region as the stack. You can specify either the @StackPolicyDuringUpdateBody@ or the @StackPolicyDuringUpdateURL@ parameter, but not both.
--
-- If you want to update protected resources, specify a temporary overriding stack policy during this update. If you do not specify a stack policy, the current policy that is associated with the stack will be used.
  , stackPolicyURL :: Core.Maybe Types.StackPolicyURL
    -- ^ Location of a file containing the updated stack policy. The URL must point to a policy (max size: 16KB) located in an S3 bucket in the same Region as the stack. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
--
-- You might update the stack policy, for example, in order to protect a new resource that you created during a stack update. If you do not specify a stack policy, the current policy that is associated with the stack is unchanged.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Key-value pairs to associate with this stack. AWS CloudFormation also propagates these tags to supported resources in the stack. You can specify a maximum number of 50 tags.
--
-- If you don't specify this parameter, AWS CloudFormation doesn't modify the stack's tags. If you specify an empty value, AWS CloudFormation removes all associated tags.
  , templateBody :: Core.Maybe Types.TemplateBody
    -- ^ Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.)
--
-- Conditional: You must specify only one of the following parameters: @TemplateBody@ , @TemplateURL@ , or set the @UsePreviousTemplate@ to @true@ .
  , templateURL :: Core.Maybe Types.TemplateURL
    -- ^ Location of file containing the template body. The URL must point to a template that is located in an Amazon S3 bucket. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters: @TemplateBody@ , @TemplateURL@ , or set the @UsePreviousTemplate@ to @true@ .
  , usePreviousTemplate :: Core.Maybe Core.Bool
    -- ^ Reuse the existing template that is associated with the stack that you are updating.
--
-- Conditional: You must specify only one of the following parameters: @TemplateBody@ , @TemplateURL@ , or set the @UsePreviousTemplate@ to @true@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateStack' value with any optional fields omitted.
mkUpdateStack
    :: Types.StackName -- ^ 'stackName'
    -> UpdateStack
mkUpdateStack stackName
  = UpdateStack'{stackName, capabilities = Core.Nothing,
                 clientRequestToken = Core.Nothing, notificationARNs = Core.Nothing,
                 parameters = Core.Nothing, resourceTypes = Core.Nothing,
                 roleARN = Core.Nothing, rollbackConfiguration = Core.Nothing,
                 stackPolicyBody = Core.Nothing,
                 stackPolicyDuringUpdateBody = Core.Nothing,
                 stackPolicyDuringUpdateURL = Core.Nothing,
                 stackPolicyURL = Core.Nothing, tags = Core.Nothing,
                 templateBody = Core.Nothing, templateURL = Core.Nothing,
                 usePreviousTemplate = Core.Nothing}

-- | The name or unique stack ID of the stack to update.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usStackName :: Lens.Lens' UpdateStack Types.StackName
usStackName = Lens.field @"stackName"
{-# INLINEABLE usStackName #-}
{-# DEPRECATED stackName "Use generic-lens or generic-optics with 'stackName' instead"  #-}

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
usCapabilities :: Lens.Lens' UpdateStack (Core.Maybe [Types.Capability])
usCapabilities = Lens.field @"capabilities"
{-# INLINEABLE usCapabilities #-}
{-# DEPRECATED capabilities "Use generic-lens or generic-optics with 'capabilities' instead"  #-}

-- | A unique identifier for this @UpdateStack@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to update a stack with the same name. You might retry @UpdateStack@ requests to ensure that AWS CloudFormation successfully received them.
--
-- All events triggered by a given stack operation are assigned the same client request token, which you can use to track operations. For example, if you execute a @CreateStack@ operation with the token @token1@ , then all the @StackEvents@ generated by that operation will have @ClientRequestToken@ set as @token1@ .
-- In the console, stack operations display the client request token on the Events tab. Stack operations that are initiated from the console use the token format /Console-StackOperation-ID/ , which helps you easily identify the stack operation . For example, if you create a stack using the console, each stack event would be assigned the same token in the following format: @Console-CreateStack-7f59c3cf-00d2-40c7-b2ff-e75db0987002@ . 
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usClientRequestToken :: Lens.Lens' UpdateStack (Core.Maybe Types.ClientRequestToken)
usClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE usClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | Amazon Simple Notification Service topic Amazon Resource Names (ARNs) that AWS CloudFormation associates with the stack. Specify an empty list to remove all notification topics.
--
-- /Note:/ Consider using 'notificationARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usNotificationARNs :: Lens.Lens' UpdateStack (Core.Maybe [Types.NotificationARN])
usNotificationARNs = Lens.field @"notificationARNs"
{-# INLINEABLE usNotificationARNs #-}
{-# DEPRECATED notificationARNs "Use generic-lens or generic-optics with 'notificationARNs' instead"  #-}

-- | A list of @Parameter@ structures that specify input parameters for the stack. For more information, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter> data type.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usParameters :: Lens.Lens' UpdateStack (Core.Maybe [Types.Parameter])
usParameters = Lens.field @"parameters"
{-# INLINEABLE usParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | The template resource types that you have permissions to work with for this update stack action, such as @AWS::EC2::Instance@ , @AWS::EC2::*@ , or @Custom::MyCustomInstance@ .
--
-- If the list of resource types doesn't include a resource that you're updating, the stack update fails. By default, AWS CloudFormation grants permissions to all resource types. AWS Identity and Access Management (IAM) uses this parameter for AWS CloudFormation-specific condition keys in IAM policies. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html Controlling Access with AWS Identity and Access Management> .
--
-- /Note:/ Consider using 'resourceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usResourceTypes :: Lens.Lens' UpdateStack (Core.Maybe [Types.ResourceType])
usResourceTypes = Lens.field @"resourceTypes"
{-# INLINEABLE usResourceTypes #-}
{-# DEPRECATED resourceTypes "Use generic-lens or generic-optics with 'resourceTypes' instead"  #-}

-- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that AWS CloudFormation assumes to update the stack. AWS CloudFormation uses the role's credentials to make calls on your behalf. AWS CloudFormation always uses this role for all future operations on the stack. As long as users have permission to operate on the stack, AWS CloudFormation uses this role even if the users don't have permission to pass it. Ensure that the role grants least privilege.
--
-- If you don't specify a value, AWS CloudFormation uses the role that was previously associated with the stack. If no role is available, AWS CloudFormation uses a temporary session that is generated from your user credentials.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usRoleARN :: Lens.Lens' UpdateStack (Core.Maybe Types.RoleARN)
usRoleARN = Lens.field @"roleARN"
{-# INLINEABLE usRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

-- | The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
--
-- /Note:/ Consider using 'rollbackConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usRollbackConfiguration :: Lens.Lens' UpdateStack (Core.Maybe Types.RollbackConfiguration)
usRollbackConfiguration = Lens.field @"rollbackConfiguration"
{-# INLINEABLE usRollbackConfiguration #-}
{-# DEPRECATED rollbackConfiguration "Use generic-lens or generic-optics with 'rollbackConfiguration' instead"  #-}

-- | Structure containing a new stack policy body. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
--
-- You might update the stack policy, for example, in order to protect a new resource that you created during a stack update. If you do not specify a stack policy, the current policy that is associated with the stack is unchanged.
--
-- /Note:/ Consider using 'stackPolicyBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usStackPolicyBody :: Lens.Lens' UpdateStack (Core.Maybe Types.StackPolicyBody)
usStackPolicyBody = Lens.field @"stackPolicyBody"
{-# INLINEABLE usStackPolicyBody #-}
{-# DEPRECATED stackPolicyBody "Use generic-lens or generic-optics with 'stackPolicyBody' instead"  #-}

-- | Structure containing the temporary overriding stack policy body. You can specify either the @StackPolicyDuringUpdateBody@ or the @StackPolicyDuringUpdateURL@ parameter, but not both.
--
-- If you want to update protected resources, specify a temporary overriding stack policy during this update. If you do not specify a stack policy, the current policy that is associated with the stack will be used.
--
-- /Note:/ Consider using 'stackPolicyDuringUpdateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usStackPolicyDuringUpdateBody :: Lens.Lens' UpdateStack (Core.Maybe Types.StackPolicyDuringUpdateBody)
usStackPolicyDuringUpdateBody = Lens.field @"stackPolicyDuringUpdateBody"
{-# INLINEABLE usStackPolicyDuringUpdateBody #-}
{-# DEPRECATED stackPolicyDuringUpdateBody "Use generic-lens or generic-optics with 'stackPolicyDuringUpdateBody' instead"  #-}

-- | Location of a file containing the temporary overriding stack policy. The URL must point to a policy (max size: 16KB) located in an S3 bucket in the same Region as the stack. You can specify either the @StackPolicyDuringUpdateBody@ or the @StackPolicyDuringUpdateURL@ parameter, but not both.
--
-- If you want to update protected resources, specify a temporary overriding stack policy during this update. If you do not specify a stack policy, the current policy that is associated with the stack will be used.
--
-- /Note:/ Consider using 'stackPolicyDuringUpdateURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usStackPolicyDuringUpdateURL :: Lens.Lens' UpdateStack (Core.Maybe Types.StackPolicyDuringUpdateURL)
usStackPolicyDuringUpdateURL = Lens.field @"stackPolicyDuringUpdateURL"
{-# INLINEABLE usStackPolicyDuringUpdateURL #-}
{-# DEPRECATED stackPolicyDuringUpdateURL "Use generic-lens or generic-optics with 'stackPolicyDuringUpdateURL' instead"  #-}

-- | Location of a file containing the updated stack policy. The URL must point to a policy (max size: 16KB) located in an S3 bucket in the same Region as the stack. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
--
-- You might update the stack policy, for example, in order to protect a new resource that you created during a stack update. If you do not specify a stack policy, the current policy that is associated with the stack is unchanged.
--
-- /Note:/ Consider using 'stackPolicyURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usStackPolicyURL :: Lens.Lens' UpdateStack (Core.Maybe Types.StackPolicyURL)
usStackPolicyURL = Lens.field @"stackPolicyURL"
{-# INLINEABLE usStackPolicyURL #-}
{-# DEPRECATED stackPolicyURL "Use generic-lens or generic-optics with 'stackPolicyURL' instead"  #-}

-- | Key-value pairs to associate with this stack. AWS CloudFormation also propagates these tags to supported resources in the stack. You can specify a maximum number of 50 tags.
--
-- If you don't specify this parameter, AWS CloudFormation doesn't modify the stack's tags. If you specify an empty value, AWS CloudFormation removes all associated tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usTags :: Lens.Lens' UpdateStack (Core.Maybe [Types.Tag])
usTags = Lens.field @"tags"
{-# INLINEABLE usTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.)
--
-- Conditional: You must specify only one of the following parameters: @TemplateBody@ , @TemplateURL@ , or set the @UsePreviousTemplate@ to @true@ .
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usTemplateBody :: Lens.Lens' UpdateStack (Core.Maybe Types.TemplateBody)
usTemplateBody = Lens.field @"templateBody"
{-# INLINEABLE usTemplateBody #-}
{-# DEPRECATED templateBody "Use generic-lens or generic-optics with 'templateBody' instead"  #-}

-- | Location of file containing the template body. The URL must point to a template that is located in an Amazon S3 bucket. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters: @TemplateBody@ , @TemplateURL@ , or set the @UsePreviousTemplate@ to @true@ .
--
-- /Note:/ Consider using 'templateURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usTemplateURL :: Lens.Lens' UpdateStack (Core.Maybe Types.TemplateURL)
usTemplateURL = Lens.field @"templateURL"
{-# INLINEABLE usTemplateURL #-}
{-# DEPRECATED templateURL "Use generic-lens or generic-optics with 'templateURL' instead"  #-}

-- | Reuse the existing template that is associated with the stack that you are updating.
--
-- Conditional: You must specify only one of the following parameters: @TemplateBody@ , @TemplateURL@ , or set the @UsePreviousTemplate@ to @true@ .
--
-- /Note:/ Consider using 'usePreviousTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usUsePreviousTemplate :: Lens.Lens' UpdateStack (Core.Maybe Core.Bool)
usUsePreviousTemplate = Lens.field @"usePreviousTemplate"
{-# INLINEABLE usUsePreviousTemplate #-}
{-# DEPRECATED usePreviousTemplate "Use generic-lens or generic-optics with 'usePreviousTemplate' instead"  #-}

instance Core.ToQuery UpdateStack where
        toQuery UpdateStack{..}
          = Core.toQueryPair "Action" ("UpdateStack" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-05-15" :: Core.Text)
              Core.<> Core.toQueryPair "StackName" stackName
              Core.<>
              Core.toQueryPair "Capabilities"
                (Core.maybe Core.mempty (Core.toQueryList "member") capabilities)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientRequestToken")
                clientRequestToken
              Core.<>
              Core.toQueryPair "NotificationARNs"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   notificationARNs)
              Core.<>
              Core.toQueryPair "Parameters"
                (Core.maybe Core.mempty (Core.toQueryList "member") parameters)
              Core.<>
              Core.toQueryPair "ResourceTypes"
                (Core.maybe Core.mempty (Core.toQueryList "member") resourceTypes)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "RoleARN") roleARN
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "RollbackConfiguration")
                rollbackConfiguration
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "StackPolicyBody")
                stackPolicyBody
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "StackPolicyDuringUpdateBody")
                stackPolicyDuringUpdateBody
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "StackPolicyDuringUpdateURL")
                stackPolicyDuringUpdateURL
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "StackPolicyURL")
                stackPolicyURL
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

instance Core.ToHeaders UpdateStack where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest UpdateStack where
        type Rs UpdateStack = UpdateStackResponse
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
          = Response.receiveXMLWrapper "UpdateStackResult"
              (\ s h x ->
                 UpdateStackResponse' Core.<$>
                   (x Core..@? "StackId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The output for an 'UpdateStack' action.
--
-- /See:/ 'mkUpdateStackResponse' smart constructor.
data UpdateStackResponse = UpdateStackResponse'
  { stackId :: Core.Maybe Types.StackId
    -- ^ Unique identifier of the stack.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateStackResponse' value with any optional fields omitted.
mkUpdateStackResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateStackResponse
mkUpdateStackResponse responseStatus
  = UpdateStackResponse'{stackId = Core.Nothing, responseStatus}

-- | Unique identifier of the stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsStackId :: Lens.Lens' UpdateStackResponse (Core.Maybe Types.StackId)
usrrsStackId = Lens.field @"stackId"
{-# INLINEABLE usrrsStackId #-}
{-# DEPRECATED stackId "Use generic-lens or generic-optics with 'stackId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsResponseStatus :: Lens.Lens' UpdateStackResponse Core.Int
usrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE usrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
