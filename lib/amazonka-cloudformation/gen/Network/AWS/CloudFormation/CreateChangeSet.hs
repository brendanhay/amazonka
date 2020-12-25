{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.CreateChangeSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a list of changes that will be applied to a stack so that you can review the changes before executing them. You can create a change set for a stack that doesn't exist or an existing stack. If you create a change set for a stack that doesn't exist, the change set shows all of the resources that AWS CloudFormation will create. If you create a change set for an existing stack, AWS CloudFormation compares the stack's information with the information that you submit in the change set and lists the differences. Use change sets to understand which resources AWS CloudFormation will create or change, and how it will change resources in an existing stack, before you create or update a stack.
--
-- To create a change set for a stack that doesn't exist, for the @ChangeSetType@ parameter, specify @CREATE@ . To create a change set for an existing stack, specify @UPDATE@ for the @ChangeSetType@ parameter. To create a change set for an import operation, specify @IMPORT@ for the @ChangeSetType@ parameter. After the @CreateChangeSet@ call successfully completes, AWS CloudFormation starts creating the change set. To check the status of the change set or to review it, use the 'DescribeChangeSet' action.
-- When you are satisfied with the changes the change set will make, execute the change set by using the 'ExecuteChangeSet' action. AWS CloudFormation doesn't make changes until you execute the change set.
-- To create a change set for the entire stack hierachy, set @IncludeNestedStacks@ to @True@ .
module Network.AWS.CloudFormation.CreateChangeSet
  ( -- * Creating a request
    CreateChangeSet (..),
    mkCreateChangeSet,

    -- ** Request lenses
    ccsStackName,
    ccsChangeSetName,
    ccsCapabilities,
    ccsChangeSetType,
    ccsClientToken,
    ccsDescription,
    ccsIncludeNestedStacks,
    ccsNotificationARNs,
    ccsParameters,
    ccsResourceTypes,
    ccsResourcesToImport,
    ccsRoleARN,
    ccsRollbackConfiguration,
    ccsTags,
    ccsTemplateBody,
    ccsTemplateURL,
    ccsUsePreviousTemplate,

    -- * Destructuring the response
    CreateChangeSetResponse (..),
    mkCreateChangeSetResponse,

    -- ** Response lenses
    ccsrrsId,
    ccsrrsStackId,
    ccsrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the 'CreateChangeSet' action.
--
-- /See:/ 'mkCreateChangeSet' smart constructor.
data CreateChangeSet = CreateChangeSet'
  { -- | The name or the unique ID of the stack for which you are creating a change set. AWS CloudFormation generates the change set by comparing this stack's information with the information that you submit, such as a modified template or different parameter input values.
    stackName :: Types.StackName,
    -- | The name of the change set. The name must be unique among all change sets that are associated with the specified stack.
    --
    -- A change set name can contain only alphanumeric, case sensitive characters and hyphens. It must start with an alphabetic character and cannot exceed 128 characters.
    changeSetName :: Types.ChangeSetName,
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
    -- For more information on macros, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-macros.html Using AWS CloudFormation Macros to Perform Custom Processing on Templates> .
    capabilities :: Core.Maybe [Types.Capability],
    -- | The type of change set operation. To create a change set for a new stack, specify @CREATE@ . To create a change set for an existing stack, specify @UPDATE@ . To create a change set for an import operation, specify @IMPORT@ .
    --
    -- If you create a change set for a new stack, AWS Cloudformation creates a stack with a unique stack ID, but no template or resources. The stack will be in the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-describing-stacks.html#d0e11995 @REVIEW_IN_PROGRESS@ > state until you execute the change set.
    -- By default, AWS CloudFormation specifies @UPDATE@ . You can't use the @UPDATE@ type to create a change set for a new stack or the @CREATE@ type to create a change set for an existing stack.
    changeSetType :: Core.Maybe Types.ChangeSetType,
    -- | A unique identifier for this @CreateChangeSet@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to create another change set with the same name. You might retry @CreateChangeSet@ requests to ensure that AWS CloudFormation successfully received them.
    clientToken :: Core.Maybe Types.ClientToken,
    -- | A description to help you identify this change set.
    description :: Core.Maybe Types.Description,
    -- | Creates a change set for the all nested stacks specified in the template. The default behavior of this action is set to @False@ . To include nested sets in a change set, specify @True@ .
    includeNestedStacks :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Names (ARNs) of Amazon Simple Notification Service (Amazon SNS) topics that AWS CloudFormation associates with the stack. To remove all associated notification topics, specify an empty list.
    notificationARNs :: Core.Maybe [Types.NotificationARN],
    -- | A list of @Parameter@ structures that specify input parameters for the change set. For more information, see the 'Parameter' data type.
    parameters :: Core.Maybe [Types.Parameter],
    -- | The template resource types that you have permissions to work with if you execute this change set, such as @AWS::EC2::Instance@ , @AWS::EC2::*@ , or @Custom::MyCustomInstance@ .
    --
    -- If the list of resource types doesn't include a resource type that you're updating, the stack update fails. By default, AWS CloudFormation grants permissions to all resource types. AWS Identity and Access Management (IAM) uses this parameter for condition keys in IAM policies for AWS CloudFormation. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html Controlling Access with AWS Identity and Access Management> in the AWS CloudFormation User Guide.
    resourceTypes :: Core.Maybe [Types.ResourceType],
    -- | The resources to import into your stack.
    resourcesToImport :: Core.Maybe [Types.ResourceToImport],
    -- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that AWS CloudFormation assumes when executing the change set. AWS CloudFormation uses the role's credentials to make calls on your behalf. AWS CloudFormation uses this role for all future operations on the stack. As long as users have permission to operate on the stack, AWS CloudFormation uses this role even if the users don't have permission to pass it. Ensure that the role grants least privilege.
    --
    -- If you don't specify a value, AWS CloudFormation uses the role that was previously associated with the stack. If no role is available, AWS CloudFormation uses a temporary session that is generated from your user credentials.
    roleARN :: Core.Maybe Types.RoleARN,
    -- | The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
    rollbackConfiguration :: Core.Maybe Types.RollbackConfiguration,
    -- | Key-value pairs to associate with this stack. AWS CloudFormation also propagates these tags to resources in the stack. You can specify a maximum of 50 tags.
    tags :: Core.Maybe [Types.Tag],
    -- | A structure that contains the body of the revised template, with a minimum length of 1 byte and a maximum length of 51,200 bytes. AWS CloudFormation generates the change set by comparing this template with the template of the stack that you specified.
    --
    -- Conditional: You must specify only @TemplateBody@ or @TemplateURL@ .
    templateBody :: Core.Maybe Types.TemplateBody,
    -- | The location of the file that contains the revised template. The URL must point to a template (max size: 460,800 bytes) that is located in an S3 bucket. AWS CloudFormation generates the change set by comparing this template with the stack that you specified.
    --
    -- Conditional: You must specify only @TemplateBody@ or @TemplateURL@ .
    templateURL :: Core.Maybe Types.TemplateURL,
    -- | Whether to reuse the template that is associated with the stack to create the change set.
    usePreviousTemplate :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateChangeSet' value with any optional fields omitted.
mkCreateChangeSet ::
  -- | 'stackName'
  Types.StackName ->
  -- | 'changeSetName'
  Types.ChangeSetName ->
  CreateChangeSet
mkCreateChangeSet stackName changeSetName =
  CreateChangeSet'
    { stackName,
      changeSetName,
      capabilities = Core.Nothing,
      changeSetType = Core.Nothing,
      clientToken = Core.Nothing,
      description = Core.Nothing,
      includeNestedStacks = Core.Nothing,
      notificationARNs = Core.Nothing,
      parameters = Core.Nothing,
      resourceTypes = Core.Nothing,
      resourcesToImport = Core.Nothing,
      roleARN = Core.Nothing,
      rollbackConfiguration = Core.Nothing,
      tags = Core.Nothing,
      templateBody = Core.Nothing,
      templateURL = Core.Nothing,
      usePreviousTemplate = Core.Nothing
    }

-- | The name or the unique ID of the stack for which you are creating a change set. AWS CloudFormation generates the change set by comparing this stack's information with the information that you submit, such as a modified template or different parameter input values.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsStackName :: Lens.Lens' CreateChangeSet Types.StackName
ccsStackName = Lens.field @"stackName"
{-# DEPRECATED ccsStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

-- | The name of the change set. The name must be unique among all change sets that are associated with the specified stack.
--
-- A change set name can contain only alphanumeric, case sensitive characters and hyphens. It must start with an alphabetic character and cannot exceed 128 characters.
--
-- /Note:/ Consider using 'changeSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsChangeSetName :: Lens.Lens' CreateChangeSet Types.ChangeSetName
ccsChangeSetName = Lens.field @"changeSetName"
{-# DEPRECATED ccsChangeSetName "Use generic-lens or generic-optics with 'changeSetName' instead." #-}

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
-- For more information on macros, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-macros.html Using AWS CloudFormation Macros to Perform Custom Processing on Templates> .
--
--
--
-- /Note:/ Consider using 'capabilities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsCapabilities :: Lens.Lens' CreateChangeSet (Core.Maybe [Types.Capability])
ccsCapabilities = Lens.field @"capabilities"
{-# DEPRECATED ccsCapabilities "Use generic-lens or generic-optics with 'capabilities' instead." #-}

-- | The type of change set operation. To create a change set for a new stack, specify @CREATE@ . To create a change set for an existing stack, specify @UPDATE@ . To create a change set for an import operation, specify @IMPORT@ .
--
-- If you create a change set for a new stack, AWS Cloudformation creates a stack with a unique stack ID, but no template or resources. The stack will be in the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-describing-stacks.html#d0e11995 @REVIEW_IN_PROGRESS@ > state until you execute the change set.
-- By default, AWS CloudFormation specifies @UPDATE@ . You can't use the @UPDATE@ type to create a change set for a new stack or the @CREATE@ type to create a change set for an existing stack.
--
-- /Note:/ Consider using 'changeSetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsChangeSetType :: Lens.Lens' CreateChangeSet (Core.Maybe Types.ChangeSetType)
ccsChangeSetType = Lens.field @"changeSetType"
{-# DEPRECATED ccsChangeSetType "Use generic-lens or generic-optics with 'changeSetType' instead." #-}

-- | A unique identifier for this @CreateChangeSet@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to create another change set with the same name. You might retry @CreateChangeSet@ requests to ensure that AWS CloudFormation successfully received them.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsClientToken :: Lens.Lens' CreateChangeSet (Core.Maybe Types.ClientToken)
ccsClientToken = Lens.field @"clientToken"
{-# DEPRECATED ccsClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | A description to help you identify this change set.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsDescription :: Lens.Lens' CreateChangeSet (Core.Maybe Types.Description)
ccsDescription = Lens.field @"description"
{-# DEPRECATED ccsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Creates a change set for the all nested stacks specified in the template. The default behavior of this action is set to @False@ . To include nested sets in a change set, specify @True@ .
--
-- /Note:/ Consider using 'includeNestedStacks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsIncludeNestedStacks :: Lens.Lens' CreateChangeSet (Core.Maybe Core.Bool)
ccsIncludeNestedStacks = Lens.field @"includeNestedStacks"
{-# DEPRECATED ccsIncludeNestedStacks "Use generic-lens or generic-optics with 'includeNestedStacks' instead." #-}

-- | The Amazon Resource Names (ARNs) of Amazon Simple Notification Service (Amazon SNS) topics that AWS CloudFormation associates with the stack. To remove all associated notification topics, specify an empty list.
--
-- /Note:/ Consider using 'notificationARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsNotificationARNs :: Lens.Lens' CreateChangeSet (Core.Maybe [Types.NotificationARN])
ccsNotificationARNs = Lens.field @"notificationARNs"
{-# DEPRECATED ccsNotificationARNs "Use generic-lens or generic-optics with 'notificationARNs' instead." #-}

-- | A list of @Parameter@ structures that specify input parameters for the change set. For more information, see the 'Parameter' data type.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsParameters :: Lens.Lens' CreateChangeSet (Core.Maybe [Types.Parameter])
ccsParameters = Lens.field @"parameters"
{-# DEPRECATED ccsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The template resource types that you have permissions to work with if you execute this change set, such as @AWS::EC2::Instance@ , @AWS::EC2::*@ , or @Custom::MyCustomInstance@ .
--
-- If the list of resource types doesn't include a resource type that you're updating, the stack update fails. By default, AWS CloudFormation grants permissions to all resource types. AWS Identity and Access Management (IAM) uses this parameter for condition keys in IAM policies for AWS CloudFormation. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html Controlling Access with AWS Identity and Access Management> in the AWS CloudFormation User Guide.
--
-- /Note:/ Consider using 'resourceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsResourceTypes :: Lens.Lens' CreateChangeSet (Core.Maybe [Types.ResourceType])
ccsResourceTypes = Lens.field @"resourceTypes"
{-# DEPRECATED ccsResourceTypes "Use generic-lens or generic-optics with 'resourceTypes' instead." #-}

-- | The resources to import into your stack.
--
-- /Note:/ Consider using 'resourcesToImport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsResourcesToImport :: Lens.Lens' CreateChangeSet (Core.Maybe [Types.ResourceToImport])
ccsResourcesToImport = Lens.field @"resourcesToImport"
{-# DEPRECATED ccsResourcesToImport "Use generic-lens or generic-optics with 'resourcesToImport' instead." #-}

-- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that AWS CloudFormation assumes when executing the change set. AWS CloudFormation uses the role's credentials to make calls on your behalf. AWS CloudFormation uses this role for all future operations on the stack. As long as users have permission to operate on the stack, AWS CloudFormation uses this role even if the users don't have permission to pass it. Ensure that the role grants least privilege.
--
-- If you don't specify a value, AWS CloudFormation uses the role that was previously associated with the stack. If no role is available, AWS CloudFormation uses a temporary session that is generated from your user credentials.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsRoleARN :: Lens.Lens' CreateChangeSet (Core.Maybe Types.RoleARN)
ccsRoleARN = Lens.field @"roleARN"
{-# DEPRECATED ccsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
--
-- /Note:/ Consider using 'rollbackConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsRollbackConfiguration :: Lens.Lens' CreateChangeSet (Core.Maybe Types.RollbackConfiguration)
ccsRollbackConfiguration = Lens.field @"rollbackConfiguration"
{-# DEPRECATED ccsRollbackConfiguration "Use generic-lens or generic-optics with 'rollbackConfiguration' instead." #-}

-- | Key-value pairs to associate with this stack. AWS CloudFormation also propagates these tags to resources in the stack. You can specify a maximum of 50 tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsTags :: Lens.Lens' CreateChangeSet (Core.Maybe [Types.Tag])
ccsTags = Lens.field @"tags"
{-# DEPRECATED ccsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A structure that contains the body of the revised template, with a minimum length of 1 byte and a maximum length of 51,200 bytes. AWS CloudFormation generates the change set by comparing this template with the template of the stack that you specified.
--
-- Conditional: You must specify only @TemplateBody@ or @TemplateURL@ .
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsTemplateBody :: Lens.Lens' CreateChangeSet (Core.Maybe Types.TemplateBody)
ccsTemplateBody = Lens.field @"templateBody"
{-# DEPRECATED ccsTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | The location of the file that contains the revised template. The URL must point to a template (max size: 460,800 bytes) that is located in an S3 bucket. AWS CloudFormation generates the change set by comparing this template with the stack that you specified.
--
-- Conditional: You must specify only @TemplateBody@ or @TemplateURL@ .
--
-- /Note:/ Consider using 'templateURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsTemplateURL :: Lens.Lens' CreateChangeSet (Core.Maybe Types.TemplateURL)
ccsTemplateURL = Lens.field @"templateURL"
{-# DEPRECATED ccsTemplateURL "Use generic-lens or generic-optics with 'templateURL' instead." #-}

-- | Whether to reuse the template that is associated with the stack to create the change set.
--
-- /Note:/ Consider using 'usePreviousTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsUsePreviousTemplate :: Lens.Lens' CreateChangeSet (Core.Maybe Core.Bool)
ccsUsePreviousTemplate = Lens.field @"usePreviousTemplate"
{-# DEPRECATED ccsUsePreviousTemplate "Use generic-lens or generic-optics with 'usePreviousTemplate' instead." #-}

instance Core.AWSRequest CreateChangeSet where
  type Rs CreateChangeSet = CreateChangeSetResponse
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
            ( Core.pure ("Action", "CreateChangeSet")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "StackName" stackName)
                Core.<> (Core.toQueryValue "ChangeSetName" changeSetName)
                Core.<> ( Core.toQueryValue
                            "Capabilities"
                            (Core.toQueryList "member" Core.<$> capabilities)
                        )
                Core.<> (Core.toQueryValue "ChangeSetType" Core.<$> changeSetType)
                Core.<> (Core.toQueryValue "ClientToken" Core.<$> clientToken)
                Core.<> (Core.toQueryValue "Description" Core.<$> description)
                Core.<> ( Core.toQueryValue "IncludeNestedStacks"
                            Core.<$> includeNestedStacks
                        )
                Core.<> ( Core.toQueryValue
                            "NotificationARNs"
                            (Core.toQueryList "member" Core.<$> notificationARNs)
                        )
                Core.<> ( Core.toQueryValue
                            "Parameters"
                            (Core.toQueryList "member" Core.<$> parameters)
                        )
                Core.<> ( Core.toQueryValue
                            "ResourceTypes"
                            (Core.toQueryList "member" Core.<$> resourceTypes)
                        )
                Core.<> ( Core.toQueryValue
                            "ResourcesToImport"
                            (Core.toQueryList "member" Core.<$> resourcesToImport)
                        )
                Core.<> (Core.toQueryValue "RoleARN" Core.<$> roleARN)
                Core.<> ( Core.toQueryValue "RollbackConfiguration"
                            Core.<$> rollbackConfiguration
                        )
                Core.<> ( Core.toQueryValue
                            "Tags"
                            (Core.toQueryList "member" Core.<$> tags)
                        )
                Core.<> (Core.toQueryValue "TemplateBody" Core.<$> templateBody)
                Core.<> (Core.toQueryValue "TemplateURL" Core.<$> templateURL)
                Core.<> ( Core.toQueryValue "UsePreviousTemplate"
                            Core.<$> usePreviousTemplate
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateChangeSetResult"
      ( \s h x ->
          CreateChangeSetResponse'
            Core.<$> (x Core..@? "Id")
            Core.<*> (x Core..@? "StackId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The output for the 'CreateChangeSet' action.
--
-- /See:/ 'mkCreateChangeSetResponse' smart constructor.
data CreateChangeSetResponse = CreateChangeSetResponse'
  { -- | The Amazon Resource Name (ARN) of the change set.
    id :: Core.Maybe Types.ChangeSetId,
    -- | The unique ID of the stack.
    stackId :: Core.Maybe Types.StackId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateChangeSetResponse' value with any optional fields omitted.
mkCreateChangeSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateChangeSetResponse
mkCreateChangeSetResponse responseStatus =
  CreateChangeSetResponse'
    { id = Core.Nothing,
      stackId = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the change set.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsrrsId :: Lens.Lens' CreateChangeSetResponse (Core.Maybe Types.ChangeSetId)
ccsrrsId = Lens.field @"id"
{-# DEPRECATED ccsrrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The unique ID of the stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsrrsStackId :: Lens.Lens' CreateChangeSetResponse (Core.Maybe Types.StackId)
ccsrrsStackId = Lens.field @"stackId"
{-# DEPRECATED ccsrrsStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsrrsResponseStatus :: Lens.Lens' CreateChangeSetResponse Core.Int
ccsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
