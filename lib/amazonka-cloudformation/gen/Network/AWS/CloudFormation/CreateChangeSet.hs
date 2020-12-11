{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    ccsChangeSetType,
    ccsUsePreviousTemplate,
    ccsClientToken,
    ccsNotificationARNs,
    ccsIncludeNestedStacks,
    ccsResourcesToImport,
    ccsParameters,
    ccsTemplateBody,
    ccsTemplateURL,
    ccsDescription,
    ccsCapabilities,
    ccsRollbackConfiguration,
    ccsResourceTypes,
    ccsTags,
    ccsRoleARN,
    ccsStackName,
    ccsChangeSetName,

    -- * Destructuring the response
    CreateChangeSetResponse (..),
    mkCreateChangeSetResponse,

    -- ** Response lenses
    ccsrsId,
    ccsrsStackId,
    ccsrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the 'CreateChangeSet' action.
--
-- /See:/ 'mkCreateChangeSet' smart constructor.
data CreateChangeSet = CreateChangeSet'
  { changeSetType ::
      Lude.Maybe ChangeSetType,
    usePreviousTemplate :: Lude.Maybe Lude.Bool,
    clientToken :: Lude.Maybe Lude.Text,
    notificationARNs :: Lude.Maybe [Lude.Text],
    includeNestedStacks :: Lude.Maybe Lude.Bool,
    resourcesToImport :: Lude.Maybe [ResourceToImport],
    parameters :: Lude.Maybe [Parameter],
    templateBody :: Lude.Maybe Lude.Text,
    templateURL :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    capabilities :: Lude.Maybe [Capability],
    rollbackConfiguration :: Lude.Maybe RollbackConfiguration,
    resourceTypes :: Lude.Maybe [Lude.Text],
    tags :: Lude.Maybe [Tag],
    roleARN :: Lude.Maybe Lude.Text,
    stackName :: Lude.Text,
    changeSetName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateChangeSet' with the minimum fields required to make a request.
--
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
-- For more information on macros, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-macros.html Using AWS CloudFormation Macros to Perform Custom Processing on Templates> .
--
--
-- * 'changeSetName' - The name of the change set. The name must be unique among all change sets that are associated with the specified stack.
--
-- A change set name can contain only alphanumeric, case sensitive characters and hyphens. It must start with an alphabetic character and cannot exceed 128 characters.
-- * 'changeSetType' - The type of change set operation. To create a change set for a new stack, specify @CREATE@ . To create a change set for an existing stack, specify @UPDATE@ . To create a change set for an import operation, specify @IMPORT@ .
--
-- If you create a change set for a new stack, AWS Cloudformation creates a stack with a unique stack ID, but no template or resources. The stack will be in the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-describing-stacks.html#d0e11995 @REVIEW_IN_PROGRESS@ > state until you execute the change set.
-- By default, AWS CloudFormation specifies @UPDATE@ . You can't use the @UPDATE@ type to create a change set for a new stack or the @CREATE@ type to create a change set for an existing stack.
-- * 'clientToken' - A unique identifier for this @CreateChangeSet@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to create another change set with the same name. You might retry @CreateChangeSet@ requests to ensure that AWS CloudFormation successfully received them.
-- * 'description' - A description to help you identify this change set.
-- * 'includeNestedStacks' - Creates a change set for the all nested stacks specified in the template. The default behavior of this action is set to @False@ . To include nested sets in a change set, specify @True@ .
-- * 'notificationARNs' - The Amazon Resource Names (ARNs) of Amazon Simple Notification Service (Amazon SNS) topics that AWS CloudFormation associates with the stack. To remove all associated notification topics, specify an empty list.
-- * 'parameters' - A list of @Parameter@ structures that specify input parameters for the change set. For more information, see the 'Parameter' data type.
-- * 'resourceTypes' - The template resource types that you have permissions to work with if you execute this change set, such as @AWS::EC2::Instance@ , @AWS::EC2::*@ , or @Custom::MyCustomInstance@ .
--
-- If the list of resource types doesn't include a resource type that you're updating, the stack update fails. By default, AWS CloudFormation grants permissions to all resource types. AWS Identity and Access Management (IAM) uses this parameter for condition keys in IAM policies for AWS CloudFormation. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html Controlling Access with AWS Identity and Access Management> in the AWS CloudFormation User Guide.
-- * 'resourcesToImport' - The resources to import into your stack.
-- * 'roleARN' - The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that AWS CloudFormation assumes when executing the change set. AWS CloudFormation uses the role's credentials to make calls on your behalf. AWS CloudFormation uses this role for all future operations on the stack. As long as users have permission to operate on the stack, AWS CloudFormation uses this role even if the users don't have permission to pass it. Ensure that the role grants least privilege.
--
-- If you don't specify a value, AWS CloudFormation uses the role that was previously associated with the stack. If no role is available, AWS CloudFormation uses a temporary session that is generated from your user credentials.
-- * 'rollbackConfiguration' - The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
-- * 'stackName' - The name or the unique ID of the stack for which you are creating a change set. AWS CloudFormation generates the change set by comparing this stack's information with the information that you submit, such as a modified template or different parameter input values.
-- * 'tags' - Key-value pairs to associate with this stack. AWS CloudFormation also propagates these tags to resources in the stack. You can specify a maximum of 50 tags.
-- * 'templateBody' - A structure that contains the body of the revised template, with a minimum length of 1 byte and a maximum length of 51,200 bytes. AWS CloudFormation generates the change set by comparing this template with the template of the stack that you specified.
--
-- Conditional: You must specify only @TemplateBody@ or @TemplateURL@ .
-- * 'templateURL' - The location of the file that contains the revised template. The URL must point to a template (max size: 460,800 bytes) that is located in an S3 bucket. AWS CloudFormation generates the change set by comparing this template with the stack that you specified.
--
-- Conditional: You must specify only @TemplateBody@ or @TemplateURL@ .
-- * 'usePreviousTemplate' - Whether to reuse the template that is associated with the stack to create the change set.
mkCreateChangeSet ::
  -- | 'stackName'
  Lude.Text ->
  -- | 'changeSetName'
  Lude.Text ->
  CreateChangeSet
mkCreateChangeSet pStackName_ pChangeSetName_ =
  CreateChangeSet'
    { changeSetType = Lude.Nothing,
      usePreviousTemplate = Lude.Nothing,
      clientToken = Lude.Nothing,
      notificationARNs = Lude.Nothing,
      includeNestedStacks = Lude.Nothing,
      resourcesToImport = Lude.Nothing,
      parameters = Lude.Nothing,
      templateBody = Lude.Nothing,
      templateURL = Lude.Nothing,
      description = Lude.Nothing,
      capabilities = Lude.Nothing,
      rollbackConfiguration = Lude.Nothing,
      resourceTypes = Lude.Nothing,
      tags = Lude.Nothing,
      roleARN = Lude.Nothing,
      stackName = pStackName_,
      changeSetName = pChangeSetName_
    }

-- | The type of change set operation. To create a change set for a new stack, specify @CREATE@ . To create a change set for an existing stack, specify @UPDATE@ . To create a change set for an import operation, specify @IMPORT@ .
--
-- If you create a change set for a new stack, AWS Cloudformation creates a stack with a unique stack ID, but no template or resources. The stack will be in the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-describing-stacks.html#d0e11995 @REVIEW_IN_PROGRESS@ > state until you execute the change set.
-- By default, AWS CloudFormation specifies @UPDATE@ . You can't use the @UPDATE@ type to create a change set for a new stack or the @CREATE@ type to create a change set for an existing stack.
--
-- /Note:/ Consider using 'changeSetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsChangeSetType :: Lens.Lens' CreateChangeSet (Lude.Maybe ChangeSetType)
ccsChangeSetType = Lens.lens (changeSetType :: CreateChangeSet -> Lude.Maybe ChangeSetType) (\s a -> s {changeSetType = a} :: CreateChangeSet)
{-# DEPRECATED ccsChangeSetType "Use generic-lens or generic-optics with 'changeSetType' instead." #-}

-- | Whether to reuse the template that is associated with the stack to create the change set.
--
-- /Note:/ Consider using 'usePreviousTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsUsePreviousTemplate :: Lens.Lens' CreateChangeSet (Lude.Maybe Lude.Bool)
ccsUsePreviousTemplate = Lens.lens (usePreviousTemplate :: CreateChangeSet -> Lude.Maybe Lude.Bool) (\s a -> s {usePreviousTemplate = a} :: CreateChangeSet)
{-# DEPRECATED ccsUsePreviousTemplate "Use generic-lens or generic-optics with 'usePreviousTemplate' instead." #-}

-- | A unique identifier for this @CreateChangeSet@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to create another change set with the same name. You might retry @CreateChangeSet@ requests to ensure that AWS CloudFormation successfully received them.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsClientToken :: Lens.Lens' CreateChangeSet (Lude.Maybe Lude.Text)
ccsClientToken = Lens.lens (clientToken :: CreateChangeSet -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateChangeSet)
{-# DEPRECATED ccsClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The Amazon Resource Names (ARNs) of Amazon Simple Notification Service (Amazon SNS) topics that AWS CloudFormation associates with the stack. To remove all associated notification topics, specify an empty list.
--
-- /Note:/ Consider using 'notificationARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsNotificationARNs :: Lens.Lens' CreateChangeSet (Lude.Maybe [Lude.Text])
ccsNotificationARNs = Lens.lens (notificationARNs :: CreateChangeSet -> Lude.Maybe [Lude.Text]) (\s a -> s {notificationARNs = a} :: CreateChangeSet)
{-# DEPRECATED ccsNotificationARNs "Use generic-lens or generic-optics with 'notificationARNs' instead." #-}

-- | Creates a change set for the all nested stacks specified in the template. The default behavior of this action is set to @False@ . To include nested sets in a change set, specify @True@ .
--
-- /Note:/ Consider using 'includeNestedStacks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsIncludeNestedStacks :: Lens.Lens' CreateChangeSet (Lude.Maybe Lude.Bool)
ccsIncludeNestedStacks = Lens.lens (includeNestedStacks :: CreateChangeSet -> Lude.Maybe Lude.Bool) (\s a -> s {includeNestedStacks = a} :: CreateChangeSet)
{-# DEPRECATED ccsIncludeNestedStacks "Use generic-lens or generic-optics with 'includeNestedStacks' instead." #-}

-- | The resources to import into your stack.
--
-- /Note:/ Consider using 'resourcesToImport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsResourcesToImport :: Lens.Lens' CreateChangeSet (Lude.Maybe [ResourceToImport])
ccsResourcesToImport = Lens.lens (resourcesToImport :: CreateChangeSet -> Lude.Maybe [ResourceToImport]) (\s a -> s {resourcesToImport = a} :: CreateChangeSet)
{-# DEPRECATED ccsResourcesToImport "Use generic-lens or generic-optics with 'resourcesToImport' instead." #-}

-- | A list of @Parameter@ structures that specify input parameters for the change set. For more information, see the 'Parameter' data type.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsParameters :: Lens.Lens' CreateChangeSet (Lude.Maybe [Parameter])
ccsParameters = Lens.lens (parameters :: CreateChangeSet -> Lude.Maybe [Parameter]) (\s a -> s {parameters = a} :: CreateChangeSet)
{-# DEPRECATED ccsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | A structure that contains the body of the revised template, with a minimum length of 1 byte and a maximum length of 51,200 bytes. AWS CloudFormation generates the change set by comparing this template with the template of the stack that you specified.
--
-- Conditional: You must specify only @TemplateBody@ or @TemplateURL@ .
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsTemplateBody :: Lens.Lens' CreateChangeSet (Lude.Maybe Lude.Text)
ccsTemplateBody = Lens.lens (templateBody :: CreateChangeSet -> Lude.Maybe Lude.Text) (\s a -> s {templateBody = a} :: CreateChangeSet)
{-# DEPRECATED ccsTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | The location of the file that contains the revised template. The URL must point to a template (max size: 460,800 bytes) that is located in an S3 bucket. AWS CloudFormation generates the change set by comparing this template with the stack that you specified.
--
-- Conditional: You must specify only @TemplateBody@ or @TemplateURL@ .
--
-- /Note:/ Consider using 'templateURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsTemplateURL :: Lens.Lens' CreateChangeSet (Lude.Maybe Lude.Text)
ccsTemplateURL = Lens.lens (templateURL :: CreateChangeSet -> Lude.Maybe Lude.Text) (\s a -> s {templateURL = a} :: CreateChangeSet)
{-# DEPRECATED ccsTemplateURL "Use generic-lens or generic-optics with 'templateURL' instead." #-}

-- | A description to help you identify this change set.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsDescription :: Lens.Lens' CreateChangeSet (Lude.Maybe Lude.Text)
ccsDescription = Lens.lens (description :: CreateChangeSet -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateChangeSet)
{-# DEPRECATED ccsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

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
ccsCapabilities :: Lens.Lens' CreateChangeSet (Lude.Maybe [Capability])
ccsCapabilities = Lens.lens (capabilities :: CreateChangeSet -> Lude.Maybe [Capability]) (\s a -> s {capabilities = a} :: CreateChangeSet)
{-# DEPRECATED ccsCapabilities "Use generic-lens or generic-optics with 'capabilities' instead." #-}

-- | The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
--
-- /Note:/ Consider using 'rollbackConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsRollbackConfiguration :: Lens.Lens' CreateChangeSet (Lude.Maybe RollbackConfiguration)
ccsRollbackConfiguration = Lens.lens (rollbackConfiguration :: CreateChangeSet -> Lude.Maybe RollbackConfiguration) (\s a -> s {rollbackConfiguration = a} :: CreateChangeSet)
{-# DEPRECATED ccsRollbackConfiguration "Use generic-lens or generic-optics with 'rollbackConfiguration' instead." #-}

-- | The template resource types that you have permissions to work with if you execute this change set, such as @AWS::EC2::Instance@ , @AWS::EC2::*@ , or @Custom::MyCustomInstance@ .
--
-- If the list of resource types doesn't include a resource type that you're updating, the stack update fails. By default, AWS CloudFormation grants permissions to all resource types. AWS Identity and Access Management (IAM) uses this parameter for condition keys in IAM policies for AWS CloudFormation. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html Controlling Access with AWS Identity and Access Management> in the AWS CloudFormation User Guide.
--
-- /Note:/ Consider using 'resourceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsResourceTypes :: Lens.Lens' CreateChangeSet (Lude.Maybe [Lude.Text])
ccsResourceTypes = Lens.lens (resourceTypes :: CreateChangeSet -> Lude.Maybe [Lude.Text]) (\s a -> s {resourceTypes = a} :: CreateChangeSet)
{-# DEPRECATED ccsResourceTypes "Use generic-lens or generic-optics with 'resourceTypes' instead." #-}

-- | Key-value pairs to associate with this stack. AWS CloudFormation also propagates these tags to resources in the stack. You can specify a maximum of 50 tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsTags :: Lens.Lens' CreateChangeSet (Lude.Maybe [Tag])
ccsTags = Lens.lens (tags :: CreateChangeSet -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateChangeSet)
{-# DEPRECATED ccsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that AWS CloudFormation assumes when executing the change set. AWS CloudFormation uses the role's credentials to make calls on your behalf. AWS CloudFormation uses this role for all future operations on the stack. As long as users have permission to operate on the stack, AWS CloudFormation uses this role even if the users don't have permission to pass it. Ensure that the role grants least privilege.
--
-- If you don't specify a value, AWS CloudFormation uses the role that was previously associated with the stack. If no role is available, AWS CloudFormation uses a temporary session that is generated from your user credentials.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsRoleARN :: Lens.Lens' CreateChangeSet (Lude.Maybe Lude.Text)
ccsRoleARN = Lens.lens (roleARN :: CreateChangeSet -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: CreateChangeSet)
{-# DEPRECATED ccsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The name or the unique ID of the stack for which you are creating a change set. AWS CloudFormation generates the change set by comparing this stack's information with the information that you submit, such as a modified template or different parameter input values.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsStackName :: Lens.Lens' CreateChangeSet Lude.Text
ccsStackName = Lens.lens (stackName :: CreateChangeSet -> Lude.Text) (\s a -> s {stackName = a} :: CreateChangeSet)
{-# DEPRECATED ccsStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

-- | The name of the change set. The name must be unique among all change sets that are associated with the specified stack.
--
-- A change set name can contain only alphanumeric, case sensitive characters and hyphens. It must start with an alphabetic character and cannot exceed 128 characters.
--
-- /Note:/ Consider using 'changeSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsChangeSetName :: Lens.Lens' CreateChangeSet Lude.Text
ccsChangeSetName = Lens.lens (changeSetName :: CreateChangeSet -> Lude.Text) (\s a -> s {changeSetName = a} :: CreateChangeSet)
{-# DEPRECATED ccsChangeSetName "Use generic-lens or generic-optics with 'changeSetName' instead." #-}

instance Lude.AWSRequest CreateChangeSet where
  type Rs CreateChangeSet = CreateChangeSetResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "CreateChangeSetResult"
      ( \s h x ->
          CreateChangeSetResponse'
            Lude.<$> (x Lude..@? "Id")
            Lude.<*> (x Lude..@? "StackId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateChangeSet where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateChangeSet where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateChangeSet where
  toQuery CreateChangeSet' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateChangeSet" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "ChangeSetType" Lude.=: changeSetType,
        "UsePreviousTemplate" Lude.=: usePreviousTemplate,
        "ClientToken" Lude.=: clientToken,
        "NotificationARNs"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> notificationARNs),
        "IncludeNestedStacks" Lude.=: includeNestedStacks,
        "ResourcesToImport"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> resourcesToImport),
        "Parameters"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> parameters),
        "TemplateBody" Lude.=: templateBody,
        "TemplateURL" Lude.=: templateURL,
        "Description" Lude.=: description,
        "Capabilities"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> capabilities),
        "RollbackConfiguration" Lude.=: rollbackConfiguration,
        "ResourceTypes"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> resourceTypes),
        "Tags"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> tags),
        "RoleARN" Lude.=: roleARN,
        "StackName" Lude.=: stackName,
        "ChangeSetName" Lude.=: changeSetName
      ]

-- | The output for the 'CreateChangeSet' action.
--
-- /See:/ 'mkCreateChangeSetResponse' smart constructor.
data CreateChangeSetResponse = CreateChangeSetResponse'
  { id ::
      Lude.Maybe Lude.Text,
    stackId :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateChangeSetResponse' with the minimum fields required to make a request.
--
-- * 'id' - The Amazon Resource Name (ARN) of the change set.
-- * 'responseStatus' - The response status code.
-- * 'stackId' - The unique ID of the stack.
mkCreateChangeSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateChangeSetResponse
mkCreateChangeSetResponse pResponseStatus_ =
  CreateChangeSetResponse'
    { id = Lude.Nothing,
      stackId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the change set.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsrsId :: Lens.Lens' CreateChangeSetResponse (Lude.Maybe Lude.Text)
ccsrsId = Lens.lens (id :: CreateChangeSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: CreateChangeSetResponse)
{-# DEPRECATED ccsrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The unique ID of the stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsrsStackId :: Lens.Lens' CreateChangeSetResponse (Lude.Maybe Lude.Text)
ccsrsStackId = Lens.lens (stackId :: CreateChangeSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: CreateChangeSetResponse)
{-# DEPRECATED ccsrsStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsrsResponseStatus :: Lens.Lens' CreateChangeSetResponse Lude.Int
ccsrsResponseStatus = Lens.lens (responseStatus :: CreateChangeSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateChangeSetResponse)
{-# DEPRECATED ccsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
