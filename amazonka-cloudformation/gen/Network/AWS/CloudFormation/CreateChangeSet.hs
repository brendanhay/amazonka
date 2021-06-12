{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.CreateChangeSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a list of changes that will be applied to a stack so that you
-- can review the changes before executing them. You can create a change
-- set for a stack that doesn\'t exist or an existing stack. If you create
-- a change set for a stack that doesn\'t exist, the change set shows all
-- of the resources that AWS CloudFormation will create. If you create a
-- change set for an existing stack, AWS CloudFormation compares the
-- stack\'s information with the information that you submit in the change
-- set and lists the differences. Use change sets to understand which
-- resources AWS CloudFormation will create or change, and how it will
-- change resources in an existing stack, before you create or update a
-- stack.
--
-- To create a change set for a stack that doesn\'t exist, for the
-- @ChangeSetType@ parameter, specify @CREATE@. To create a change set for
-- an existing stack, specify @UPDATE@ for the @ChangeSetType@ parameter.
-- To create a change set for an import operation, specify @IMPORT@ for the
-- @ChangeSetType@ parameter. After the @CreateChangeSet@ call successfully
-- completes, AWS CloudFormation starts creating the change set. To check
-- the status of the change set or to review it, use the DescribeChangeSet
-- action.
--
-- When you are satisfied with the changes the change set will make,
-- execute the change set by using the ExecuteChangeSet action. AWS
-- CloudFormation doesn\'t make changes until you execute the change set.
--
-- To create a change set for the entire stack hierachy, set
-- @IncludeNestedStacks@ to @True@.
module Network.AWS.CloudFormation.CreateChangeSet
  ( -- * Creating a Request
    CreateChangeSet (..),
    newCreateChangeSet,

    -- * Request Lenses
    createChangeSet_resourcesToImport,
    createChangeSet_includeNestedStacks,
    createChangeSet_roleARN,
    createChangeSet_resourceTypes,
    createChangeSet_capabilities,
    createChangeSet_templateURL,
    createChangeSet_notificationARNs,
    createChangeSet_changeSetType,
    createChangeSet_tags,
    createChangeSet_rollbackConfiguration,
    createChangeSet_description,
    createChangeSet_templateBody,
    createChangeSet_parameters,
    createChangeSet_clientToken,
    createChangeSet_usePreviousTemplate,
    createChangeSet_stackName,
    createChangeSet_changeSetName,

    -- * Destructuring the Response
    CreateChangeSetResponse (..),
    newCreateChangeSetResponse,

    -- * Response Lenses
    createChangeSetResponse_stackId,
    createChangeSetResponse_id,
    createChangeSetResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the CreateChangeSet action.
--
-- /See:/ 'newCreateChangeSet' smart constructor.
data CreateChangeSet = CreateChangeSet'
  { -- | The resources to import into your stack.
    resourcesToImport :: Core.Maybe [ResourceToImport],
    -- | Creates a change set for the all nested stacks specified in the
    -- template. The default behavior of this action is set to @False@. To
    -- include nested sets in a change set, specify @True@.
    includeNestedStacks :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management
    -- (IAM) role that AWS CloudFormation assumes when executing the change
    -- set. AWS CloudFormation uses the role\'s credentials to make calls on
    -- your behalf. AWS CloudFormation uses this role for all future operations
    -- on the stack. As long as users have permission to operate on the stack,
    -- AWS CloudFormation uses this role even if the users don\'t have
    -- permission to pass it. Ensure that the role grants least privilege.
    --
    -- If you don\'t specify a value, AWS CloudFormation uses the role that was
    -- previously associated with the stack. If no role is available, AWS
    -- CloudFormation uses a temporary session that is generated from your user
    -- credentials.
    roleARN :: Core.Maybe Core.Text,
    -- | The template resource types that you have permissions to work with if
    -- you execute this change set, such as @AWS::EC2::Instance@,
    -- @AWS::EC2::*@, or @Custom::MyCustomInstance@.
    --
    -- If the list of resource types doesn\'t include a resource type that
    -- you\'re updating, the stack update fails. By default, AWS CloudFormation
    -- grants permissions to all resource types. AWS Identity and Access
    -- Management (IAM) uses this parameter for condition keys in IAM policies
    -- for AWS CloudFormation. For more information, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html Controlling Access with AWS Identity and Access Management>
    -- in the AWS CloudFormation User Guide.
    resourceTypes :: Core.Maybe [Core.Text],
    -- | In some cases, you must explicitly acknowledge that your stack template
    -- contains certain capabilities in order for AWS CloudFormation to create
    -- the stack.
    --
    -- -   @CAPABILITY_IAM@ and @CAPABILITY_NAMED_IAM@
    --
    --     Some stack templates might include resources that can affect
    --     permissions in your AWS account; for example, by creating new AWS
    --     Identity and Access Management (IAM) users. For those stacks, you
    --     must explicitly acknowledge this by specifying one of these
    --     capabilities.
    --
    --     The following IAM resources require you to specify either the
    --     @CAPABILITY_IAM@ or @CAPABILITY_NAMED_IAM@ capability.
    --
    --     -   If you have IAM resources, you can specify either capability.
    --
    --     -   If you have IAM resources with custom names, you /must/ specify
    --         @CAPABILITY_NAMED_IAM@.
    --
    --     -   If you don\'t specify either of these capabilities, AWS
    --         CloudFormation returns an @InsufficientCapabilities@ error.
    --
    --     If your stack template contains these resources, we recommend that
    --     you review all permissions associated with them and edit their
    --     permissions if necessary.
    --
    --     -   <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-accesskey.html AWS::IAM::AccessKey>
    --
    --     -   <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-group.html AWS::IAM::Group>
    --
    --     -   <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-instanceprofile.html AWS::IAM::InstanceProfile>
    --
    --     -   <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-policy.html AWS::IAM::Policy>
    --
    --     -   <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-role.html AWS::IAM::Role>
    --
    --     -   <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-user.html AWS::IAM::User>
    --
    --     -   <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-addusertogroup.html AWS::IAM::UserToGroupAddition>
    --
    --     For more information, see
    --     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates>.
    --
    -- -   @CAPABILITY_AUTO_EXPAND@
    --
    --     Some template contain macros. Macros perform custom processing on
    --     templates; this can include simple actions like find-and-replace
    --     operations, all the way to extensive transformations of entire
    --     templates. Because of this, users typically create a change set from
    --     the processed template, so that they can review the changes
    --     resulting from the macros before actually creating the stack. If
    --     your stack template contains one or more macros, and you choose to
    --     create a stack directly from the processed template, without first
    --     reviewing the resulting changes in a change set, you must
    --     acknowledge this capability. This includes the
    --     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/create-reusable-transform-function-snippets-and-add-to-your-template-with-aws-include-transform.html AWS::Include>
    --     and
    --     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/transform-aws-serverless.html AWS::Serverless>
    --     transforms, which are macros hosted by AWS CloudFormation.
    --
    --     This capacity does not apply to creating change sets, and specifying
    --     it when creating change sets has no effect.
    --
    --     If you want to create a stack from a stack template that contains
    --     macros /and/ nested stacks, you must create or update the stack
    --     directly from the template using the CreateStack or UpdateStack
    --     action, and specifying this capability.
    --
    --     For more information on macros, see
    --     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-macros.html Using AWS CloudFormation Macros to Perform Custom Processing on Templates>.
    capabilities :: Core.Maybe [Capability],
    -- | The location of the file that contains the revised template. The URL
    -- must point to a template (max size: 460,800 bytes) that is located in an
    -- S3 bucket or a Systems Manager document. AWS CloudFormation generates
    -- the change set by comparing this template with the stack that you
    -- specified.
    --
    -- Conditional: You must specify only @TemplateBody@ or @TemplateURL@.
    templateURL :: Core.Maybe Core.Text,
    -- | The Amazon Resource Names (ARNs) of Amazon Simple Notification Service
    -- (Amazon SNS) topics that AWS CloudFormation associates with the stack.
    -- To remove all associated notification topics, specify an empty list.
    notificationARNs :: Core.Maybe [Core.Text],
    -- | The type of change set operation. To create a change set for a new
    -- stack, specify @CREATE@. To create a change set for an existing stack,
    -- specify @UPDATE@. To create a change set for an import operation,
    -- specify @IMPORT@.
    --
    -- If you create a change set for a new stack, AWS Cloudformation creates a
    -- stack with a unique stack ID, but no template or resources. The stack
    -- will be in the
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-describing-stacks.html#d0e11995 REVIEW_IN_PROGRESS>
    -- state until you execute the change set.
    --
    -- By default, AWS CloudFormation specifies @UPDATE@. You can\'t use the
    -- @UPDATE@ type to create a change set for a new stack or the @CREATE@
    -- type to create a change set for an existing stack.
    changeSetType :: Core.Maybe ChangeSetType,
    -- | Key-value pairs to associate with this stack. AWS CloudFormation also
    -- propagates these tags to resources in the stack. You can specify a
    -- maximum of 50 tags.
    tags :: Core.Maybe [Tag],
    -- | The rollback triggers for AWS CloudFormation to monitor during stack
    -- creation and updating operations, and for the specified monitoring
    -- period afterwards.
    rollbackConfiguration :: Core.Maybe RollbackConfiguration,
    -- | A description to help you identify this change set.
    description :: Core.Maybe Core.Text,
    -- | A structure that contains the body of the revised template, with a
    -- minimum length of 1 byte and a maximum length of 51,200 bytes. AWS
    -- CloudFormation generates the change set by comparing this template with
    -- the template of the stack that you specified.
    --
    -- Conditional: You must specify only @TemplateBody@ or @TemplateURL@.
    templateBody :: Core.Maybe Core.Text,
    -- | A list of @Parameter@ structures that specify input parameters for the
    -- change set. For more information, see the Parameter data type.
    parameters :: Core.Maybe [Parameter],
    -- | A unique identifier for this @CreateChangeSet@ request. Specify this
    -- token if you plan to retry requests so that AWS CloudFormation knows
    -- that you\'re not attempting to create another change set with the same
    -- name. You might retry @CreateChangeSet@ requests to ensure that AWS
    -- CloudFormation successfully received them.
    clientToken :: Core.Maybe Core.Text,
    -- | Whether to reuse the template that is associated with the stack to
    -- create the change set.
    usePreviousTemplate :: Core.Maybe Core.Bool,
    -- | The name or the unique ID of the stack for which you are creating a
    -- change set. AWS CloudFormation generates the change set by comparing
    -- this stack\'s information with the information that you submit, such as
    -- a modified template or different parameter input values.
    stackName :: Core.Text,
    -- | The name of the change set. The name must be unique among all change
    -- sets that are associated with the specified stack.
    --
    -- A change set name can contain only alphanumeric, case sensitive
    -- characters and hyphens. It must start with an alphabetic character and
    -- cannot exceed 128 characters.
    changeSetName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateChangeSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourcesToImport', 'createChangeSet_resourcesToImport' - The resources to import into your stack.
--
-- 'includeNestedStacks', 'createChangeSet_includeNestedStacks' - Creates a change set for the all nested stacks specified in the
-- template. The default behavior of this action is set to @False@. To
-- include nested sets in a change set, specify @True@.
--
-- 'roleARN', 'createChangeSet_roleARN' - The Amazon Resource Name (ARN) of an AWS Identity and Access Management
-- (IAM) role that AWS CloudFormation assumes when executing the change
-- set. AWS CloudFormation uses the role\'s credentials to make calls on
-- your behalf. AWS CloudFormation uses this role for all future operations
-- on the stack. As long as users have permission to operate on the stack,
-- AWS CloudFormation uses this role even if the users don\'t have
-- permission to pass it. Ensure that the role grants least privilege.
--
-- If you don\'t specify a value, AWS CloudFormation uses the role that was
-- previously associated with the stack. If no role is available, AWS
-- CloudFormation uses a temporary session that is generated from your user
-- credentials.
--
-- 'resourceTypes', 'createChangeSet_resourceTypes' - The template resource types that you have permissions to work with if
-- you execute this change set, such as @AWS::EC2::Instance@,
-- @AWS::EC2::*@, or @Custom::MyCustomInstance@.
--
-- If the list of resource types doesn\'t include a resource type that
-- you\'re updating, the stack update fails. By default, AWS CloudFormation
-- grants permissions to all resource types. AWS Identity and Access
-- Management (IAM) uses this parameter for condition keys in IAM policies
-- for AWS CloudFormation. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html Controlling Access with AWS Identity and Access Management>
-- in the AWS CloudFormation User Guide.
--
-- 'capabilities', 'createChangeSet_capabilities' - In some cases, you must explicitly acknowledge that your stack template
-- contains certain capabilities in order for AWS CloudFormation to create
-- the stack.
--
-- -   @CAPABILITY_IAM@ and @CAPABILITY_NAMED_IAM@
--
--     Some stack templates might include resources that can affect
--     permissions in your AWS account; for example, by creating new AWS
--     Identity and Access Management (IAM) users. For those stacks, you
--     must explicitly acknowledge this by specifying one of these
--     capabilities.
--
--     The following IAM resources require you to specify either the
--     @CAPABILITY_IAM@ or @CAPABILITY_NAMED_IAM@ capability.
--
--     -   If you have IAM resources, you can specify either capability.
--
--     -   If you have IAM resources with custom names, you /must/ specify
--         @CAPABILITY_NAMED_IAM@.
--
--     -   If you don\'t specify either of these capabilities, AWS
--         CloudFormation returns an @InsufficientCapabilities@ error.
--
--     If your stack template contains these resources, we recommend that
--     you review all permissions associated with them and edit their
--     permissions if necessary.
--
--     -   <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-accesskey.html AWS::IAM::AccessKey>
--
--     -   <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-group.html AWS::IAM::Group>
--
--     -   <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-instanceprofile.html AWS::IAM::InstanceProfile>
--
--     -   <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-policy.html AWS::IAM::Policy>
--
--     -   <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-role.html AWS::IAM::Role>
--
--     -   <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-user.html AWS::IAM::User>
--
--     -   <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-addusertogroup.html AWS::IAM::UserToGroupAddition>
--
--     For more information, see
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates>.
--
-- -   @CAPABILITY_AUTO_EXPAND@
--
--     Some template contain macros. Macros perform custom processing on
--     templates; this can include simple actions like find-and-replace
--     operations, all the way to extensive transformations of entire
--     templates. Because of this, users typically create a change set from
--     the processed template, so that they can review the changes
--     resulting from the macros before actually creating the stack. If
--     your stack template contains one or more macros, and you choose to
--     create a stack directly from the processed template, without first
--     reviewing the resulting changes in a change set, you must
--     acknowledge this capability. This includes the
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/create-reusable-transform-function-snippets-and-add-to-your-template-with-aws-include-transform.html AWS::Include>
--     and
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/transform-aws-serverless.html AWS::Serverless>
--     transforms, which are macros hosted by AWS CloudFormation.
--
--     This capacity does not apply to creating change sets, and specifying
--     it when creating change sets has no effect.
--
--     If you want to create a stack from a stack template that contains
--     macros /and/ nested stacks, you must create or update the stack
--     directly from the template using the CreateStack or UpdateStack
--     action, and specifying this capability.
--
--     For more information on macros, see
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-macros.html Using AWS CloudFormation Macros to Perform Custom Processing on Templates>.
--
-- 'templateURL', 'createChangeSet_templateURL' - The location of the file that contains the revised template. The URL
-- must point to a template (max size: 460,800 bytes) that is located in an
-- S3 bucket or a Systems Manager document. AWS CloudFormation generates
-- the change set by comparing this template with the stack that you
-- specified.
--
-- Conditional: You must specify only @TemplateBody@ or @TemplateURL@.
--
-- 'notificationARNs', 'createChangeSet_notificationARNs' - The Amazon Resource Names (ARNs) of Amazon Simple Notification Service
-- (Amazon SNS) topics that AWS CloudFormation associates with the stack.
-- To remove all associated notification topics, specify an empty list.
--
-- 'changeSetType', 'createChangeSet_changeSetType' - The type of change set operation. To create a change set for a new
-- stack, specify @CREATE@. To create a change set for an existing stack,
-- specify @UPDATE@. To create a change set for an import operation,
-- specify @IMPORT@.
--
-- If you create a change set for a new stack, AWS Cloudformation creates a
-- stack with a unique stack ID, but no template or resources. The stack
-- will be in the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-describing-stacks.html#d0e11995 REVIEW_IN_PROGRESS>
-- state until you execute the change set.
--
-- By default, AWS CloudFormation specifies @UPDATE@. You can\'t use the
-- @UPDATE@ type to create a change set for a new stack or the @CREATE@
-- type to create a change set for an existing stack.
--
-- 'tags', 'createChangeSet_tags' - Key-value pairs to associate with this stack. AWS CloudFormation also
-- propagates these tags to resources in the stack. You can specify a
-- maximum of 50 tags.
--
-- 'rollbackConfiguration', 'createChangeSet_rollbackConfiguration' - The rollback triggers for AWS CloudFormation to monitor during stack
-- creation and updating operations, and for the specified monitoring
-- period afterwards.
--
-- 'description', 'createChangeSet_description' - A description to help you identify this change set.
--
-- 'templateBody', 'createChangeSet_templateBody' - A structure that contains the body of the revised template, with a
-- minimum length of 1 byte and a maximum length of 51,200 bytes. AWS
-- CloudFormation generates the change set by comparing this template with
-- the template of the stack that you specified.
--
-- Conditional: You must specify only @TemplateBody@ or @TemplateURL@.
--
-- 'parameters', 'createChangeSet_parameters' - A list of @Parameter@ structures that specify input parameters for the
-- change set. For more information, see the Parameter data type.
--
-- 'clientToken', 'createChangeSet_clientToken' - A unique identifier for this @CreateChangeSet@ request. Specify this
-- token if you plan to retry requests so that AWS CloudFormation knows
-- that you\'re not attempting to create another change set with the same
-- name. You might retry @CreateChangeSet@ requests to ensure that AWS
-- CloudFormation successfully received them.
--
-- 'usePreviousTemplate', 'createChangeSet_usePreviousTemplate' - Whether to reuse the template that is associated with the stack to
-- create the change set.
--
-- 'stackName', 'createChangeSet_stackName' - The name or the unique ID of the stack for which you are creating a
-- change set. AWS CloudFormation generates the change set by comparing
-- this stack\'s information with the information that you submit, such as
-- a modified template or different parameter input values.
--
-- 'changeSetName', 'createChangeSet_changeSetName' - The name of the change set. The name must be unique among all change
-- sets that are associated with the specified stack.
--
-- A change set name can contain only alphanumeric, case sensitive
-- characters and hyphens. It must start with an alphabetic character and
-- cannot exceed 128 characters.
newCreateChangeSet ::
  -- | 'stackName'
  Core.Text ->
  -- | 'changeSetName'
  Core.Text ->
  CreateChangeSet
newCreateChangeSet pStackName_ pChangeSetName_ =
  CreateChangeSet'
    { resourcesToImport = Core.Nothing,
      includeNestedStacks = Core.Nothing,
      roleARN = Core.Nothing,
      resourceTypes = Core.Nothing,
      capabilities = Core.Nothing,
      templateURL = Core.Nothing,
      notificationARNs = Core.Nothing,
      changeSetType = Core.Nothing,
      tags = Core.Nothing,
      rollbackConfiguration = Core.Nothing,
      description = Core.Nothing,
      templateBody = Core.Nothing,
      parameters = Core.Nothing,
      clientToken = Core.Nothing,
      usePreviousTemplate = Core.Nothing,
      stackName = pStackName_,
      changeSetName = pChangeSetName_
    }

-- | The resources to import into your stack.
createChangeSet_resourcesToImport :: Lens.Lens' CreateChangeSet (Core.Maybe [ResourceToImport])
createChangeSet_resourcesToImport = Lens.lens (\CreateChangeSet' {resourcesToImport} -> resourcesToImport) (\s@CreateChangeSet' {} a -> s {resourcesToImport = a} :: CreateChangeSet) Core.. Lens.mapping Lens._Coerce

-- | Creates a change set for the all nested stacks specified in the
-- template. The default behavior of this action is set to @False@. To
-- include nested sets in a change set, specify @True@.
createChangeSet_includeNestedStacks :: Lens.Lens' CreateChangeSet (Core.Maybe Core.Bool)
createChangeSet_includeNestedStacks = Lens.lens (\CreateChangeSet' {includeNestedStacks} -> includeNestedStacks) (\s@CreateChangeSet' {} a -> s {includeNestedStacks = a} :: CreateChangeSet)

-- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management
-- (IAM) role that AWS CloudFormation assumes when executing the change
-- set. AWS CloudFormation uses the role\'s credentials to make calls on
-- your behalf. AWS CloudFormation uses this role for all future operations
-- on the stack. As long as users have permission to operate on the stack,
-- AWS CloudFormation uses this role even if the users don\'t have
-- permission to pass it. Ensure that the role grants least privilege.
--
-- If you don\'t specify a value, AWS CloudFormation uses the role that was
-- previously associated with the stack. If no role is available, AWS
-- CloudFormation uses a temporary session that is generated from your user
-- credentials.
createChangeSet_roleARN :: Lens.Lens' CreateChangeSet (Core.Maybe Core.Text)
createChangeSet_roleARN = Lens.lens (\CreateChangeSet' {roleARN} -> roleARN) (\s@CreateChangeSet' {} a -> s {roleARN = a} :: CreateChangeSet)

-- | The template resource types that you have permissions to work with if
-- you execute this change set, such as @AWS::EC2::Instance@,
-- @AWS::EC2::*@, or @Custom::MyCustomInstance@.
--
-- If the list of resource types doesn\'t include a resource type that
-- you\'re updating, the stack update fails. By default, AWS CloudFormation
-- grants permissions to all resource types. AWS Identity and Access
-- Management (IAM) uses this parameter for condition keys in IAM policies
-- for AWS CloudFormation. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html Controlling Access with AWS Identity and Access Management>
-- in the AWS CloudFormation User Guide.
createChangeSet_resourceTypes :: Lens.Lens' CreateChangeSet (Core.Maybe [Core.Text])
createChangeSet_resourceTypes = Lens.lens (\CreateChangeSet' {resourceTypes} -> resourceTypes) (\s@CreateChangeSet' {} a -> s {resourceTypes = a} :: CreateChangeSet) Core.. Lens.mapping Lens._Coerce

-- | In some cases, you must explicitly acknowledge that your stack template
-- contains certain capabilities in order for AWS CloudFormation to create
-- the stack.
--
-- -   @CAPABILITY_IAM@ and @CAPABILITY_NAMED_IAM@
--
--     Some stack templates might include resources that can affect
--     permissions in your AWS account; for example, by creating new AWS
--     Identity and Access Management (IAM) users. For those stacks, you
--     must explicitly acknowledge this by specifying one of these
--     capabilities.
--
--     The following IAM resources require you to specify either the
--     @CAPABILITY_IAM@ or @CAPABILITY_NAMED_IAM@ capability.
--
--     -   If you have IAM resources, you can specify either capability.
--
--     -   If you have IAM resources with custom names, you /must/ specify
--         @CAPABILITY_NAMED_IAM@.
--
--     -   If you don\'t specify either of these capabilities, AWS
--         CloudFormation returns an @InsufficientCapabilities@ error.
--
--     If your stack template contains these resources, we recommend that
--     you review all permissions associated with them and edit their
--     permissions if necessary.
--
--     -   <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-accesskey.html AWS::IAM::AccessKey>
--
--     -   <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-group.html AWS::IAM::Group>
--
--     -   <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-instanceprofile.html AWS::IAM::InstanceProfile>
--
--     -   <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-policy.html AWS::IAM::Policy>
--
--     -   <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-role.html AWS::IAM::Role>
--
--     -   <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-user.html AWS::IAM::User>
--
--     -   <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-addusertogroup.html AWS::IAM::UserToGroupAddition>
--
--     For more information, see
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates>.
--
-- -   @CAPABILITY_AUTO_EXPAND@
--
--     Some template contain macros. Macros perform custom processing on
--     templates; this can include simple actions like find-and-replace
--     operations, all the way to extensive transformations of entire
--     templates. Because of this, users typically create a change set from
--     the processed template, so that they can review the changes
--     resulting from the macros before actually creating the stack. If
--     your stack template contains one or more macros, and you choose to
--     create a stack directly from the processed template, without first
--     reviewing the resulting changes in a change set, you must
--     acknowledge this capability. This includes the
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/create-reusable-transform-function-snippets-and-add-to-your-template-with-aws-include-transform.html AWS::Include>
--     and
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/transform-aws-serverless.html AWS::Serverless>
--     transforms, which are macros hosted by AWS CloudFormation.
--
--     This capacity does not apply to creating change sets, and specifying
--     it when creating change sets has no effect.
--
--     If you want to create a stack from a stack template that contains
--     macros /and/ nested stacks, you must create or update the stack
--     directly from the template using the CreateStack or UpdateStack
--     action, and specifying this capability.
--
--     For more information on macros, see
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-macros.html Using AWS CloudFormation Macros to Perform Custom Processing on Templates>.
createChangeSet_capabilities :: Lens.Lens' CreateChangeSet (Core.Maybe [Capability])
createChangeSet_capabilities = Lens.lens (\CreateChangeSet' {capabilities} -> capabilities) (\s@CreateChangeSet' {} a -> s {capabilities = a} :: CreateChangeSet) Core.. Lens.mapping Lens._Coerce

-- | The location of the file that contains the revised template. The URL
-- must point to a template (max size: 460,800 bytes) that is located in an
-- S3 bucket or a Systems Manager document. AWS CloudFormation generates
-- the change set by comparing this template with the stack that you
-- specified.
--
-- Conditional: You must specify only @TemplateBody@ or @TemplateURL@.
createChangeSet_templateURL :: Lens.Lens' CreateChangeSet (Core.Maybe Core.Text)
createChangeSet_templateURL = Lens.lens (\CreateChangeSet' {templateURL} -> templateURL) (\s@CreateChangeSet' {} a -> s {templateURL = a} :: CreateChangeSet)

-- | The Amazon Resource Names (ARNs) of Amazon Simple Notification Service
-- (Amazon SNS) topics that AWS CloudFormation associates with the stack.
-- To remove all associated notification topics, specify an empty list.
createChangeSet_notificationARNs :: Lens.Lens' CreateChangeSet (Core.Maybe [Core.Text])
createChangeSet_notificationARNs = Lens.lens (\CreateChangeSet' {notificationARNs} -> notificationARNs) (\s@CreateChangeSet' {} a -> s {notificationARNs = a} :: CreateChangeSet) Core.. Lens.mapping Lens._Coerce

-- | The type of change set operation. To create a change set for a new
-- stack, specify @CREATE@. To create a change set for an existing stack,
-- specify @UPDATE@. To create a change set for an import operation,
-- specify @IMPORT@.
--
-- If you create a change set for a new stack, AWS Cloudformation creates a
-- stack with a unique stack ID, but no template or resources. The stack
-- will be in the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-describing-stacks.html#d0e11995 REVIEW_IN_PROGRESS>
-- state until you execute the change set.
--
-- By default, AWS CloudFormation specifies @UPDATE@. You can\'t use the
-- @UPDATE@ type to create a change set for a new stack or the @CREATE@
-- type to create a change set for an existing stack.
createChangeSet_changeSetType :: Lens.Lens' CreateChangeSet (Core.Maybe ChangeSetType)
createChangeSet_changeSetType = Lens.lens (\CreateChangeSet' {changeSetType} -> changeSetType) (\s@CreateChangeSet' {} a -> s {changeSetType = a} :: CreateChangeSet)

-- | Key-value pairs to associate with this stack. AWS CloudFormation also
-- propagates these tags to resources in the stack. You can specify a
-- maximum of 50 tags.
createChangeSet_tags :: Lens.Lens' CreateChangeSet (Core.Maybe [Tag])
createChangeSet_tags = Lens.lens (\CreateChangeSet' {tags} -> tags) (\s@CreateChangeSet' {} a -> s {tags = a} :: CreateChangeSet) Core.. Lens.mapping Lens._Coerce

-- | The rollback triggers for AWS CloudFormation to monitor during stack
-- creation and updating operations, and for the specified monitoring
-- period afterwards.
createChangeSet_rollbackConfiguration :: Lens.Lens' CreateChangeSet (Core.Maybe RollbackConfiguration)
createChangeSet_rollbackConfiguration = Lens.lens (\CreateChangeSet' {rollbackConfiguration} -> rollbackConfiguration) (\s@CreateChangeSet' {} a -> s {rollbackConfiguration = a} :: CreateChangeSet)

-- | A description to help you identify this change set.
createChangeSet_description :: Lens.Lens' CreateChangeSet (Core.Maybe Core.Text)
createChangeSet_description = Lens.lens (\CreateChangeSet' {description} -> description) (\s@CreateChangeSet' {} a -> s {description = a} :: CreateChangeSet)

-- | A structure that contains the body of the revised template, with a
-- minimum length of 1 byte and a maximum length of 51,200 bytes. AWS
-- CloudFormation generates the change set by comparing this template with
-- the template of the stack that you specified.
--
-- Conditional: You must specify only @TemplateBody@ or @TemplateURL@.
createChangeSet_templateBody :: Lens.Lens' CreateChangeSet (Core.Maybe Core.Text)
createChangeSet_templateBody = Lens.lens (\CreateChangeSet' {templateBody} -> templateBody) (\s@CreateChangeSet' {} a -> s {templateBody = a} :: CreateChangeSet)

-- | A list of @Parameter@ structures that specify input parameters for the
-- change set. For more information, see the Parameter data type.
createChangeSet_parameters :: Lens.Lens' CreateChangeSet (Core.Maybe [Parameter])
createChangeSet_parameters = Lens.lens (\CreateChangeSet' {parameters} -> parameters) (\s@CreateChangeSet' {} a -> s {parameters = a} :: CreateChangeSet) Core.. Lens.mapping Lens._Coerce

-- | A unique identifier for this @CreateChangeSet@ request. Specify this
-- token if you plan to retry requests so that AWS CloudFormation knows
-- that you\'re not attempting to create another change set with the same
-- name. You might retry @CreateChangeSet@ requests to ensure that AWS
-- CloudFormation successfully received them.
createChangeSet_clientToken :: Lens.Lens' CreateChangeSet (Core.Maybe Core.Text)
createChangeSet_clientToken = Lens.lens (\CreateChangeSet' {clientToken} -> clientToken) (\s@CreateChangeSet' {} a -> s {clientToken = a} :: CreateChangeSet)

-- | Whether to reuse the template that is associated with the stack to
-- create the change set.
createChangeSet_usePreviousTemplate :: Lens.Lens' CreateChangeSet (Core.Maybe Core.Bool)
createChangeSet_usePreviousTemplate = Lens.lens (\CreateChangeSet' {usePreviousTemplate} -> usePreviousTemplate) (\s@CreateChangeSet' {} a -> s {usePreviousTemplate = a} :: CreateChangeSet)

-- | The name or the unique ID of the stack for which you are creating a
-- change set. AWS CloudFormation generates the change set by comparing
-- this stack\'s information with the information that you submit, such as
-- a modified template or different parameter input values.
createChangeSet_stackName :: Lens.Lens' CreateChangeSet Core.Text
createChangeSet_stackName = Lens.lens (\CreateChangeSet' {stackName} -> stackName) (\s@CreateChangeSet' {} a -> s {stackName = a} :: CreateChangeSet)

-- | The name of the change set. The name must be unique among all change
-- sets that are associated with the specified stack.
--
-- A change set name can contain only alphanumeric, case sensitive
-- characters and hyphens. It must start with an alphabetic character and
-- cannot exceed 128 characters.
createChangeSet_changeSetName :: Lens.Lens' CreateChangeSet Core.Text
createChangeSet_changeSetName = Lens.lens (\CreateChangeSet' {changeSetName} -> changeSetName) (\s@CreateChangeSet' {} a -> s {changeSetName = a} :: CreateChangeSet)

instance Core.AWSRequest CreateChangeSet where
  type
    AWSResponse CreateChangeSet =
      CreateChangeSetResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateChangeSetResult"
      ( \s h x ->
          CreateChangeSetResponse'
            Core.<$> (x Core..@? "StackId")
            Core.<*> (x Core..@? "Id")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateChangeSet

instance Core.NFData CreateChangeSet

instance Core.ToHeaders CreateChangeSet where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateChangeSet where
  toPath = Core.const "/"

instance Core.ToQuery CreateChangeSet where
  toQuery CreateChangeSet' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateChangeSet" :: Core.ByteString),
        "Version" Core.=: ("2010-05-15" :: Core.ByteString),
        "ResourcesToImport"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> resourcesToImport
            ),
        "IncludeNestedStacks" Core.=: includeNestedStacks,
        "RoleARN" Core.=: roleARN,
        "ResourceTypes"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> resourceTypes),
        "Capabilities"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> capabilities),
        "TemplateURL" Core.=: templateURL,
        "NotificationARNs"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> notificationARNs
            ),
        "ChangeSetType" Core.=: changeSetType,
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> tags),
        "RollbackConfiguration"
          Core.=: rollbackConfiguration,
        "Description" Core.=: description,
        "TemplateBody" Core.=: templateBody,
        "Parameters"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> parameters),
        "ClientToken" Core.=: clientToken,
        "UsePreviousTemplate" Core.=: usePreviousTemplate,
        "StackName" Core.=: stackName,
        "ChangeSetName" Core.=: changeSetName
      ]

-- | The output for the CreateChangeSet action.
--
-- /See:/ 'newCreateChangeSetResponse' smart constructor.
data CreateChangeSetResponse = CreateChangeSetResponse'
  { -- | The unique ID of the stack.
    stackId :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the change set.
    id :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateChangeSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackId', 'createChangeSetResponse_stackId' - The unique ID of the stack.
--
-- 'id', 'createChangeSetResponse_id' - The Amazon Resource Name (ARN) of the change set.
--
-- 'httpStatus', 'createChangeSetResponse_httpStatus' - The response's http status code.
newCreateChangeSetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateChangeSetResponse
newCreateChangeSetResponse pHttpStatus_ =
  CreateChangeSetResponse'
    { stackId = Core.Nothing,
      id = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique ID of the stack.
createChangeSetResponse_stackId :: Lens.Lens' CreateChangeSetResponse (Core.Maybe Core.Text)
createChangeSetResponse_stackId = Lens.lens (\CreateChangeSetResponse' {stackId} -> stackId) (\s@CreateChangeSetResponse' {} a -> s {stackId = a} :: CreateChangeSetResponse)

-- | The Amazon Resource Name (ARN) of the change set.
createChangeSetResponse_id :: Lens.Lens' CreateChangeSetResponse (Core.Maybe Core.Text)
createChangeSetResponse_id = Lens.lens (\CreateChangeSetResponse' {id} -> id) (\s@CreateChangeSetResponse' {} a -> s {id = a} :: CreateChangeSetResponse)

-- | The response's http status code.
createChangeSetResponse_httpStatus :: Lens.Lens' CreateChangeSetResponse Core.Int
createChangeSetResponse_httpStatus = Lens.lens (\CreateChangeSetResponse' {httpStatus} -> httpStatus) (\s@CreateChangeSetResponse' {} a -> s {httpStatus = a} :: CreateChangeSetResponse)

instance Core.NFData CreateChangeSetResponse
