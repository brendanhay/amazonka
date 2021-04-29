{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudFormation.UpdateStack
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a stack as specified in the template. After the call completes
-- successfully, the stack update starts. You can check the status of the
-- stack via the DescribeStacks action.
--
-- To get a copy of the template for an existing stack, you can use the
-- GetTemplate action.
--
-- For more information about creating an update template, updating a
-- stack, and monitoring the progress of the update, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-updating-stacks.html Updating a Stack>.
module Network.AWS.CloudFormation.UpdateStack
  ( -- * Creating a Request
    UpdateStack (..),
    newUpdateStack,

    -- * Request Lenses
    updateStack_stackPolicyDuringUpdateBody,
    updateStack_roleARN,
    updateStack_resourceTypes,
    updateStack_capabilities,
    updateStack_stackPolicyBody,
    updateStack_templateURL,
    updateStack_notificationARNs,
    updateStack_stackPolicyURL,
    updateStack_stackPolicyDuringUpdateURL,
    updateStack_tags,
    updateStack_rollbackConfiguration,
    updateStack_clientRequestToken,
    updateStack_templateBody,
    updateStack_parameters,
    updateStack_usePreviousTemplate,
    updateStack_stackName,

    -- * Destructuring the Response
    UpdateStackResponse (..),
    newUpdateStackResponse,

    -- * Response Lenses
    updateStackResponse_stackId,
    updateStackResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for an UpdateStack action.
--
-- /See:/ 'newUpdateStack' smart constructor.
data UpdateStack = UpdateStack'
  { -- | Structure containing the temporary overriding stack policy body. You can
    -- specify either the @StackPolicyDuringUpdateBody@ or the
    -- @StackPolicyDuringUpdateURL@ parameter, but not both.
    --
    -- If you want to update protected resources, specify a temporary
    -- overriding stack policy during this update. If you do not specify a
    -- stack policy, the current policy that is associated with the stack will
    -- be used.
    stackPolicyDuringUpdateBody :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management
    -- (IAM) role that AWS CloudFormation assumes to update the stack. AWS
    -- CloudFormation uses the role\'s credentials to make calls on your
    -- behalf. AWS CloudFormation always uses this role for all future
    -- operations on the stack. As long as users have permission to operate on
    -- the stack, AWS CloudFormation uses this role even if the users don\'t
    -- have permission to pass it. Ensure that the role grants least privilege.
    --
    -- If you don\'t specify a value, AWS CloudFormation uses the role that was
    -- previously associated with the stack. If no role is available, AWS
    -- CloudFormation uses a temporary session that is generated from your user
    -- credentials.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | The template resource types that you have permissions to work with for
    -- this update stack action, such as @AWS::EC2::Instance@, @AWS::EC2::*@,
    -- or @Custom::MyCustomInstance@.
    --
    -- If the list of resource types doesn\'t include a resource that you\'re
    -- updating, the stack update fails. By default, AWS CloudFormation grants
    -- permissions to all resource types. AWS Identity and Access Management
    -- (IAM) uses this parameter for AWS CloudFormation-specific condition keys
    -- in IAM policies. For more information, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html Controlling Access with AWS Identity and Access Management>.
    resourceTypes :: Prelude.Maybe [Prelude.Text],
    -- | In some cases, you must explicitly acknowledge that your stack template
    -- contains certain capabilities in order for AWS CloudFormation to update
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
    --     resulting from the macros before actually updating the stack. If
    --     your stack template contains one or more macros, and you choose to
    --     update a stack directly from the processed template, without first
    --     reviewing the resulting changes in a change set, you must
    --     acknowledge this capability. This includes the
    --     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/create-reusable-transform-function-snippets-and-add-to-your-template-with-aws-include-transform.html AWS::Include>
    --     and
    --     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/transform-aws-serverless.html AWS::Serverless>
    --     transforms, which are macros hosted by AWS CloudFormation.
    --
    --     If you want to update a stack from a stack template that contains
    --     macros /and/ nested stacks, you must update the stack directly from
    --     the template using this capability.
    --
    --     You should only update stacks directly from a stack template that
    --     contains macros if you know what processing the macro performs.
    --
    --     Each macro relies on an underlying Lambda service function for
    --     processing stack templates. Be aware that the Lambda function owner
    --     can update the function operation without AWS CloudFormation being
    --     notified.
    --
    --     For more information, see
    --     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-macros.html Using AWS CloudFormation Macros to Perform Custom Processing on Templates>.
    capabilities :: Prelude.Maybe [Capability],
    -- | Structure containing a new stack policy body. You can specify either the
    -- @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
    --
    -- You might update the stack policy, for example, in order to protect a
    -- new resource that you created during a stack update. If you do not
    -- specify a stack policy, the current policy that is associated with the
    -- stack is unchanged.
    stackPolicyBody :: Prelude.Maybe Prelude.Text,
    -- | Location of file containing the template body. The URL must point to a
    -- template that is located in an Amazon S3 bucket or a Systems Manager
    -- document. For more information, go to
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
    -- in the AWS CloudFormation User Guide.
    --
    -- Conditional: You must specify only one of the following parameters:
    -- @TemplateBody@, @TemplateURL@, or set the @UsePreviousTemplate@ to
    -- @true@.
    templateURL :: Prelude.Maybe Prelude.Text,
    -- | Amazon Simple Notification Service topic Amazon Resource Names (ARNs)
    -- that AWS CloudFormation associates with the stack. Specify an empty list
    -- to remove all notification topics.
    notificationARNs :: Prelude.Maybe [Prelude.Text],
    -- | Location of a file containing the updated stack policy. The URL must
    -- point to a policy (max size: 16KB) located in an S3 bucket in the same
    -- Region as the stack. You can specify either the @StackPolicyBody@ or the
    -- @StackPolicyURL@ parameter, but not both.
    --
    -- You might update the stack policy, for example, in order to protect a
    -- new resource that you created during a stack update. If you do not
    -- specify a stack policy, the current policy that is associated with the
    -- stack is unchanged.
    stackPolicyURL :: Prelude.Maybe Prelude.Text,
    -- | Location of a file containing the temporary overriding stack policy. The
    -- URL must point to a policy (max size: 16KB) located in an S3 bucket in
    -- the same Region as the stack. You can specify either the
    -- @StackPolicyDuringUpdateBody@ or the @StackPolicyDuringUpdateURL@
    -- parameter, but not both.
    --
    -- If you want to update protected resources, specify a temporary
    -- overriding stack policy during this update. If you do not specify a
    -- stack policy, the current policy that is associated with the stack will
    -- be used.
    stackPolicyDuringUpdateURL :: Prelude.Maybe Prelude.Text,
    -- | Key-value pairs to associate with this stack. AWS CloudFormation also
    -- propagates these tags to supported resources in the stack. You can
    -- specify a maximum number of 50 tags.
    --
    -- If you don\'t specify this parameter, AWS CloudFormation doesn\'t modify
    -- the stack\'s tags. If you specify an empty value, AWS CloudFormation
    -- removes all associated tags.
    tags :: Prelude.Maybe [Tag],
    -- | The rollback triggers for AWS CloudFormation to monitor during stack
    -- creation and updating operations, and for the specified monitoring
    -- period afterwards.
    rollbackConfiguration :: Prelude.Maybe RollbackConfiguration,
    -- | A unique identifier for this @UpdateStack@ request. Specify this token
    -- if you plan to retry requests so that AWS CloudFormation knows that
    -- you\'re not attempting to update a stack with the same name. You might
    -- retry @UpdateStack@ requests to ensure that AWS CloudFormation
    -- successfully received them.
    --
    -- All events triggered by a given stack operation are assigned the same
    -- client request token, which you can use to track operations. For
    -- example, if you execute a @CreateStack@ operation with the token
    -- @token1@, then all the @StackEvents@ generated by that operation will
    -- have @ClientRequestToken@ set as @token1@.
    --
    -- In the console, stack operations display the client request token on the
    -- Events tab. Stack operations that are initiated from the console use the
    -- token format /Console-StackOperation-ID/, which helps you easily
    -- identify the stack operation . For example, if you create a stack using
    -- the console, each stack event would be assigned the same token in the
    -- following format:
    -- @Console-CreateStack-7f59c3cf-00d2-40c7-b2ff-e75db0987002@.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | Structure containing the template body with a minimum length of 1 byte
    -- and a maximum length of 51,200 bytes. (For more information, go to
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
    -- in the AWS CloudFormation User Guide.)
    --
    -- Conditional: You must specify only one of the following parameters:
    -- @TemplateBody@, @TemplateURL@, or set the @UsePreviousTemplate@ to
    -- @true@.
    templateBody :: Prelude.Maybe Prelude.Text,
    -- | A list of @Parameter@ structures that specify input parameters for the
    -- stack. For more information, see the
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter>
    -- data type.
    parameters :: Prelude.Maybe [Parameter],
    -- | Reuse the existing template that is associated with the stack that you
    -- are updating.
    --
    -- Conditional: You must specify only one of the following parameters:
    -- @TemplateBody@, @TemplateURL@, or set the @UsePreviousTemplate@ to
    -- @true@.
    usePreviousTemplate :: Prelude.Maybe Prelude.Bool,
    -- | The name or unique stack ID of the stack to update.
    stackName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateStack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackPolicyDuringUpdateBody', 'updateStack_stackPolicyDuringUpdateBody' - Structure containing the temporary overriding stack policy body. You can
-- specify either the @StackPolicyDuringUpdateBody@ or the
-- @StackPolicyDuringUpdateURL@ parameter, but not both.
--
-- If you want to update protected resources, specify a temporary
-- overriding stack policy during this update. If you do not specify a
-- stack policy, the current policy that is associated with the stack will
-- be used.
--
-- 'roleARN', 'updateStack_roleARN' - The Amazon Resource Name (ARN) of an AWS Identity and Access Management
-- (IAM) role that AWS CloudFormation assumes to update the stack. AWS
-- CloudFormation uses the role\'s credentials to make calls on your
-- behalf. AWS CloudFormation always uses this role for all future
-- operations on the stack. As long as users have permission to operate on
-- the stack, AWS CloudFormation uses this role even if the users don\'t
-- have permission to pass it. Ensure that the role grants least privilege.
--
-- If you don\'t specify a value, AWS CloudFormation uses the role that was
-- previously associated with the stack. If no role is available, AWS
-- CloudFormation uses a temporary session that is generated from your user
-- credentials.
--
-- 'resourceTypes', 'updateStack_resourceTypes' - The template resource types that you have permissions to work with for
-- this update stack action, such as @AWS::EC2::Instance@, @AWS::EC2::*@,
-- or @Custom::MyCustomInstance@.
--
-- If the list of resource types doesn\'t include a resource that you\'re
-- updating, the stack update fails. By default, AWS CloudFormation grants
-- permissions to all resource types. AWS Identity and Access Management
-- (IAM) uses this parameter for AWS CloudFormation-specific condition keys
-- in IAM policies. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html Controlling Access with AWS Identity and Access Management>.
--
-- 'capabilities', 'updateStack_capabilities' - In some cases, you must explicitly acknowledge that your stack template
-- contains certain capabilities in order for AWS CloudFormation to update
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
--     resulting from the macros before actually updating the stack. If
--     your stack template contains one or more macros, and you choose to
--     update a stack directly from the processed template, without first
--     reviewing the resulting changes in a change set, you must
--     acknowledge this capability. This includes the
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/create-reusable-transform-function-snippets-and-add-to-your-template-with-aws-include-transform.html AWS::Include>
--     and
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/transform-aws-serverless.html AWS::Serverless>
--     transforms, which are macros hosted by AWS CloudFormation.
--
--     If you want to update a stack from a stack template that contains
--     macros /and/ nested stacks, you must update the stack directly from
--     the template using this capability.
--
--     You should only update stacks directly from a stack template that
--     contains macros if you know what processing the macro performs.
--
--     Each macro relies on an underlying Lambda service function for
--     processing stack templates. Be aware that the Lambda function owner
--     can update the function operation without AWS CloudFormation being
--     notified.
--
--     For more information, see
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-macros.html Using AWS CloudFormation Macros to Perform Custom Processing on Templates>.
--
-- 'stackPolicyBody', 'updateStack_stackPolicyBody' - Structure containing a new stack policy body. You can specify either the
-- @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
--
-- You might update the stack policy, for example, in order to protect a
-- new resource that you created during a stack update. If you do not
-- specify a stack policy, the current policy that is associated with the
-- stack is unchanged.
--
-- 'templateURL', 'updateStack_templateURL' - Location of file containing the template body. The URL must point to a
-- template that is located in an Amazon S3 bucket or a Systems Manager
-- document. For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters:
-- @TemplateBody@, @TemplateURL@, or set the @UsePreviousTemplate@ to
-- @true@.
--
-- 'notificationARNs', 'updateStack_notificationARNs' - Amazon Simple Notification Service topic Amazon Resource Names (ARNs)
-- that AWS CloudFormation associates with the stack. Specify an empty list
-- to remove all notification topics.
--
-- 'stackPolicyURL', 'updateStack_stackPolicyURL' - Location of a file containing the updated stack policy. The URL must
-- point to a policy (max size: 16KB) located in an S3 bucket in the same
-- Region as the stack. You can specify either the @StackPolicyBody@ or the
-- @StackPolicyURL@ parameter, but not both.
--
-- You might update the stack policy, for example, in order to protect a
-- new resource that you created during a stack update. If you do not
-- specify a stack policy, the current policy that is associated with the
-- stack is unchanged.
--
-- 'stackPolicyDuringUpdateURL', 'updateStack_stackPolicyDuringUpdateURL' - Location of a file containing the temporary overriding stack policy. The
-- URL must point to a policy (max size: 16KB) located in an S3 bucket in
-- the same Region as the stack. You can specify either the
-- @StackPolicyDuringUpdateBody@ or the @StackPolicyDuringUpdateURL@
-- parameter, but not both.
--
-- If you want to update protected resources, specify a temporary
-- overriding stack policy during this update. If you do not specify a
-- stack policy, the current policy that is associated with the stack will
-- be used.
--
-- 'tags', 'updateStack_tags' - Key-value pairs to associate with this stack. AWS CloudFormation also
-- propagates these tags to supported resources in the stack. You can
-- specify a maximum number of 50 tags.
--
-- If you don\'t specify this parameter, AWS CloudFormation doesn\'t modify
-- the stack\'s tags. If you specify an empty value, AWS CloudFormation
-- removes all associated tags.
--
-- 'rollbackConfiguration', 'updateStack_rollbackConfiguration' - The rollback triggers for AWS CloudFormation to monitor during stack
-- creation and updating operations, and for the specified monitoring
-- period afterwards.
--
-- 'clientRequestToken', 'updateStack_clientRequestToken' - A unique identifier for this @UpdateStack@ request. Specify this token
-- if you plan to retry requests so that AWS CloudFormation knows that
-- you\'re not attempting to update a stack with the same name. You might
-- retry @UpdateStack@ requests to ensure that AWS CloudFormation
-- successfully received them.
--
-- All events triggered by a given stack operation are assigned the same
-- client request token, which you can use to track operations. For
-- example, if you execute a @CreateStack@ operation with the token
-- @token1@, then all the @StackEvents@ generated by that operation will
-- have @ClientRequestToken@ set as @token1@.
--
-- In the console, stack operations display the client request token on the
-- Events tab. Stack operations that are initiated from the console use the
-- token format /Console-StackOperation-ID/, which helps you easily
-- identify the stack operation . For example, if you create a stack using
-- the console, each stack event would be assigned the same token in the
-- following format:
-- @Console-CreateStack-7f59c3cf-00d2-40c7-b2ff-e75db0987002@.
--
-- 'templateBody', 'updateStack_templateBody' - Structure containing the template body with a minimum length of 1 byte
-- and a maximum length of 51,200 bytes. (For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.)
--
-- Conditional: You must specify only one of the following parameters:
-- @TemplateBody@, @TemplateURL@, or set the @UsePreviousTemplate@ to
-- @true@.
--
-- 'parameters', 'updateStack_parameters' - A list of @Parameter@ structures that specify input parameters for the
-- stack. For more information, see the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter>
-- data type.
--
-- 'usePreviousTemplate', 'updateStack_usePreviousTemplate' - Reuse the existing template that is associated with the stack that you
-- are updating.
--
-- Conditional: You must specify only one of the following parameters:
-- @TemplateBody@, @TemplateURL@, or set the @UsePreviousTemplate@ to
-- @true@.
--
-- 'stackName', 'updateStack_stackName' - The name or unique stack ID of the stack to update.
newUpdateStack ::
  -- | 'stackName'
  Prelude.Text ->
  UpdateStack
newUpdateStack pStackName_ =
  UpdateStack'
    { stackPolicyDuringUpdateBody =
        Prelude.Nothing,
      roleARN = Prelude.Nothing,
      resourceTypes = Prelude.Nothing,
      capabilities = Prelude.Nothing,
      stackPolicyBody = Prelude.Nothing,
      templateURL = Prelude.Nothing,
      notificationARNs = Prelude.Nothing,
      stackPolicyURL = Prelude.Nothing,
      stackPolicyDuringUpdateURL = Prelude.Nothing,
      tags = Prelude.Nothing,
      rollbackConfiguration = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      templateBody = Prelude.Nothing,
      parameters = Prelude.Nothing,
      usePreviousTemplate = Prelude.Nothing,
      stackName = pStackName_
    }

-- | Structure containing the temporary overriding stack policy body. You can
-- specify either the @StackPolicyDuringUpdateBody@ or the
-- @StackPolicyDuringUpdateURL@ parameter, but not both.
--
-- If you want to update protected resources, specify a temporary
-- overriding stack policy during this update. If you do not specify a
-- stack policy, the current policy that is associated with the stack will
-- be used.
updateStack_stackPolicyDuringUpdateBody :: Lens.Lens' UpdateStack (Prelude.Maybe Prelude.Text)
updateStack_stackPolicyDuringUpdateBody = Lens.lens (\UpdateStack' {stackPolicyDuringUpdateBody} -> stackPolicyDuringUpdateBody) (\s@UpdateStack' {} a -> s {stackPolicyDuringUpdateBody = a} :: UpdateStack)

-- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management
-- (IAM) role that AWS CloudFormation assumes to update the stack. AWS
-- CloudFormation uses the role\'s credentials to make calls on your
-- behalf. AWS CloudFormation always uses this role for all future
-- operations on the stack. As long as users have permission to operate on
-- the stack, AWS CloudFormation uses this role even if the users don\'t
-- have permission to pass it. Ensure that the role grants least privilege.
--
-- If you don\'t specify a value, AWS CloudFormation uses the role that was
-- previously associated with the stack. If no role is available, AWS
-- CloudFormation uses a temporary session that is generated from your user
-- credentials.
updateStack_roleARN :: Lens.Lens' UpdateStack (Prelude.Maybe Prelude.Text)
updateStack_roleARN = Lens.lens (\UpdateStack' {roleARN} -> roleARN) (\s@UpdateStack' {} a -> s {roleARN = a} :: UpdateStack)

-- | The template resource types that you have permissions to work with for
-- this update stack action, such as @AWS::EC2::Instance@, @AWS::EC2::*@,
-- or @Custom::MyCustomInstance@.
--
-- If the list of resource types doesn\'t include a resource that you\'re
-- updating, the stack update fails. By default, AWS CloudFormation grants
-- permissions to all resource types. AWS Identity and Access Management
-- (IAM) uses this parameter for AWS CloudFormation-specific condition keys
-- in IAM policies. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html Controlling Access with AWS Identity and Access Management>.
updateStack_resourceTypes :: Lens.Lens' UpdateStack (Prelude.Maybe [Prelude.Text])
updateStack_resourceTypes = Lens.lens (\UpdateStack' {resourceTypes} -> resourceTypes) (\s@UpdateStack' {} a -> s {resourceTypes = a} :: UpdateStack) Prelude.. Lens.mapping Prelude._Coerce

-- | In some cases, you must explicitly acknowledge that your stack template
-- contains certain capabilities in order for AWS CloudFormation to update
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
--     resulting from the macros before actually updating the stack. If
--     your stack template contains one or more macros, and you choose to
--     update a stack directly from the processed template, without first
--     reviewing the resulting changes in a change set, you must
--     acknowledge this capability. This includes the
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/create-reusable-transform-function-snippets-and-add-to-your-template-with-aws-include-transform.html AWS::Include>
--     and
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/transform-aws-serverless.html AWS::Serverless>
--     transforms, which are macros hosted by AWS CloudFormation.
--
--     If you want to update a stack from a stack template that contains
--     macros /and/ nested stacks, you must update the stack directly from
--     the template using this capability.
--
--     You should only update stacks directly from a stack template that
--     contains macros if you know what processing the macro performs.
--
--     Each macro relies on an underlying Lambda service function for
--     processing stack templates. Be aware that the Lambda function owner
--     can update the function operation without AWS CloudFormation being
--     notified.
--
--     For more information, see
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-macros.html Using AWS CloudFormation Macros to Perform Custom Processing on Templates>.
updateStack_capabilities :: Lens.Lens' UpdateStack (Prelude.Maybe [Capability])
updateStack_capabilities = Lens.lens (\UpdateStack' {capabilities} -> capabilities) (\s@UpdateStack' {} a -> s {capabilities = a} :: UpdateStack) Prelude.. Lens.mapping Prelude._Coerce

-- | Structure containing a new stack policy body. You can specify either the
-- @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
--
-- You might update the stack policy, for example, in order to protect a
-- new resource that you created during a stack update. If you do not
-- specify a stack policy, the current policy that is associated with the
-- stack is unchanged.
updateStack_stackPolicyBody :: Lens.Lens' UpdateStack (Prelude.Maybe Prelude.Text)
updateStack_stackPolicyBody = Lens.lens (\UpdateStack' {stackPolicyBody} -> stackPolicyBody) (\s@UpdateStack' {} a -> s {stackPolicyBody = a} :: UpdateStack)

-- | Location of file containing the template body. The URL must point to a
-- template that is located in an Amazon S3 bucket or a Systems Manager
-- document. For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters:
-- @TemplateBody@, @TemplateURL@, or set the @UsePreviousTemplate@ to
-- @true@.
updateStack_templateURL :: Lens.Lens' UpdateStack (Prelude.Maybe Prelude.Text)
updateStack_templateURL = Lens.lens (\UpdateStack' {templateURL} -> templateURL) (\s@UpdateStack' {} a -> s {templateURL = a} :: UpdateStack)

-- | Amazon Simple Notification Service topic Amazon Resource Names (ARNs)
-- that AWS CloudFormation associates with the stack. Specify an empty list
-- to remove all notification topics.
updateStack_notificationARNs :: Lens.Lens' UpdateStack (Prelude.Maybe [Prelude.Text])
updateStack_notificationARNs = Lens.lens (\UpdateStack' {notificationARNs} -> notificationARNs) (\s@UpdateStack' {} a -> s {notificationARNs = a} :: UpdateStack) Prelude.. Lens.mapping Prelude._Coerce

-- | Location of a file containing the updated stack policy. The URL must
-- point to a policy (max size: 16KB) located in an S3 bucket in the same
-- Region as the stack. You can specify either the @StackPolicyBody@ or the
-- @StackPolicyURL@ parameter, but not both.
--
-- You might update the stack policy, for example, in order to protect a
-- new resource that you created during a stack update. If you do not
-- specify a stack policy, the current policy that is associated with the
-- stack is unchanged.
updateStack_stackPolicyURL :: Lens.Lens' UpdateStack (Prelude.Maybe Prelude.Text)
updateStack_stackPolicyURL = Lens.lens (\UpdateStack' {stackPolicyURL} -> stackPolicyURL) (\s@UpdateStack' {} a -> s {stackPolicyURL = a} :: UpdateStack)

-- | Location of a file containing the temporary overriding stack policy. The
-- URL must point to a policy (max size: 16KB) located in an S3 bucket in
-- the same Region as the stack. You can specify either the
-- @StackPolicyDuringUpdateBody@ or the @StackPolicyDuringUpdateURL@
-- parameter, but not both.
--
-- If you want to update protected resources, specify a temporary
-- overriding stack policy during this update. If you do not specify a
-- stack policy, the current policy that is associated with the stack will
-- be used.
updateStack_stackPolicyDuringUpdateURL :: Lens.Lens' UpdateStack (Prelude.Maybe Prelude.Text)
updateStack_stackPolicyDuringUpdateURL = Lens.lens (\UpdateStack' {stackPolicyDuringUpdateURL} -> stackPolicyDuringUpdateURL) (\s@UpdateStack' {} a -> s {stackPolicyDuringUpdateURL = a} :: UpdateStack)

-- | Key-value pairs to associate with this stack. AWS CloudFormation also
-- propagates these tags to supported resources in the stack. You can
-- specify a maximum number of 50 tags.
--
-- If you don\'t specify this parameter, AWS CloudFormation doesn\'t modify
-- the stack\'s tags. If you specify an empty value, AWS CloudFormation
-- removes all associated tags.
updateStack_tags :: Lens.Lens' UpdateStack (Prelude.Maybe [Tag])
updateStack_tags = Lens.lens (\UpdateStack' {tags} -> tags) (\s@UpdateStack' {} a -> s {tags = a} :: UpdateStack) Prelude.. Lens.mapping Prelude._Coerce

-- | The rollback triggers for AWS CloudFormation to monitor during stack
-- creation and updating operations, and for the specified monitoring
-- period afterwards.
updateStack_rollbackConfiguration :: Lens.Lens' UpdateStack (Prelude.Maybe RollbackConfiguration)
updateStack_rollbackConfiguration = Lens.lens (\UpdateStack' {rollbackConfiguration} -> rollbackConfiguration) (\s@UpdateStack' {} a -> s {rollbackConfiguration = a} :: UpdateStack)

-- | A unique identifier for this @UpdateStack@ request. Specify this token
-- if you plan to retry requests so that AWS CloudFormation knows that
-- you\'re not attempting to update a stack with the same name. You might
-- retry @UpdateStack@ requests to ensure that AWS CloudFormation
-- successfully received them.
--
-- All events triggered by a given stack operation are assigned the same
-- client request token, which you can use to track operations. For
-- example, if you execute a @CreateStack@ operation with the token
-- @token1@, then all the @StackEvents@ generated by that operation will
-- have @ClientRequestToken@ set as @token1@.
--
-- In the console, stack operations display the client request token on the
-- Events tab. Stack operations that are initiated from the console use the
-- token format /Console-StackOperation-ID/, which helps you easily
-- identify the stack operation . For example, if you create a stack using
-- the console, each stack event would be assigned the same token in the
-- following format:
-- @Console-CreateStack-7f59c3cf-00d2-40c7-b2ff-e75db0987002@.
updateStack_clientRequestToken :: Lens.Lens' UpdateStack (Prelude.Maybe Prelude.Text)
updateStack_clientRequestToken = Lens.lens (\UpdateStack' {clientRequestToken} -> clientRequestToken) (\s@UpdateStack' {} a -> s {clientRequestToken = a} :: UpdateStack)

-- | Structure containing the template body with a minimum length of 1 byte
-- and a maximum length of 51,200 bytes. (For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.)
--
-- Conditional: You must specify only one of the following parameters:
-- @TemplateBody@, @TemplateURL@, or set the @UsePreviousTemplate@ to
-- @true@.
updateStack_templateBody :: Lens.Lens' UpdateStack (Prelude.Maybe Prelude.Text)
updateStack_templateBody = Lens.lens (\UpdateStack' {templateBody} -> templateBody) (\s@UpdateStack' {} a -> s {templateBody = a} :: UpdateStack)

-- | A list of @Parameter@ structures that specify input parameters for the
-- stack. For more information, see the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter>
-- data type.
updateStack_parameters :: Lens.Lens' UpdateStack (Prelude.Maybe [Parameter])
updateStack_parameters = Lens.lens (\UpdateStack' {parameters} -> parameters) (\s@UpdateStack' {} a -> s {parameters = a} :: UpdateStack) Prelude.. Lens.mapping Prelude._Coerce

-- | Reuse the existing template that is associated with the stack that you
-- are updating.
--
-- Conditional: You must specify only one of the following parameters:
-- @TemplateBody@, @TemplateURL@, or set the @UsePreviousTemplate@ to
-- @true@.
updateStack_usePreviousTemplate :: Lens.Lens' UpdateStack (Prelude.Maybe Prelude.Bool)
updateStack_usePreviousTemplate = Lens.lens (\UpdateStack' {usePreviousTemplate} -> usePreviousTemplate) (\s@UpdateStack' {} a -> s {usePreviousTemplate = a} :: UpdateStack)

-- | The name or unique stack ID of the stack to update.
updateStack_stackName :: Lens.Lens' UpdateStack Prelude.Text
updateStack_stackName = Lens.lens (\UpdateStack' {stackName} -> stackName) (\s@UpdateStack' {} a -> s {stackName = a} :: UpdateStack)

instance Prelude.AWSRequest UpdateStack where
  type Rs UpdateStack = UpdateStackResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "UpdateStackResult"
      ( \s h x ->
          UpdateStackResponse'
            Prelude.<$> (x Prelude..@? "StackId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateStack

instance Prelude.NFData UpdateStack

instance Prelude.ToHeaders UpdateStack where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath UpdateStack where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateStack where
  toQuery UpdateStack' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("UpdateStack" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-15" :: Prelude.ByteString),
        "StackPolicyDuringUpdateBody"
          Prelude.=: stackPolicyDuringUpdateBody,
        "RoleARN" Prelude.=: roleARN,
        "ResourceTypes"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> resourceTypes
            ),
        "Capabilities"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> capabilities
            ),
        "StackPolicyBody" Prelude.=: stackPolicyBody,
        "TemplateURL" Prelude.=: templateURL,
        "NotificationARNs"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> notificationARNs
            ),
        "StackPolicyURL" Prelude.=: stackPolicyURL,
        "StackPolicyDuringUpdateURL"
          Prelude.=: stackPolicyDuringUpdateURL,
        "Tags"
          Prelude.=: Prelude.toQuery
            (Prelude.toQueryList "member" Prelude.<$> tags),
        "RollbackConfiguration"
          Prelude.=: rollbackConfiguration,
        "ClientRequestToken" Prelude.=: clientRequestToken,
        "TemplateBody" Prelude.=: templateBody,
        "Parameters"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> parameters
            ),
        "UsePreviousTemplate" Prelude.=: usePreviousTemplate,
        "StackName" Prelude.=: stackName
      ]

-- | The output for an UpdateStack action.
--
-- /See:/ 'newUpdateStackResponse' smart constructor.
data UpdateStackResponse = UpdateStackResponse'
  { -- | Unique identifier of the stack.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateStackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackId', 'updateStackResponse_stackId' - Unique identifier of the stack.
--
-- 'httpStatus', 'updateStackResponse_httpStatus' - The response's http status code.
newUpdateStackResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateStackResponse
newUpdateStackResponse pHttpStatus_ =
  UpdateStackResponse'
    { stackId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Unique identifier of the stack.
updateStackResponse_stackId :: Lens.Lens' UpdateStackResponse (Prelude.Maybe Prelude.Text)
updateStackResponse_stackId = Lens.lens (\UpdateStackResponse' {stackId} -> stackId) (\s@UpdateStackResponse' {} a -> s {stackId = a} :: UpdateStackResponse)

-- | The response's http status code.
updateStackResponse_httpStatus :: Lens.Lens' UpdateStackResponse Prelude.Int
updateStackResponse_httpStatus = Lens.lens (\UpdateStackResponse' {httpStatus} -> httpStatus) (\s@UpdateStackResponse' {} a -> s {httpStatus = a} :: UpdateStackResponse)

instance Prelude.NFData UpdateStackResponse
