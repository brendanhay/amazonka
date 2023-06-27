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
-- Module      : Amazonka.CloudFormation.UpdateStack
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a stack as specified in the template. After the call completes
-- successfully, the stack update starts. You can check the status of the
-- stack through the DescribeStacks action.
--
-- To get a copy of the template for an existing stack, you can use the
-- GetTemplate action.
--
-- For more information about creating an update template, updating a
-- stack, and monitoring the progress of the update, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-updating-stacks.html Updating a Stack>.
module Amazonka.CloudFormation.UpdateStack
  ( -- * Creating a Request
    UpdateStack (..),
    newUpdateStack,

    -- * Request Lenses
    updateStack_capabilities,
    updateStack_clientRequestToken,
    updateStack_disableRollback,
    updateStack_notificationARNs,
    updateStack_parameters,
    updateStack_resourceTypes,
    updateStack_roleARN,
    updateStack_rollbackConfiguration,
    updateStack_stackPolicyBody,
    updateStack_stackPolicyDuringUpdateBody,
    updateStack_stackPolicyDuringUpdateURL,
    updateStack_stackPolicyURL,
    updateStack_tags,
    updateStack_templateBody,
    updateStack_templateURL,
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

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for an UpdateStack action.
--
-- /See:/ 'newUpdateStack' smart constructor.
data UpdateStack = UpdateStack'
  { -- | In some cases, you must explicitly acknowledge that your stack template
    -- contains certain capabilities in order for CloudFormation to update the
    -- stack.
    --
    -- -   @CAPABILITY_IAM@ and @CAPABILITY_NAMED_IAM@
    --
    --     Some stack templates might include resources that can affect
    --     permissions in your Amazon Web Services account; for example, by
    --     creating new Identity and Access Management (IAM) users. For those
    --     stacks, you must explicitly acknowledge this by specifying one of
    --     these capabilities.
    --
    --     The following IAM resources require you to specify either the
    --     @CAPABILITY_IAM@ or @CAPABILITY_NAMED_IAM@ capability.
    --
    --     -   If you have IAM resources, you can specify either capability.
    --
    --     -   If you have IAM resources with custom names, you /must/ specify
    --         @CAPABILITY_NAMED_IAM@.
    --
    --     -   If you don\'t specify either of these capabilities,
    --         CloudFormation returns an @InsufficientCapabilities@ error.
    --
    --     If your stack template contains these resources, we suggest that you
    --     review all permissions associated with them and edit their
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
    --     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in CloudFormation Templates>.
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
    --     transforms, which are macros hosted by CloudFormation.
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
    --     can update the function operation without CloudFormation being
    --     notified.
    --
    --     For more information, see
    --     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-macros.html Using CloudFormation Macros to Perform Custom Processing on Templates>.
    capabilities :: Prelude.Maybe [Capability],
    -- | A unique identifier for this @UpdateStack@ request. Specify this token
    -- if you plan to retry requests so that CloudFormation knows that you\'re
    -- not attempting to update a stack with the same name. You might retry
    -- @UpdateStack@ requests to ensure that CloudFormation successfully
    -- received them.
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
    -- | Preserve the state of previously provisioned resources when an operation
    -- fails.
    --
    -- Default: @False@
    disableRollback :: Prelude.Maybe Prelude.Bool,
    -- | Amazon Simple Notification Service topic Amazon Resource Names (ARNs)
    -- that CloudFormation associates with the stack. Specify an empty list to
    -- remove all notification topics.
    notificationARNs :: Prelude.Maybe [Prelude.Text],
    -- | A list of @Parameter@ structures that specify input parameters for the
    -- stack. For more information, see the
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter>
    -- data type.
    parameters :: Prelude.Maybe [Parameter],
    -- | The template resource types that you have permissions to work with for
    -- this update stack action, such as @AWS::EC2::Instance@, @AWS::EC2::*@,
    -- or @Custom::MyCustomInstance@.
    --
    -- If the list of resource types doesn\'t include a resource that you\'re
    -- updating, the stack update fails. By default, CloudFormation grants
    -- permissions to all resource types. Identity and Access Management (IAM)
    -- uses this parameter for CloudFormation-specific condition keys in IAM
    -- policies. For more information, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html Controlling Access with Identity and Access Management>.
    resourceTypes :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of an Identity and Access Management
    -- (IAM) role that CloudFormation assumes to update the stack.
    -- CloudFormation uses the role\'s credentials to make calls on your
    -- behalf. CloudFormation always uses this role for all future operations
    -- on the stack. Provided that users have permission to operate on the
    -- stack, CloudFormation uses this role even if the users don\'t have
    -- permission to pass it. Ensure that the role grants least privilege.
    --
    -- If you don\'t specify a value, CloudFormation uses the role that was
    -- previously associated with the stack. If no role is available,
    -- CloudFormation uses a temporary session that is generated from your user
    -- credentials.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | The rollback triggers for CloudFormation to monitor during stack
    -- creation and updating operations, and for the specified monitoring
    -- period afterwards.
    rollbackConfiguration :: Prelude.Maybe RollbackConfiguration,
    -- | Structure containing a new stack policy body. You can specify either the
    -- @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
    --
    -- You might update the stack policy, for example, in order to protect a
    -- new resource that you created during a stack update. If you don\'t
    -- specify a stack policy, the current policy that is associated with the
    -- stack is unchanged.
    stackPolicyBody :: Prelude.Maybe Prelude.Text,
    -- | Structure containing the temporary overriding stack policy body. You can
    -- specify either the @StackPolicyDuringUpdateBody@ or the
    -- @StackPolicyDuringUpdateURL@ parameter, but not both.
    --
    -- If you want to update protected resources, specify a temporary
    -- overriding stack policy during this update. If you don\'t specify a
    -- stack policy, the current policy that is associated with the stack will
    -- be used.
    stackPolicyDuringUpdateBody :: Prelude.Maybe Prelude.Text,
    -- | Location of a file containing the temporary overriding stack policy. The
    -- URL must point to a policy (max size: 16KB) located in an S3 bucket in
    -- the same Region as the stack. You can specify either the
    -- @StackPolicyDuringUpdateBody@ or the @StackPolicyDuringUpdateURL@
    -- parameter, but not both.
    --
    -- If you want to update protected resources, specify a temporary
    -- overriding stack policy during this update. If you don\'t specify a
    -- stack policy, the current policy that is associated with the stack will
    -- be used.
    stackPolicyDuringUpdateURL :: Prelude.Maybe Prelude.Text,
    -- | Location of a file containing the updated stack policy. The URL must
    -- point to a policy (max size: 16KB) located in an S3 bucket in the same
    -- Region as the stack. You can specify either the @StackPolicyBody@ or the
    -- @StackPolicyURL@ parameter, but not both.
    --
    -- You might update the stack policy, for example, in order to protect a
    -- new resource that you created during a stack update. If you don\'t
    -- specify a stack policy, the current policy that is associated with the
    -- stack is unchanged.
    stackPolicyURL :: Prelude.Maybe Prelude.Text,
    -- | Key-value pairs to associate with this stack. CloudFormation also
    -- propagates these tags to supported resources in the stack. You can
    -- specify a maximum number of 50 tags.
    --
    -- If you don\'t specify this parameter, CloudFormation doesn\'t modify the
    -- stack\'s tags. If you specify an empty value, CloudFormation removes all
    -- associated tags.
    tags :: Prelude.Maybe [Tag],
    -- | Structure containing the template body with a minimum length of 1 byte
    -- and a maximum length of 51,200 bytes. (For more information, go to
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
    -- in the CloudFormation User Guide.)
    --
    -- Conditional: You must specify only one of the following parameters:
    -- @TemplateBody@, @TemplateURL@, or set the @UsePreviousTemplate@ to
    -- @true@.
    templateBody :: Prelude.Maybe Prelude.Text,
    -- | Location of file containing the template body. The URL must point to a
    -- template that\'s located in an Amazon S3 bucket or a Systems Manager
    -- document. For more information, go to
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
    -- in the CloudFormation User Guide.
    --
    -- Conditional: You must specify only one of the following parameters:
    -- @TemplateBody@, @TemplateURL@, or set the @UsePreviousTemplate@ to
    -- @true@.
    templateURL :: Prelude.Maybe Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capabilities', 'updateStack_capabilities' - In some cases, you must explicitly acknowledge that your stack template
-- contains certain capabilities in order for CloudFormation to update the
-- stack.
--
-- -   @CAPABILITY_IAM@ and @CAPABILITY_NAMED_IAM@
--
--     Some stack templates might include resources that can affect
--     permissions in your Amazon Web Services account; for example, by
--     creating new Identity and Access Management (IAM) users. For those
--     stacks, you must explicitly acknowledge this by specifying one of
--     these capabilities.
--
--     The following IAM resources require you to specify either the
--     @CAPABILITY_IAM@ or @CAPABILITY_NAMED_IAM@ capability.
--
--     -   If you have IAM resources, you can specify either capability.
--
--     -   If you have IAM resources with custom names, you /must/ specify
--         @CAPABILITY_NAMED_IAM@.
--
--     -   If you don\'t specify either of these capabilities,
--         CloudFormation returns an @InsufficientCapabilities@ error.
--
--     If your stack template contains these resources, we suggest that you
--     review all permissions associated with them and edit their
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
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in CloudFormation Templates>.
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
--     transforms, which are macros hosted by CloudFormation.
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
--     can update the function operation without CloudFormation being
--     notified.
--
--     For more information, see
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-macros.html Using CloudFormation Macros to Perform Custom Processing on Templates>.
--
-- 'clientRequestToken', 'updateStack_clientRequestToken' - A unique identifier for this @UpdateStack@ request. Specify this token
-- if you plan to retry requests so that CloudFormation knows that you\'re
-- not attempting to update a stack with the same name. You might retry
-- @UpdateStack@ requests to ensure that CloudFormation successfully
-- received them.
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
-- 'disableRollback', 'updateStack_disableRollback' - Preserve the state of previously provisioned resources when an operation
-- fails.
--
-- Default: @False@
--
-- 'notificationARNs', 'updateStack_notificationARNs' - Amazon Simple Notification Service topic Amazon Resource Names (ARNs)
-- that CloudFormation associates with the stack. Specify an empty list to
-- remove all notification topics.
--
-- 'parameters', 'updateStack_parameters' - A list of @Parameter@ structures that specify input parameters for the
-- stack. For more information, see the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter>
-- data type.
--
-- 'resourceTypes', 'updateStack_resourceTypes' - The template resource types that you have permissions to work with for
-- this update stack action, such as @AWS::EC2::Instance@, @AWS::EC2::*@,
-- or @Custom::MyCustomInstance@.
--
-- If the list of resource types doesn\'t include a resource that you\'re
-- updating, the stack update fails. By default, CloudFormation grants
-- permissions to all resource types. Identity and Access Management (IAM)
-- uses this parameter for CloudFormation-specific condition keys in IAM
-- policies. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html Controlling Access with Identity and Access Management>.
--
-- 'roleARN', 'updateStack_roleARN' - The Amazon Resource Name (ARN) of an Identity and Access Management
-- (IAM) role that CloudFormation assumes to update the stack.
-- CloudFormation uses the role\'s credentials to make calls on your
-- behalf. CloudFormation always uses this role for all future operations
-- on the stack. Provided that users have permission to operate on the
-- stack, CloudFormation uses this role even if the users don\'t have
-- permission to pass it. Ensure that the role grants least privilege.
--
-- If you don\'t specify a value, CloudFormation uses the role that was
-- previously associated with the stack. If no role is available,
-- CloudFormation uses a temporary session that is generated from your user
-- credentials.
--
-- 'rollbackConfiguration', 'updateStack_rollbackConfiguration' - The rollback triggers for CloudFormation to monitor during stack
-- creation and updating operations, and for the specified monitoring
-- period afterwards.
--
-- 'stackPolicyBody', 'updateStack_stackPolicyBody' - Structure containing a new stack policy body. You can specify either the
-- @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
--
-- You might update the stack policy, for example, in order to protect a
-- new resource that you created during a stack update. If you don\'t
-- specify a stack policy, the current policy that is associated with the
-- stack is unchanged.
--
-- 'stackPolicyDuringUpdateBody', 'updateStack_stackPolicyDuringUpdateBody' - Structure containing the temporary overriding stack policy body. You can
-- specify either the @StackPolicyDuringUpdateBody@ or the
-- @StackPolicyDuringUpdateURL@ parameter, but not both.
--
-- If you want to update protected resources, specify a temporary
-- overriding stack policy during this update. If you don\'t specify a
-- stack policy, the current policy that is associated with the stack will
-- be used.
--
-- 'stackPolicyDuringUpdateURL', 'updateStack_stackPolicyDuringUpdateURL' - Location of a file containing the temporary overriding stack policy. The
-- URL must point to a policy (max size: 16KB) located in an S3 bucket in
-- the same Region as the stack. You can specify either the
-- @StackPolicyDuringUpdateBody@ or the @StackPolicyDuringUpdateURL@
-- parameter, but not both.
--
-- If you want to update protected resources, specify a temporary
-- overriding stack policy during this update. If you don\'t specify a
-- stack policy, the current policy that is associated with the stack will
-- be used.
--
-- 'stackPolicyURL', 'updateStack_stackPolicyURL' - Location of a file containing the updated stack policy. The URL must
-- point to a policy (max size: 16KB) located in an S3 bucket in the same
-- Region as the stack. You can specify either the @StackPolicyBody@ or the
-- @StackPolicyURL@ parameter, but not both.
--
-- You might update the stack policy, for example, in order to protect a
-- new resource that you created during a stack update. If you don\'t
-- specify a stack policy, the current policy that is associated with the
-- stack is unchanged.
--
-- 'tags', 'updateStack_tags' - Key-value pairs to associate with this stack. CloudFormation also
-- propagates these tags to supported resources in the stack. You can
-- specify a maximum number of 50 tags.
--
-- If you don\'t specify this parameter, CloudFormation doesn\'t modify the
-- stack\'s tags. If you specify an empty value, CloudFormation removes all
-- associated tags.
--
-- 'templateBody', 'updateStack_templateBody' - Structure containing the template body with a minimum length of 1 byte
-- and a maximum length of 51,200 bytes. (For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the CloudFormation User Guide.)
--
-- Conditional: You must specify only one of the following parameters:
-- @TemplateBody@, @TemplateURL@, or set the @UsePreviousTemplate@ to
-- @true@.
--
-- 'templateURL', 'updateStack_templateURL' - Location of file containing the template body. The URL must point to a
-- template that\'s located in an Amazon S3 bucket or a Systems Manager
-- document. For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters:
-- @TemplateBody@, @TemplateURL@, or set the @UsePreviousTemplate@ to
-- @true@.
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
    { capabilities = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      disableRollback = Prelude.Nothing,
      notificationARNs = Prelude.Nothing,
      parameters = Prelude.Nothing,
      resourceTypes = Prelude.Nothing,
      roleARN = Prelude.Nothing,
      rollbackConfiguration = Prelude.Nothing,
      stackPolicyBody = Prelude.Nothing,
      stackPolicyDuringUpdateBody = Prelude.Nothing,
      stackPolicyDuringUpdateURL = Prelude.Nothing,
      stackPolicyURL = Prelude.Nothing,
      tags = Prelude.Nothing,
      templateBody = Prelude.Nothing,
      templateURL = Prelude.Nothing,
      usePreviousTemplate = Prelude.Nothing,
      stackName = pStackName_
    }

-- | In some cases, you must explicitly acknowledge that your stack template
-- contains certain capabilities in order for CloudFormation to update the
-- stack.
--
-- -   @CAPABILITY_IAM@ and @CAPABILITY_NAMED_IAM@
--
--     Some stack templates might include resources that can affect
--     permissions in your Amazon Web Services account; for example, by
--     creating new Identity and Access Management (IAM) users. For those
--     stacks, you must explicitly acknowledge this by specifying one of
--     these capabilities.
--
--     The following IAM resources require you to specify either the
--     @CAPABILITY_IAM@ or @CAPABILITY_NAMED_IAM@ capability.
--
--     -   If you have IAM resources, you can specify either capability.
--
--     -   If you have IAM resources with custom names, you /must/ specify
--         @CAPABILITY_NAMED_IAM@.
--
--     -   If you don\'t specify either of these capabilities,
--         CloudFormation returns an @InsufficientCapabilities@ error.
--
--     If your stack template contains these resources, we suggest that you
--     review all permissions associated with them and edit their
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
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in CloudFormation Templates>.
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
--     transforms, which are macros hosted by CloudFormation.
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
--     can update the function operation without CloudFormation being
--     notified.
--
--     For more information, see
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-macros.html Using CloudFormation Macros to Perform Custom Processing on Templates>.
updateStack_capabilities :: Lens.Lens' UpdateStack (Prelude.Maybe [Capability])
updateStack_capabilities = Lens.lens (\UpdateStack' {capabilities} -> capabilities) (\s@UpdateStack' {} a -> s {capabilities = a} :: UpdateStack) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier for this @UpdateStack@ request. Specify this token
-- if you plan to retry requests so that CloudFormation knows that you\'re
-- not attempting to update a stack with the same name. You might retry
-- @UpdateStack@ requests to ensure that CloudFormation successfully
-- received them.
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

-- | Preserve the state of previously provisioned resources when an operation
-- fails.
--
-- Default: @False@
updateStack_disableRollback :: Lens.Lens' UpdateStack (Prelude.Maybe Prelude.Bool)
updateStack_disableRollback = Lens.lens (\UpdateStack' {disableRollback} -> disableRollback) (\s@UpdateStack' {} a -> s {disableRollback = a} :: UpdateStack)

-- | Amazon Simple Notification Service topic Amazon Resource Names (ARNs)
-- that CloudFormation associates with the stack. Specify an empty list to
-- remove all notification topics.
updateStack_notificationARNs :: Lens.Lens' UpdateStack (Prelude.Maybe [Prelude.Text])
updateStack_notificationARNs = Lens.lens (\UpdateStack' {notificationARNs} -> notificationARNs) (\s@UpdateStack' {} a -> s {notificationARNs = a} :: UpdateStack) Prelude.. Lens.mapping Lens.coerced

-- | A list of @Parameter@ structures that specify input parameters for the
-- stack. For more information, see the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter>
-- data type.
updateStack_parameters :: Lens.Lens' UpdateStack (Prelude.Maybe [Parameter])
updateStack_parameters = Lens.lens (\UpdateStack' {parameters} -> parameters) (\s@UpdateStack' {} a -> s {parameters = a} :: UpdateStack) Prelude.. Lens.mapping Lens.coerced

-- | The template resource types that you have permissions to work with for
-- this update stack action, such as @AWS::EC2::Instance@, @AWS::EC2::*@,
-- or @Custom::MyCustomInstance@.
--
-- If the list of resource types doesn\'t include a resource that you\'re
-- updating, the stack update fails. By default, CloudFormation grants
-- permissions to all resource types. Identity and Access Management (IAM)
-- uses this parameter for CloudFormation-specific condition keys in IAM
-- policies. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html Controlling Access with Identity and Access Management>.
updateStack_resourceTypes :: Lens.Lens' UpdateStack (Prelude.Maybe [Prelude.Text])
updateStack_resourceTypes = Lens.lens (\UpdateStack' {resourceTypes} -> resourceTypes) (\s@UpdateStack' {} a -> s {resourceTypes = a} :: UpdateStack) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of an Identity and Access Management
-- (IAM) role that CloudFormation assumes to update the stack.
-- CloudFormation uses the role\'s credentials to make calls on your
-- behalf. CloudFormation always uses this role for all future operations
-- on the stack. Provided that users have permission to operate on the
-- stack, CloudFormation uses this role even if the users don\'t have
-- permission to pass it. Ensure that the role grants least privilege.
--
-- If you don\'t specify a value, CloudFormation uses the role that was
-- previously associated with the stack. If no role is available,
-- CloudFormation uses a temporary session that is generated from your user
-- credentials.
updateStack_roleARN :: Lens.Lens' UpdateStack (Prelude.Maybe Prelude.Text)
updateStack_roleARN = Lens.lens (\UpdateStack' {roleARN} -> roleARN) (\s@UpdateStack' {} a -> s {roleARN = a} :: UpdateStack)

-- | The rollback triggers for CloudFormation to monitor during stack
-- creation and updating operations, and for the specified monitoring
-- period afterwards.
updateStack_rollbackConfiguration :: Lens.Lens' UpdateStack (Prelude.Maybe RollbackConfiguration)
updateStack_rollbackConfiguration = Lens.lens (\UpdateStack' {rollbackConfiguration} -> rollbackConfiguration) (\s@UpdateStack' {} a -> s {rollbackConfiguration = a} :: UpdateStack)

-- | Structure containing a new stack policy body. You can specify either the
-- @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
--
-- You might update the stack policy, for example, in order to protect a
-- new resource that you created during a stack update. If you don\'t
-- specify a stack policy, the current policy that is associated with the
-- stack is unchanged.
updateStack_stackPolicyBody :: Lens.Lens' UpdateStack (Prelude.Maybe Prelude.Text)
updateStack_stackPolicyBody = Lens.lens (\UpdateStack' {stackPolicyBody} -> stackPolicyBody) (\s@UpdateStack' {} a -> s {stackPolicyBody = a} :: UpdateStack)

-- | Structure containing the temporary overriding stack policy body. You can
-- specify either the @StackPolicyDuringUpdateBody@ or the
-- @StackPolicyDuringUpdateURL@ parameter, but not both.
--
-- If you want to update protected resources, specify a temporary
-- overriding stack policy during this update. If you don\'t specify a
-- stack policy, the current policy that is associated with the stack will
-- be used.
updateStack_stackPolicyDuringUpdateBody :: Lens.Lens' UpdateStack (Prelude.Maybe Prelude.Text)
updateStack_stackPolicyDuringUpdateBody = Lens.lens (\UpdateStack' {stackPolicyDuringUpdateBody} -> stackPolicyDuringUpdateBody) (\s@UpdateStack' {} a -> s {stackPolicyDuringUpdateBody = a} :: UpdateStack)

-- | Location of a file containing the temporary overriding stack policy. The
-- URL must point to a policy (max size: 16KB) located in an S3 bucket in
-- the same Region as the stack. You can specify either the
-- @StackPolicyDuringUpdateBody@ or the @StackPolicyDuringUpdateURL@
-- parameter, but not both.
--
-- If you want to update protected resources, specify a temporary
-- overriding stack policy during this update. If you don\'t specify a
-- stack policy, the current policy that is associated with the stack will
-- be used.
updateStack_stackPolicyDuringUpdateURL :: Lens.Lens' UpdateStack (Prelude.Maybe Prelude.Text)
updateStack_stackPolicyDuringUpdateURL = Lens.lens (\UpdateStack' {stackPolicyDuringUpdateURL} -> stackPolicyDuringUpdateURL) (\s@UpdateStack' {} a -> s {stackPolicyDuringUpdateURL = a} :: UpdateStack)

-- | Location of a file containing the updated stack policy. The URL must
-- point to a policy (max size: 16KB) located in an S3 bucket in the same
-- Region as the stack. You can specify either the @StackPolicyBody@ or the
-- @StackPolicyURL@ parameter, but not both.
--
-- You might update the stack policy, for example, in order to protect a
-- new resource that you created during a stack update. If you don\'t
-- specify a stack policy, the current policy that is associated with the
-- stack is unchanged.
updateStack_stackPolicyURL :: Lens.Lens' UpdateStack (Prelude.Maybe Prelude.Text)
updateStack_stackPolicyURL = Lens.lens (\UpdateStack' {stackPolicyURL} -> stackPolicyURL) (\s@UpdateStack' {} a -> s {stackPolicyURL = a} :: UpdateStack)

-- | Key-value pairs to associate with this stack. CloudFormation also
-- propagates these tags to supported resources in the stack. You can
-- specify a maximum number of 50 tags.
--
-- If you don\'t specify this parameter, CloudFormation doesn\'t modify the
-- stack\'s tags. If you specify an empty value, CloudFormation removes all
-- associated tags.
updateStack_tags :: Lens.Lens' UpdateStack (Prelude.Maybe [Tag])
updateStack_tags = Lens.lens (\UpdateStack' {tags} -> tags) (\s@UpdateStack' {} a -> s {tags = a} :: UpdateStack) Prelude.. Lens.mapping Lens.coerced

-- | Structure containing the template body with a minimum length of 1 byte
-- and a maximum length of 51,200 bytes. (For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the CloudFormation User Guide.)
--
-- Conditional: You must specify only one of the following parameters:
-- @TemplateBody@, @TemplateURL@, or set the @UsePreviousTemplate@ to
-- @true@.
updateStack_templateBody :: Lens.Lens' UpdateStack (Prelude.Maybe Prelude.Text)
updateStack_templateBody = Lens.lens (\UpdateStack' {templateBody} -> templateBody) (\s@UpdateStack' {} a -> s {templateBody = a} :: UpdateStack)

-- | Location of file containing the template body. The URL must point to a
-- template that\'s located in an Amazon S3 bucket or a Systems Manager
-- document. For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters:
-- @TemplateBody@, @TemplateURL@, or set the @UsePreviousTemplate@ to
-- @true@.
updateStack_templateURL :: Lens.Lens' UpdateStack (Prelude.Maybe Prelude.Text)
updateStack_templateURL = Lens.lens (\UpdateStack' {templateURL} -> templateURL) (\s@UpdateStack' {} a -> s {templateURL = a} :: UpdateStack)

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

instance Core.AWSRequest UpdateStack where
  type AWSResponse UpdateStack = UpdateStackResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "UpdateStackResult"
      ( \s h x ->
          UpdateStackResponse'
            Prelude.<$> (x Data..@? "StackId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateStack where
  hashWithSalt _salt UpdateStack' {..} =
    _salt
      `Prelude.hashWithSalt` capabilities
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` disableRollback
      `Prelude.hashWithSalt` notificationARNs
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` resourceTypes
      `Prelude.hashWithSalt` roleARN
      `Prelude.hashWithSalt` rollbackConfiguration
      `Prelude.hashWithSalt` stackPolicyBody
      `Prelude.hashWithSalt` stackPolicyDuringUpdateBody
      `Prelude.hashWithSalt` stackPolicyDuringUpdateURL
      `Prelude.hashWithSalt` stackPolicyURL
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` templateBody
      `Prelude.hashWithSalt` templateURL
      `Prelude.hashWithSalt` usePreviousTemplate
      `Prelude.hashWithSalt` stackName

instance Prelude.NFData UpdateStack where
  rnf UpdateStack' {..} =
    Prelude.rnf capabilities
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf disableRollback
      `Prelude.seq` Prelude.rnf notificationARNs
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf resourceTypes
      `Prelude.seq` Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf rollbackConfiguration
      `Prelude.seq` Prelude.rnf stackPolicyBody
      `Prelude.seq` Prelude.rnf stackPolicyDuringUpdateBody
      `Prelude.seq` Prelude.rnf stackPolicyDuringUpdateURL
      `Prelude.seq` Prelude.rnf stackPolicyURL
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf templateBody
      `Prelude.seq` Prelude.rnf templateURL
      `Prelude.seq` Prelude.rnf usePreviousTemplate
      `Prelude.seq` Prelude.rnf stackName

instance Data.ToHeaders UpdateStack where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath UpdateStack where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateStack where
  toQuery UpdateStack' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("UpdateStack" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "Capabilities"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> capabilities),
        "ClientRequestToken" Data.=: clientRequestToken,
        "DisableRollback" Data.=: disableRollback,
        "NotificationARNs"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> notificationARNs
            ),
        "Parameters"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> parameters),
        "ResourceTypes"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> resourceTypes
            ),
        "RoleARN" Data.=: roleARN,
        "RollbackConfiguration"
          Data.=: rollbackConfiguration,
        "StackPolicyBody" Data.=: stackPolicyBody,
        "StackPolicyDuringUpdateBody"
          Data.=: stackPolicyDuringUpdateBody,
        "StackPolicyDuringUpdateURL"
          Data.=: stackPolicyDuringUpdateURL,
        "StackPolicyURL" Data.=: stackPolicyURL,
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> tags),
        "TemplateBody" Data.=: templateBody,
        "TemplateURL" Data.=: templateURL,
        "UsePreviousTemplate" Data.=: usePreviousTemplate,
        "StackName" Data.=: stackName
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData UpdateStackResponse where
  rnf UpdateStackResponse' {..} =
    Prelude.rnf stackId
      `Prelude.seq` Prelude.rnf httpStatus
