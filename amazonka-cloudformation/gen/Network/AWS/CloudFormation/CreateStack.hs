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
-- Module      : Network.AWS.CloudFormation.CreateStack
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a stack as specified in the template. After the call completes
-- successfully, the stack creation starts. You can check the status of the
-- stack via the DescribeStacks API.
module Network.AWS.CloudFormation.CreateStack
  ( -- * Creating a Request
    CreateStack (..),
    newCreateStack,

    -- * Request Lenses
    createStack_onFailure,
    createStack_roleARN,
    createStack_resourceTypes,
    createStack_capabilities,
    createStack_stackPolicyBody,
    createStack_enableTerminationProtection,
    createStack_templateURL,
    createStack_notificationARNs,
    createStack_stackPolicyURL,
    createStack_tags,
    createStack_timeoutInMinutes,
    createStack_rollbackConfiguration,
    createStack_clientRequestToken,
    createStack_templateBody,
    createStack_disableRollback,
    createStack_parameters,
    createStack_stackName,

    -- * Destructuring the Response
    CreateStackResponse (..),
    newCreateStackResponse,

    -- * Response Lenses
    createStackResponse_stackId,
    createStackResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for CreateStack action.
--
-- /See:/ 'newCreateStack' smart constructor.
data CreateStack = CreateStack'
  { -- | Determines what action will be taken if stack creation fails. This must
    -- be one of: DO_NOTHING, ROLLBACK, or DELETE. You can specify either
    -- @OnFailure@ or @DisableRollback@, but not both.
    --
    -- Default: @ROLLBACK@
    onFailure :: Prelude.Maybe OnFailure,
    -- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management
    -- (IAM) role that AWS CloudFormation assumes to create the stack. AWS
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
    -- this create stack action, such as @AWS::EC2::Instance@, @AWS::EC2::*@,
    -- or @Custom::MyCustomInstance@. Use the following syntax to describe
    -- template resource types: @AWS::*@ (for all AWS resource), @Custom::*@
    -- (for all custom resources), @Custom::logical_ID @ (for a specific custom
    -- resource), @AWS::service_name::*@ (for all resources of a particular AWS
    -- service), and @AWS::service_name::resource_logical_ID @ (for a specific
    -- AWS resource).
    --
    -- If the list of resource types doesn\'t include a resource that you\'re
    -- creating, the stack creation fails. By default, AWS CloudFormation
    -- grants permissions to all resource types. AWS Identity and Access
    -- Management (IAM) uses this parameter for AWS CloudFormation-specific
    -- condition keys in IAM policies. For more information, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html Controlling Access with AWS Identity and Access Management>.
    resourceTypes :: Prelude.Maybe [Prelude.Text],
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
    --     If you want to create a stack from a stack template that contains
    --     macros /and/ nested stacks, you must create the stack directly from
    --     the template using this capability.
    --
    --     You should only create stacks directly from a stack template that
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
    -- | Structure containing the stack policy body. For more information, go to
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources>
    -- in the /AWS CloudFormation User Guide/. You can specify either the
    -- @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
    stackPolicyBody :: Prelude.Maybe Prelude.Text,
    -- | Whether to enable termination protection on the specified stack. If a
    -- user attempts to delete a stack with termination protection enabled, the
    -- operation fails and the stack remains unchanged. For more information,
    -- see
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html Protecting a Stack From Being Deleted>
    -- in the /AWS CloudFormation User Guide/. Termination protection is
    -- disabled on stacks by default.
    --
    -- For
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html nested stacks>,
    -- termination protection is set on the root stack and cannot be changed
    -- directly on the nested stack.
    enableTerminationProtection :: Prelude.Maybe Prelude.Bool,
    -- | Location of file containing the template body. The URL must point to a
    -- template (max size: 460,800 bytes) that is located in an Amazon S3
    -- bucket or a Systems Manager document. For more information, go to the
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
    -- in the AWS CloudFormation User Guide.
    --
    -- Conditional: You must specify either the @TemplateBody@ or the
    -- @TemplateURL@ parameter, but not both.
    templateURL :: Prelude.Maybe Prelude.Text,
    -- | The Simple Notification Service (SNS) topic ARNs to publish stack
    -- related events. You can find your SNS topic ARNs using the SNS console
    -- or your Command Line Interface (CLI).
    notificationARNs :: Prelude.Maybe [Prelude.Text],
    -- | Location of a file containing the stack policy. The URL must point to a
    -- policy (maximum size: 16 KB) located in an S3 bucket in the same Region
    -- as the stack. You can specify either the @StackPolicyBody@ or the
    -- @StackPolicyURL@ parameter, but not both.
    stackPolicyURL :: Prelude.Maybe Prelude.Text,
    -- | Key-value pairs to associate with this stack. AWS CloudFormation also
    -- propagates these tags to the resources created in the stack. A maximum
    -- number of 50 tags can be specified.
    tags :: Prelude.Maybe [Tag],
    -- | The amount of time that can pass before the stack status becomes
    -- CREATE_FAILED; if @DisableRollback@ is not set or is set to @false@, the
    -- stack will be rolled back.
    timeoutInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The rollback triggers for AWS CloudFormation to monitor during stack
    -- creation and updating operations, and for the specified monitoring
    -- period afterwards.
    rollbackConfiguration :: Prelude.Maybe RollbackConfiguration,
    -- | A unique identifier for this @CreateStack@ request. Specify this token
    -- if you plan to retry requests so that AWS CloudFormation knows that
    -- you\'re not attempting to create a stack with the same name. You might
    -- retry @CreateStack@ requests to ensure that AWS CloudFormation
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
    -- and a maximum length of 51,200 bytes. For more information, go to
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
    -- in the AWS CloudFormation User Guide.
    --
    -- Conditional: You must specify either the @TemplateBody@ or the
    -- @TemplateURL@ parameter, but not both.
    templateBody :: Prelude.Maybe Prelude.Text,
    -- | Set to @true@ to disable rollback of the stack if stack creation failed.
    -- You can specify either @DisableRollback@ or @OnFailure@, but not both.
    --
    -- Default: @false@
    disableRollback :: Prelude.Maybe Prelude.Bool,
    -- | A list of @Parameter@ structures that specify input parameters for the
    -- stack. For more information, see the
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter>
    -- data type.
    parameters :: Prelude.Maybe [Parameter],
    -- | The name that is associated with the stack. The name must be unique in
    -- the Region in which you are creating the stack.
    --
    -- A stack name can contain only alphanumeric characters (case sensitive)
    -- and hyphens. It must start with an alphabetic character and cannot be
    -- longer than 128 characters.
    stackName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'onFailure', 'createStack_onFailure' - Determines what action will be taken if stack creation fails. This must
-- be one of: DO_NOTHING, ROLLBACK, or DELETE. You can specify either
-- @OnFailure@ or @DisableRollback@, but not both.
--
-- Default: @ROLLBACK@
--
-- 'roleARN', 'createStack_roleARN' - The Amazon Resource Name (ARN) of an AWS Identity and Access Management
-- (IAM) role that AWS CloudFormation assumes to create the stack. AWS
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
-- 'resourceTypes', 'createStack_resourceTypes' - The template resource types that you have permissions to work with for
-- this create stack action, such as @AWS::EC2::Instance@, @AWS::EC2::*@,
-- or @Custom::MyCustomInstance@. Use the following syntax to describe
-- template resource types: @AWS::*@ (for all AWS resource), @Custom::*@
-- (for all custom resources), @Custom::logical_ID @ (for a specific custom
-- resource), @AWS::service_name::*@ (for all resources of a particular AWS
-- service), and @AWS::service_name::resource_logical_ID @ (for a specific
-- AWS resource).
--
-- If the list of resource types doesn\'t include a resource that you\'re
-- creating, the stack creation fails. By default, AWS CloudFormation
-- grants permissions to all resource types. AWS Identity and Access
-- Management (IAM) uses this parameter for AWS CloudFormation-specific
-- condition keys in IAM policies. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html Controlling Access with AWS Identity and Access Management>.
--
-- 'capabilities', 'createStack_capabilities' - In some cases, you must explicitly acknowledge that your stack template
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
--     If you want to create a stack from a stack template that contains
--     macros /and/ nested stacks, you must create the stack directly from
--     the template using this capability.
--
--     You should only create stacks directly from a stack template that
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
-- 'stackPolicyBody', 'createStack_stackPolicyBody' - Structure containing the stack policy body. For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources>
-- in the /AWS CloudFormation User Guide/. You can specify either the
-- @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
--
-- 'enableTerminationProtection', 'createStack_enableTerminationProtection' - Whether to enable termination protection on the specified stack. If a
-- user attempts to delete a stack with termination protection enabled, the
-- operation fails and the stack remains unchanged. For more information,
-- see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html Protecting a Stack From Being Deleted>
-- in the /AWS CloudFormation User Guide/. Termination protection is
-- disabled on stacks by default.
--
-- For
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html nested stacks>,
-- termination protection is set on the root stack and cannot be changed
-- directly on the nested stack.
--
-- 'templateURL', 'createStack_templateURL' - Location of file containing the template body. The URL must point to a
-- template (max size: 460,800 bytes) that is located in an Amazon S3
-- bucket or a Systems Manager document. For more information, go to the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify either the @TemplateBody@ or the
-- @TemplateURL@ parameter, but not both.
--
-- 'notificationARNs', 'createStack_notificationARNs' - The Simple Notification Service (SNS) topic ARNs to publish stack
-- related events. You can find your SNS topic ARNs using the SNS console
-- or your Command Line Interface (CLI).
--
-- 'stackPolicyURL', 'createStack_stackPolicyURL' - Location of a file containing the stack policy. The URL must point to a
-- policy (maximum size: 16 KB) located in an S3 bucket in the same Region
-- as the stack. You can specify either the @StackPolicyBody@ or the
-- @StackPolicyURL@ parameter, but not both.
--
-- 'tags', 'createStack_tags' - Key-value pairs to associate with this stack. AWS CloudFormation also
-- propagates these tags to the resources created in the stack. A maximum
-- number of 50 tags can be specified.
--
-- 'timeoutInMinutes', 'createStack_timeoutInMinutes' - The amount of time that can pass before the stack status becomes
-- CREATE_FAILED; if @DisableRollback@ is not set or is set to @false@, the
-- stack will be rolled back.
--
-- 'rollbackConfiguration', 'createStack_rollbackConfiguration' - The rollback triggers for AWS CloudFormation to monitor during stack
-- creation and updating operations, and for the specified monitoring
-- period afterwards.
--
-- 'clientRequestToken', 'createStack_clientRequestToken' - A unique identifier for this @CreateStack@ request. Specify this token
-- if you plan to retry requests so that AWS CloudFormation knows that
-- you\'re not attempting to create a stack with the same name. You might
-- retry @CreateStack@ requests to ensure that AWS CloudFormation
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
-- 'templateBody', 'createStack_templateBody' - Structure containing the template body with a minimum length of 1 byte
-- and a maximum length of 51,200 bytes. For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify either the @TemplateBody@ or the
-- @TemplateURL@ parameter, but not both.
--
-- 'disableRollback', 'createStack_disableRollback' - Set to @true@ to disable rollback of the stack if stack creation failed.
-- You can specify either @DisableRollback@ or @OnFailure@, but not both.
--
-- Default: @false@
--
-- 'parameters', 'createStack_parameters' - A list of @Parameter@ structures that specify input parameters for the
-- stack. For more information, see the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter>
-- data type.
--
-- 'stackName', 'createStack_stackName' - The name that is associated with the stack. The name must be unique in
-- the Region in which you are creating the stack.
--
-- A stack name can contain only alphanumeric characters (case sensitive)
-- and hyphens. It must start with an alphabetic character and cannot be
-- longer than 128 characters.
newCreateStack ::
  -- | 'stackName'
  Prelude.Text ->
  CreateStack
newCreateStack pStackName_ =
  CreateStack'
    { onFailure = Prelude.Nothing,
      roleARN = Prelude.Nothing,
      resourceTypes = Prelude.Nothing,
      capabilities = Prelude.Nothing,
      stackPolicyBody = Prelude.Nothing,
      enableTerminationProtection = Prelude.Nothing,
      templateURL = Prelude.Nothing,
      notificationARNs = Prelude.Nothing,
      stackPolicyURL = Prelude.Nothing,
      tags = Prelude.Nothing,
      timeoutInMinutes = Prelude.Nothing,
      rollbackConfiguration = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      templateBody = Prelude.Nothing,
      disableRollback = Prelude.Nothing,
      parameters = Prelude.Nothing,
      stackName = pStackName_
    }

-- | Determines what action will be taken if stack creation fails. This must
-- be one of: DO_NOTHING, ROLLBACK, or DELETE. You can specify either
-- @OnFailure@ or @DisableRollback@, but not both.
--
-- Default: @ROLLBACK@
createStack_onFailure :: Lens.Lens' CreateStack (Prelude.Maybe OnFailure)
createStack_onFailure = Lens.lens (\CreateStack' {onFailure} -> onFailure) (\s@CreateStack' {} a -> s {onFailure = a} :: CreateStack)

-- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management
-- (IAM) role that AWS CloudFormation assumes to create the stack. AWS
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
createStack_roleARN :: Lens.Lens' CreateStack (Prelude.Maybe Prelude.Text)
createStack_roleARN = Lens.lens (\CreateStack' {roleARN} -> roleARN) (\s@CreateStack' {} a -> s {roleARN = a} :: CreateStack)

-- | The template resource types that you have permissions to work with for
-- this create stack action, such as @AWS::EC2::Instance@, @AWS::EC2::*@,
-- or @Custom::MyCustomInstance@. Use the following syntax to describe
-- template resource types: @AWS::*@ (for all AWS resource), @Custom::*@
-- (for all custom resources), @Custom::logical_ID @ (for a specific custom
-- resource), @AWS::service_name::*@ (for all resources of a particular AWS
-- service), and @AWS::service_name::resource_logical_ID @ (for a specific
-- AWS resource).
--
-- If the list of resource types doesn\'t include a resource that you\'re
-- creating, the stack creation fails. By default, AWS CloudFormation
-- grants permissions to all resource types. AWS Identity and Access
-- Management (IAM) uses this parameter for AWS CloudFormation-specific
-- condition keys in IAM policies. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html Controlling Access with AWS Identity and Access Management>.
createStack_resourceTypes :: Lens.Lens' CreateStack (Prelude.Maybe [Prelude.Text])
createStack_resourceTypes = Lens.lens (\CreateStack' {resourceTypes} -> resourceTypes) (\s@CreateStack' {} a -> s {resourceTypes = a} :: CreateStack) Prelude.. Lens.mapping Lens._Coerce

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
--     If you want to create a stack from a stack template that contains
--     macros /and/ nested stacks, you must create the stack directly from
--     the template using this capability.
--
--     You should only create stacks directly from a stack template that
--     contains macros if you know what processing the macro performs.
--
--     Each macro relies on an underlying Lambda service function for
--     processing stack templates. Be aware that the Lambda function owner
--     can update the function operation without AWS CloudFormation being
--     notified.
--
--     For more information, see
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-macros.html Using AWS CloudFormation Macros to Perform Custom Processing on Templates>.
createStack_capabilities :: Lens.Lens' CreateStack (Prelude.Maybe [Capability])
createStack_capabilities = Lens.lens (\CreateStack' {capabilities} -> capabilities) (\s@CreateStack' {} a -> s {capabilities = a} :: CreateStack) Prelude.. Lens.mapping Lens._Coerce

-- | Structure containing the stack policy body. For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources>
-- in the /AWS CloudFormation User Guide/. You can specify either the
-- @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
createStack_stackPolicyBody :: Lens.Lens' CreateStack (Prelude.Maybe Prelude.Text)
createStack_stackPolicyBody = Lens.lens (\CreateStack' {stackPolicyBody} -> stackPolicyBody) (\s@CreateStack' {} a -> s {stackPolicyBody = a} :: CreateStack)

-- | Whether to enable termination protection on the specified stack. If a
-- user attempts to delete a stack with termination protection enabled, the
-- operation fails and the stack remains unchanged. For more information,
-- see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html Protecting a Stack From Being Deleted>
-- in the /AWS CloudFormation User Guide/. Termination protection is
-- disabled on stacks by default.
--
-- For
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html nested stacks>,
-- termination protection is set on the root stack and cannot be changed
-- directly on the nested stack.
createStack_enableTerminationProtection :: Lens.Lens' CreateStack (Prelude.Maybe Prelude.Bool)
createStack_enableTerminationProtection = Lens.lens (\CreateStack' {enableTerminationProtection} -> enableTerminationProtection) (\s@CreateStack' {} a -> s {enableTerminationProtection = a} :: CreateStack)

-- | Location of file containing the template body. The URL must point to a
-- template (max size: 460,800 bytes) that is located in an Amazon S3
-- bucket or a Systems Manager document. For more information, go to the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify either the @TemplateBody@ or the
-- @TemplateURL@ parameter, but not both.
createStack_templateURL :: Lens.Lens' CreateStack (Prelude.Maybe Prelude.Text)
createStack_templateURL = Lens.lens (\CreateStack' {templateURL} -> templateURL) (\s@CreateStack' {} a -> s {templateURL = a} :: CreateStack)

-- | The Simple Notification Service (SNS) topic ARNs to publish stack
-- related events. You can find your SNS topic ARNs using the SNS console
-- or your Command Line Interface (CLI).
createStack_notificationARNs :: Lens.Lens' CreateStack (Prelude.Maybe [Prelude.Text])
createStack_notificationARNs = Lens.lens (\CreateStack' {notificationARNs} -> notificationARNs) (\s@CreateStack' {} a -> s {notificationARNs = a} :: CreateStack) Prelude.. Lens.mapping Lens._Coerce

-- | Location of a file containing the stack policy. The URL must point to a
-- policy (maximum size: 16 KB) located in an S3 bucket in the same Region
-- as the stack. You can specify either the @StackPolicyBody@ or the
-- @StackPolicyURL@ parameter, but not both.
createStack_stackPolicyURL :: Lens.Lens' CreateStack (Prelude.Maybe Prelude.Text)
createStack_stackPolicyURL = Lens.lens (\CreateStack' {stackPolicyURL} -> stackPolicyURL) (\s@CreateStack' {} a -> s {stackPolicyURL = a} :: CreateStack)

-- | Key-value pairs to associate with this stack. AWS CloudFormation also
-- propagates these tags to the resources created in the stack. A maximum
-- number of 50 tags can be specified.
createStack_tags :: Lens.Lens' CreateStack (Prelude.Maybe [Tag])
createStack_tags = Lens.lens (\CreateStack' {tags} -> tags) (\s@CreateStack' {} a -> s {tags = a} :: CreateStack) Prelude.. Lens.mapping Lens._Coerce

-- | The amount of time that can pass before the stack status becomes
-- CREATE_FAILED; if @DisableRollback@ is not set or is set to @false@, the
-- stack will be rolled back.
createStack_timeoutInMinutes :: Lens.Lens' CreateStack (Prelude.Maybe Prelude.Natural)
createStack_timeoutInMinutes = Lens.lens (\CreateStack' {timeoutInMinutes} -> timeoutInMinutes) (\s@CreateStack' {} a -> s {timeoutInMinutes = a} :: CreateStack)

-- | The rollback triggers for AWS CloudFormation to monitor during stack
-- creation and updating operations, and for the specified monitoring
-- period afterwards.
createStack_rollbackConfiguration :: Lens.Lens' CreateStack (Prelude.Maybe RollbackConfiguration)
createStack_rollbackConfiguration = Lens.lens (\CreateStack' {rollbackConfiguration} -> rollbackConfiguration) (\s@CreateStack' {} a -> s {rollbackConfiguration = a} :: CreateStack)

-- | A unique identifier for this @CreateStack@ request. Specify this token
-- if you plan to retry requests so that AWS CloudFormation knows that
-- you\'re not attempting to create a stack with the same name. You might
-- retry @CreateStack@ requests to ensure that AWS CloudFormation
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
createStack_clientRequestToken :: Lens.Lens' CreateStack (Prelude.Maybe Prelude.Text)
createStack_clientRequestToken = Lens.lens (\CreateStack' {clientRequestToken} -> clientRequestToken) (\s@CreateStack' {} a -> s {clientRequestToken = a} :: CreateStack)

-- | Structure containing the template body with a minimum length of 1 byte
-- and a maximum length of 51,200 bytes. For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify either the @TemplateBody@ or the
-- @TemplateURL@ parameter, but not both.
createStack_templateBody :: Lens.Lens' CreateStack (Prelude.Maybe Prelude.Text)
createStack_templateBody = Lens.lens (\CreateStack' {templateBody} -> templateBody) (\s@CreateStack' {} a -> s {templateBody = a} :: CreateStack)

-- | Set to @true@ to disable rollback of the stack if stack creation failed.
-- You can specify either @DisableRollback@ or @OnFailure@, but not both.
--
-- Default: @false@
createStack_disableRollback :: Lens.Lens' CreateStack (Prelude.Maybe Prelude.Bool)
createStack_disableRollback = Lens.lens (\CreateStack' {disableRollback} -> disableRollback) (\s@CreateStack' {} a -> s {disableRollback = a} :: CreateStack)

-- | A list of @Parameter@ structures that specify input parameters for the
-- stack. For more information, see the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter>
-- data type.
createStack_parameters :: Lens.Lens' CreateStack (Prelude.Maybe [Parameter])
createStack_parameters = Lens.lens (\CreateStack' {parameters} -> parameters) (\s@CreateStack' {} a -> s {parameters = a} :: CreateStack) Prelude.. Lens.mapping Lens._Coerce

-- | The name that is associated with the stack. The name must be unique in
-- the Region in which you are creating the stack.
--
-- A stack name can contain only alphanumeric characters (case sensitive)
-- and hyphens. It must start with an alphabetic character and cannot be
-- longer than 128 characters.
createStack_stackName :: Lens.Lens' CreateStack Prelude.Text
createStack_stackName = Lens.lens (\CreateStack' {stackName} -> stackName) (\s@CreateStack' {} a -> s {stackName = a} :: CreateStack)

instance Core.AWSRequest CreateStack where
  type AWSResponse CreateStack = CreateStackResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateStackResult"
      ( \s h x ->
          CreateStackResponse'
            Prelude.<$> (x Core..@? "StackId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateStack

instance Prelude.NFData CreateStack

instance Core.ToHeaders CreateStack where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateStack where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateStack where
  toQuery CreateStack' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateStack" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-15" :: Prelude.ByteString),
        "OnFailure" Core.=: onFailure,
        "RoleARN" Core.=: roleARN,
        "ResourceTypes"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> resourceTypes
            ),
        "Capabilities"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> capabilities),
        "StackPolicyBody" Core.=: stackPolicyBody,
        "EnableTerminationProtection"
          Core.=: enableTerminationProtection,
        "TemplateURL" Core.=: templateURL,
        "NotificationARNs"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> notificationARNs
            ),
        "StackPolicyURL" Core.=: stackPolicyURL,
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> tags),
        "TimeoutInMinutes" Core.=: timeoutInMinutes,
        "RollbackConfiguration"
          Core.=: rollbackConfiguration,
        "ClientRequestToken" Core.=: clientRequestToken,
        "TemplateBody" Core.=: templateBody,
        "DisableRollback" Core.=: disableRollback,
        "Parameters"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> parameters),
        "StackName" Core.=: stackName
      ]

-- | The output for a CreateStack action.
--
-- /See:/ 'newCreateStackResponse' smart constructor.
data CreateStackResponse = CreateStackResponse'
  { -- | Unique identifier of the stack.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackId', 'createStackResponse_stackId' - Unique identifier of the stack.
--
-- 'httpStatus', 'createStackResponse_httpStatus' - The response's http status code.
newCreateStackResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateStackResponse
newCreateStackResponse pHttpStatus_ =
  CreateStackResponse'
    { stackId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Unique identifier of the stack.
createStackResponse_stackId :: Lens.Lens' CreateStackResponse (Prelude.Maybe Prelude.Text)
createStackResponse_stackId = Lens.lens (\CreateStackResponse' {stackId} -> stackId) (\s@CreateStackResponse' {} a -> s {stackId = a} :: CreateStackResponse)

-- | The response's http status code.
createStackResponse_httpStatus :: Lens.Lens' CreateStackResponse Prelude.Int
createStackResponse_httpStatus = Lens.lens (\CreateStackResponse' {httpStatus} -> httpStatus) (\s@CreateStackResponse' {} a -> s {httpStatus = a} :: CreateStackResponse)

instance Prelude.NFData CreateStackResponse
