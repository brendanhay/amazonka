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
-- Module      : Network.AWS.CloudFormation.CreateStackSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a stack set.
module Network.AWS.CloudFormation.CreateStackSet
  ( -- * Creating a Request
    CreateStackSet (..),
    newCreateStackSet,

    -- * Request Lenses
    createStackSet_permissionModel,
    createStackSet_executionRoleName,
    createStackSet_capabilities,
    createStackSet_templateURL,
    createStackSet_callAs,
    createStackSet_administrationRoleARN,
    createStackSet_tags,
    createStackSet_autoDeployment,
    createStackSet_description,
    createStackSet_clientRequestToken,
    createStackSet_templateBody,
    createStackSet_parameters,
    createStackSet_stackSetName,

    -- * Destructuring the Response
    CreateStackSetResponse (..),
    newCreateStackSetResponse,

    -- * Response Lenses
    createStackSetResponse_stackSetId,
    createStackSetResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateStackSet' smart constructor.
data CreateStackSet = CreateStackSet'
  { -- | Describes how the IAM roles required for stack set operations are
    -- created. By default, @SELF-MANAGED@ is specified.
    --
    -- -   With @self-managed@ permissions, you must create the administrator
    --     and execution roles required to deploy to target accounts. For more
    --     information, see
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions>.
    --
    -- -   With @service-managed@ permissions, StackSets automatically creates
    --     the IAM roles required to deploy to accounts managed by AWS
    --     Organizations. For more information, see
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions>.
    permissionModel :: Prelude.Maybe PermissionModels,
    -- | The name of the IAM execution role to use to create the stack set. If
    -- you do not specify an execution role, AWS CloudFormation uses the
    -- @AWSCloudFormationStackSetExecutionRole@ role for the stack set
    -- operation.
    --
    -- Specify an IAM role only if you are using customized execution roles to
    -- control which stack resources users and groups can include in their
    -- stack sets.
    executionRoleName :: Prelude.Maybe Prelude.Text,
    -- | In some cases, you must explicitly acknowledge that your stack set
    -- template contains certain capabilities in order for AWS CloudFormation
    -- to create the stack set and related stack instances.
    --
    -- -   @CAPABILITY_IAM@ and @CAPABILITY_NAMED_IAM@
    --
    --     Some stack templates might include resources that can affect
    --     permissions in your AWS account; for example, by creating new AWS
    --     Identity and Access Management (IAM) users. For those stack sets,
    --     you must explicitly acknowledge this by specifying one of these
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
    --     Some templates contain macros. If your stack template contains one
    --     or more macros, and you choose to create a stack directly from the
    --     processed template, without first reviewing the resulting changes in
    --     a change set, you must acknowledge this capability. For more
    --     information, see
    --     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-macros.html Using AWS CloudFormation Macros to Perform Custom Processing on Templates>.
    --
    --     Stack sets do not currently support macros in stack templates. (This
    --     includes the
    --     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/create-reusable-transform-function-snippets-and-add-to-your-template-with-aws-include-transform.html AWS::Include>
    --     and
    --     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/transform-aws-serverless.html AWS::Serverless>
    --     transforms, which are macros hosted by AWS CloudFormation.) Even if
    --     you specify this capability, if you include a macro in your template
    --     the stack set operation will fail.
    capabilities :: Prelude.Maybe [Capability],
    -- | The location of the file that contains the template body. The URL must
    -- point to a template (maximum size: 460,800 bytes) that\'s located in an
    -- Amazon S3 bucket or a Systems Manager document. For more information,
    -- see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
    -- in the AWS CloudFormation User Guide.
    --
    -- Conditional: You must specify either the TemplateBody or the TemplateURL
    -- parameter, but not both.
    templateURL :: Prelude.Maybe Prelude.Text,
    -- | [Service-managed permissions] Specifies whether you are acting as an
    -- account administrator in the organization\'s management account or as a
    -- delegated administrator in a member account.
    --
    -- By default, @SELF@ is specified. Use @SELF@ for stack sets with
    -- self-managed permissions.
    --
    -- -   To create a stack set with service-managed permissions while signed
    --     in to the management account, specify @SELF@.
    --
    -- -   To create a stack set with service-managed permissions while signed
    --     in to a delegated administrator account, specify @DELEGATED_ADMIN@.
    --
    --     Your AWS account must be registered as a delegated admin in the
    --     management account. For more information, see
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
    --     in the /AWS CloudFormation User Guide/.
    --
    -- Stack sets with service-managed permissions are created in the
    -- management account, including stack sets that are created by delegated
    -- administrators.
    callAs :: Prelude.Maybe CallAs,
    -- | The Amazon Resource Number (ARN) of the IAM role to use to create this
    -- stack set.
    --
    -- Specify an IAM role only if you are using customized administrator roles
    -- to control which users or groups can manage specific stack sets within
    -- the same administrator account. For more information, see
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Prerequisites: Granting Permissions for Stack Set Operations>
    -- in the /AWS CloudFormation User Guide/.
    administrationRoleARN :: Prelude.Maybe Prelude.Text,
    -- | The key-value pairs to associate with this stack set and the stacks
    -- created from it. AWS CloudFormation also propagates these tags to
    -- supported resources that are created in the stacks. A maximum number of
    -- 50 tags can be specified.
    --
    -- If you specify tags as part of a @CreateStackSet@ action, AWS
    -- CloudFormation checks to see if you have the required IAM permission to
    -- tag resources. If you don\'t, the entire @CreateStackSet@ action fails
    -- with an @access denied@ error, and the stack set is not created.
    tags :: Prelude.Maybe [Tag],
    -- | Describes whether StackSets automatically deploys to AWS Organizations
    -- accounts that are added to the target organization or organizational
    -- unit (OU). Specify only if @PermissionModel@ is @SERVICE_MANAGED@.
    autoDeployment :: Prelude.Maybe AutoDeployment,
    -- | A description of the stack set. You can use the description to identify
    -- the stack set\'s purpose or other important information.
    description :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for this @CreateStackSet@ request. Specify this
    -- token if you plan to retry requests so that AWS CloudFormation knows
    -- that you\'re not attempting to create another stack set with the same
    -- name. You might retry @CreateStackSet@ requests to ensure that AWS
    -- CloudFormation successfully received them.
    --
    -- If you don\'t specify an operation ID, the SDK generates one
    -- automatically.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The structure that contains the template body, with a minimum length of
    -- 1 byte and a maximum length of 51,200 bytes. For more information, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
    -- in the AWS CloudFormation User Guide.
    --
    -- Conditional: You must specify either the TemplateBody or the TemplateURL
    -- parameter, but not both.
    templateBody :: Prelude.Maybe Prelude.Text,
    -- | The input parameters for the stack set template.
    parameters :: Prelude.Maybe [Parameter],
    -- | The name to associate with the stack set. The name must be unique in the
    -- Region where you create your stack set.
    --
    -- A stack name can contain only alphanumeric characters (case-sensitive)
    -- and hyphens. It must start with an alphabetic character and can\'t be
    -- longer than 128 characters.
    stackSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateStackSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permissionModel', 'createStackSet_permissionModel' - Describes how the IAM roles required for stack set operations are
-- created. By default, @SELF-MANAGED@ is specified.
--
-- -   With @self-managed@ permissions, you must create the administrator
--     and execution roles required to deploy to target accounts. For more
--     information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions>.
--
-- -   With @service-managed@ permissions, StackSets automatically creates
--     the IAM roles required to deploy to accounts managed by AWS
--     Organizations. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions>.
--
-- 'executionRoleName', 'createStackSet_executionRoleName' - The name of the IAM execution role to use to create the stack set. If
-- you do not specify an execution role, AWS CloudFormation uses the
-- @AWSCloudFormationStackSetExecutionRole@ role for the stack set
-- operation.
--
-- Specify an IAM role only if you are using customized execution roles to
-- control which stack resources users and groups can include in their
-- stack sets.
--
-- 'capabilities', 'createStackSet_capabilities' - In some cases, you must explicitly acknowledge that your stack set
-- template contains certain capabilities in order for AWS CloudFormation
-- to create the stack set and related stack instances.
--
-- -   @CAPABILITY_IAM@ and @CAPABILITY_NAMED_IAM@
--
--     Some stack templates might include resources that can affect
--     permissions in your AWS account; for example, by creating new AWS
--     Identity and Access Management (IAM) users. For those stack sets,
--     you must explicitly acknowledge this by specifying one of these
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
--     Some templates contain macros. If your stack template contains one
--     or more macros, and you choose to create a stack directly from the
--     processed template, without first reviewing the resulting changes in
--     a change set, you must acknowledge this capability. For more
--     information, see
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-macros.html Using AWS CloudFormation Macros to Perform Custom Processing on Templates>.
--
--     Stack sets do not currently support macros in stack templates. (This
--     includes the
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/create-reusable-transform-function-snippets-and-add-to-your-template-with-aws-include-transform.html AWS::Include>
--     and
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/transform-aws-serverless.html AWS::Serverless>
--     transforms, which are macros hosted by AWS CloudFormation.) Even if
--     you specify this capability, if you include a macro in your template
--     the stack set operation will fail.
--
-- 'templateURL', 'createStackSet_templateURL' - The location of the file that contains the template body. The URL must
-- point to a template (maximum size: 460,800 bytes) that\'s located in an
-- Amazon S3 bucket or a Systems Manager document. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify either the TemplateBody or the TemplateURL
-- parameter, but not both.
--
-- 'callAs', 'createStackSet_callAs' - [Service-managed permissions] Specifies whether you are acting as an
-- account administrator in the organization\'s management account or as a
-- delegated administrator in a member account.
--
-- By default, @SELF@ is specified. Use @SELF@ for stack sets with
-- self-managed permissions.
--
-- -   To create a stack set with service-managed permissions while signed
--     in to the management account, specify @SELF@.
--
-- -   To create a stack set with service-managed permissions while signed
--     in to a delegated administrator account, specify @DELEGATED_ADMIN@.
--
--     Your AWS account must be registered as a delegated admin in the
--     management account. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
--     in the /AWS CloudFormation User Guide/.
--
-- Stack sets with service-managed permissions are created in the
-- management account, including stack sets that are created by delegated
-- administrators.
--
-- 'administrationRoleARN', 'createStackSet_administrationRoleARN' - The Amazon Resource Number (ARN) of the IAM role to use to create this
-- stack set.
--
-- Specify an IAM role only if you are using customized administrator roles
-- to control which users or groups can manage specific stack sets within
-- the same administrator account. For more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Prerequisites: Granting Permissions for Stack Set Operations>
-- in the /AWS CloudFormation User Guide/.
--
-- 'tags', 'createStackSet_tags' - The key-value pairs to associate with this stack set and the stacks
-- created from it. AWS CloudFormation also propagates these tags to
-- supported resources that are created in the stacks. A maximum number of
-- 50 tags can be specified.
--
-- If you specify tags as part of a @CreateStackSet@ action, AWS
-- CloudFormation checks to see if you have the required IAM permission to
-- tag resources. If you don\'t, the entire @CreateStackSet@ action fails
-- with an @access denied@ error, and the stack set is not created.
--
-- 'autoDeployment', 'createStackSet_autoDeployment' - Describes whether StackSets automatically deploys to AWS Organizations
-- accounts that are added to the target organization or organizational
-- unit (OU). Specify only if @PermissionModel@ is @SERVICE_MANAGED@.
--
-- 'description', 'createStackSet_description' - A description of the stack set. You can use the description to identify
-- the stack set\'s purpose or other important information.
--
-- 'clientRequestToken', 'createStackSet_clientRequestToken' - A unique identifier for this @CreateStackSet@ request. Specify this
-- token if you plan to retry requests so that AWS CloudFormation knows
-- that you\'re not attempting to create another stack set with the same
-- name. You might retry @CreateStackSet@ requests to ensure that AWS
-- CloudFormation successfully received them.
--
-- If you don\'t specify an operation ID, the SDK generates one
-- automatically.
--
-- 'templateBody', 'createStackSet_templateBody' - The structure that contains the template body, with a minimum length of
-- 1 byte and a maximum length of 51,200 bytes. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify either the TemplateBody or the TemplateURL
-- parameter, but not both.
--
-- 'parameters', 'createStackSet_parameters' - The input parameters for the stack set template.
--
-- 'stackSetName', 'createStackSet_stackSetName' - The name to associate with the stack set. The name must be unique in the
-- Region where you create your stack set.
--
-- A stack name can contain only alphanumeric characters (case-sensitive)
-- and hyphens. It must start with an alphabetic character and can\'t be
-- longer than 128 characters.
newCreateStackSet ::
  -- | 'stackSetName'
  Prelude.Text ->
  CreateStackSet
newCreateStackSet pStackSetName_ =
  CreateStackSet'
    { permissionModel = Prelude.Nothing,
      executionRoleName = Prelude.Nothing,
      capabilities = Prelude.Nothing,
      templateURL = Prelude.Nothing,
      callAs = Prelude.Nothing,
      administrationRoleARN = Prelude.Nothing,
      tags = Prelude.Nothing,
      autoDeployment = Prelude.Nothing,
      description = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      templateBody = Prelude.Nothing,
      parameters = Prelude.Nothing,
      stackSetName = pStackSetName_
    }

-- | Describes how the IAM roles required for stack set operations are
-- created. By default, @SELF-MANAGED@ is specified.
--
-- -   With @self-managed@ permissions, you must create the administrator
--     and execution roles required to deploy to target accounts. For more
--     information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions>.
--
-- -   With @service-managed@ permissions, StackSets automatically creates
--     the IAM roles required to deploy to accounts managed by AWS
--     Organizations. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions>.
createStackSet_permissionModel :: Lens.Lens' CreateStackSet (Prelude.Maybe PermissionModels)
createStackSet_permissionModel = Lens.lens (\CreateStackSet' {permissionModel} -> permissionModel) (\s@CreateStackSet' {} a -> s {permissionModel = a} :: CreateStackSet)

-- | The name of the IAM execution role to use to create the stack set. If
-- you do not specify an execution role, AWS CloudFormation uses the
-- @AWSCloudFormationStackSetExecutionRole@ role for the stack set
-- operation.
--
-- Specify an IAM role only if you are using customized execution roles to
-- control which stack resources users and groups can include in their
-- stack sets.
createStackSet_executionRoleName :: Lens.Lens' CreateStackSet (Prelude.Maybe Prelude.Text)
createStackSet_executionRoleName = Lens.lens (\CreateStackSet' {executionRoleName} -> executionRoleName) (\s@CreateStackSet' {} a -> s {executionRoleName = a} :: CreateStackSet)

-- | In some cases, you must explicitly acknowledge that your stack set
-- template contains certain capabilities in order for AWS CloudFormation
-- to create the stack set and related stack instances.
--
-- -   @CAPABILITY_IAM@ and @CAPABILITY_NAMED_IAM@
--
--     Some stack templates might include resources that can affect
--     permissions in your AWS account; for example, by creating new AWS
--     Identity and Access Management (IAM) users. For those stack sets,
--     you must explicitly acknowledge this by specifying one of these
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
--     Some templates contain macros. If your stack template contains one
--     or more macros, and you choose to create a stack directly from the
--     processed template, without first reviewing the resulting changes in
--     a change set, you must acknowledge this capability. For more
--     information, see
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-macros.html Using AWS CloudFormation Macros to Perform Custom Processing on Templates>.
--
--     Stack sets do not currently support macros in stack templates. (This
--     includes the
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/create-reusable-transform-function-snippets-and-add-to-your-template-with-aws-include-transform.html AWS::Include>
--     and
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/transform-aws-serverless.html AWS::Serverless>
--     transforms, which are macros hosted by AWS CloudFormation.) Even if
--     you specify this capability, if you include a macro in your template
--     the stack set operation will fail.
createStackSet_capabilities :: Lens.Lens' CreateStackSet (Prelude.Maybe [Capability])
createStackSet_capabilities = Lens.lens (\CreateStackSet' {capabilities} -> capabilities) (\s@CreateStackSet' {} a -> s {capabilities = a} :: CreateStackSet) Prelude.. Lens.mapping Prelude._Coerce

-- | The location of the file that contains the template body. The URL must
-- point to a template (maximum size: 460,800 bytes) that\'s located in an
-- Amazon S3 bucket or a Systems Manager document. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify either the TemplateBody or the TemplateURL
-- parameter, but not both.
createStackSet_templateURL :: Lens.Lens' CreateStackSet (Prelude.Maybe Prelude.Text)
createStackSet_templateURL = Lens.lens (\CreateStackSet' {templateURL} -> templateURL) (\s@CreateStackSet' {} a -> s {templateURL = a} :: CreateStackSet)

-- | [Service-managed permissions] Specifies whether you are acting as an
-- account administrator in the organization\'s management account or as a
-- delegated administrator in a member account.
--
-- By default, @SELF@ is specified. Use @SELF@ for stack sets with
-- self-managed permissions.
--
-- -   To create a stack set with service-managed permissions while signed
--     in to the management account, specify @SELF@.
--
-- -   To create a stack set with service-managed permissions while signed
--     in to a delegated administrator account, specify @DELEGATED_ADMIN@.
--
--     Your AWS account must be registered as a delegated admin in the
--     management account. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
--     in the /AWS CloudFormation User Guide/.
--
-- Stack sets with service-managed permissions are created in the
-- management account, including stack sets that are created by delegated
-- administrators.
createStackSet_callAs :: Lens.Lens' CreateStackSet (Prelude.Maybe CallAs)
createStackSet_callAs = Lens.lens (\CreateStackSet' {callAs} -> callAs) (\s@CreateStackSet' {} a -> s {callAs = a} :: CreateStackSet)

-- | The Amazon Resource Number (ARN) of the IAM role to use to create this
-- stack set.
--
-- Specify an IAM role only if you are using customized administrator roles
-- to control which users or groups can manage specific stack sets within
-- the same administrator account. For more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Prerequisites: Granting Permissions for Stack Set Operations>
-- in the /AWS CloudFormation User Guide/.
createStackSet_administrationRoleARN :: Lens.Lens' CreateStackSet (Prelude.Maybe Prelude.Text)
createStackSet_administrationRoleARN = Lens.lens (\CreateStackSet' {administrationRoleARN} -> administrationRoleARN) (\s@CreateStackSet' {} a -> s {administrationRoleARN = a} :: CreateStackSet)

-- | The key-value pairs to associate with this stack set and the stacks
-- created from it. AWS CloudFormation also propagates these tags to
-- supported resources that are created in the stacks. A maximum number of
-- 50 tags can be specified.
--
-- If you specify tags as part of a @CreateStackSet@ action, AWS
-- CloudFormation checks to see if you have the required IAM permission to
-- tag resources. If you don\'t, the entire @CreateStackSet@ action fails
-- with an @access denied@ error, and the stack set is not created.
createStackSet_tags :: Lens.Lens' CreateStackSet (Prelude.Maybe [Tag])
createStackSet_tags = Lens.lens (\CreateStackSet' {tags} -> tags) (\s@CreateStackSet' {} a -> s {tags = a} :: CreateStackSet) Prelude.. Lens.mapping Prelude._Coerce

-- | Describes whether StackSets automatically deploys to AWS Organizations
-- accounts that are added to the target organization or organizational
-- unit (OU). Specify only if @PermissionModel@ is @SERVICE_MANAGED@.
createStackSet_autoDeployment :: Lens.Lens' CreateStackSet (Prelude.Maybe AutoDeployment)
createStackSet_autoDeployment = Lens.lens (\CreateStackSet' {autoDeployment} -> autoDeployment) (\s@CreateStackSet' {} a -> s {autoDeployment = a} :: CreateStackSet)

-- | A description of the stack set. You can use the description to identify
-- the stack set\'s purpose or other important information.
createStackSet_description :: Lens.Lens' CreateStackSet (Prelude.Maybe Prelude.Text)
createStackSet_description = Lens.lens (\CreateStackSet' {description} -> description) (\s@CreateStackSet' {} a -> s {description = a} :: CreateStackSet)

-- | A unique identifier for this @CreateStackSet@ request. Specify this
-- token if you plan to retry requests so that AWS CloudFormation knows
-- that you\'re not attempting to create another stack set with the same
-- name. You might retry @CreateStackSet@ requests to ensure that AWS
-- CloudFormation successfully received them.
--
-- If you don\'t specify an operation ID, the SDK generates one
-- automatically.
createStackSet_clientRequestToken :: Lens.Lens' CreateStackSet (Prelude.Maybe Prelude.Text)
createStackSet_clientRequestToken = Lens.lens (\CreateStackSet' {clientRequestToken} -> clientRequestToken) (\s@CreateStackSet' {} a -> s {clientRequestToken = a} :: CreateStackSet)

-- | The structure that contains the template body, with a minimum length of
-- 1 byte and a maximum length of 51,200 bytes. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify either the TemplateBody or the TemplateURL
-- parameter, but not both.
createStackSet_templateBody :: Lens.Lens' CreateStackSet (Prelude.Maybe Prelude.Text)
createStackSet_templateBody = Lens.lens (\CreateStackSet' {templateBody} -> templateBody) (\s@CreateStackSet' {} a -> s {templateBody = a} :: CreateStackSet)

-- | The input parameters for the stack set template.
createStackSet_parameters :: Lens.Lens' CreateStackSet (Prelude.Maybe [Parameter])
createStackSet_parameters = Lens.lens (\CreateStackSet' {parameters} -> parameters) (\s@CreateStackSet' {} a -> s {parameters = a} :: CreateStackSet) Prelude.. Lens.mapping Prelude._Coerce

-- | The name to associate with the stack set. The name must be unique in the
-- Region where you create your stack set.
--
-- A stack name can contain only alphanumeric characters (case-sensitive)
-- and hyphens. It must start with an alphabetic character and can\'t be
-- longer than 128 characters.
createStackSet_stackSetName :: Lens.Lens' CreateStackSet Prelude.Text
createStackSet_stackSetName = Lens.lens (\CreateStackSet' {stackSetName} -> stackSetName) (\s@CreateStackSet' {} a -> s {stackSetName = a} :: CreateStackSet)

instance Prelude.AWSRequest CreateStackSet where
  type Rs CreateStackSet = CreateStackSetResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateStackSetResult"
      ( \s h x ->
          CreateStackSetResponse'
            Prelude.<$> (x Prelude..@? "StackSetId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateStackSet

instance Prelude.NFData CreateStackSet

instance Prelude.ToHeaders CreateStackSet where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CreateStackSet where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateStackSet where
  toQuery CreateStackSet' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("CreateStackSet" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-15" :: Prelude.ByteString),
        "PermissionModel" Prelude.=: permissionModel,
        "ExecutionRoleName" Prelude.=: executionRoleName,
        "Capabilities"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> capabilities
            ),
        "TemplateURL" Prelude.=: templateURL,
        "CallAs" Prelude.=: callAs,
        "AdministrationRoleARN"
          Prelude.=: administrationRoleARN,
        "Tags"
          Prelude.=: Prelude.toQuery
            (Prelude.toQueryList "member" Prelude.<$> tags),
        "AutoDeployment" Prelude.=: autoDeployment,
        "Description" Prelude.=: description,
        "ClientRequestToken" Prelude.=: clientRequestToken,
        "TemplateBody" Prelude.=: templateBody,
        "Parameters"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> parameters
            ),
        "StackSetName" Prelude.=: stackSetName
      ]

-- | /See:/ 'newCreateStackSetResponse' smart constructor.
data CreateStackSetResponse = CreateStackSetResponse'
  { -- | The ID of the stack set that you\'re creating.
    stackSetId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateStackSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackSetId', 'createStackSetResponse_stackSetId' - The ID of the stack set that you\'re creating.
--
-- 'httpStatus', 'createStackSetResponse_httpStatus' - The response's http status code.
newCreateStackSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateStackSetResponse
newCreateStackSetResponse pHttpStatus_ =
  CreateStackSetResponse'
    { stackSetId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the stack set that you\'re creating.
createStackSetResponse_stackSetId :: Lens.Lens' CreateStackSetResponse (Prelude.Maybe Prelude.Text)
createStackSetResponse_stackSetId = Lens.lens (\CreateStackSetResponse' {stackSetId} -> stackSetId) (\s@CreateStackSetResponse' {} a -> s {stackSetId = a} :: CreateStackSetResponse)

-- | The response's http status code.
createStackSetResponse_httpStatus :: Lens.Lens' CreateStackSetResponse Prelude.Int
createStackSetResponse_httpStatus = Lens.lens (\CreateStackSetResponse' {httpStatus} -> httpStatus) (\s@CreateStackSetResponse' {} a -> s {httpStatus = a} :: CreateStackSetResponse)

instance Prelude.NFData CreateStackSetResponse
