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
-- Module      : Amazonka.CloudFormation.UpdateStackSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the stack set, and associated stack instances in the specified
-- accounts and Amazon Web Services Regions.
--
-- Even if the stack set operation created by updating the stack set fails
-- (completely or partially, below or above a specified failure tolerance),
-- the stack set is updated with your changes. Subsequent
-- CreateStackInstances calls on the specified stack set use the updated
-- stack set.
module Amazonka.CloudFormation.UpdateStackSet
  ( -- * Creating a Request
    UpdateStackSet (..),
    newUpdateStackSet,

    -- * Request Lenses
    updateStackSet_tags,
    updateStackSet_operationPreferences,
    updateStackSet_callAs,
    updateStackSet_regions,
    updateStackSet_usePreviousTemplate,
    updateStackSet_templateBody,
    updateStackSet_operationId,
    updateStackSet_description,
    updateStackSet_autoDeployment,
    updateStackSet_accounts,
    updateStackSet_capabilities,
    updateStackSet_managedExecution,
    updateStackSet_executionRoleName,
    updateStackSet_administrationRoleARN,
    updateStackSet_deploymentTargets,
    updateStackSet_permissionModel,
    updateStackSet_templateURL,
    updateStackSet_parameters,
    updateStackSet_stackSetName,

    -- * Destructuring the Response
    UpdateStackSetResponse (..),
    newUpdateStackSetResponse,

    -- * Response Lenses
    updateStackSetResponse_operationId,
    updateStackSetResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateStackSet' smart constructor.
data UpdateStackSet = UpdateStackSet'
  { -- | The key-value pairs to associate with this stack set and the stacks
    -- created from it. CloudFormation also propagates these tags to supported
    -- resources that are created in the stacks. You can specify a maximum
    -- number of 50 tags.
    --
    -- If you specify tags for this parameter, those tags replace any list of
    -- tags that are currently associated with this stack set. This means:
    --
    -- -   If you don\'t specify this parameter, CloudFormation doesn\'t modify
    --     the stack\'s tags.
    --
    -- -   If you specify /any/ tags using this parameter, you must specify
    --     /all/ the tags that you want associated with this stack set, even
    --     tags you\'ve specified before (for example, when creating the stack
    --     set or during a previous update of the stack set.). Any tags that
    --     you don\'t include in the updated list of tags are removed from the
    --     stack set, and therefore from the stacks and resources as well.
    --
    -- -   If you specify an empty value, CloudFormation removes all currently
    --     associated tags.
    --
    -- If you specify new tags as part of an @UpdateStackSet@ action,
    -- CloudFormation checks to see if you have the required IAM permission to
    -- tag resources. If you omit tags that are currently associated with the
    -- stack set from the list of tags you specify, CloudFormation assumes that
    -- you want to remove those tags from the stack set, and checks to see if
    -- you have permission to untag resources. If you don\'t have the necessary
    -- permission(s), the entire @UpdateStackSet@ action fails with an
    -- @access denied@ error, and the stack set is not updated.
    tags :: Prelude.Maybe [Tag],
    -- | Preferences for how CloudFormation performs this stack set operation.
    operationPreferences :: Prelude.Maybe StackSetOperationPreferences,
    -- | [Service-managed permissions] Specifies whether you are acting as an
    -- account administrator in the organization\'s management account or as a
    -- delegated administrator in a member account.
    --
    -- By default, @SELF@ is specified. Use @SELF@ for stack sets with
    -- self-managed permissions.
    --
    -- -   If you are signed in to the management account, specify @SELF@.
    --
    -- -   If you are signed in to a delegated administrator account, specify
    --     @DELEGATED_ADMIN@.
    --
    --     Your Amazon Web Services account must be registered as a delegated
    --     administrator in the management account. For more information, see
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
    --     in the /CloudFormation User Guide/.
    callAs :: Prelude.Maybe CallAs,
    -- | The Amazon Web Services Regions in which to update associated stack
    -- instances. If you specify Regions, you must also specify accounts in
    -- which to update stack set instances.
    --
    -- To update /all/ the stack instances associated with this stack set, do
    -- not specify the @Accounts@ or @Regions@ properties.
    --
    -- If the stack set update includes changes to the template (that is, if
    -- the @TemplateBody@ or @TemplateURL@ properties are specified), or the
    -- @Parameters@ property, CloudFormation marks all stack instances with a
    -- status of @OUTDATED@ prior to updating the stack instances in the
    -- specified accounts and Regions. If the stack set update does not include
    -- changes to the template or parameters, CloudFormation updates the stack
    -- instances in the specified accounts and Regions, while leaving all other
    -- stack instances with their existing stack instance status.
    regions :: Prelude.Maybe [Prelude.Text],
    -- | Use the existing template that\'s associated with the stack set that
    -- you\'re updating.
    --
    -- Conditional: You must specify only one of the following parameters:
    -- @TemplateBody@ or @TemplateURL@—or set @UsePreviousTemplate@ to true.
    usePreviousTemplate :: Prelude.Maybe Prelude.Bool,
    -- | The structure that contains the template body, with a minimum length of
    -- 1 byte and a maximum length of 51,200 bytes. For more information, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
    -- in the CloudFormation User Guide.
    --
    -- Conditional: You must specify only one of the following parameters:
    -- @TemplateBody@ or @TemplateURL@—or set @UsePreviousTemplate@ to true.
    templateBody :: Prelude.Maybe Prelude.Text,
    -- | The unique ID for this stack set operation.
    --
    -- The operation ID also functions as an idempotency token, to ensure that
    -- CloudFormation performs the stack set operation only once, even if you
    -- retry the request multiple times. You might retry stack set operation
    -- requests to ensure that CloudFormation successfully received them.
    --
    -- If you don\'t specify an operation ID, CloudFormation generates one
    -- automatically.
    --
    -- Repeating this stack set operation with a new operation ID retries all
    -- stack instances whose status is @OUTDATED@.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | A brief description of updates that you are making.
    description :: Prelude.Maybe Prelude.Text,
    -- | [Service-managed permissions] Describes whether StackSets automatically
    -- deploys to Organizations accounts that are added to a target
    -- organization or organizational unit (OU).
    --
    -- If you specify @AutoDeployment@, don\'t specify @DeploymentTargets@ or
    -- @Regions@.
    autoDeployment :: Prelude.Maybe AutoDeployment,
    -- | [Self-managed permissions] The accounts in which to update associated
    -- stack instances. If you specify accounts, you must also specify the
    -- Amazon Web Services Regions in which to update stack set instances.
    --
    -- To update /all/ the stack instances associated with this stack set,
    -- don\'t specify the @Accounts@ or @Regions@ properties.
    --
    -- If the stack set update includes changes to the template (that is, if
    -- the @TemplateBody@ or @TemplateURL@ properties are specified), or the
    -- @Parameters@ property, CloudFormation marks all stack instances with a
    -- status of @OUTDATED@ prior to updating the stack instances in the
    -- specified accounts and Amazon Web Services Regions. If the stack set
    -- update does not include changes to the template or parameters,
    -- CloudFormation updates the stack instances in the specified accounts and
    -- Amazon Web Services Regions, while leaving all other stack instances
    -- with their existing stack instance status.
    accounts :: Prelude.Maybe [Prelude.Text],
    -- | In some cases, you must explicitly acknowledge that your stack template
    -- contains certain capabilities in order for CloudFormation to update the
    -- stack set and its associated stack instances.
    --
    -- -   @CAPABILITY_IAM@ and @CAPABILITY_NAMED_IAM@
    --
    --     Some stack templates might include resources that can affect
    --     permissions in your Amazon Web Services account; for example, by
    --     creating new Identity and Access Management (IAM) users. For those
    --     stacks sets, you must explicitly acknowledge this by specifying one
    --     of these capabilities.
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
    --     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in CloudFormation Templates>.
    --
    -- -   @CAPABILITY_AUTO_EXPAND@
    --
    --     Some templates reference macros. If your stack set template
    --     references one or more macros, you must update the stack set
    --     directly from the processed template, without first reviewing the
    --     resulting changes in a change set. To update the stack set directly,
    --     you must acknowledge this capability. For more information, see
    --     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-macros.html Using CloudFormation Macros to Perform Custom Processing on Templates>.
    --
    --     Stack sets with service-managed permissions do not currently support
    --     the use of macros in templates. (This includes the
    --     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/create-reusable-transform-function-snippets-and-add-to-your-template-with-aws-include-transform.html AWS::Include>
    --     and
    --     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/transform-aws-serverless.html AWS::Serverless>
    --     transforms, which are macros hosted by CloudFormation.) Even if you
    --     specify this capability for a stack set with service-managed
    --     permissions, if you reference a macro in your template the stack set
    --     operation will fail.
    capabilities :: Prelude.Maybe [Capability],
    -- | Describes whether StackSets performs non-conflicting operations
    -- concurrently and queues conflicting operations.
    managedExecution :: Prelude.Maybe ManagedExecution,
    -- | The name of the IAM execution role to use to update the stack set. If
    -- you do not specify an execution role, CloudFormation uses the
    -- @AWSCloudFormationStackSetExecutionRole@ role for the stack set
    -- operation.
    --
    -- Specify an IAM role only if you are using customized execution roles to
    -- control which stack resources users and groups can include in their
    -- stack sets.
    --
    -- If you specify a customized execution role, CloudFormation uses that
    -- role to update the stack. If you do not specify a customized execution
    -- role, CloudFormation performs the update using the role previously
    -- associated with the stack set, so long as you have permissions to
    -- perform operations on the stack set.
    executionRoleName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role to use to update this
    -- stack set.
    --
    -- Specify an IAM role only if you are using customized administrator roles
    -- to control which users or groups can manage specific stack sets within
    -- the same administrator account. For more information, see
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Granting Permissions for Stack Set Operations>
    -- in the /CloudFormation User Guide/.
    --
    -- If you specified a customized administrator role when you created the
    -- stack set, you must specify a customized administrator role, even if it
    -- is the same customized administrator role used with this stack set
    -- previously.
    administrationRoleARN :: Prelude.Maybe Prelude.Text,
    -- | [Service-managed permissions] The Organizations accounts in which to
    -- update associated stack instances.
    --
    -- To update all the stack instances associated with this stack set, do not
    -- specify @DeploymentTargets@ or @Regions@.
    --
    -- If the stack set update includes changes to the template (that is, if
    -- @TemplateBody@ or @TemplateURL@ is specified), or the @Parameters@,
    -- CloudFormation marks all stack instances with a status of @OUTDATED@
    -- prior to updating the stack instances in the specified accounts and
    -- Amazon Web Services Regions. If the stack set update doesn\'t include
    -- changes to the template or parameters, CloudFormation updates the stack
    -- instances in the specified accounts and Regions, while leaving all other
    -- stack instances with their existing stack instance status.
    deploymentTargets :: Prelude.Maybe DeploymentTargets,
    -- | Describes how the IAM roles required for stack set operations are
    -- created. You cannot modify @PermissionModel@ if there are stack
    -- instances associated with your stack set.
    --
    -- -   With @self-managed@ permissions, you must create the administrator
    --     and execution roles required to deploy to target accounts. For more
    --     information, see
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions>.
    --
    -- -   With @service-managed@ permissions, StackSets automatically creates
    --     the IAM roles required to deploy to accounts managed by
    --     Organizations. For more information, see
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions>.
    permissionModel :: Prelude.Maybe PermissionModels,
    -- | The location of the file that contains the template body. The URL must
    -- point to a template (maximum size: 460,800 bytes) that is located in an
    -- Amazon S3 bucket or a Systems Manager document. For more information,
    -- see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
    -- in the CloudFormation User Guide.
    --
    -- Conditional: You must specify only one of the following parameters:
    -- @TemplateBody@ or @TemplateURL@—or set @UsePreviousTemplate@ to true.
    templateURL :: Prelude.Maybe Prelude.Text,
    -- | A list of input parameters for the stack set template.
    parameters :: Prelude.Maybe [Parameter],
    -- | The name or unique ID of the stack set that you want to update.
    stackSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStackSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'updateStackSet_tags' - The key-value pairs to associate with this stack set and the stacks
-- created from it. CloudFormation also propagates these tags to supported
-- resources that are created in the stacks. You can specify a maximum
-- number of 50 tags.
--
-- If you specify tags for this parameter, those tags replace any list of
-- tags that are currently associated with this stack set. This means:
--
-- -   If you don\'t specify this parameter, CloudFormation doesn\'t modify
--     the stack\'s tags.
--
-- -   If you specify /any/ tags using this parameter, you must specify
--     /all/ the tags that you want associated with this stack set, even
--     tags you\'ve specified before (for example, when creating the stack
--     set or during a previous update of the stack set.). Any tags that
--     you don\'t include in the updated list of tags are removed from the
--     stack set, and therefore from the stacks and resources as well.
--
-- -   If you specify an empty value, CloudFormation removes all currently
--     associated tags.
--
-- If you specify new tags as part of an @UpdateStackSet@ action,
-- CloudFormation checks to see if you have the required IAM permission to
-- tag resources. If you omit tags that are currently associated with the
-- stack set from the list of tags you specify, CloudFormation assumes that
-- you want to remove those tags from the stack set, and checks to see if
-- you have permission to untag resources. If you don\'t have the necessary
-- permission(s), the entire @UpdateStackSet@ action fails with an
-- @access denied@ error, and the stack set is not updated.
--
-- 'operationPreferences', 'updateStackSet_operationPreferences' - Preferences for how CloudFormation performs this stack set operation.
--
-- 'callAs', 'updateStackSet_callAs' - [Service-managed permissions] Specifies whether you are acting as an
-- account administrator in the organization\'s management account or as a
-- delegated administrator in a member account.
--
-- By default, @SELF@ is specified. Use @SELF@ for stack sets with
-- self-managed permissions.
--
-- -   If you are signed in to the management account, specify @SELF@.
--
-- -   If you are signed in to a delegated administrator account, specify
--     @DELEGATED_ADMIN@.
--
--     Your Amazon Web Services account must be registered as a delegated
--     administrator in the management account. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
--     in the /CloudFormation User Guide/.
--
-- 'regions', 'updateStackSet_regions' - The Amazon Web Services Regions in which to update associated stack
-- instances. If you specify Regions, you must also specify accounts in
-- which to update stack set instances.
--
-- To update /all/ the stack instances associated with this stack set, do
-- not specify the @Accounts@ or @Regions@ properties.
--
-- If the stack set update includes changes to the template (that is, if
-- the @TemplateBody@ or @TemplateURL@ properties are specified), or the
-- @Parameters@ property, CloudFormation marks all stack instances with a
-- status of @OUTDATED@ prior to updating the stack instances in the
-- specified accounts and Regions. If the stack set update does not include
-- changes to the template or parameters, CloudFormation updates the stack
-- instances in the specified accounts and Regions, while leaving all other
-- stack instances with their existing stack instance status.
--
-- 'usePreviousTemplate', 'updateStackSet_usePreviousTemplate' - Use the existing template that\'s associated with the stack set that
-- you\'re updating.
--
-- Conditional: You must specify only one of the following parameters:
-- @TemplateBody@ or @TemplateURL@—or set @UsePreviousTemplate@ to true.
--
-- 'templateBody', 'updateStackSet_templateBody' - The structure that contains the template body, with a minimum length of
-- 1 byte and a maximum length of 51,200 bytes. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters:
-- @TemplateBody@ or @TemplateURL@—or set @UsePreviousTemplate@ to true.
--
-- 'operationId', 'updateStackSet_operationId' - The unique ID for this stack set operation.
--
-- The operation ID also functions as an idempotency token, to ensure that
-- CloudFormation performs the stack set operation only once, even if you
-- retry the request multiple times. You might retry stack set operation
-- requests to ensure that CloudFormation successfully received them.
--
-- If you don\'t specify an operation ID, CloudFormation generates one
-- automatically.
--
-- Repeating this stack set operation with a new operation ID retries all
-- stack instances whose status is @OUTDATED@.
--
-- 'description', 'updateStackSet_description' - A brief description of updates that you are making.
--
-- 'autoDeployment', 'updateStackSet_autoDeployment' - [Service-managed permissions] Describes whether StackSets automatically
-- deploys to Organizations accounts that are added to a target
-- organization or organizational unit (OU).
--
-- If you specify @AutoDeployment@, don\'t specify @DeploymentTargets@ or
-- @Regions@.
--
-- 'accounts', 'updateStackSet_accounts' - [Self-managed permissions] The accounts in which to update associated
-- stack instances. If you specify accounts, you must also specify the
-- Amazon Web Services Regions in which to update stack set instances.
--
-- To update /all/ the stack instances associated with this stack set,
-- don\'t specify the @Accounts@ or @Regions@ properties.
--
-- If the stack set update includes changes to the template (that is, if
-- the @TemplateBody@ or @TemplateURL@ properties are specified), or the
-- @Parameters@ property, CloudFormation marks all stack instances with a
-- status of @OUTDATED@ prior to updating the stack instances in the
-- specified accounts and Amazon Web Services Regions. If the stack set
-- update does not include changes to the template or parameters,
-- CloudFormation updates the stack instances in the specified accounts and
-- Amazon Web Services Regions, while leaving all other stack instances
-- with their existing stack instance status.
--
-- 'capabilities', 'updateStackSet_capabilities' - In some cases, you must explicitly acknowledge that your stack template
-- contains certain capabilities in order for CloudFormation to update the
-- stack set and its associated stack instances.
--
-- -   @CAPABILITY_IAM@ and @CAPABILITY_NAMED_IAM@
--
--     Some stack templates might include resources that can affect
--     permissions in your Amazon Web Services account; for example, by
--     creating new Identity and Access Management (IAM) users. For those
--     stacks sets, you must explicitly acknowledge this by specifying one
--     of these capabilities.
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
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in CloudFormation Templates>.
--
-- -   @CAPABILITY_AUTO_EXPAND@
--
--     Some templates reference macros. If your stack set template
--     references one or more macros, you must update the stack set
--     directly from the processed template, without first reviewing the
--     resulting changes in a change set. To update the stack set directly,
--     you must acknowledge this capability. For more information, see
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-macros.html Using CloudFormation Macros to Perform Custom Processing on Templates>.
--
--     Stack sets with service-managed permissions do not currently support
--     the use of macros in templates. (This includes the
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/create-reusable-transform-function-snippets-and-add-to-your-template-with-aws-include-transform.html AWS::Include>
--     and
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/transform-aws-serverless.html AWS::Serverless>
--     transforms, which are macros hosted by CloudFormation.) Even if you
--     specify this capability for a stack set with service-managed
--     permissions, if you reference a macro in your template the stack set
--     operation will fail.
--
-- 'managedExecution', 'updateStackSet_managedExecution' - Describes whether StackSets performs non-conflicting operations
-- concurrently and queues conflicting operations.
--
-- 'executionRoleName', 'updateStackSet_executionRoleName' - The name of the IAM execution role to use to update the stack set. If
-- you do not specify an execution role, CloudFormation uses the
-- @AWSCloudFormationStackSetExecutionRole@ role for the stack set
-- operation.
--
-- Specify an IAM role only if you are using customized execution roles to
-- control which stack resources users and groups can include in their
-- stack sets.
--
-- If you specify a customized execution role, CloudFormation uses that
-- role to update the stack. If you do not specify a customized execution
-- role, CloudFormation performs the update using the role previously
-- associated with the stack set, so long as you have permissions to
-- perform operations on the stack set.
--
-- 'administrationRoleARN', 'updateStackSet_administrationRoleARN' - The Amazon Resource Name (ARN) of the IAM role to use to update this
-- stack set.
--
-- Specify an IAM role only if you are using customized administrator roles
-- to control which users or groups can manage specific stack sets within
-- the same administrator account. For more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Granting Permissions for Stack Set Operations>
-- in the /CloudFormation User Guide/.
--
-- If you specified a customized administrator role when you created the
-- stack set, you must specify a customized administrator role, even if it
-- is the same customized administrator role used with this stack set
-- previously.
--
-- 'deploymentTargets', 'updateStackSet_deploymentTargets' - [Service-managed permissions] The Organizations accounts in which to
-- update associated stack instances.
--
-- To update all the stack instances associated with this stack set, do not
-- specify @DeploymentTargets@ or @Regions@.
--
-- If the stack set update includes changes to the template (that is, if
-- @TemplateBody@ or @TemplateURL@ is specified), or the @Parameters@,
-- CloudFormation marks all stack instances with a status of @OUTDATED@
-- prior to updating the stack instances in the specified accounts and
-- Amazon Web Services Regions. If the stack set update doesn\'t include
-- changes to the template or parameters, CloudFormation updates the stack
-- instances in the specified accounts and Regions, while leaving all other
-- stack instances with their existing stack instance status.
--
-- 'permissionModel', 'updateStackSet_permissionModel' - Describes how the IAM roles required for stack set operations are
-- created. You cannot modify @PermissionModel@ if there are stack
-- instances associated with your stack set.
--
-- -   With @self-managed@ permissions, you must create the administrator
--     and execution roles required to deploy to target accounts. For more
--     information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions>.
--
-- -   With @service-managed@ permissions, StackSets automatically creates
--     the IAM roles required to deploy to accounts managed by
--     Organizations. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions>.
--
-- 'templateURL', 'updateStackSet_templateURL' - The location of the file that contains the template body. The URL must
-- point to a template (maximum size: 460,800 bytes) that is located in an
-- Amazon S3 bucket or a Systems Manager document. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters:
-- @TemplateBody@ or @TemplateURL@—or set @UsePreviousTemplate@ to true.
--
-- 'parameters', 'updateStackSet_parameters' - A list of input parameters for the stack set template.
--
-- 'stackSetName', 'updateStackSet_stackSetName' - The name or unique ID of the stack set that you want to update.
newUpdateStackSet ::
  -- | 'stackSetName'
  Prelude.Text ->
  UpdateStackSet
newUpdateStackSet pStackSetName_ =
  UpdateStackSet'
    { tags = Prelude.Nothing,
      operationPreferences = Prelude.Nothing,
      callAs = Prelude.Nothing,
      regions = Prelude.Nothing,
      usePreviousTemplate = Prelude.Nothing,
      templateBody = Prelude.Nothing,
      operationId = Prelude.Nothing,
      description = Prelude.Nothing,
      autoDeployment = Prelude.Nothing,
      accounts = Prelude.Nothing,
      capabilities = Prelude.Nothing,
      managedExecution = Prelude.Nothing,
      executionRoleName = Prelude.Nothing,
      administrationRoleARN = Prelude.Nothing,
      deploymentTargets = Prelude.Nothing,
      permissionModel = Prelude.Nothing,
      templateURL = Prelude.Nothing,
      parameters = Prelude.Nothing,
      stackSetName = pStackSetName_
    }

-- | The key-value pairs to associate with this stack set and the stacks
-- created from it. CloudFormation also propagates these tags to supported
-- resources that are created in the stacks. You can specify a maximum
-- number of 50 tags.
--
-- If you specify tags for this parameter, those tags replace any list of
-- tags that are currently associated with this stack set. This means:
--
-- -   If you don\'t specify this parameter, CloudFormation doesn\'t modify
--     the stack\'s tags.
--
-- -   If you specify /any/ tags using this parameter, you must specify
--     /all/ the tags that you want associated with this stack set, even
--     tags you\'ve specified before (for example, when creating the stack
--     set or during a previous update of the stack set.). Any tags that
--     you don\'t include in the updated list of tags are removed from the
--     stack set, and therefore from the stacks and resources as well.
--
-- -   If you specify an empty value, CloudFormation removes all currently
--     associated tags.
--
-- If you specify new tags as part of an @UpdateStackSet@ action,
-- CloudFormation checks to see if you have the required IAM permission to
-- tag resources. If you omit tags that are currently associated with the
-- stack set from the list of tags you specify, CloudFormation assumes that
-- you want to remove those tags from the stack set, and checks to see if
-- you have permission to untag resources. If you don\'t have the necessary
-- permission(s), the entire @UpdateStackSet@ action fails with an
-- @access denied@ error, and the stack set is not updated.
updateStackSet_tags :: Lens.Lens' UpdateStackSet (Prelude.Maybe [Tag])
updateStackSet_tags = Lens.lens (\UpdateStackSet' {tags} -> tags) (\s@UpdateStackSet' {} a -> s {tags = a} :: UpdateStackSet) Prelude.. Lens.mapping Lens.coerced

-- | Preferences for how CloudFormation performs this stack set operation.
updateStackSet_operationPreferences :: Lens.Lens' UpdateStackSet (Prelude.Maybe StackSetOperationPreferences)
updateStackSet_operationPreferences = Lens.lens (\UpdateStackSet' {operationPreferences} -> operationPreferences) (\s@UpdateStackSet' {} a -> s {operationPreferences = a} :: UpdateStackSet)

-- | [Service-managed permissions] Specifies whether you are acting as an
-- account administrator in the organization\'s management account or as a
-- delegated administrator in a member account.
--
-- By default, @SELF@ is specified. Use @SELF@ for stack sets with
-- self-managed permissions.
--
-- -   If you are signed in to the management account, specify @SELF@.
--
-- -   If you are signed in to a delegated administrator account, specify
--     @DELEGATED_ADMIN@.
--
--     Your Amazon Web Services account must be registered as a delegated
--     administrator in the management account. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
--     in the /CloudFormation User Guide/.
updateStackSet_callAs :: Lens.Lens' UpdateStackSet (Prelude.Maybe CallAs)
updateStackSet_callAs = Lens.lens (\UpdateStackSet' {callAs} -> callAs) (\s@UpdateStackSet' {} a -> s {callAs = a} :: UpdateStackSet)

-- | The Amazon Web Services Regions in which to update associated stack
-- instances. If you specify Regions, you must also specify accounts in
-- which to update stack set instances.
--
-- To update /all/ the stack instances associated with this stack set, do
-- not specify the @Accounts@ or @Regions@ properties.
--
-- If the stack set update includes changes to the template (that is, if
-- the @TemplateBody@ or @TemplateURL@ properties are specified), or the
-- @Parameters@ property, CloudFormation marks all stack instances with a
-- status of @OUTDATED@ prior to updating the stack instances in the
-- specified accounts and Regions. If the stack set update does not include
-- changes to the template or parameters, CloudFormation updates the stack
-- instances in the specified accounts and Regions, while leaving all other
-- stack instances with their existing stack instance status.
updateStackSet_regions :: Lens.Lens' UpdateStackSet (Prelude.Maybe [Prelude.Text])
updateStackSet_regions = Lens.lens (\UpdateStackSet' {regions} -> regions) (\s@UpdateStackSet' {} a -> s {regions = a} :: UpdateStackSet) Prelude.. Lens.mapping Lens.coerced

-- | Use the existing template that\'s associated with the stack set that
-- you\'re updating.
--
-- Conditional: You must specify only one of the following parameters:
-- @TemplateBody@ or @TemplateURL@—or set @UsePreviousTemplate@ to true.
updateStackSet_usePreviousTemplate :: Lens.Lens' UpdateStackSet (Prelude.Maybe Prelude.Bool)
updateStackSet_usePreviousTemplate = Lens.lens (\UpdateStackSet' {usePreviousTemplate} -> usePreviousTemplate) (\s@UpdateStackSet' {} a -> s {usePreviousTemplate = a} :: UpdateStackSet)

-- | The structure that contains the template body, with a minimum length of
-- 1 byte and a maximum length of 51,200 bytes. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters:
-- @TemplateBody@ or @TemplateURL@—or set @UsePreviousTemplate@ to true.
updateStackSet_templateBody :: Lens.Lens' UpdateStackSet (Prelude.Maybe Prelude.Text)
updateStackSet_templateBody = Lens.lens (\UpdateStackSet' {templateBody} -> templateBody) (\s@UpdateStackSet' {} a -> s {templateBody = a} :: UpdateStackSet)

-- | The unique ID for this stack set operation.
--
-- The operation ID also functions as an idempotency token, to ensure that
-- CloudFormation performs the stack set operation only once, even if you
-- retry the request multiple times. You might retry stack set operation
-- requests to ensure that CloudFormation successfully received them.
--
-- If you don\'t specify an operation ID, CloudFormation generates one
-- automatically.
--
-- Repeating this stack set operation with a new operation ID retries all
-- stack instances whose status is @OUTDATED@.
updateStackSet_operationId :: Lens.Lens' UpdateStackSet (Prelude.Maybe Prelude.Text)
updateStackSet_operationId = Lens.lens (\UpdateStackSet' {operationId} -> operationId) (\s@UpdateStackSet' {} a -> s {operationId = a} :: UpdateStackSet)

-- | A brief description of updates that you are making.
updateStackSet_description :: Lens.Lens' UpdateStackSet (Prelude.Maybe Prelude.Text)
updateStackSet_description = Lens.lens (\UpdateStackSet' {description} -> description) (\s@UpdateStackSet' {} a -> s {description = a} :: UpdateStackSet)

-- | [Service-managed permissions] Describes whether StackSets automatically
-- deploys to Organizations accounts that are added to a target
-- organization or organizational unit (OU).
--
-- If you specify @AutoDeployment@, don\'t specify @DeploymentTargets@ or
-- @Regions@.
updateStackSet_autoDeployment :: Lens.Lens' UpdateStackSet (Prelude.Maybe AutoDeployment)
updateStackSet_autoDeployment = Lens.lens (\UpdateStackSet' {autoDeployment} -> autoDeployment) (\s@UpdateStackSet' {} a -> s {autoDeployment = a} :: UpdateStackSet)

-- | [Self-managed permissions] The accounts in which to update associated
-- stack instances. If you specify accounts, you must also specify the
-- Amazon Web Services Regions in which to update stack set instances.
--
-- To update /all/ the stack instances associated with this stack set,
-- don\'t specify the @Accounts@ or @Regions@ properties.
--
-- If the stack set update includes changes to the template (that is, if
-- the @TemplateBody@ or @TemplateURL@ properties are specified), or the
-- @Parameters@ property, CloudFormation marks all stack instances with a
-- status of @OUTDATED@ prior to updating the stack instances in the
-- specified accounts and Amazon Web Services Regions. If the stack set
-- update does not include changes to the template or parameters,
-- CloudFormation updates the stack instances in the specified accounts and
-- Amazon Web Services Regions, while leaving all other stack instances
-- with their existing stack instance status.
updateStackSet_accounts :: Lens.Lens' UpdateStackSet (Prelude.Maybe [Prelude.Text])
updateStackSet_accounts = Lens.lens (\UpdateStackSet' {accounts} -> accounts) (\s@UpdateStackSet' {} a -> s {accounts = a} :: UpdateStackSet) Prelude.. Lens.mapping Lens.coerced

-- | In some cases, you must explicitly acknowledge that your stack template
-- contains certain capabilities in order for CloudFormation to update the
-- stack set and its associated stack instances.
--
-- -   @CAPABILITY_IAM@ and @CAPABILITY_NAMED_IAM@
--
--     Some stack templates might include resources that can affect
--     permissions in your Amazon Web Services account; for example, by
--     creating new Identity and Access Management (IAM) users. For those
--     stacks sets, you must explicitly acknowledge this by specifying one
--     of these capabilities.
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
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in CloudFormation Templates>.
--
-- -   @CAPABILITY_AUTO_EXPAND@
--
--     Some templates reference macros. If your stack set template
--     references one or more macros, you must update the stack set
--     directly from the processed template, without first reviewing the
--     resulting changes in a change set. To update the stack set directly,
--     you must acknowledge this capability. For more information, see
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-macros.html Using CloudFormation Macros to Perform Custom Processing on Templates>.
--
--     Stack sets with service-managed permissions do not currently support
--     the use of macros in templates. (This includes the
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/create-reusable-transform-function-snippets-and-add-to-your-template-with-aws-include-transform.html AWS::Include>
--     and
--     <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/transform-aws-serverless.html AWS::Serverless>
--     transforms, which are macros hosted by CloudFormation.) Even if you
--     specify this capability for a stack set with service-managed
--     permissions, if you reference a macro in your template the stack set
--     operation will fail.
updateStackSet_capabilities :: Lens.Lens' UpdateStackSet (Prelude.Maybe [Capability])
updateStackSet_capabilities = Lens.lens (\UpdateStackSet' {capabilities} -> capabilities) (\s@UpdateStackSet' {} a -> s {capabilities = a} :: UpdateStackSet) Prelude.. Lens.mapping Lens.coerced

-- | Describes whether StackSets performs non-conflicting operations
-- concurrently and queues conflicting operations.
updateStackSet_managedExecution :: Lens.Lens' UpdateStackSet (Prelude.Maybe ManagedExecution)
updateStackSet_managedExecution = Lens.lens (\UpdateStackSet' {managedExecution} -> managedExecution) (\s@UpdateStackSet' {} a -> s {managedExecution = a} :: UpdateStackSet)

-- | The name of the IAM execution role to use to update the stack set. If
-- you do not specify an execution role, CloudFormation uses the
-- @AWSCloudFormationStackSetExecutionRole@ role for the stack set
-- operation.
--
-- Specify an IAM role only if you are using customized execution roles to
-- control which stack resources users and groups can include in their
-- stack sets.
--
-- If you specify a customized execution role, CloudFormation uses that
-- role to update the stack. If you do not specify a customized execution
-- role, CloudFormation performs the update using the role previously
-- associated with the stack set, so long as you have permissions to
-- perform operations on the stack set.
updateStackSet_executionRoleName :: Lens.Lens' UpdateStackSet (Prelude.Maybe Prelude.Text)
updateStackSet_executionRoleName = Lens.lens (\UpdateStackSet' {executionRoleName} -> executionRoleName) (\s@UpdateStackSet' {} a -> s {executionRoleName = a} :: UpdateStackSet)

-- | The Amazon Resource Name (ARN) of the IAM role to use to update this
-- stack set.
--
-- Specify an IAM role only if you are using customized administrator roles
-- to control which users or groups can manage specific stack sets within
-- the same administrator account. For more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Granting Permissions for Stack Set Operations>
-- in the /CloudFormation User Guide/.
--
-- If you specified a customized administrator role when you created the
-- stack set, you must specify a customized administrator role, even if it
-- is the same customized administrator role used with this stack set
-- previously.
updateStackSet_administrationRoleARN :: Lens.Lens' UpdateStackSet (Prelude.Maybe Prelude.Text)
updateStackSet_administrationRoleARN = Lens.lens (\UpdateStackSet' {administrationRoleARN} -> administrationRoleARN) (\s@UpdateStackSet' {} a -> s {administrationRoleARN = a} :: UpdateStackSet)

-- | [Service-managed permissions] The Organizations accounts in which to
-- update associated stack instances.
--
-- To update all the stack instances associated with this stack set, do not
-- specify @DeploymentTargets@ or @Regions@.
--
-- If the stack set update includes changes to the template (that is, if
-- @TemplateBody@ or @TemplateURL@ is specified), or the @Parameters@,
-- CloudFormation marks all stack instances with a status of @OUTDATED@
-- prior to updating the stack instances in the specified accounts and
-- Amazon Web Services Regions. If the stack set update doesn\'t include
-- changes to the template or parameters, CloudFormation updates the stack
-- instances in the specified accounts and Regions, while leaving all other
-- stack instances with their existing stack instance status.
updateStackSet_deploymentTargets :: Lens.Lens' UpdateStackSet (Prelude.Maybe DeploymentTargets)
updateStackSet_deploymentTargets = Lens.lens (\UpdateStackSet' {deploymentTargets} -> deploymentTargets) (\s@UpdateStackSet' {} a -> s {deploymentTargets = a} :: UpdateStackSet)

-- | Describes how the IAM roles required for stack set operations are
-- created. You cannot modify @PermissionModel@ if there are stack
-- instances associated with your stack set.
--
-- -   With @self-managed@ permissions, you must create the administrator
--     and execution roles required to deploy to target accounts. For more
--     information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions>.
--
-- -   With @service-managed@ permissions, StackSets automatically creates
--     the IAM roles required to deploy to accounts managed by
--     Organizations. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions>.
updateStackSet_permissionModel :: Lens.Lens' UpdateStackSet (Prelude.Maybe PermissionModels)
updateStackSet_permissionModel = Lens.lens (\UpdateStackSet' {permissionModel} -> permissionModel) (\s@UpdateStackSet' {} a -> s {permissionModel = a} :: UpdateStackSet)

-- | The location of the file that contains the template body. The URL must
-- point to a template (maximum size: 460,800 bytes) that is located in an
-- Amazon S3 bucket or a Systems Manager document. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters:
-- @TemplateBody@ or @TemplateURL@—or set @UsePreviousTemplate@ to true.
updateStackSet_templateURL :: Lens.Lens' UpdateStackSet (Prelude.Maybe Prelude.Text)
updateStackSet_templateURL = Lens.lens (\UpdateStackSet' {templateURL} -> templateURL) (\s@UpdateStackSet' {} a -> s {templateURL = a} :: UpdateStackSet)

-- | A list of input parameters for the stack set template.
updateStackSet_parameters :: Lens.Lens' UpdateStackSet (Prelude.Maybe [Parameter])
updateStackSet_parameters = Lens.lens (\UpdateStackSet' {parameters} -> parameters) (\s@UpdateStackSet' {} a -> s {parameters = a} :: UpdateStackSet) Prelude.. Lens.mapping Lens.coerced

-- | The name or unique ID of the stack set that you want to update.
updateStackSet_stackSetName :: Lens.Lens' UpdateStackSet Prelude.Text
updateStackSet_stackSetName = Lens.lens (\UpdateStackSet' {stackSetName} -> stackSetName) (\s@UpdateStackSet' {} a -> s {stackSetName = a} :: UpdateStackSet)

instance Core.AWSRequest UpdateStackSet where
  type
    AWSResponse UpdateStackSet =
      UpdateStackSetResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "UpdateStackSetResult"
      ( \s h x ->
          UpdateStackSetResponse'
            Prelude.<$> (x Data..@? "OperationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateStackSet where
  hashWithSalt _salt UpdateStackSet' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` operationPreferences
      `Prelude.hashWithSalt` callAs
      `Prelude.hashWithSalt` regions
      `Prelude.hashWithSalt` usePreviousTemplate
      `Prelude.hashWithSalt` templateBody
      `Prelude.hashWithSalt` operationId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` autoDeployment
      `Prelude.hashWithSalt` accounts
      `Prelude.hashWithSalt` capabilities
      `Prelude.hashWithSalt` managedExecution
      `Prelude.hashWithSalt` executionRoleName
      `Prelude.hashWithSalt` administrationRoleARN
      `Prelude.hashWithSalt` deploymentTargets
      `Prelude.hashWithSalt` permissionModel
      `Prelude.hashWithSalt` templateURL
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` stackSetName

instance Prelude.NFData UpdateStackSet where
  rnf UpdateStackSet' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf operationPreferences
      `Prelude.seq` Prelude.rnf callAs
      `Prelude.seq` Prelude.rnf regions
      `Prelude.seq` Prelude.rnf usePreviousTemplate
      `Prelude.seq` Prelude.rnf templateBody
      `Prelude.seq` Prelude.rnf operationId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf autoDeployment
      `Prelude.seq` Prelude.rnf accounts
      `Prelude.seq` Prelude.rnf capabilities
      `Prelude.seq` Prelude.rnf managedExecution
      `Prelude.seq` Prelude.rnf executionRoleName
      `Prelude.seq` Prelude.rnf administrationRoleARN
      `Prelude.seq` Prelude.rnf deploymentTargets
      `Prelude.seq` Prelude.rnf permissionModel
      `Prelude.seq` Prelude.rnf templateURL
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf stackSetName

instance Data.ToHeaders UpdateStackSet where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath UpdateStackSet where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateStackSet where
  toQuery UpdateStackSet' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("UpdateStackSet" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> tags),
        "OperationPreferences" Data.=: operationPreferences,
        "CallAs" Data.=: callAs,
        "Regions"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> regions),
        "UsePreviousTemplate" Data.=: usePreviousTemplate,
        "TemplateBody" Data.=: templateBody,
        "OperationId" Data.=: operationId,
        "Description" Data.=: description,
        "AutoDeployment" Data.=: autoDeployment,
        "Accounts"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> accounts),
        "Capabilities"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> capabilities),
        "ManagedExecution" Data.=: managedExecution,
        "ExecutionRoleName" Data.=: executionRoleName,
        "AdministrationRoleARN"
          Data.=: administrationRoleARN,
        "DeploymentTargets" Data.=: deploymentTargets,
        "PermissionModel" Data.=: permissionModel,
        "TemplateURL" Data.=: templateURL,
        "Parameters"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> parameters),
        "StackSetName" Data.=: stackSetName
      ]

-- | /See:/ 'newUpdateStackSetResponse' smart constructor.
data UpdateStackSetResponse = UpdateStackSetResponse'
  { -- | The unique ID for this stack set operation.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStackSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'updateStackSetResponse_operationId' - The unique ID for this stack set operation.
--
-- 'httpStatus', 'updateStackSetResponse_httpStatus' - The response's http status code.
newUpdateStackSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateStackSetResponse
newUpdateStackSetResponse pHttpStatus_ =
  UpdateStackSetResponse'
    { operationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique ID for this stack set operation.
updateStackSetResponse_operationId :: Lens.Lens' UpdateStackSetResponse (Prelude.Maybe Prelude.Text)
updateStackSetResponse_operationId = Lens.lens (\UpdateStackSetResponse' {operationId} -> operationId) (\s@UpdateStackSetResponse' {} a -> s {operationId = a} :: UpdateStackSetResponse)

-- | The response's http status code.
updateStackSetResponse_httpStatus :: Lens.Lens' UpdateStackSetResponse Prelude.Int
updateStackSetResponse_httpStatus = Lens.lens (\UpdateStackSetResponse' {httpStatus} -> httpStatus) (\s@UpdateStackSetResponse' {} a -> s {httpStatus = a} :: UpdateStackSetResponse)

instance Prelude.NFData UpdateStackSetResponse where
  rnf UpdateStackSetResponse' {..} =
    Prelude.rnf operationId
      `Prelude.seq` Prelude.rnf httpStatus
