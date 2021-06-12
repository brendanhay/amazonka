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
-- Module      : Network.AWS.CloudFormation.UpdateStackSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the stack set, and associated stack instances in the specified
-- accounts and Regions.
--
-- Even if the stack set operation created by updating the stack set fails
-- (completely or partially, below or above a specified failure tolerance),
-- the stack set is updated with your changes. Subsequent
-- CreateStackInstances calls on the specified stack set use the updated
-- stack set.
module Network.AWS.CloudFormation.UpdateStackSet
  ( -- * Creating a Request
    UpdateStackSet (..),
    newUpdateStackSet,

    -- * Request Lenses
    updateStackSet_permissionModel,
    updateStackSet_executionRoleName,
    updateStackSet_capabilities,
    updateStackSet_templateURL,
    updateStackSet_deploymentTargets,
    updateStackSet_operationId,
    updateStackSet_callAs,
    updateStackSet_operationPreferences,
    updateStackSet_accounts,
    updateStackSet_administrationRoleARN,
    updateStackSet_tags,
    updateStackSet_autoDeployment,
    updateStackSet_description,
    updateStackSet_regions,
    updateStackSet_templateBody,
    updateStackSet_parameters,
    updateStackSet_usePreviousTemplate,
    updateStackSet_stackSetName,

    -- * Destructuring the Response
    UpdateStackSetResponse (..),
    newUpdateStackSetResponse,

    -- * Response Lenses
    updateStackSetResponse_operationId,
    updateStackSetResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateStackSet' smart constructor.
data UpdateStackSet = UpdateStackSet'
  { -- | Describes how the IAM roles required for stack set operations are
    -- created. You cannot modify @PermissionModel@ if there are stack
    -- instances associated with your stack set.
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
    permissionModel :: Core.Maybe PermissionModels,
    -- | The name of the IAM execution role to use to update the stack set. If
    -- you do not specify an execution role, AWS CloudFormation uses the
    -- @AWSCloudFormationStackSetExecutionRole@ role for the stack set
    -- operation.
    --
    -- Specify an IAM role only if you are using customized execution roles to
    -- control which stack resources users and groups can include in their
    -- stack sets.
    --
    -- If you specify a customized execution role, AWS CloudFormation uses that
    -- role to update the stack. If you do not specify a customized execution
    -- role, AWS CloudFormation performs the update using the role previously
    -- associated with the stack set, so long as you have permissions to
    -- perform operations on the stack set.
    executionRoleName :: Core.Maybe Core.Text,
    -- | In some cases, you must explicitly acknowledge that your stack template
    -- contains certain capabilities in order for AWS CloudFormation to update
    -- the stack set and its associated stack instances.
    --
    -- -   @CAPABILITY_IAM@ and @CAPABILITY_NAMED_IAM@
    --
    --     Some stack templates might include resources that can affect
    --     permissions in your AWS account; for example, by creating new AWS
    --     Identity and Access Management (IAM) users. For those stacks sets,
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
    --     or more macros, and you choose to update a stack directly from the
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
    capabilities :: Core.Maybe [Capability],
    -- | The location of the file that contains the template body. The URL must
    -- point to a template (maximum size: 460,800 bytes) that is located in an
    -- Amazon S3 bucket or a Systems Manager document. For more information,
    -- see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
    -- in the AWS CloudFormation User Guide.
    --
    -- Conditional: You must specify only one of the following parameters:
    -- @TemplateBody@ or @TemplateURL@—or set @UsePreviousTemplate@ to true.
    templateURL :: Core.Maybe Core.Text,
    -- | [Service-managed permissions] The AWS Organizations accounts in which to
    -- update associated stack instances.
    --
    -- To update all the stack instances associated with this stack set, do not
    -- specify @DeploymentTargets@ or @Regions@.
    --
    -- If the stack set update includes changes to the template (that is, if
    -- @TemplateBody@ or @TemplateURL@ is specified), or the @Parameters@, AWS
    -- CloudFormation marks all stack instances with a status of @OUTDATED@
    -- prior to updating the stack instances in the specified accounts and
    -- Regions. If the stack set update does not include changes to the
    -- template or parameters, AWS CloudFormation updates the stack instances
    -- in the specified accounts and Regions, while leaving all other stack
    -- instances with their existing stack instance status.
    deploymentTargets :: Core.Maybe DeploymentTargets,
    -- | The unique ID for this stack set operation.
    --
    -- The operation ID also functions as an idempotency token, to ensure that
    -- AWS CloudFormation performs the stack set operation only once, even if
    -- you retry the request multiple times. You might retry stack set
    -- operation requests to ensure that AWS CloudFormation successfully
    -- received them.
    --
    -- If you don\'t specify an operation ID, AWS CloudFormation generates one
    -- automatically.
    --
    -- Repeating this stack set operation with a new operation ID retries all
    -- stack instances whose status is @OUTDATED@.
    operationId :: Core.Maybe Core.Text,
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
    --     Your AWS account must be registered as a delegated administrator in
    --     the management account. For more information, see
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
    --     in the /AWS CloudFormation User Guide/.
    callAs :: Core.Maybe CallAs,
    -- | Preferences for how AWS CloudFormation performs this stack set
    -- operation.
    operationPreferences :: Core.Maybe StackSetOperationPreferences,
    -- | [Self-managed permissions] The accounts in which to update associated
    -- stack instances. If you specify accounts, you must also specify the
    -- Regions in which to update stack set instances.
    --
    -- To update /all/ the stack instances associated with this stack set, do
    -- not specify the @Accounts@ or @Regions@ properties.
    --
    -- If the stack set update includes changes to the template (that is, if
    -- the @TemplateBody@ or @TemplateURL@ properties are specified), or the
    -- @Parameters@ property, AWS CloudFormation marks all stack instances with
    -- a status of @OUTDATED@ prior to updating the stack instances in the
    -- specified accounts and Regions. If the stack set update does not include
    -- changes to the template or parameters, AWS CloudFormation updates the
    -- stack instances in the specified accounts and Regions, while leaving all
    -- other stack instances with their existing stack instance status.
    accounts :: Core.Maybe [Core.Text],
    -- | The Amazon Resource Number (ARN) of the IAM role to use to update this
    -- stack set.
    --
    -- Specify an IAM role only if you are using customized administrator roles
    -- to control which users or groups can manage specific stack sets within
    -- the same administrator account. For more information, see
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Granting Permissions for Stack Set Operations>
    -- in the /AWS CloudFormation User Guide/.
    --
    -- If you specified a customized administrator role when you created the
    -- stack set, you must specify a customized administrator role, even if it
    -- is the same customized administrator role used with this stack set
    -- previously.
    administrationRoleARN :: Core.Maybe Core.Text,
    -- | The key-value pairs to associate with this stack set and the stacks
    -- created from it. AWS CloudFormation also propagates these tags to
    -- supported resources that are created in the stacks. You can specify a
    -- maximum number of 50 tags.
    --
    -- If you specify tags for this parameter, those tags replace any list of
    -- tags that are currently associated with this stack set. This means:
    --
    -- -   If you don\'t specify this parameter, AWS CloudFormation doesn\'t
    --     modify the stack\'s tags.
    --
    -- -   If you specify /any/ tags using this parameter, you must specify
    --     /all/ the tags that you want associated with this stack set, even
    --     tags you\'ve specifed before (for example, when creating the stack
    --     set or during a previous update of the stack set.). Any tags that
    --     you don\'t include in the updated list of tags are removed from the
    --     stack set, and therefore from the stacks and resources as well.
    --
    -- -   If you specify an empty value, AWS CloudFormation removes all
    --     currently associated tags.
    --
    -- If you specify new tags as part of an @UpdateStackSet@ action, AWS
    -- CloudFormation checks to see if you have the required IAM permission to
    -- tag resources. If you omit tags that are currently associated with the
    -- stack set from the list of tags you specify, AWS CloudFormation assumes
    -- that you want to remove those tags from the stack set, and checks to see
    -- if you have permission to untag resources. If you don\'t have the
    -- necessary permission(s), the entire @UpdateStackSet@ action fails with
    -- an @access denied@ error, and the stack set is not updated.
    tags :: Core.Maybe [Tag],
    -- | [Service-managed permissions] Describes whether StackSets automatically
    -- deploys to AWS Organizations accounts that are added to a target
    -- organization or organizational unit (OU).
    --
    -- If you specify @AutoDeployment@, do not specify @DeploymentTargets@ or
    -- @Regions@.
    autoDeployment :: Core.Maybe AutoDeployment,
    -- | A brief description of updates that you are making.
    description :: Core.Maybe Core.Text,
    -- | The Regions in which to update associated stack instances. If you
    -- specify Regions, you must also specify accounts in which to update stack
    -- set instances.
    --
    -- To update /all/ the stack instances associated with this stack set, do
    -- not specify the @Accounts@ or @Regions@ properties.
    --
    -- If the stack set update includes changes to the template (that is, if
    -- the @TemplateBody@ or @TemplateURL@ properties are specified), or the
    -- @Parameters@ property, AWS CloudFormation marks all stack instances with
    -- a status of @OUTDATED@ prior to updating the stack instances in the
    -- specified accounts and Regions. If the stack set update does not include
    -- changes to the template or parameters, AWS CloudFormation updates the
    -- stack instances in the specified accounts and Regions, while leaving all
    -- other stack instances with their existing stack instance status.
    regions :: Core.Maybe [Core.Text],
    -- | The structure that contains the template body, with a minimum length of
    -- 1 byte and a maximum length of 51,200 bytes. For more information, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
    -- in the AWS CloudFormation User Guide.
    --
    -- Conditional: You must specify only one of the following parameters:
    -- @TemplateBody@ or @TemplateURL@—or set @UsePreviousTemplate@ to true.
    templateBody :: Core.Maybe Core.Text,
    -- | A list of input parameters for the stack set template.
    parameters :: Core.Maybe [Parameter],
    -- | Use the existing template that\'s associated with the stack set that
    -- you\'re updating.
    --
    -- Conditional: You must specify only one of the following parameters:
    -- @TemplateBody@ or @TemplateURL@—or set @UsePreviousTemplate@ to true.
    usePreviousTemplate :: Core.Maybe Core.Bool,
    -- | The name or unique ID of the stack set that you want to update.
    stackSetName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateStackSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
--     the IAM roles required to deploy to accounts managed by AWS
--     Organizations. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions>.
--
-- 'executionRoleName', 'updateStackSet_executionRoleName' - The name of the IAM execution role to use to update the stack set. If
-- you do not specify an execution role, AWS CloudFormation uses the
-- @AWSCloudFormationStackSetExecutionRole@ role for the stack set
-- operation.
--
-- Specify an IAM role only if you are using customized execution roles to
-- control which stack resources users and groups can include in their
-- stack sets.
--
-- If you specify a customized execution role, AWS CloudFormation uses that
-- role to update the stack. If you do not specify a customized execution
-- role, AWS CloudFormation performs the update using the role previously
-- associated with the stack set, so long as you have permissions to
-- perform operations on the stack set.
--
-- 'capabilities', 'updateStackSet_capabilities' - In some cases, you must explicitly acknowledge that your stack template
-- contains certain capabilities in order for AWS CloudFormation to update
-- the stack set and its associated stack instances.
--
-- -   @CAPABILITY_IAM@ and @CAPABILITY_NAMED_IAM@
--
--     Some stack templates might include resources that can affect
--     permissions in your AWS account; for example, by creating new AWS
--     Identity and Access Management (IAM) users. For those stacks sets,
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
--     or more macros, and you choose to update a stack directly from the
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
-- 'templateURL', 'updateStackSet_templateURL' - The location of the file that contains the template body. The URL must
-- point to a template (maximum size: 460,800 bytes) that is located in an
-- Amazon S3 bucket or a Systems Manager document. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters:
-- @TemplateBody@ or @TemplateURL@—or set @UsePreviousTemplate@ to true.
--
-- 'deploymentTargets', 'updateStackSet_deploymentTargets' - [Service-managed permissions] The AWS Organizations accounts in which to
-- update associated stack instances.
--
-- To update all the stack instances associated with this stack set, do not
-- specify @DeploymentTargets@ or @Regions@.
--
-- If the stack set update includes changes to the template (that is, if
-- @TemplateBody@ or @TemplateURL@ is specified), or the @Parameters@, AWS
-- CloudFormation marks all stack instances with a status of @OUTDATED@
-- prior to updating the stack instances in the specified accounts and
-- Regions. If the stack set update does not include changes to the
-- template or parameters, AWS CloudFormation updates the stack instances
-- in the specified accounts and Regions, while leaving all other stack
-- instances with their existing stack instance status.
--
-- 'operationId', 'updateStackSet_operationId' - The unique ID for this stack set operation.
--
-- The operation ID also functions as an idempotency token, to ensure that
-- AWS CloudFormation performs the stack set operation only once, even if
-- you retry the request multiple times. You might retry stack set
-- operation requests to ensure that AWS CloudFormation successfully
-- received them.
--
-- If you don\'t specify an operation ID, AWS CloudFormation generates one
-- automatically.
--
-- Repeating this stack set operation with a new operation ID retries all
-- stack instances whose status is @OUTDATED@.
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
--     Your AWS account must be registered as a delegated administrator in
--     the management account. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
--     in the /AWS CloudFormation User Guide/.
--
-- 'operationPreferences', 'updateStackSet_operationPreferences' - Preferences for how AWS CloudFormation performs this stack set
-- operation.
--
-- 'accounts', 'updateStackSet_accounts' - [Self-managed permissions] The accounts in which to update associated
-- stack instances. If you specify accounts, you must also specify the
-- Regions in which to update stack set instances.
--
-- To update /all/ the stack instances associated with this stack set, do
-- not specify the @Accounts@ or @Regions@ properties.
--
-- If the stack set update includes changes to the template (that is, if
-- the @TemplateBody@ or @TemplateURL@ properties are specified), or the
-- @Parameters@ property, AWS CloudFormation marks all stack instances with
-- a status of @OUTDATED@ prior to updating the stack instances in the
-- specified accounts and Regions. If the stack set update does not include
-- changes to the template or parameters, AWS CloudFormation updates the
-- stack instances in the specified accounts and Regions, while leaving all
-- other stack instances with their existing stack instance status.
--
-- 'administrationRoleARN', 'updateStackSet_administrationRoleARN' - The Amazon Resource Number (ARN) of the IAM role to use to update this
-- stack set.
--
-- Specify an IAM role only if you are using customized administrator roles
-- to control which users or groups can manage specific stack sets within
-- the same administrator account. For more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Granting Permissions for Stack Set Operations>
-- in the /AWS CloudFormation User Guide/.
--
-- If you specified a customized administrator role when you created the
-- stack set, you must specify a customized administrator role, even if it
-- is the same customized administrator role used with this stack set
-- previously.
--
-- 'tags', 'updateStackSet_tags' - The key-value pairs to associate with this stack set and the stacks
-- created from it. AWS CloudFormation also propagates these tags to
-- supported resources that are created in the stacks. You can specify a
-- maximum number of 50 tags.
--
-- If you specify tags for this parameter, those tags replace any list of
-- tags that are currently associated with this stack set. This means:
--
-- -   If you don\'t specify this parameter, AWS CloudFormation doesn\'t
--     modify the stack\'s tags.
--
-- -   If you specify /any/ tags using this parameter, you must specify
--     /all/ the tags that you want associated with this stack set, even
--     tags you\'ve specifed before (for example, when creating the stack
--     set or during a previous update of the stack set.). Any tags that
--     you don\'t include in the updated list of tags are removed from the
--     stack set, and therefore from the stacks and resources as well.
--
-- -   If you specify an empty value, AWS CloudFormation removes all
--     currently associated tags.
--
-- If you specify new tags as part of an @UpdateStackSet@ action, AWS
-- CloudFormation checks to see if you have the required IAM permission to
-- tag resources. If you omit tags that are currently associated with the
-- stack set from the list of tags you specify, AWS CloudFormation assumes
-- that you want to remove those tags from the stack set, and checks to see
-- if you have permission to untag resources. If you don\'t have the
-- necessary permission(s), the entire @UpdateStackSet@ action fails with
-- an @access denied@ error, and the stack set is not updated.
--
-- 'autoDeployment', 'updateStackSet_autoDeployment' - [Service-managed permissions] Describes whether StackSets automatically
-- deploys to AWS Organizations accounts that are added to a target
-- organization or organizational unit (OU).
--
-- If you specify @AutoDeployment@, do not specify @DeploymentTargets@ or
-- @Regions@.
--
-- 'description', 'updateStackSet_description' - A brief description of updates that you are making.
--
-- 'regions', 'updateStackSet_regions' - The Regions in which to update associated stack instances. If you
-- specify Regions, you must also specify accounts in which to update stack
-- set instances.
--
-- To update /all/ the stack instances associated with this stack set, do
-- not specify the @Accounts@ or @Regions@ properties.
--
-- If the stack set update includes changes to the template (that is, if
-- the @TemplateBody@ or @TemplateURL@ properties are specified), or the
-- @Parameters@ property, AWS CloudFormation marks all stack instances with
-- a status of @OUTDATED@ prior to updating the stack instances in the
-- specified accounts and Regions. If the stack set update does not include
-- changes to the template or parameters, AWS CloudFormation updates the
-- stack instances in the specified accounts and Regions, while leaving all
-- other stack instances with their existing stack instance status.
--
-- 'templateBody', 'updateStackSet_templateBody' - The structure that contains the template body, with a minimum length of
-- 1 byte and a maximum length of 51,200 bytes. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters:
-- @TemplateBody@ or @TemplateURL@—or set @UsePreviousTemplate@ to true.
--
-- 'parameters', 'updateStackSet_parameters' - A list of input parameters for the stack set template.
--
-- 'usePreviousTemplate', 'updateStackSet_usePreviousTemplate' - Use the existing template that\'s associated with the stack set that
-- you\'re updating.
--
-- Conditional: You must specify only one of the following parameters:
-- @TemplateBody@ or @TemplateURL@—or set @UsePreviousTemplate@ to true.
--
-- 'stackSetName', 'updateStackSet_stackSetName' - The name or unique ID of the stack set that you want to update.
newUpdateStackSet ::
  -- | 'stackSetName'
  Core.Text ->
  UpdateStackSet
newUpdateStackSet pStackSetName_ =
  UpdateStackSet'
    { permissionModel = Core.Nothing,
      executionRoleName = Core.Nothing,
      capabilities = Core.Nothing,
      templateURL = Core.Nothing,
      deploymentTargets = Core.Nothing,
      operationId = Core.Nothing,
      callAs = Core.Nothing,
      operationPreferences = Core.Nothing,
      accounts = Core.Nothing,
      administrationRoleARN = Core.Nothing,
      tags = Core.Nothing,
      autoDeployment = Core.Nothing,
      description = Core.Nothing,
      regions = Core.Nothing,
      templateBody = Core.Nothing,
      parameters = Core.Nothing,
      usePreviousTemplate = Core.Nothing,
      stackSetName = pStackSetName_
    }

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
--     the IAM roles required to deploy to accounts managed by AWS
--     Organizations. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions>.
updateStackSet_permissionModel :: Lens.Lens' UpdateStackSet (Core.Maybe PermissionModels)
updateStackSet_permissionModel = Lens.lens (\UpdateStackSet' {permissionModel} -> permissionModel) (\s@UpdateStackSet' {} a -> s {permissionModel = a} :: UpdateStackSet)

-- | The name of the IAM execution role to use to update the stack set. If
-- you do not specify an execution role, AWS CloudFormation uses the
-- @AWSCloudFormationStackSetExecutionRole@ role for the stack set
-- operation.
--
-- Specify an IAM role only if you are using customized execution roles to
-- control which stack resources users and groups can include in their
-- stack sets.
--
-- If you specify a customized execution role, AWS CloudFormation uses that
-- role to update the stack. If you do not specify a customized execution
-- role, AWS CloudFormation performs the update using the role previously
-- associated with the stack set, so long as you have permissions to
-- perform operations on the stack set.
updateStackSet_executionRoleName :: Lens.Lens' UpdateStackSet (Core.Maybe Core.Text)
updateStackSet_executionRoleName = Lens.lens (\UpdateStackSet' {executionRoleName} -> executionRoleName) (\s@UpdateStackSet' {} a -> s {executionRoleName = a} :: UpdateStackSet)

-- | In some cases, you must explicitly acknowledge that your stack template
-- contains certain capabilities in order for AWS CloudFormation to update
-- the stack set and its associated stack instances.
--
-- -   @CAPABILITY_IAM@ and @CAPABILITY_NAMED_IAM@
--
--     Some stack templates might include resources that can affect
--     permissions in your AWS account; for example, by creating new AWS
--     Identity and Access Management (IAM) users. For those stacks sets,
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
--     or more macros, and you choose to update a stack directly from the
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
updateStackSet_capabilities :: Lens.Lens' UpdateStackSet (Core.Maybe [Capability])
updateStackSet_capabilities = Lens.lens (\UpdateStackSet' {capabilities} -> capabilities) (\s@UpdateStackSet' {} a -> s {capabilities = a} :: UpdateStackSet) Core.. Lens.mapping Lens._Coerce

-- | The location of the file that contains the template body. The URL must
-- point to a template (maximum size: 460,800 bytes) that is located in an
-- Amazon S3 bucket or a Systems Manager document. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters:
-- @TemplateBody@ or @TemplateURL@—or set @UsePreviousTemplate@ to true.
updateStackSet_templateURL :: Lens.Lens' UpdateStackSet (Core.Maybe Core.Text)
updateStackSet_templateURL = Lens.lens (\UpdateStackSet' {templateURL} -> templateURL) (\s@UpdateStackSet' {} a -> s {templateURL = a} :: UpdateStackSet)

-- | [Service-managed permissions] The AWS Organizations accounts in which to
-- update associated stack instances.
--
-- To update all the stack instances associated with this stack set, do not
-- specify @DeploymentTargets@ or @Regions@.
--
-- If the stack set update includes changes to the template (that is, if
-- @TemplateBody@ or @TemplateURL@ is specified), or the @Parameters@, AWS
-- CloudFormation marks all stack instances with a status of @OUTDATED@
-- prior to updating the stack instances in the specified accounts and
-- Regions. If the stack set update does not include changes to the
-- template or parameters, AWS CloudFormation updates the stack instances
-- in the specified accounts and Regions, while leaving all other stack
-- instances with their existing stack instance status.
updateStackSet_deploymentTargets :: Lens.Lens' UpdateStackSet (Core.Maybe DeploymentTargets)
updateStackSet_deploymentTargets = Lens.lens (\UpdateStackSet' {deploymentTargets} -> deploymentTargets) (\s@UpdateStackSet' {} a -> s {deploymentTargets = a} :: UpdateStackSet)

-- | The unique ID for this stack set operation.
--
-- The operation ID also functions as an idempotency token, to ensure that
-- AWS CloudFormation performs the stack set operation only once, even if
-- you retry the request multiple times. You might retry stack set
-- operation requests to ensure that AWS CloudFormation successfully
-- received them.
--
-- If you don\'t specify an operation ID, AWS CloudFormation generates one
-- automatically.
--
-- Repeating this stack set operation with a new operation ID retries all
-- stack instances whose status is @OUTDATED@.
updateStackSet_operationId :: Lens.Lens' UpdateStackSet (Core.Maybe Core.Text)
updateStackSet_operationId = Lens.lens (\UpdateStackSet' {operationId} -> operationId) (\s@UpdateStackSet' {} a -> s {operationId = a} :: UpdateStackSet)

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
--     Your AWS account must be registered as a delegated administrator in
--     the management account. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
--     in the /AWS CloudFormation User Guide/.
updateStackSet_callAs :: Lens.Lens' UpdateStackSet (Core.Maybe CallAs)
updateStackSet_callAs = Lens.lens (\UpdateStackSet' {callAs} -> callAs) (\s@UpdateStackSet' {} a -> s {callAs = a} :: UpdateStackSet)

-- | Preferences for how AWS CloudFormation performs this stack set
-- operation.
updateStackSet_operationPreferences :: Lens.Lens' UpdateStackSet (Core.Maybe StackSetOperationPreferences)
updateStackSet_operationPreferences = Lens.lens (\UpdateStackSet' {operationPreferences} -> operationPreferences) (\s@UpdateStackSet' {} a -> s {operationPreferences = a} :: UpdateStackSet)

-- | [Self-managed permissions] The accounts in which to update associated
-- stack instances. If you specify accounts, you must also specify the
-- Regions in which to update stack set instances.
--
-- To update /all/ the stack instances associated with this stack set, do
-- not specify the @Accounts@ or @Regions@ properties.
--
-- If the stack set update includes changes to the template (that is, if
-- the @TemplateBody@ or @TemplateURL@ properties are specified), or the
-- @Parameters@ property, AWS CloudFormation marks all stack instances with
-- a status of @OUTDATED@ prior to updating the stack instances in the
-- specified accounts and Regions. If the stack set update does not include
-- changes to the template or parameters, AWS CloudFormation updates the
-- stack instances in the specified accounts and Regions, while leaving all
-- other stack instances with their existing stack instance status.
updateStackSet_accounts :: Lens.Lens' UpdateStackSet (Core.Maybe [Core.Text])
updateStackSet_accounts = Lens.lens (\UpdateStackSet' {accounts} -> accounts) (\s@UpdateStackSet' {} a -> s {accounts = a} :: UpdateStackSet) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Number (ARN) of the IAM role to use to update this
-- stack set.
--
-- Specify an IAM role only if you are using customized administrator roles
-- to control which users or groups can manage specific stack sets within
-- the same administrator account. For more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Granting Permissions for Stack Set Operations>
-- in the /AWS CloudFormation User Guide/.
--
-- If you specified a customized administrator role when you created the
-- stack set, you must specify a customized administrator role, even if it
-- is the same customized administrator role used with this stack set
-- previously.
updateStackSet_administrationRoleARN :: Lens.Lens' UpdateStackSet (Core.Maybe Core.Text)
updateStackSet_administrationRoleARN = Lens.lens (\UpdateStackSet' {administrationRoleARN} -> administrationRoleARN) (\s@UpdateStackSet' {} a -> s {administrationRoleARN = a} :: UpdateStackSet)

-- | The key-value pairs to associate with this stack set and the stacks
-- created from it. AWS CloudFormation also propagates these tags to
-- supported resources that are created in the stacks. You can specify a
-- maximum number of 50 tags.
--
-- If you specify tags for this parameter, those tags replace any list of
-- tags that are currently associated with this stack set. This means:
--
-- -   If you don\'t specify this parameter, AWS CloudFormation doesn\'t
--     modify the stack\'s tags.
--
-- -   If you specify /any/ tags using this parameter, you must specify
--     /all/ the tags that you want associated with this stack set, even
--     tags you\'ve specifed before (for example, when creating the stack
--     set or during a previous update of the stack set.). Any tags that
--     you don\'t include in the updated list of tags are removed from the
--     stack set, and therefore from the stacks and resources as well.
--
-- -   If you specify an empty value, AWS CloudFormation removes all
--     currently associated tags.
--
-- If you specify new tags as part of an @UpdateStackSet@ action, AWS
-- CloudFormation checks to see if you have the required IAM permission to
-- tag resources. If you omit tags that are currently associated with the
-- stack set from the list of tags you specify, AWS CloudFormation assumes
-- that you want to remove those tags from the stack set, and checks to see
-- if you have permission to untag resources. If you don\'t have the
-- necessary permission(s), the entire @UpdateStackSet@ action fails with
-- an @access denied@ error, and the stack set is not updated.
updateStackSet_tags :: Lens.Lens' UpdateStackSet (Core.Maybe [Tag])
updateStackSet_tags = Lens.lens (\UpdateStackSet' {tags} -> tags) (\s@UpdateStackSet' {} a -> s {tags = a} :: UpdateStackSet) Core.. Lens.mapping Lens._Coerce

-- | [Service-managed permissions] Describes whether StackSets automatically
-- deploys to AWS Organizations accounts that are added to a target
-- organization or organizational unit (OU).
--
-- If you specify @AutoDeployment@, do not specify @DeploymentTargets@ or
-- @Regions@.
updateStackSet_autoDeployment :: Lens.Lens' UpdateStackSet (Core.Maybe AutoDeployment)
updateStackSet_autoDeployment = Lens.lens (\UpdateStackSet' {autoDeployment} -> autoDeployment) (\s@UpdateStackSet' {} a -> s {autoDeployment = a} :: UpdateStackSet)

-- | A brief description of updates that you are making.
updateStackSet_description :: Lens.Lens' UpdateStackSet (Core.Maybe Core.Text)
updateStackSet_description = Lens.lens (\UpdateStackSet' {description} -> description) (\s@UpdateStackSet' {} a -> s {description = a} :: UpdateStackSet)

-- | The Regions in which to update associated stack instances. If you
-- specify Regions, you must also specify accounts in which to update stack
-- set instances.
--
-- To update /all/ the stack instances associated with this stack set, do
-- not specify the @Accounts@ or @Regions@ properties.
--
-- If the stack set update includes changes to the template (that is, if
-- the @TemplateBody@ or @TemplateURL@ properties are specified), or the
-- @Parameters@ property, AWS CloudFormation marks all stack instances with
-- a status of @OUTDATED@ prior to updating the stack instances in the
-- specified accounts and Regions. If the stack set update does not include
-- changes to the template or parameters, AWS CloudFormation updates the
-- stack instances in the specified accounts and Regions, while leaving all
-- other stack instances with their existing stack instance status.
updateStackSet_regions :: Lens.Lens' UpdateStackSet (Core.Maybe [Core.Text])
updateStackSet_regions = Lens.lens (\UpdateStackSet' {regions} -> regions) (\s@UpdateStackSet' {} a -> s {regions = a} :: UpdateStackSet) Core.. Lens.mapping Lens._Coerce

-- | The structure that contains the template body, with a minimum length of
-- 1 byte and a maximum length of 51,200 bytes. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters:
-- @TemplateBody@ or @TemplateURL@—or set @UsePreviousTemplate@ to true.
updateStackSet_templateBody :: Lens.Lens' UpdateStackSet (Core.Maybe Core.Text)
updateStackSet_templateBody = Lens.lens (\UpdateStackSet' {templateBody} -> templateBody) (\s@UpdateStackSet' {} a -> s {templateBody = a} :: UpdateStackSet)

-- | A list of input parameters for the stack set template.
updateStackSet_parameters :: Lens.Lens' UpdateStackSet (Core.Maybe [Parameter])
updateStackSet_parameters = Lens.lens (\UpdateStackSet' {parameters} -> parameters) (\s@UpdateStackSet' {} a -> s {parameters = a} :: UpdateStackSet) Core.. Lens.mapping Lens._Coerce

-- | Use the existing template that\'s associated with the stack set that
-- you\'re updating.
--
-- Conditional: You must specify only one of the following parameters:
-- @TemplateBody@ or @TemplateURL@—or set @UsePreviousTemplate@ to true.
updateStackSet_usePreviousTemplate :: Lens.Lens' UpdateStackSet (Core.Maybe Core.Bool)
updateStackSet_usePreviousTemplate = Lens.lens (\UpdateStackSet' {usePreviousTemplate} -> usePreviousTemplate) (\s@UpdateStackSet' {} a -> s {usePreviousTemplate = a} :: UpdateStackSet)

-- | The name or unique ID of the stack set that you want to update.
updateStackSet_stackSetName :: Lens.Lens' UpdateStackSet Core.Text
updateStackSet_stackSetName = Lens.lens (\UpdateStackSet' {stackSetName} -> stackSetName) (\s@UpdateStackSet' {} a -> s {stackSetName = a} :: UpdateStackSet)

instance Core.AWSRequest UpdateStackSet where
  type
    AWSResponse UpdateStackSet =
      UpdateStackSetResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "UpdateStackSetResult"
      ( \s h x ->
          UpdateStackSetResponse'
            Core.<$> (x Core..@? "OperationId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateStackSet

instance Core.NFData UpdateStackSet

instance Core.ToHeaders UpdateStackSet where
  toHeaders = Core.const Core.mempty

instance Core.ToPath UpdateStackSet where
  toPath = Core.const "/"

instance Core.ToQuery UpdateStackSet where
  toQuery UpdateStackSet' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("UpdateStackSet" :: Core.ByteString),
        "Version" Core.=: ("2010-05-15" :: Core.ByteString),
        "PermissionModel" Core.=: permissionModel,
        "ExecutionRoleName" Core.=: executionRoleName,
        "Capabilities"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> capabilities),
        "TemplateURL" Core.=: templateURL,
        "DeploymentTargets" Core.=: deploymentTargets,
        "OperationId" Core.=: operationId,
        "CallAs" Core.=: callAs,
        "OperationPreferences" Core.=: operationPreferences,
        "Accounts"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> accounts),
        "AdministrationRoleARN"
          Core.=: administrationRoleARN,
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> tags),
        "AutoDeployment" Core.=: autoDeployment,
        "Description" Core.=: description,
        "Regions"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> regions),
        "TemplateBody" Core.=: templateBody,
        "Parameters"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> parameters),
        "UsePreviousTemplate" Core.=: usePreviousTemplate,
        "StackSetName" Core.=: stackSetName
      ]

-- | /See:/ 'newUpdateStackSetResponse' smart constructor.
data UpdateStackSetResponse = UpdateStackSetResponse'
  { -- | The unique ID for this stack set operation.
    operationId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateStackSetResponse
newUpdateStackSetResponse pHttpStatus_ =
  UpdateStackSetResponse'
    { operationId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique ID for this stack set operation.
updateStackSetResponse_operationId :: Lens.Lens' UpdateStackSetResponse (Core.Maybe Core.Text)
updateStackSetResponse_operationId = Lens.lens (\UpdateStackSetResponse' {operationId} -> operationId) (\s@UpdateStackSetResponse' {} a -> s {operationId = a} :: UpdateStackSetResponse)

-- | The response's http status code.
updateStackSetResponse_httpStatus :: Lens.Lens' UpdateStackSetResponse Core.Int
updateStackSetResponse_httpStatus = Lens.lens (\UpdateStackSetResponse' {httpStatus} -> httpStatus) (\s@UpdateStackSetResponse' {} a -> s {httpStatus = a} :: UpdateStackSetResponse)

instance Core.NFData UpdateStackSetResponse
