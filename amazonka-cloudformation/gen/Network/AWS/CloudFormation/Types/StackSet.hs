{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSet where

import Network.AWS.CloudFormation.Types.AutoDeployment
import Network.AWS.CloudFormation.Types.Capability
import Network.AWS.CloudFormation.Types.Parameter
import Network.AWS.CloudFormation.Types.PermissionModels
import Network.AWS.CloudFormation.Types.StackSetDriftDetectionDetails
import Network.AWS.CloudFormation.Types.StackSetStatus
import Network.AWS.CloudFormation.Types.Tag
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A structure that contains information about a stack set. A stack set
-- enables you to provision stacks into AWS accounts and across Regions by
-- using a single CloudFormation template. In the stack set, you specify
-- the template to use, as well as any parameters and capabilities that the
-- template requires.
--
-- /See:/ 'newStackSet' smart constructor.
data StackSet = StackSet'
  { -- | The status of the stack set.
    status :: Core.Maybe StackSetStatus,
    -- | Describes how the IAM roles required for stack set operations are
    -- created.
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
    -- | The name of the IAM execution role used to create or update the stack
    -- set.
    --
    -- Use customized execution roles to control which stack resources users
    -- and groups can include in their stack sets.
    executionRoleName :: Core.Maybe Core.Text,
    -- | The capabilities that are allowed in the stack set. Some stack set
    -- templates might include resources that can affect permissions in your
    -- AWS account—for example, by creating new AWS Identity and Access
    -- Management (IAM) users. For more information, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates.>
    capabilities :: Core.Maybe [Capability],
    -- | [Service-managed permissions] The organization root ID or organizational
    -- unit (OU) IDs that you specified for
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets>.
    organizationalUnitIds :: Core.Maybe [Core.Text],
    -- | The Amazon Resource Number (ARN) of the IAM role used to create or
    -- update the stack set.
    --
    -- Use customized administrator roles to control which users or groups can
    -- manage specific stack sets within the same administrator account. For
    -- more information, see
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Prerequisites: Granting Permissions for Stack Set Operations>
    -- in the /AWS CloudFormation User Guide/.
    administrationRoleARN :: Core.Maybe Core.Text,
    -- | Detailed information about the drift status of the stack set.
    --
    -- For stack sets, contains information about the last /completed/ drift
    -- operation performed on the stack set. Information about drift operations
    -- currently in progress is not included.
    stackSetDriftDetectionDetails :: Core.Maybe StackSetDriftDetectionDetails,
    -- | The ID of the stack set.
    stackSetId :: Core.Maybe Core.Text,
    -- | A list of tags that specify information about the stack set. A maximum
    -- number of 50 tags can be specified.
    tags :: Core.Maybe [Tag],
    -- | The Amazon Resource Number (ARN) of the stack set.
    stackSetARN :: Core.Maybe Core.Text,
    -- | [Service-managed permissions] Describes whether StackSets automatically
    -- deploys to AWS Organizations accounts that are added to a target
    -- organization or organizational unit (OU).
    autoDeployment :: Core.Maybe AutoDeployment,
    -- | A description of the stack set that you specify when the stack set is
    -- created or updated.
    description :: Core.Maybe Core.Text,
    -- | The name that\'s associated with the stack set.
    stackSetName :: Core.Maybe Core.Text,
    -- | The structure that contains the body of the template that was used to
    -- create or update the stack set.
    templateBody :: Core.Maybe Core.Text,
    -- | A list of input parameters for a stack set.
    parameters :: Core.Maybe [Parameter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StackSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'stackSet_status' - The status of the stack set.
--
-- 'permissionModel', 'stackSet_permissionModel' - Describes how the IAM roles required for stack set operations are
-- created.
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
-- 'executionRoleName', 'stackSet_executionRoleName' - The name of the IAM execution role used to create or update the stack
-- set.
--
-- Use customized execution roles to control which stack resources users
-- and groups can include in their stack sets.
--
-- 'capabilities', 'stackSet_capabilities' - The capabilities that are allowed in the stack set. Some stack set
-- templates might include resources that can affect permissions in your
-- AWS account—for example, by creating new AWS Identity and Access
-- Management (IAM) users. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates.>
--
-- 'organizationalUnitIds', 'stackSet_organizationalUnitIds' - [Service-managed permissions] The organization root ID or organizational
-- unit (OU) IDs that you specified for
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets>.
--
-- 'administrationRoleARN', 'stackSet_administrationRoleARN' - The Amazon Resource Number (ARN) of the IAM role used to create or
-- update the stack set.
--
-- Use customized administrator roles to control which users or groups can
-- manage specific stack sets within the same administrator account. For
-- more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Prerequisites: Granting Permissions for Stack Set Operations>
-- in the /AWS CloudFormation User Guide/.
--
-- 'stackSetDriftDetectionDetails', 'stackSet_stackSetDriftDetectionDetails' - Detailed information about the drift status of the stack set.
--
-- For stack sets, contains information about the last /completed/ drift
-- operation performed on the stack set. Information about drift operations
-- currently in progress is not included.
--
-- 'stackSetId', 'stackSet_stackSetId' - The ID of the stack set.
--
-- 'tags', 'stackSet_tags' - A list of tags that specify information about the stack set. A maximum
-- number of 50 tags can be specified.
--
-- 'stackSetARN', 'stackSet_stackSetARN' - The Amazon Resource Number (ARN) of the stack set.
--
-- 'autoDeployment', 'stackSet_autoDeployment' - [Service-managed permissions] Describes whether StackSets automatically
-- deploys to AWS Organizations accounts that are added to a target
-- organization or organizational unit (OU).
--
-- 'description', 'stackSet_description' - A description of the stack set that you specify when the stack set is
-- created or updated.
--
-- 'stackSetName', 'stackSet_stackSetName' - The name that\'s associated with the stack set.
--
-- 'templateBody', 'stackSet_templateBody' - The structure that contains the body of the template that was used to
-- create or update the stack set.
--
-- 'parameters', 'stackSet_parameters' - A list of input parameters for a stack set.
newStackSet ::
  StackSet
newStackSet =
  StackSet'
    { status = Core.Nothing,
      permissionModel = Core.Nothing,
      executionRoleName = Core.Nothing,
      capabilities = Core.Nothing,
      organizationalUnitIds = Core.Nothing,
      administrationRoleARN = Core.Nothing,
      stackSetDriftDetectionDetails = Core.Nothing,
      stackSetId = Core.Nothing,
      tags = Core.Nothing,
      stackSetARN = Core.Nothing,
      autoDeployment = Core.Nothing,
      description = Core.Nothing,
      stackSetName = Core.Nothing,
      templateBody = Core.Nothing,
      parameters = Core.Nothing
    }

-- | The status of the stack set.
stackSet_status :: Lens.Lens' StackSet (Core.Maybe StackSetStatus)
stackSet_status = Lens.lens (\StackSet' {status} -> status) (\s@StackSet' {} a -> s {status = a} :: StackSet)

-- | Describes how the IAM roles required for stack set operations are
-- created.
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
stackSet_permissionModel :: Lens.Lens' StackSet (Core.Maybe PermissionModels)
stackSet_permissionModel = Lens.lens (\StackSet' {permissionModel} -> permissionModel) (\s@StackSet' {} a -> s {permissionModel = a} :: StackSet)

-- | The name of the IAM execution role used to create or update the stack
-- set.
--
-- Use customized execution roles to control which stack resources users
-- and groups can include in their stack sets.
stackSet_executionRoleName :: Lens.Lens' StackSet (Core.Maybe Core.Text)
stackSet_executionRoleName = Lens.lens (\StackSet' {executionRoleName} -> executionRoleName) (\s@StackSet' {} a -> s {executionRoleName = a} :: StackSet)

-- | The capabilities that are allowed in the stack set. Some stack set
-- templates might include resources that can affect permissions in your
-- AWS account—for example, by creating new AWS Identity and Access
-- Management (IAM) users. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates.>
stackSet_capabilities :: Lens.Lens' StackSet (Core.Maybe [Capability])
stackSet_capabilities = Lens.lens (\StackSet' {capabilities} -> capabilities) (\s@StackSet' {} a -> s {capabilities = a} :: StackSet) Core.. Lens.mapping Lens._Coerce

-- | [Service-managed permissions] The organization root ID or organizational
-- unit (OU) IDs that you specified for
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets>.
stackSet_organizationalUnitIds :: Lens.Lens' StackSet (Core.Maybe [Core.Text])
stackSet_organizationalUnitIds = Lens.lens (\StackSet' {organizationalUnitIds} -> organizationalUnitIds) (\s@StackSet' {} a -> s {organizationalUnitIds = a} :: StackSet) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Number (ARN) of the IAM role used to create or
-- update the stack set.
--
-- Use customized administrator roles to control which users or groups can
-- manage specific stack sets within the same administrator account. For
-- more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Prerequisites: Granting Permissions for Stack Set Operations>
-- in the /AWS CloudFormation User Guide/.
stackSet_administrationRoleARN :: Lens.Lens' StackSet (Core.Maybe Core.Text)
stackSet_administrationRoleARN = Lens.lens (\StackSet' {administrationRoleARN} -> administrationRoleARN) (\s@StackSet' {} a -> s {administrationRoleARN = a} :: StackSet)

-- | Detailed information about the drift status of the stack set.
--
-- For stack sets, contains information about the last /completed/ drift
-- operation performed on the stack set. Information about drift operations
-- currently in progress is not included.
stackSet_stackSetDriftDetectionDetails :: Lens.Lens' StackSet (Core.Maybe StackSetDriftDetectionDetails)
stackSet_stackSetDriftDetectionDetails = Lens.lens (\StackSet' {stackSetDriftDetectionDetails} -> stackSetDriftDetectionDetails) (\s@StackSet' {} a -> s {stackSetDriftDetectionDetails = a} :: StackSet)

-- | The ID of the stack set.
stackSet_stackSetId :: Lens.Lens' StackSet (Core.Maybe Core.Text)
stackSet_stackSetId = Lens.lens (\StackSet' {stackSetId} -> stackSetId) (\s@StackSet' {} a -> s {stackSetId = a} :: StackSet)

-- | A list of tags that specify information about the stack set. A maximum
-- number of 50 tags can be specified.
stackSet_tags :: Lens.Lens' StackSet (Core.Maybe [Tag])
stackSet_tags = Lens.lens (\StackSet' {tags} -> tags) (\s@StackSet' {} a -> s {tags = a} :: StackSet) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Number (ARN) of the stack set.
stackSet_stackSetARN :: Lens.Lens' StackSet (Core.Maybe Core.Text)
stackSet_stackSetARN = Lens.lens (\StackSet' {stackSetARN} -> stackSetARN) (\s@StackSet' {} a -> s {stackSetARN = a} :: StackSet)

-- | [Service-managed permissions] Describes whether StackSets automatically
-- deploys to AWS Organizations accounts that are added to a target
-- organization or organizational unit (OU).
stackSet_autoDeployment :: Lens.Lens' StackSet (Core.Maybe AutoDeployment)
stackSet_autoDeployment = Lens.lens (\StackSet' {autoDeployment} -> autoDeployment) (\s@StackSet' {} a -> s {autoDeployment = a} :: StackSet)

-- | A description of the stack set that you specify when the stack set is
-- created or updated.
stackSet_description :: Lens.Lens' StackSet (Core.Maybe Core.Text)
stackSet_description = Lens.lens (\StackSet' {description} -> description) (\s@StackSet' {} a -> s {description = a} :: StackSet)

-- | The name that\'s associated with the stack set.
stackSet_stackSetName :: Lens.Lens' StackSet (Core.Maybe Core.Text)
stackSet_stackSetName = Lens.lens (\StackSet' {stackSetName} -> stackSetName) (\s@StackSet' {} a -> s {stackSetName = a} :: StackSet)

-- | The structure that contains the body of the template that was used to
-- create or update the stack set.
stackSet_templateBody :: Lens.Lens' StackSet (Core.Maybe Core.Text)
stackSet_templateBody = Lens.lens (\StackSet' {templateBody} -> templateBody) (\s@StackSet' {} a -> s {templateBody = a} :: StackSet)

-- | A list of input parameters for a stack set.
stackSet_parameters :: Lens.Lens' StackSet (Core.Maybe [Parameter])
stackSet_parameters = Lens.lens (\StackSet' {parameters} -> parameters) (\s@StackSet' {} a -> s {parameters = a} :: StackSet) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML StackSet where
  parseXML x =
    StackSet'
      Core.<$> (x Core..@? "Status")
      Core.<*> (x Core..@? "PermissionModel")
      Core.<*> (x Core..@? "ExecutionRoleName")
      Core.<*> ( x Core..@? "Capabilities" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> ( x Core..@? "OrganizationalUnitIds"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "AdministrationRoleARN")
      Core.<*> (x Core..@? "StackSetDriftDetectionDetails")
      Core.<*> (x Core..@? "StackSetId")
      Core.<*> ( x Core..@? "Tags" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "StackSetARN")
      Core.<*> (x Core..@? "AutoDeployment")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "StackSetName")
      Core.<*> (x Core..@? "TemplateBody")
      Core.<*> ( x Core..@? "Parameters" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )

instance Core.Hashable StackSet

instance Core.NFData StackSet
