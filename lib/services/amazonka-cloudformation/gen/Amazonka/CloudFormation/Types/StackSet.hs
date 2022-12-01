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
-- Module      : Amazonka.CloudFormation.Types.StackSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.StackSet where

import Amazonka.CloudFormation.Types.AutoDeployment
import Amazonka.CloudFormation.Types.Capability
import Amazonka.CloudFormation.Types.ManagedExecution
import Amazonka.CloudFormation.Types.Parameter
import Amazonka.CloudFormation.Types.PermissionModels
import Amazonka.CloudFormation.Types.StackSetDriftDetectionDetails
import Amazonka.CloudFormation.Types.StackSetStatus
import Amazonka.CloudFormation.Types.Tag
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains information about a stack set. A stack set
-- enables you to provision stacks into Amazon Web Services accounts and
-- across Regions by using a single CloudFormation template. In the stack
-- set, you specify the template to use, in addition to any parameters and
-- capabilities that the template requires.
--
-- /See:/ 'newStackSet' smart constructor.
data StackSet = StackSet'
  { -- | A list of tags that specify information about the stack set. A maximum
    -- number of 50 tags can be specified.
    tags :: Prelude.Maybe [Tag],
    -- | The name that\'s associated with the stack set.
    stackSetName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the stack set.
    stackSetARN :: Prelude.Maybe Prelude.Text,
    -- | The ID of the stack set.
    stackSetId :: Prelude.Maybe Prelude.Text,
    -- | The structure that contains the body of the template that was used to
    -- create or update the stack set.
    templateBody :: Prelude.Maybe Prelude.Text,
    -- | The status of the stack set.
    status :: Prelude.Maybe StackSetStatus,
    -- | A description of the stack set that you specify when the stack set is
    -- created or updated.
    description :: Prelude.Maybe Prelude.Text,
    -- | [Service-managed permissions] Describes whether StackSets automatically
    -- deploys to Organizations accounts that are added to a target
    -- organization or organizational unit (OU).
    autoDeployment :: Prelude.Maybe AutoDeployment,
    -- | [Service-managed permissions] The organization root ID or organizational
    -- unit (OU) IDs that you specified for
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets>.
    organizationalUnitIds :: Prelude.Maybe [Prelude.Text],
    -- | The capabilities that are allowed in the stack set. Some stack set
    -- templates might include resources that can affect permissions in your
    -- Amazon Web Services account—for example, by creating new Identity and
    -- Access Management (IAM) users. For more information, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in CloudFormation Templates.>
    capabilities :: Prelude.Maybe [Capability],
    -- | Describes whether StackSets performs non-conflicting operations
    -- concurrently and queues conflicting operations.
    managedExecution :: Prelude.Maybe ManagedExecution,
    -- | The name of the IAM execution role used to create or update the stack
    -- set.
    --
    -- Use customized execution roles to control which stack resources users
    -- and groups can include in their stack sets.
    executionRoleName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role used to create or update
    -- the stack set.
    --
    -- Use customized administrator roles to control which users or groups can
    -- manage specific stack sets within the same administrator account. For
    -- more information, see
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Prerequisites: Granting Permissions for Stack Set Operations>
    -- in the /CloudFormation User Guide/.
    administrationRoleARN :: Prelude.Maybe Prelude.Text,
    -- | Describes how the IAM roles required for stack set operations are
    -- created.
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
    -- | Detailed information about the drift status of the stack set.
    --
    -- For stack sets, contains information about the last /completed/ drift
    -- operation performed on the stack set. Information about drift operations
    -- currently in progress isn\'t included.
    stackSetDriftDetectionDetails :: Prelude.Maybe StackSetDriftDetectionDetails,
    -- | A list of input parameters for a stack set.
    parameters :: Prelude.Maybe [Parameter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StackSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'stackSet_tags' - A list of tags that specify information about the stack set. A maximum
-- number of 50 tags can be specified.
--
-- 'stackSetName', 'stackSet_stackSetName' - The name that\'s associated with the stack set.
--
-- 'stackSetARN', 'stackSet_stackSetARN' - The Amazon Resource Name (ARN) of the stack set.
--
-- 'stackSetId', 'stackSet_stackSetId' - The ID of the stack set.
--
-- 'templateBody', 'stackSet_templateBody' - The structure that contains the body of the template that was used to
-- create or update the stack set.
--
-- 'status', 'stackSet_status' - The status of the stack set.
--
-- 'description', 'stackSet_description' - A description of the stack set that you specify when the stack set is
-- created or updated.
--
-- 'autoDeployment', 'stackSet_autoDeployment' - [Service-managed permissions] Describes whether StackSets automatically
-- deploys to Organizations accounts that are added to a target
-- organization or organizational unit (OU).
--
-- 'organizationalUnitIds', 'stackSet_organizationalUnitIds' - [Service-managed permissions] The organization root ID or organizational
-- unit (OU) IDs that you specified for
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets>.
--
-- 'capabilities', 'stackSet_capabilities' - The capabilities that are allowed in the stack set. Some stack set
-- templates might include resources that can affect permissions in your
-- Amazon Web Services account—for example, by creating new Identity and
-- Access Management (IAM) users. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in CloudFormation Templates.>
--
-- 'managedExecution', 'stackSet_managedExecution' - Describes whether StackSets performs non-conflicting operations
-- concurrently and queues conflicting operations.
--
-- 'executionRoleName', 'stackSet_executionRoleName' - The name of the IAM execution role used to create or update the stack
-- set.
--
-- Use customized execution roles to control which stack resources users
-- and groups can include in their stack sets.
--
-- 'administrationRoleARN', 'stackSet_administrationRoleARN' - The Amazon Resource Name (ARN) of the IAM role used to create or update
-- the stack set.
--
-- Use customized administrator roles to control which users or groups can
-- manage specific stack sets within the same administrator account. For
-- more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Prerequisites: Granting Permissions for Stack Set Operations>
-- in the /CloudFormation User Guide/.
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
--     the IAM roles required to deploy to accounts managed by
--     Organizations. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions>.
--
-- 'stackSetDriftDetectionDetails', 'stackSet_stackSetDriftDetectionDetails' - Detailed information about the drift status of the stack set.
--
-- For stack sets, contains information about the last /completed/ drift
-- operation performed on the stack set. Information about drift operations
-- currently in progress isn\'t included.
--
-- 'parameters', 'stackSet_parameters' - A list of input parameters for a stack set.
newStackSet ::
  StackSet
newStackSet =
  StackSet'
    { tags = Prelude.Nothing,
      stackSetName = Prelude.Nothing,
      stackSetARN = Prelude.Nothing,
      stackSetId = Prelude.Nothing,
      templateBody = Prelude.Nothing,
      status = Prelude.Nothing,
      description = Prelude.Nothing,
      autoDeployment = Prelude.Nothing,
      organizationalUnitIds = Prelude.Nothing,
      capabilities = Prelude.Nothing,
      managedExecution = Prelude.Nothing,
      executionRoleName = Prelude.Nothing,
      administrationRoleARN = Prelude.Nothing,
      permissionModel = Prelude.Nothing,
      stackSetDriftDetectionDetails = Prelude.Nothing,
      parameters = Prelude.Nothing
    }

-- | A list of tags that specify information about the stack set. A maximum
-- number of 50 tags can be specified.
stackSet_tags :: Lens.Lens' StackSet (Prelude.Maybe [Tag])
stackSet_tags = Lens.lens (\StackSet' {tags} -> tags) (\s@StackSet' {} a -> s {tags = a} :: StackSet) Prelude.. Lens.mapping Lens.coerced

-- | The name that\'s associated with the stack set.
stackSet_stackSetName :: Lens.Lens' StackSet (Prelude.Maybe Prelude.Text)
stackSet_stackSetName = Lens.lens (\StackSet' {stackSetName} -> stackSetName) (\s@StackSet' {} a -> s {stackSetName = a} :: StackSet)

-- | The Amazon Resource Name (ARN) of the stack set.
stackSet_stackSetARN :: Lens.Lens' StackSet (Prelude.Maybe Prelude.Text)
stackSet_stackSetARN = Lens.lens (\StackSet' {stackSetARN} -> stackSetARN) (\s@StackSet' {} a -> s {stackSetARN = a} :: StackSet)

-- | The ID of the stack set.
stackSet_stackSetId :: Lens.Lens' StackSet (Prelude.Maybe Prelude.Text)
stackSet_stackSetId = Lens.lens (\StackSet' {stackSetId} -> stackSetId) (\s@StackSet' {} a -> s {stackSetId = a} :: StackSet)

-- | The structure that contains the body of the template that was used to
-- create or update the stack set.
stackSet_templateBody :: Lens.Lens' StackSet (Prelude.Maybe Prelude.Text)
stackSet_templateBody = Lens.lens (\StackSet' {templateBody} -> templateBody) (\s@StackSet' {} a -> s {templateBody = a} :: StackSet)

-- | The status of the stack set.
stackSet_status :: Lens.Lens' StackSet (Prelude.Maybe StackSetStatus)
stackSet_status = Lens.lens (\StackSet' {status} -> status) (\s@StackSet' {} a -> s {status = a} :: StackSet)

-- | A description of the stack set that you specify when the stack set is
-- created or updated.
stackSet_description :: Lens.Lens' StackSet (Prelude.Maybe Prelude.Text)
stackSet_description = Lens.lens (\StackSet' {description} -> description) (\s@StackSet' {} a -> s {description = a} :: StackSet)

-- | [Service-managed permissions] Describes whether StackSets automatically
-- deploys to Organizations accounts that are added to a target
-- organization or organizational unit (OU).
stackSet_autoDeployment :: Lens.Lens' StackSet (Prelude.Maybe AutoDeployment)
stackSet_autoDeployment = Lens.lens (\StackSet' {autoDeployment} -> autoDeployment) (\s@StackSet' {} a -> s {autoDeployment = a} :: StackSet)

-- | [Service-managed permissions] The organization root ID or organizational
-- unit (OU) IDs that you specified for
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets>.
stackSet_organizationalUnitIds :: Lens.Lens' StackSet (Prelude.Maybe [Prelude.Text])
stackSet_organizationalUnitIds = Lens.lens (\StackSet' {organizationalUnitIds} -> organizationalUnitIds) (\s@StackSet' {} a -> s {organizationalUnitIds = a} :: StackSet) Prelude.. Lens.mapping Lens.coerced

-- | The capabilities that are allowed in the stack set. Some stack set
-- templates might include resources that can affect permissions in your
-- Amazon Web Services account—for example, by creating new Identity and
-- Access Management (IAM) users. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in CloudFormation Templates.>
stackSet_capabilities :: Lens.Lens' StackSet (Prelude.Maybe [Capability])
stackSet_capabilities = Lens.lens (\StackSet' {capabilities} -> capabilities) (\s@StackSet' {} a -> s {capabilities = a} :: StackSet) Prelude.. Lens.mapping Lens.coerced

-- | Describes whether StackSets performs non-conflicting operations
-- concurrently and queues conflicting operations.
stackSet_managedExecution :: Lens.Lens' StackSet (Prelude.Maybe ManagedExecution)
stackSet_managedExecution = Lens.lens (\StackSet' {managedExecution} -> managedExecution) (\s@StackSet' {} a -> s {managedExecution = a} :: StackSet)

-- | The name of the IAM execution role used to create or update the stack
-- set.
--
-- Use customized execution roles to control which stack resources users
-- and groups can include in their stack sets.
stackSet_executionRoleName :: Lens.Lens' StackSet (Prelude.Maybe Prelude.Text)
stackSet_executionRoleName = Lens.lens (\StackSet' {executionRoleName} -> executionRoleName) (\s@StackSet' {} a -> s {executionRoleName = a} :: StackSet)

-- | The Amazon Resource Name (ARN) of the IAM role used to create or update
-- the stack set.
--
-- Use customized administrator roles to control which users or groups can
-- manage specific stack sets within the same administrator account. For
-- more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Prerequisites: Granting Permissions for Stack Set Operations>
-- in the /CloudFormation User Guide/.
stackSet_administrationRoleARN :: Lens.Lens' StackSet (Prelude.Maybe Prelude.Text)
stackSet_administrationRoleARN = Lens.lens (\StackSet' {administrationRoleARN} -> administrationRoleARN) (\s@StackSet' {} a -> s {administrationRoleARN = a} :: StackSet)

-- | Describes how the IAM roles required for stack set operations are
-- created.
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
stackSet_permissionModel :: Lens.Lens' StackSet (Prelude.Maybe PermissionModels)
stackSet_permissionModel = Lens.lens (\StackSet' {permissionModel} -> permissionModel) (\s@StackSet' {} a -> s {permissionModel = a} :: StackSet)

-- | Detailed information about the drift status of the stack set.
--
-- For stack sets, contains information about the last /completed/ drift
-- operation performed on the stack set. Information about drift operations
-- currently in progress isn\'t included.
stackSet_stackSetDriftDetectionDetails :: Lens.Lens' StackSet (Prelude.Maybe StackSetDriftDetectionDetails)
stackSet_stackSetDriftDetectionDetails = Lens.lens (\StackSet' {stackSetDriftDetectionDetails} -> stackSetDriftDetectionDetails) (\s@StackSet' {} a -> s {stackSetDriftDetectionDetails = a} :: StackSet)

-- | A list of input parameters for a stack set.
stackSet_parameters :: Lens.Lens' StackSet (Prelude.Maybe [Parameter])
stackSet_parameters = Lens.lens (\StackSet' {parameters} -> parameters) (\s@StackSet' {} a -> s {parameters = a} :: StackSet) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML StackSet where
  parseXML x =
    StackSet'
      Prelude.<$> ( x Core..@? "Tags" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "StackSetName")
      Prelude.<*> (x Core..@? "StackSetARN")
      Prelude.<*> (x Core..@? "StackSetId")
      Prelude.<*> (x Core..@? "TemplateBody")
      Prelude.<*> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "Description")
      Prelude.<*> (x Core..@? "AutoDeployment")
      Prelude.<*> ( x Core..@? "OrganizationalUnitIds"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> ( x Core..@? "Capabilities" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "ManagedExecution")
      Prelude.<*> (x Core..@? "ExecutionRoleName")
      Prelude.<*> (x Core..@? "AdministrationRoleARN")
      Prelude.<*> (x Core..@? "PermissionModel")
      Prelude.<*> (x Core..@? "StackSetDriftDetectionDetails")
      Prelude.<*> ( x Core..@? "Parameters" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )

instance Prelude.Hashable StackSet where
  hashWithSalt _salt StackSet' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` stackSetName
      `Prelude.hashWithSalt` stackSetARN
      `Prelude.hashWithSalt` stackSetId
      `Prelude.hashWithSalt` templateBody
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` autoDeployment
      `Prelude.hashWithSalt` organizationalUnitIds
      `Prelude.hashWithSalt` capabilities
      `Prelude.hashWithSalt` managedExecution
      `Prelude.hashWithSalt` executionRoleName
      `Prelude.hashWithSalt` administrationRoleARN
      `Prelude.hashWithSalt` permissionModel
      `Prelude.hashWithSalt` stackSetDriftDetectionDetails
      `Prelude.hashWithSalt` parameters

instance Prelude.NFData StackSet where
  rnf StackSet' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf stackSetName
      `Prelude.seq` Prelude.rnf stackSetARN
      `Prelude.seq` Prelude.rnf stackSetId
      `Prelude.seq` Prelude.rnf templateBody
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf autoDeployment
      `Prelude.seq` Prelude.rnf organizationalUnitIds
      `Prelude.seq` Prelude.rnf capabilities
      `Prelude.seq` Prelude.rnf managedExecution
      `Prelude.seq` Prelude.rnf executionRoleName
      `Prelude.seq` Prelude.rnf administrationRoleARN
      `Prelude.seq` Prelude.rnf permissionModel
      `Prelude.seq` Prelude.rnf
        stackSetDriftDetectionDetails
      `Prelude.seq` Prelude.rnf parameters
