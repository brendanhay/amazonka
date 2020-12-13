{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSet
  ( StackSet (..),

    -- * Smart constructor
    mkStackSet,

    -- * Lenses
    ssStackSetDriftDetectionDetails,
    ssStatus,
    ssAdministrationRoleARN,
    ssAutoDeployment,
    ssOrganizationalUnitIds,
    ssStackSetARN,
    ssPermissionModel,
    ssParameters,
    ssTemplateBody,
    ssStackSetName,
    ssDescription,
    ssCapabilities,
    ssTags,
    ssStackSetId,
    ssExecutionRoleName,
  )
where

import Network.AWS.CloudFormation.Types.AutoDeployment
import Network.AWS.CloudFormation.Types.Capability
import Network.AWS.CloudFormation.Types.Parameter
import Network.AWS.CloudFormation.Types.PermissionModels
import Network.AWS.CloudFormation.Types.StackSetDriftDetectionDetails
import Network.AWS.CloudFormation.Types.StackSetStatus
import Network.AWS.CloudFormation.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A structure that contains information about a stack set. A stack set enables you to provision stacks into AWS accounts and across Regions by using a single CloudFormation template. In the stack set, you specify the template to use, as well as any parameters and capabilities that the template requires.
--
-- /See:/ 'mkStackSet' smart constructor.
data StackSet = StackSet'
  { -- | Detailed information about the drift status of the stack set.
    --
    -- For stack sets, contains information about the last /completed/ drift operation performed on the stack set. Information about drift operations currently in progress is not included.
    stackSetDriftDetectionDetails :: Lude.Maybe StackSetDriftDetectionDetails,
    -- | The status of the stack set.
    status :: Lude.Maybe StackSetStatus,
    -- | The Amazon Resource Number (ARN) of the IAM role used to create or update the stack set.
    --
    -- Use customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Prerequisites: Granting Permissions for Stack Set Operations> in the /AWS CloudFormation User Guide/ .
    administrationRoleARN :: Lude.Maybe Lude.Text,
    -- | [@Service-managed@ permissions] Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to a target organization or organizational unit (OU).
    autoDeployment :: Lude.Maybe AutoDeployment,
    -- | [@Service-managed@ permissions] The organization root ID or organizational unit (OU) IDs that you specified for <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets> .
    organizationalUnitIds :: Lude.Maybe [Lude.Text],
    -- | The Amazon Resource Number (ARN) of the stack set.
    stackSetARN :: Lude.Maybe Lude.Text,
    -- | Describes how the IAM roles required for stack set operations are created.
    --
    --
    --     * With @self-managed@ permissions, you must create the administrator and execution roles required to deploy to target accounts. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions> .
    --
    --
    --     * With @service-managed@ permissions, StackSets automatically creates the IAM roles required to deploy to accounts managed by AWS Organizations. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions> .
    permissionModel :: Lude.Maybe PermissionModels,
    -- | A list of input parameters for a stack set.
    parameters :: Lude.Maybe [Parameter],
    -- | The structure that contains the body of the template that was used to create or update the stack set.
    templateBody :: Lude.Maybe Lude.Text,
    -- | The name that's associated with the stack set.
    stackSetName :: Lude.Maybe Lude.Text,
    -- | A description of the stack set that you specify when the stack set is created or updated.
    description :: Lude.Maybe Lude.Text,
    -- | The capabilities that are allowed in the stack set. Some stack set templates might include resources that can affect permissions in your AWS account—for example, by creating new AWS Identity and Access Management (IAM) users. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates.>
    capabilities :: Lude.Maybe [Capability],
    -- | A list of tags that specify information about the stack set. A maximum number of 50 tags can be specified.
    tags :: Lude.Maybe [Tag],
    -- | The ID of the stack set.
    stackSetId :: Lude.Maybe Lude.Text,
    -- | The name of the IAM execution role used to create or update the stack set.
    --
    -- Use customized execution roles to control which stack resources users and groups can include in their stack sets.
    executionRoleName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StackSet' with the minimum fields required to make a request.
--
-- * 'stackSetDriftDetectionDetails' - Detailed information about the drift status of the stack set.
--
-- For stack sets, contains information about the last /completed/ drift operation performed on the stack set. Information about drift operations currently in progress is not included.
-- * 'status' - The status of the stack set.
-- * 'administrationRoleARN' - The Amazon Resource Number (ARN) of the IAM role used to create or update the stack set.
--
-- Use customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Prerequisites: Granting Permissions for Stack Set Operations> in the /AWS CloudFormation User Guide/ .
-- * 'autoDeployment' - [@Service-managed@ permissions] Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to a target organization or organizational unit (OU).
-- * 'organizationalUnitIds' - [@Service-managed@ permissions] The organization root ID or organizational unit (OU) IDs that you specified for <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets> .
-- * 'stackSetARN' - The Amazon Resource Number (ARN) of the stack set.
-- * 'permissionModel' - Describes how the IAM roles required for stack set operations are created.
--
--
--     * With @self-managed@ permissions, you must create the administrator and execution roles required to deploy to target accounts. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions> .
--
--
--     * With @service-managed@ permissions, StackSets automatically creates the IAM roles required to deploy to accounts managed by AWS Organizations. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions> .
--
--
-- * 'parameters' - A list of input parameters for a stack set.
-- * 'templateBody' - The structure that contains the body of the template that was used to create or update the stack set.
-- * 'stackSetName' - The name that's associated with the stack set.
-- * 'description' - A description of the stack set that you specify when the stack set is created or updated.
-- * 'capabilities' - The capabilities that are allowed in the stack set. Some stack set templates might include resources that can affect permissions in your AWS account—for example, by creating new AWS Identity and Access Management (IAM) users. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates.>
-- * 'tags' - A list of tags that specify information about the stack set. A maximum number of 50 tags can be specified.
-- * 'stackSetId' - The ID of the stack set.
-- * 'executionRoleName' - The name of the IAM execution role used to create or update the stack set.
--
-- Use customized execution roles to control which stack resources users and groups can include in their stack sets.
mkStackSet ::
  StackSet
mkStackSet =
  StackSet'
    { stackSetDriftDetectionDetails = Lude.Nothing,
      status = Lude.Nothing,
      administrationRoleARN = Lude.Nothing,
      autoDeployment = Lude.Nothing,
      organizationalUnitIds = Lude.Nothing,
      stackSetARN = Lude.Nothing,
      permissionModel = Lude.Nothing,
      parameters = Lude.Nothing,
      templateBody = Lude.Nothing,
      stackSetName = Lude.Nothing,
      description = Lude.Nothing,
      capabilities = Lude.Nothing,
      tags = Lude.Nothing,
      stackSetId = Lude.Nothing,
      executionRoleName = Lude.Nothing
    }

-- | Detailed information about the drift status of the stack set.
--
-- For stack sets, contains information about the last /completed/ drift operation performed on the stack set. Information about drift operations currently in progress is not included.
--
-- /Note:/ Consider using 'stackSetDriftDetectionDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStackSetDriftDetectionDetails :: Lens.Lens' StackSet (Lude.Maybe StackSetDriftDetectionDetails)
ssStackSetDriftDetectionDetails = Lens.lens (stackSetDriftDetectionDetails :: StackSet -> Lude.Maybe StackSetDriftDetectionDetails) (\s a -> s {stackSetDriftDetectionDetails = a} :: StackSet)
{-# DEPRECATED ssStackSetDriftDetectionDetails "Use generic-lens or generic-optics with 'stackSetDriftDetectionDetails' instead." #-}

-- | The status of the stack set.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStatus :: Lens.Lens' StackSet (Lude.Maybe StackSetStatus)
ssStatus = Lens.lens (status :: StackSet -> Lude.Maybe StackSetStatus) (\s a -> s {status = a} :: StackSet)
{-# DEPRECATED ssStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The Amazon Resource Number (ARN) of the IAM role used to create or update the stack set.
--
-- Use customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Prerequisites: Granting Permissions for Stack Set Operations> in the /AWS CloudFormation User Guide/ .
--
-- /Note:/ Consider using 'administrationRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssAdministrationRoleARN :: Lens.Lens' StackSet (Lude.Maybe Lude.Text)
ssAdministrationRoleARN = Lens.lens (administrationRoleARN :: StackSet -> Lude.Maybe Lude.Text) (\s a -> s {administrationRoleARN = a} :: StackSet)
{-# DEPRECATED ssAdministrationRoleARN "Use generic-lens or generic-optics with 'administrationRoleARN' instead." #-}

-- | [@Service-managed@ permissions] Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to a target organization or organizational unit (OU).
--
-- /Note:/ Consider using 'autoDeployment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssAutoDeployment :: Lens.Lens' StackSet (Lude.Maybe AutoDeployment)
ssAutoDeployment = Lens.lens (autoDeployment :: StackSet -> Lude.Maybe AutoDeployment) (\s a -> s {autoDeployment = a} :: StackSet)
{-# DEPRECATED ssAutoDeployment "Use generic-lens or generic-optics with 'autoDeployment' instead." #-}

-- | [@Service-managed@ permissions] The organization root ID or organizational unit (OU) IDs that you specified for <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets> .
--
-- /Note:/ Consider using 'organizationalUnitIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssOrganizationalUnitIds :: Lens.Lens' StackSet (Lude.Maybe [Lude.Text])
ssOrganizationalUnitIds = Lens.lens (organizationalUnitIds :: StackSet -> Lude.Maybe [Lude.Text]) (\s a -> s {organizationalUnitIds = a} :: StackSet)
{-# DEPRECATED ssOrganizationalUnitIds "Use generic-lens or generic-optics with 'organizationalUnitIds' instead." #-}

-- | The Amazon Resource Number (ARN) of the stack set.
--
-- /Note:/ Consider using 'stackSetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStackSetARN :: Lens.Lens' StackSet (Lude.Maybe Lude.Text)
ssStackSetARN = Lens.lens (stackSetARN :: StackSet -> Lude.Maybe Lude.Text) (\s a -> s {stackSetARN = a} :: StackSet)
{-# DEPRECATED ssStackSetARN "Use generic-lens or generic-optics with 'stackSetARN' instead." #-}

-- | Describes how the IAM roles required for stack set operations are created.
--
--
--     * With @self-managed@ permissions, you must create the administrator and execution roles required to deploy to target accounts. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions> .
--
--
--     * With @service-managed@ permissions, StackSets automatically creates the IAM roles required to deploy to accounts managed by AWS Organizations. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions> .
--
--
--
-- /Note:/ Consider using 'permissionModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssPermissionModel :: Lens.Lens' StackSet (Lude.Maybe PermissionModels)
ssPermissionModel = Lens.lens (permissionModel :: StackSet -> Lude.Maybe PermissionModels) (\s a -> s {permissionModel = a} :: StackSet)
{-# DEPRECATED ssPermissionModel "Use generic-lens or generic-optics with 'permissionModel' instead." #-}

-- | A list of input parameters for a stack set.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssParameters :: Lens.Lens' StackSet (Lude.Maybe [Parameter])
ssParameters = Lens.lens (parameters :: StackSet -> Lude.Maybe [Parameter]) (\s a -> s {parameters = a} :: StackSet)
{-# DEPRECATED ssParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The structure that contains the body of the template that was used to create or update the stack set.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssTemplateBody :: Lens.Lens' StackSet (Lude.Maybe Lude.Text)
ssTemplateBody = Lens.lens (templateBody :: StackSet -> Lude.Maybe Lude.Text) (\s a -> s {templateBody = a} :: StackSet)
{-# DEPRECATED ssTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | The name that's associated with the stack set.
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStackSetName :: Lens.Lens' StackSet (Lude.Maybe Lude.Text)
ssStackSetName = Lens.lens (stackSetName :: StackSet -> Lude.Maybe Lude.Text) (\s a -> s {stackSetName = a} :: StackSet)
{-# DEPRECATED ssStackSetName "Use generic-lens or generic-optics with 'stackSetName' instead." #-}

-- | A description of the stack set that you specify when the stack set is created or updated.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDescription :: Lens.Lens' StackSet (Lude.Maybe Lude.Text)
ssDescription = Lens.lens (description :: StackSet -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: StackSet)
{-# DEPRECATED ssDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The capabilities that are allowed in the stack set. Some stack set templates might include resources that can affect permissions in your AWS account—for example, by creating new AWS Identity and Access Management (IAM) users. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates.>
--
-- /Note:/ Consider using 'capabilities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssCapabilities :: Lens.Lens' StackSet (Lude.Maybe [Capability])
ssCapabilities = Lens.lens (capabilities :: StackSet -> Lude.Maybe [Capability]) (\s a -> s {capabilities = a} :: StackSet)
{-# DEPRECATED ssCapabilities "Use generic-lens or generic-optics with 'capabilities' instead." #-}

-- | A list of tags that specify information about the stack set. A maximum number of 50 tags can be specified.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssTags :: Lens.Lens' StackSet (Lude.Maybe [Tag])
ssTags = Lens.lens (tags :: StackSet -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: StackSet)
{-# DEPRECATED ssTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The ID of the stack set.
--
-- /Note:/ Consider using 'stackSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStackSetId :: Lens.Lens' StackSet (Lude.Maybe Lude.Text)
ssStackSetId = Lens.lens (stackSetId :: StackSet -> Lude.Maybe Lude.Text) (\s a -> s {stackSetId = a} :: StackSet)
{-# DEPRECATED ssStackSetId "Use generic-lens or generic-optics with 'stackSetId' instead." #-}

-- | The name of the IAM execution role used to create or update the stack set.
--
-- Use customized execution roles to control which stack resources users and groups can include in their stack sets.
--
-- /Note:/ Consider using 'executionRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssExecutionRoleName :: Lens.Lens' StackSet (Lude.Maybe Lude.Text)
ssExecutionRoleName = Lens.lens (executionRoleName :: StackSet -> Lude.Maybe Lude.Text) (\s a -> s {executionRoleName = a} :: StackSet)
{-# DEPRECATED ssExecutionRoleName "Use generic-lens or generic-optics with 'executionRoleName' instead." #-}

instance Lude.FromXML StackSet where
  parseXML x =
    StackSet'
      Lude.<$> (x Lude..@? "StackSetDriftDetectionDetails")
      Lude.<*> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "AdministrationRoleARN")
      Lude.<*> (x Lude..@? "AutoDeployment")
      Lude.<*> ( x Lude..@? "OrganizationalUnitIds" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "StackSetARN")
      Lude.<*> (x Lude..@? "PermissionModel")
      Lude.<*> ( x Lude..@? "Parameters" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "TemplateBody")
      Lude.<*> (x Lude..@? "StackSetName")
      Lude.<*> (x Lude..@? "Description")
      Lude.<*> ( x Lude..@? "Capabilities" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> ( x Lude..@? "Tags" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "StackSetId")
      Lude.<*> (x Lude..@? "ExecutionRoleName")
