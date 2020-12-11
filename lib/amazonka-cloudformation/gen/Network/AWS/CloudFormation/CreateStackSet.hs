{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.CreateStackSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a stack set.
module Network.AWS.CloudFormation.CreateStackSet
  ( -- * Creating a request
    CreateStackSet (..),
    mkCreateStackSet,

    -- ** Request lenses
    cssAdministrationRoleARN,
    cssAutoDeployment,
    cssPermissionModel,
    cssParameters,
    cssTemplateBody,
    cssTemplateURL,
    cssClientRequestToken,
    cssDescription,
    cssCapabilities,
    cssTags,
    cssExecutionRoleName,
    cssStackSetName,

    -- * Destructuring the response
    CreateStackSetResponse (..),
    mkCreateStackSetResponse,

    -- ** Response lenses
    cssrsStackSetId,
    cssrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateStackSet' smart constructor.
data CreateStackSet = CreateStackSet'
  { administrationRoleARN ::
      Lude.Maybe Lude.Text,
    autoDeployment :: Lude.Maybe AutoDeployment,
    permissionModel :: Lude.Maybe PermissionModels,
    parameters :: Lude.Maybe [Parameter],
    templateBody :: Lude.Maybe Lude.Text,
    templateURL :: Lude.Maybe Lude.Text,
    clientRequestToken :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    capabilities :: Lude.Maybe [Capability],
    tags :: Lude.Maybe [Tag],
    executionRoleName :: Lude.Maybe Lude.Text,
    stackSetName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStackSet' with the minimum fields required to make a request.
--
-- * 'administrationRoleARN' - The Amazon Resource Number (ARN) of the IAM role to use to create this stack set.
--
-- Specify an IAM role only if you are using customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Prerequisites: Granting Permissions for Stack Set Operations> in the /AWS CloudFormation User Guide/ .
-- * 'autoDeployment' - Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to the target organization or organizational unit (OU). Specify only if @PermissionModel@ is @SERVICE_MANAGED@ .
-- * 'capabilities' - In some cases, you must explicitly acknowledge that your stack set template contains certain capabilities in order for AWS CloudFormation to create the stack set and related stack instances.
--
--
--     * @CAPABILITY_IAM@ and @CAPABILITY_NAMED_IAM@
-- Some stack templates might include resources that can affect permissions in your AWS account; for example, by creating new AWS Identity and Access Management (IAM) users. For those stack sets, you must explicitly acknowledge this by specifying one of these capabilities.
-- The following IAM resources require you to specify either the @CAPABILITY_IAM@ or @CAPABILITY_NAMED_IAM@ capability.
--
--     * If you have IAM resources, you can specify either capability.
--
--
--     * If you have IAM resources with custom names, you /must/ specify @CAPABILITY_NAMED_IAM@ .
--
--
--     * If you don't specify either of these capabilities, AWS CloudFormation returns an @InsufficientCapabilities@ error.
--
--
-- If your stack template contains these resources, we recommend that you review all permissions associated with them and edit their permissions if necessary.
--
--     * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-accesskey.html AWS::IAM::AccessKey>
--
--
--     * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-group.html AWS::IAM::Group>
--
--
--     * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-instanceprofile.html AWS::IAM::InstanceProfile>
--
--
--     * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-policy.html AWS::IAM::Policy>
--
--
--     * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-role.html AWS::IAM::Role>
--
--
--     * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-user.html AWS::IAM::User>
--
--
--     * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-addusertogroup.html AWS::IAM::UserToGroupAddition>
--
--
-- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates> .
--
--
--     * @CAPABILITY_AUTO_EXPAND@
-- Some templates contain macros. If your stack template contains one or more macros, and you choose to create a stack directly from the processed template, without first reviewing the resulting changes in a change set, you must acknowledge this capability. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-macros.html Using AWS CloudFormation Macros to Perform Custom Processing on Templates> .
--
--
-- * 'clientRequestToken' - A unique identifier for this @CreateStackSet@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to create another stack set with the same name. You might retry @CreateStackSet@ requests to ensure that AWS CloudFormation successfully received them.
--
-- If you don't specify an operation ID, the SDK generates one automatically.
-- * 'description' - A description of the stack set. You can use the description to identify the stack set's purpose or other important information.
-- * 'executionRoleName' - The name of the IAM execution role to use to create the stack set. If you do not specify an execution role, AWS CloudFormation uses the @AWSCloudFormationStackSetExecutionRole@ role for the stack set operation.
--
-- Specify an IAM role only if you are using customized execution roles to control which stack resources users and groups can include in their stack sets.
-- * 'parameters' - The input parameters for the stack set template.
-- * 'permissionModel' - Describes how the IAM roles required for stack set operations are created. By default, @SELF-MANAGED@ is specified.
--
--
--     * With @self-managed@ permissions, you must create the administrator and execution roles required to deploy to target accounts. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions> .
--
--
--     * With @service-managed@ permissions, StackSets automatically creates the IAM roles required to deploy to accounts managed by AWS Organizations. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions> .
--
--
-- * 'stackSetName' - The name to associate with the stack set. The name must be unique in the Region where you create your stack set.
-- * 'tags' - The key-value pairs to associate with this stack set and the stacks created from it. AWS CloudFormation also propagates these tags to supported resources that are created in the stacks. A maximum number of 50 tags can be specified.
--
-- If you specify tags as part of a @CreateStackSet@ action, AWS CloudFormation checks to see if you have the required IAM permission to tag resources. If you don't, the entire @CreateStackSet@ action fails with an @access denied@ error, and the stack set is not created.
-- * 'templateBody' - The structure that contains the template body, with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify either the TemplateBody or the TemplateURL parameter, but not both.
-- * 'templateURL' - The location of the file that contains the template body. The URL must point to a template (maximum size: 460,800 bytes) that's located in an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify either the TemplateBody or the TemplateURL parameter, but not both.
mkCreateStackSet ::
  -- | 'stackSetName'
  Lude.Text ->
  CreateStackSet
mkCreateStackSet pStackSetName_ =
  CreateStackSet'
    { administrationRoleARN = Lude.Nothing,
      autoDeployment = Lude.Nothing,
      permissionModel = Lude.Nothing,
      parameters = Lude.Nothing,
      templateBody = Lude.Nothing,
      templateURL = Lude.Nothing,
      clientRequestToken = Lude.Nothing,
      description = Lude.Nothing,
      capabilities = Lude.Nothing,
      tags = Lude.Nothing,
      executionRoleName = Lude.Nothing,
      stackSetName = pStackSetName_
    }

-- | The Amazon Resource Number (ARN) of the IAM role to use to create this stack set.
--
-- Specify an IAM role only if you are using customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Prerequisites: Granting Permissions for Stack Set Operations> in the /AWS CloudFormation User Guide/ .
--
-- /Note:/ Consider using 'administrationRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssAdministrationRoleARN :: Lens.Lens' CreateStackSet (Lude.Maybe Lude.Text)
cssAdministrationRoleARN = Lens.lens (administrationRoleARN :: CreateStackSet -> Lude.Maybe Lude.Text) (\s a -> s {administrationRoleARN = a} :: CreateStackSet)
{-# DEPRECATED cssAdministrationRoleARN "Use generic-lens or generic-optics with 'administrationRoleARN' instead." #-}

-- | Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to the target organization or organizational unit (OU). Specify only if @PermissionModel@ is @SERVICE_MANAGED@ .
--
-- /Note:/ Consider using 'autoDeployment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssAutoDeployment :: Lens.Lens' CreateStackSet (Lude.Maybe AutoDeployment)
cssAutoDeployment = Lens.lens (autoDeployment :: CreateStackSet -> Lude.Maybe AutoDeployment) (\s a -> s {autoDeployment = a} :: CreateStackSet)
{-# DEPRECATED cssAutoDeployment "Use generic-lens or generic-optics with 'autoDeployment' instead." #-}

-- | Describes how the IAM roles required for stack set operations are created. By default, @SELF-MANAGED@ is specified.
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
cssPermissionModel :: Lens.Lens' CreateStackSet (Lude.Maybe PermissionModels)
cssPermissionModel = Lens.lens (permissionModel :: CreateStackSet -> Lude.Maybe PermissionModels) (\s a -> s {permissionModel = a} :: CreateStackSet)
{-# DEPRECATED cssPermissionModel "Use generic-lens or generic-optics with 'permissionModel' instead." #-}

-- | The input parameters for the stack set template.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssParameters :: Lens.Lens' CreateStackSet (Lude.Maybe [Parameter])
cssParameters = Lens.lens (parameters :: CreateStackSet -> Lude.Maybe [Parameter]) (\s a -> s {parameters = a} :: CreateStackSet)
{-# DEPRECATED cssParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The structure that contains the template body, with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify either the TemplateBody or the TemplateURL parameter, but not both.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssTemplateBody :: Lens.Lens' CreateStackSet (Lude.Maybe Lude.Text)
cssTemplateBody = Lens.lens (templateBody :: CreateStackSet -> Lude.Maybe Lude.Text) (\s a -> s {templateBody = a} :: CreateStackSet)
{-# DEPRECATED cssTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | The location of the file that contains the template body. The URL must point to a template (maximum size: 460,800 bytes) that's located in an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify either the TemplateBody or the TemplateURL parameter, but not both.
--
-- /Note:/ Consider using 'templateURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssTemplateURL :: Lens.Lens' CreateStackSet (Lude.Maybe Lude.Text)
cssTemplateURL = Lens.lens (templateURL :: CreateStackSet -> Lude.Maybe Lude.Text) (\s a -> s {templateURL = a} :: CreateStackSet)
{-# DEPRECATED cssTemplateURL "Use generic-lens or generic-optics with 'templateURL' instead." #-}

-- | A unique identifier for this @CreateStackSet@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to create another stack set with the same name. You might retry @CreateStackSet@ requests to ensure that AWS CloudFormation successfully received them.
--
-- If you don't specify an operation ID, the SDK generates one automatically.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssClientRequestToken :: Lens.Lens' CreateStackSet (Lude.Maybe Lude.Text)
cssClientRequestToken = Lens.lens (clientRequestToken :: CreateStackSet -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: CreateStackSet)
{-# DEPRECATED cssClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | A description of the stack set. You can use the description to identify the stack set's purpose or other important information.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssDescription :: Lens.Lens' CreateStackSet (Lude.Maybe Lude.Text)
cssDescription = Lens.lens (description :: CreateStackSet -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateStackSet)
{-# DEPRECATED cssDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | In some cases, you must explicitly acknowledge that your stack set template contains certain capabilities in order for AWS CloudFormation to create the stack set and related stack instances.
--
--
--     * @CAPABILITY_IAM@ and @CAPABILITY_NAMED_IAM@
-- Some stack templates might include resources that can affect permissions in your AWS account; for example, by creating new AWS Identity and Access Management (IAM) users. For those stack sets, you must explicitly acknowledge this by specifying one of these capabilities.
-- The following IAM resources require you to specify either the @CAPABILITY_IAM@ or @CAPABILITY_NAMED_IAM@ capability.
--
--     * If you have IAM resources, you can specify either capability.
--
--
--     * If you have IAM resources with custom names, you /must/ specify @CAPABILITY_NAMED_IAM@ .
--
--
--     * If you don't specify either of these capabilities, AWS CloudFormation returns an @InsufficientCapabilities@ error.
--
--
-- If your stack template contains these resources, we recommend that you review all permissions associated with them and edit their permissions if necessary.
--
--     * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-accesskey.html AWS::IAM::AccessKey>
--
--
--     * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-group.html AWS::IAM::Group>
--
--
--     * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-instanceprofile.html AWS::IAM::InstanceProfile>
--
--
--     * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-policy.html AWS::IAM::Policy>
--
--
--     * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-role.html AWS::IAM::Role>
--
--
--     * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-user.html AWS::IAM::User>
--
--
--     * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-addusertogroup.html AWS::IAM::UserToGroupAddition>
--
--
-- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates> .
--
--
--     * @CAPABILITY_AUTO_EXPAND@
-- Some templates contain macros. If your stack template contains one or more macros, and you choose to create a stack directly from the processed template, without first reviewing the resulting changes in a change set, you must acknowledge this capability. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-macros.html Using AWS CloudFormation Macros to Perform Custom Processing on Templates> .
--
--
--
-- /Note:/ Consider using 'capabilities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssCapabilities :: Lens.Lens' CreateStackSet (Lude.Maybe [Capability])
cssCapabilities = Lens.lens (capabilities :: CreateStackSet -> Lude.Maybe [Capability]) (\s a -> s {capabilities = a} :: CreateStackSet)
{-# DEPRECATED cssCapabilities "Use generic-lens or generic-optics with 'capabilities' instead." #-}

-- | The key-value pairs to associate with this stack set and the stacks created from it. AWS CloudFormation also propagates these tags to supported resources that are created in the stacks. A maximum number of 50 tags can be specified.
--
-- If you specify tags as part of a @CreateStackSet@ action, AWS CloudFormation checks to see if you have the required IAM permission to tag resources. If you don't, the entire @CreateStackSet@ action fails with an @access denied@ error, and the stack set is not created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssTags :: Lens.Lens' CreateStackSet (Lude.Maybe [Tag])
cssTags = Lens.lens (tags :: CreateStackSet -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateStackSet)
{-# DEPRECATED cssTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the IAM execution role to use to create the stack set. If you do not specify an execution role, AWS CloudFormation uses the @AWSCloudFormationStackSetExecutionRole@ role for the stack set operation.
--
-- Specify an IAM role only if you are using customized execution roles to control which stack resources users and groups can include in their stack sets.
--
-- /Note:/ Consider using 'executionRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssExecutionRoleName :: Lens.Lens' CreateStackSet (Lude.Maybe Lude.Text)
cssExecutionRoleName = Lens.lens (executionRoleName :: CreateStackSet -> Lude.Maybe Lude.Text) (\s a -> s {executionRoleName = a} :: CreateStackSet)
{-# DEPRECATED cssExecutionRoleName "Use generic-lens or generic-optics with 'executionRoleName' instead." #-}

-- | The name to associate with the stack set. The name must be unique in the Region where you create your stack set.
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssStackSetName :: Lens.Lens' CreateStackSet Lude.Text
cssStackSetName = Lens.lens (stackSetName :: CreateStackSet -> Lude.Text) (\s a -> s {stackSetName = a} :: CreateStackSet)
{-# DEPRECATED cssStackSetName "Use generic-lens or generic-optics with 'stackSetName' instead." #-}

instance Lude.AWSRequest CreateStackSet where
  type Rs CreateStackSet = CreateStackSetResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "CreateStackSetResult"
      ( \s h x ->
          CreateStackSetResponse'
            Lude.<$> (x Lude..@? "StackSetId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateStackSet where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateStackSet where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateStackSet where
  toQuery CreateStackSet' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateStackSet" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "AdministrationRoleARN" Lude.=: administrationRoleARN,
        "AutoDeployment" Lude.=: autoDeployment,
        "PermissionModel" Lude.=: permissionModel,
        "Parameters"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> parameters),
        "TemplateBody" Lude.=: templateBody,
        "TemplateURL" Lude.=: templateURL,
        "ClientRequestToken" Lude.=: clientRequestToken,
        "Description" Lude.=: description,
        "Capabilities"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> capabilities),
        "Tags"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> tags),
        "ExecutionRoleName" Lude.=: executionRoleName,
        "StackSetName" Lude.=: stackSetName
      ]

-- | /See:/ 'mkCreateStackSetResponse' smart constructor.
data CreateStackSetResponse = CreateStackSetResponse'
  { stackSetId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStackSetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'stackSetId' - The ID of the stack set that you're creating.
mkCreateStackSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateStackSetResponse
mkCreateStackSetResponse pResponseStatus_ =
  CreateStackSetResponse'
    { stackSetId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the stack set that you're creating.
--
-- /Note:/ Consider using 'stackSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssrsStackSetId :: Lens.Lens' CreateStackSetResponse (Lude.Maybe Lude.Text)
cssrsStackSetId = Lens.lens (stackSetId :: CreateStackSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {stackSetId = a} :: CreateStackSetResponse)
{-# DEPRECATED cssrsStackSetId "Use generic-lens or generic-optics with 'stackSetId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssrsResponseStatus :: Lens.Lens' CreateStackSetResponse Lude.Int
cssrsResponseStatus = Lens.lens (responseStatus :: CreateStackSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateStackSetResponse)
{-# DEPRECATED cssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
