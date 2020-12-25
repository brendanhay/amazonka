{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    cStackSetName,
    cAdministrationRoleARN,
    cAutoDeployment,
    cCapabilities,
    cClientRequestToken,
    cDescription,
    cExecutionRoleName,
    cParameters,
    cPermissionModel,
    cTags,
    cTemplateBody,
    cTemplateURL,

    -- * Destructuring the response
    CreateStackSetResponse (..),
    mkCreateStackSetResponse,

    -- ** Response lenses
    cssrrsStackSetId,
    cssrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateStackSet' smart constructor.
data CreateStackSet = CreateStackSet'
  { -- | The name to associate with the stack set. The name must be unique in the Region where you create your stack set.
    stackSetName :: Types.StackSetName,
    -- | The Amazon Resource Number (ARN) of the IAM role to use to create this stack set.
    --
    -- Specify an IAM role only if you are using customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Prerequisites: Granting Permissions for Stack Set Operations> in the /AWS CloudFormation User Guide/ .
    administrationRoleARN :: Core.Maybe Types.AdministrationRoleARN,
    -- | Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to the target organization or organizational unit (OU). Specify only if @PermissionModel@ is @SERVICE_MANAGED@ .
    autoDeployment :: Core.Maybe Types.AutoDeployment,
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
    capabilities :: Core.Maybe [Types.Capability],
    -- | A unique identifier for this @CreateStackSet@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to create another stack set with the same name. You might retry @CreateStackSet@ requests to ensure that AWS CloudFormation successfully received them.
    --
    -- If you don't specify an operation ID, the SDK generates one automatically.
    clientRequestToken :: Core.Maybe Types.ClientRequestToken,
    -- | A description of the stack set. You can use the description to identify the stack set's purpose or other important information.
    description :: Core.Maybe Types.Description,
    -- | The name of the IAM execution role to use to create the stack set. If you do not specify an execution role, AWS CloudFormation uses the @AWSCloudFormationStackSetExecutionRole@ role for the stack set operation.
    --
    -- Specify an IAM role only if you are using customized execution roles to control which stack resources users and groups can include in their stack sets.
    executionRoleName :: Core.Maybe Types.ExecutionRoleName,
    -- | The input parameters for the stack set template.
    parameters :: Core.Maybe [Types.Parameter],
    -- | Describes how the IAM roles required for stack set operations are created. By default, @SELF-MANAGED@ is specified.
    --
    --
    --     * With @self-managed@ permissions, you must create the administrator and execution roles required to deploy to target accounts. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions> .
    --
    --
    --     * With @service-managed@ permissions, StackSets automatically creates the IAM roles required to deploy to accounts managed by AWS Organizations. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions> .
    permissionModel :: Core.Maybe Types.PermissionModels,
    -- | The key-value pairs to associate with this stack set and the stacks created from it. AWS CloudFormation also propagates these tags to supported resources that are created in the stacks. A maximum number of 50 tags can be specified.
    --
    -- If you specify tags as part of a @CreateStackSet@ action, AWS CloudFormation checks to see if you have the required IAM permission to tag resources. If you don't, the entire @CreateStackSet@ action fails with an @access denied@ error, and the stack set is not created.
    tags :: Core.Maybe [Types.Tag],
    -- | The structure that contains the template body, with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
    --
    -- Conditional: You must specify either the TemplateBody or the TemplateURL parameter, but not both.
    templateBody :: Core.Maybe Types.TemplateBody,
    -- | The location of the file that contains the template body. The URL must point to a template (maximum size: 460,800 bytes) that's located in an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
    --
    -- Conditional: You must specify either the TemplateBody or the TemplateURL parameter, but not both.
    templateURL :: Core.Maybe Types.TemplateURL
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStackSet' value with any optional fields omitted.
mkCreateStackSet ::
  -- | 'stackSetName'
  Types.StackSetName ->
  CreateStackSet
mkCreateStackSet stackSetName =
  CreateStackSet'
    { stackSetName,
      administrationRoleARN = Core.Nothing,
      autoDeployment = Core.Nothing,
      capabilities = Core.Nothing,
      clientRequestToken = Core.Nothing,
      description = Core.Nothing,
      executionRoleName = Core.Nothing,
      parameters = Core.Nothing,
      permissionModel = Core.Nothing,
      tags = Core.Nothing,
      templateBody = Core.Nothing,
      templateURL = Core.Nothing
    }

-- | The name to associate with the stack set. The name must be unique in the Region where you create your stack set.
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStackSetName :: Lens.Lens' CreateStackSet Types.StackSetName
cStackSetName = Lens.field @"stackSetName"
{-# DEPRECATED cStackSetName "Use generic-lens or generic-optics with 'stackSetName' instead." #-}

-- | The Amazon Resource Number (ARN) of the IAM role to use to create this stack set.
--
-- Specify an IAM role only if you are using customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Prerequisites: Granting Permissions for Stack Set Operations> in the /AWS CloudFormation User Guide/ .
--
-- /Note:/ Consider using 'administrationRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAdministrationRoleARN :: Lens.Lens' CreateStackSet (Core.Maybe Types.AdministrationRoleARN)
cAdministrationRoleARN = Lens.field @"administrationRoleARN"
{-# DEPRECATED cAdministrationRoleARN "Use generic-lens or generic-optics with 'administrationRoleARN' instead." #-}

-- | Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to the target organization or organizational unit (OU). Specify only if @PermissionModel@ is @SERVICE_MANAGED@ .
--
-- /Note:/ Consider using 'autoDeployment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAutoDeployment :: Lens.Lens' CreateStackSet (Core.Maybe Types.AutoDeployment)
cAutoDeployment = Lens.field @"autoDeployment"
{-# DEPRECATED cAutoDeployment "Use generic-lens or generic-optics with 'autoDeployment' instead." #-}

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
cCapabilities :: Lens.Lens' CreateStackSet (Core.Maybe [Types.Capability])
cCapabilities = Lens.field @"capabilities"
{-# DEPRECATED cCapabilities "Use generic-lens or generic-optics with 'capabilities' instead." #-}

-- | A unique identifier for this @CreateStackSet@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to create another stack set with the same name. You might retry @CreateStackSet@ requests to ensure that AWS CloudFormation successfully received them.
--
-- If you don't specify an operation ID, the SDK generates one automatically.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClientRequestToken :: Lens.Lens' CreateStackSet (Core.Maybe Types.ClientRequestToken)
cClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED cClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | A description of the stack set. You can use the description to identify the stack set's purpose or other important information.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDescription :: Lens.Lens' CreateStackSet (Core.Maybe Types.Description)
cDescription = Lens.field @"description"
{-# DEPRECATED cDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the IAM execution role to use to create the stack set. If you do not specify an execution role, AWS CloudFormation uses the @AWSCloudFormationStackSetExecutionRole@ role for the stack set operation.
--
-- Specify an IAM role only if you are using customized execution roles to control which stack resources users and groups can include in their stack sets.
--
-- /Note:/ Consider using 'executionRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cExecutionRoleName :: Lens.Lens' CreateStackSet (Core.Maybe Types.ExecutionRoleName)
cExecutionRoleName = Lens.field @"executionRoleName"
{-# DEPRECATED cExecutionRoleName "Use generic-lens or generic-optics with 'executionRoleName' instead." #-}

-- | The input parameters for the stack set template.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cParameters :: Lens.Lens' CreateStackSet (Core.Maybe [Types.Parameter])
cParameters = Lens.field @"parameters"
{-# DEPRECATED cParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

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
cPermissionModel :: Lens.Lens' CreateStackSet (Core.Maybe Types.PermissionModels)
cPermissionModel = Lens.field @"permissionModel"
{-# DEPRECATED cPermissionModel "Use generic-lens or generic-optics with 'permissionModel' instead." #-}

-- | The key-value pairs to associate with this stack set and the stacks created from it. AWS CloudFormation also propagates these tags to supported resources that are created in the stacks. A maximum number of 50 tags can be specified.
--
-- If you specify tags as part of a @CreateStackSet@ action, AWS CloudFormation checks to see if you have the required IAM permission to tag resources. If you don't, the entire @CreateStackSet@ action fails with an @access denied@ error, and the stack set is not created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' CreateStackSet (Core.Maybe [Types.Tag])
cTags = Lens.field @"tags"
{-# DEPRECATED cTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The structure that contains the template body, with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify either the TemplateBody or the TemplateURL parameter, but not both.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTemplateBody :: Lens.Lens' CreateStackSet (Core.Maybe Types.TemplateBody)
cTemplateBody = Lens.field @"templateBody"
{-# DEPRECATED cTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | The location of the file that contains the template body. The URL must point to a template (maximum size: 460,800 bytes) that's located in an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify either the TemplateBody or the TemplateURL parameter, but not both.
--
-- /Note:/ Consider using 'templateURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTemplateURL :: Lens.Lens' CreateStackSet (Core.Maybe Types.TemplateURL)
cTemplateURL = Lens.field @"templateURL"
{-# DEPRECATED cTemplateURL "Use generic-lens or generic-optics with 'templateURL' instead." #-}

instance Core.AWSRequest CreateStackSet where
  type Rs CreateStackSet = CreateStackSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CreateStackSet")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "StackSetName" stackSetName)
                Core.<> ( Core.toQueryValue "AdministrationRoleARN"
                            Core.<$> administrationRoleARN
                        )
                Core.<> (Core.toQueryValue "AutoDeployment" Core.<$> autoDeployment)
                Core.<> ( Core.toQueryValue
                            "Capabilities"
                            (Core.toQueryList "member" Core.<$> capabilities)
                        )
                Core.<> ( Core.toQueryValue "ClientRequestToken"
                            Core.<$> clientRequestToken
                        )
                Core.<> (Core.toQueryValue "Description" Core.<$> description)
                Core.<> (Core.toQueryValue "ExecutionRoleName" Core.<$> executionRoleName)
                Core.<> ( Core.toQueryValue
                            "Parameters"
                            (Core.toQueryList "member" Core.<$> parameters)
                        )
                Core.<> (Core.toQueryValue "PermissionModel" Core.<$> permissionModel)
                Core.<> ( Core.toQueryValue
                            "Tags"
                            (Core.toQueryList "member" Core.<$> tags)
                        )
                Core.<> (Core.toQueryValue "TemplateBody" Core.<$> templateBody)
                Core.<> (Core.toQueryValue "TemplateURL" Core.<$> templateURL)
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateStackSetResult"
      ( \s h x ->
          CreateStackSetResponse'
            Core.<$> (x Core..@? "StackSetId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateStackSetResponse' smart constructor.
data CreateStackSetResponse = CreateStackSetResponse'
  { -- | The ID of the stack set that you're creating.
    stackSetId :: Core.Maybe Types.StackSetId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStackSetResponse' value with any optional fields omitted.
mkCreateStackSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateStackSetResponse
mkCreateStackSetResponse responseStatus =
  CreateStackSetResponse'
    { stackSetId = Core.Nothing,
      responseStatus
    }

-- | The ID of the stack set that you're creating.
--
-- /Note:/ Consider using 'stackSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssrrsStackSetId :: Lens.Lens' CreateStackSetResponse (Core.Maybe Types.StackSetId)
cssrrsStackSetId = Lens.field @"stackSetId"
{-# DEPRECATED cssrrsStackSetId "Use generic-lens or generic-optics with 'stackSetId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssrrsResponseStatus :: Lens.Lens' CreateStackSetResponse Core.Int
cssrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cssrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
