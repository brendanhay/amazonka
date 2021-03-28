{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateStackSet (..)
    , mkCreateStackSet
    -- ** Request lenses
    , cStackSetName
    , cAdministrationRoleARN
    , cAutoDeployment
    , cCapabilities
    , cClientRequestToken
    , cDescription
    , cExecutionRoleName
    , cParameters
    , cPermissionModel
    , cTags
    , cTemplateBody
    , cTemplateURL

    -- * Destructuring the response
    , CreateStackSetResponse (..)
    , mkCreateStackSetResponse
    -- ** Response lenses
    , cssrrsStackSetId
    , cssrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateStackSet' smart constructor.
data CreateStackSet = CreateStackSet'
  { stackSetName :: Types.StackSetName
    -- ^ The name to associate with the stack set. The name must be unique in the Region where you create your stack set.
  , administrationRoleARN :: Core.Maybe Types.AdministrationRoleARN
    -- ^ The Amazon Resource Number (ARN) of the IAM role to use to create this stack set. 
--
-- Specify an IAM role only if you are using customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Prerequisites: Granting Permissions for Stack Set Operations> in the /AWS CloudFormation User Guide/ .
  , autoDeployment :: Core.Maybe Types.AutoDeployment
    -- ^ Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to the target organization or organizational unit (OU). Specify only if @PermissionModel@ is @SERVICE_MANAGED@ .
  , capabilities :: Core.Maybe [Types.Capability]
    -- ^ In some cases, you must explicitly acknowledge that your stack set template contains certain capabilities in order for AWS CloudFormation to create the stack set and related stack instances.
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
  , clientRequestToken :: Core.Maybe Types.ClientRequestToken
    -- ^ A unique identifier for this @CreateStackSet@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to create another stack set with the same name. You might retry @CreateStackSet@ requests to ensure that AWS CloudFormation successfully received them.
--
-- If you don't specify an operation ID, the SDK generates one automatically. 
  , description :: Core.Maybe Types.Description
    -- ^ A description of the stack set. You can use the description to identify the stack set's purpose or other important information.
  , executionRoleName :: Core.Maybe Types.ExecutionRoleName
    -- ^ The name of the IAM execution role to use to create the stack set. If you do not specify an execution role, AWS CloudFormation uses the @AWSCloudFormationStackSetExecutionRole@ role for the stack set operation.
--
-- Specify an IAM role only if you are using customized execution roles to control which stack resources users and groups can include in their stack sets. 
  , parameters :: Core.Maybe [Types.Parameter]
    -- ^ The input parameters for the stack set template. 
  , permissionModel :: Core.Maybe Types.PermissionModels
    -- ^ Describes how the IAM roles required for stack set operations are created. By default, @SELF-MANAGED@ is specified.
--
--
--     * With @self-managed@ permissions, you must create the administrator and execution roles required to deploy to target accounts. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions> .
--
--
--     * With @service-managed@ permissions, StackSets automatically creates the IAM roles required to deploy to accounts managed by AWS Organizations. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions> .
--
--
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The key-value pairs to associate with this stack set and the stacks created from it. AWS CloudFormation also propagates these tags to supported resources that are created in the stacks. A maximum number of 50 tags can be specified.
--
-- If you specify tags as part of a @CreateStackSet@ action, AWS CloudFormation checks to see if you have the required IAM permission to tag resources. If you don't, the entire @CreateStackSet@ action fails with an @access denied@ error, and the stack set is not created.
  , templateBody :: Core.Maybe Types.TemplateBody
    -- ^ The structure that contains the template body, with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify either the TemplateBody or the TemplateURL parameter, but not both.
  , templateURL :: Core.Maybe Types.TemplateURL
    -- ^ The location of the file that contains the template body. The URL must point to a template (maximum size: 460,800 bytes) that's located in an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify either the TemplateBody or the TemplateURL parameter, but not both.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStackSet' value with any optional fields omitted.
mkCreateStackSet
    :: Types.StackSetName -- ^ 'stackSetName'
    -> CreateStackSet
mkCreateStackSet stackSetName
  = CreateStackSet'{stackSetName,
                    administrationRoleARN = Core.Nothing,
                    autoDeployment = Core.Nothing, capabilities = Core.Nothing,
                    clientRequestToken = Core.Nothing, description = Core.Nothing,
                    executionRoleName = Core.Nothing, parameters = Core.Nothing,
                    permissionModel = Core.Nothing, tags = Core.Nothing,
                    templateBody = Core.Nothing, templateURL = Core.Nothing}

-- | The name to associate with the stack set. The name must be unique in the Region where you create your stack set.
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStackSetName :: Lens.Lens' CreateStackSet Types.StackSetName
cStackSetName = Lens.field @"stackSetName"
{-# INLINEABLE cStackSetName #-}
{-# DEPRECATED stackSetName "Use generic-lens or generic-optics with 'stackSetName' instead"  #-}

-- | The Amazon Resource Number (ARN) of the IAM role to use to create this stack set. 
--
-- Specify an IAM role only if you are using customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Prerequisites: Granting Permissions for Stack Set Operations> in the /AWS CloudFormation User Guide/ .
--
-- /Note:/ Consider using 'administrationRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAdministrationRoleARN :: Lens.Lens' CreateStackSet (Core.Maybe Types.AdministrationRoleARN)
cAdministrationRoleARN = Lens.field @"administrationRoleARN"
{-# INLINEABLE cAdministrationRoleARN #-}
{-# DEPRECATED administrationRoleARN "Use generic-lens or generic-optics with 'administrationRoleARN' instead"  #-}

-- | Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to the target organization or organizational unit (OU). Specify only if @PermissionModel@ is @SERVICE_MANAGED@ .
--
-- /Note:/ Consider using 'autoDeployment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAutoDeployment :: Lens.Lens' CreateStackSet (Core.Maybe Types.AutoDeployment)
cAutoDeployment = Lens.field @"autoDeployment"
{-# INLINEABLE cAutoDeployment #-}
{-# DEPRECATED autoDeployment "Use generic-lens or generic-optics with 'autoDeployment' instead"  #-}

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
{-# INLINEABLE cCapabilities #-}
{-# DEPRECATED capabilities "Use generic-lens or generic-optics with 'capabilities' instead"  #-}

-- | A unique identifier for this @CreateStackSet@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to create another stack set with the same name. You might retry @CreateStackSet@ requests to ensure that AWS CloudFormation successfully received them.
--
-- If you don't specify an operation ID, the SDK generates one automatically. 
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClientRequestToken :: Lens.Lens' CreateStackSet (Core.Maybe Types.ClientRequestToken)
cClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE cClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | A description of the stack set. You can use the description to identify the stack set's purpose or other important information.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDescription :: Lens.Lens' CreateStackSet (Core.Maybe Types.Description)
cDescription = Lens.field @"description"
{-# INLINEABLE cDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The name of the IAM execution role to use to create the stack set. If you do not specify an execution role, AWS CloudFormation uses the @AWSCloudFormationStackSetExecutionRole@ role for the stack set operation.
--
-- Specify an IAM role only if you are using customized execution roles to control which stack resources users and groups can include in their stack sets. 
--
-- /Note:/ Consider using 'executionRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cExecutionRoleName :: Lens.Lens' CreateStackSet (Core.Maybe Types.ExecutionRoleName)
cExecutionRoleName = Lens.field @"executionRoleName"
{-# INLINEABLE cExecutionRoleName #-}
{-# DEPRECATED executionRoleName "Use generic-lens or generic-optics with 'executionRoleName' instead"  #-}

-- | The input parameters for the stack set template. 
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cParameters :: Lens.Lens' CreateStackSet (Core.Maybe [Types.Parameter])
cParameters = Lens.field @"parameters"
{-# INLINEABLE cParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

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
{-# INLINEABLE cPermissionModel #-}
{-# DEPRECATED permissionModel "Use generic-lens or generic-optics with 'permissionModel' instead"  #-}

-- | The key-value pairs to associate with this stack set and the stacks created from it. AWS CloudFormation also propagates these tags to supported resources that are created in the stacks. A maximum number of 50 tags can be specified.
--
-- If you specify tags as part of a @CreateStackSet@ action, AWS CloudFormation checks to see if you have the required IAM permission to tag resources. If you don't, the entire @CreateStackSet@ action fails with an @access denied@ error, and the stack set is not created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' CreateStackSet (Core.Maybe [Types.Tag])
cTags = Lens.field @"tags"
{-# INLINEABLE cTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The structure that contains the template body, with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify either the TemplateBody or the TemplateURL parameter, but not both.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTemplateBody :: Lens.Lens' CreateStackSet (Core.Maybe Types.TemplateBody)
cTemplateBody = Lens.field @"templateBody"
{-# INLINEABLE cTemplateBody #-}
{-# DEPRECATED templateBody "Use generic-lens or generic-optics with 'templateBody' instead"  #-}

-- | The location of the file that contains the template body. The URL must point to a template (maximum size: 460,800 bytes) that's located in an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify either the TemplateBody or the TemplateURL parameter, but not both.
--
-- /Note:/ Consider using 'templateURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTemplateURL :: Lens.Lens' CreateStackSet (Core.Maybe Types.TemplateURL)
cTemplateURL = Lens.field @"templateURL"
{-# INLINEABLE cTemplateURL #-}
{-# DEPRECATED templateURL "Use generic-lens or generic-optics with 'templateURL' instead"  #-}

instance Core.ToQuery CreateStackSet where
        toQuery CreateStackSet{..}
          = Core.toQueryPair "Action" ("CreateStackSet" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-05-15" :: Core.Text)
              Core.<> Core.toQueryPair "StackSetName" stackSetName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AdministrationRoleARN")
                administrationRoleARN
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AutoDeployment")
                autoDeployment
              Core.<>
              Core.toQueryPair "Capabilities"
                (Core.maybe Core.mempty (Core.toQueryList "member") capabilities)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientRequestToken")
                clientRequestToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ExecutionRoleName")
                executionRoleName
              Core.<>
              Core.toQueryPair "Parameters"
                (Core.maybe Core.mempty (Core.toQueryList "member") parameters)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PermissionModel")
                permissionModel
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "member") tags)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TemplateBody")
                templateBody
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TemplateURL") templateURL

instance Core.ToHeaders CreateStackSet where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateStackSet where
        type Rs CreateStackSet = CreateStackSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "CreateStackSetResult"
              (\ s h x ->
                 CreateStackSetResponse' Core.<$>
                   (x Core..@? "StackSetId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateStackSetResponse' smart constructor.
data CreateStackSetResponse = CreateStackSetResponse'
  { stackSetId :: Core.Maybe Types.StackSetId
    -- ^ The ID of the stack set that you're creating.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStackSetResponse' value with any optional fields omitted.
mkCreateStackSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateStackSetResponse
mkCreateStackSetResponse responseStatus
  = CreateStackSetResponse'{stackSetId = Core.Nothing,
                            responseStatus}

-- | The ID of the stack set that you're creating.
--
-- /Note:/ Consider using 'stackSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssrrsStackSetId :: Lens.Lens' CreateStackSetResponse (Core.Maybe Types.StackSetId)
cssrrsStackSetId = Lens.field @"stackSetId"
{-# INLINEABLE cssrrsStackSetId #-}
{-# DEPRECATED stackSetId "Use generic-lens or generic-optics with 'stackSetId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssrrsResponseStatus :: Lens.Lens' CreateStackSetResponse Core.Int
cssrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cssrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
