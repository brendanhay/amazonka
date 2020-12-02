{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.UpdateStackSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the stack set, and associated stack instances in the specified accounts and Regions.
--
--
-- Even if the stack set operation created by updating the stack set fails (completely or partially, below or above a specified failure tolerance), the stack set is updated with your changes. Subsequent 'CreateStackInstances' calls on the specified stack set use the updated stack set.
module Network.AWS.CloudFormation.UpdateStackSet
  ( -- * Creating a Request
    updateStackSet,
    UpdateStackSet,

    -- * Request Lenses
    ussAdministrationRoleARN,
    ussUsePreviousTemplate,
    ussAccounts,
    ussRegions,
    ussAutoDeployment,
    ussPermissionModel,
    ussParameters,
    ussOperationPreferences,
    ussOperationId,
    ussTemplateBody,
    ussTemplateURL,
    ussDeploymentTargets,
    ussDescription,
    ussCapabilities,
    ussTags,
    ussExecutionRoleName,
    ussStackSetName,

    -- * Destructuring the Response
    updateStackSetResponse,
    UpdateStackSetResponse,

    -- * Response Lenses
    ussrsOperationId,
    ussrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateStackSet' smart constructor.
data UpdateStackSet = UpdateStackSet'
  { _ussAdministrationRoleARN ::
      !(Maybe Text),
    _ussUsePreviousTemplate :: !(Maybe Bool),
    _ussAccounts :: !(Maybe [Text]),
    _ussRegions :: !(Maybe [Text]),
    _ussAutoDeployment :: !(Maybe AutoDeployment),
    _ussPermissionModel :: !(Maybe PermissionModels),
    _ussParameters :: !(Maybe [Parameter]),
    _ussOperationPreferences ::
      !(Maybe StackSetOperationPreferences),
    _ussOperationId :: !(Maybe Text),
    _ussTemplateBody :: !(Maybe Text),
    _ussTemplateURL :: !(Maybe Text),
    _ussDeploymentTargets :: !(Maybe DeploymentTargets),
    _ussDescription :: !(Maybe Text),
    _ussCapabilities :: !(Maybe [Capability]),
    _ussTags :: !(Maybe [Tag]),
    _ussExecutionRoleName :: !(Maybe Text),
    _ussStackSetName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateStackSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ussAdministrationRoleARN' - The Amazon Resource Number (ARN) of the IAM role to use to update this stack set. Specify an IAM role only if you are using customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Granting Permissions for Stack Set Operations> in the /AWS CloudFormation User Guide/ . If you specified a customized administrator role when you created the stack set, you must specify a customized administrator role, even if it is the same customized administrator role used with this stack set previously.
--
-- * 'ussUsePreviousTemplate' - Use the existing template that's associated with the stack set that you're updating. Conditional: You must specify only one of the following parameters: @TemplateBody@ or @TemplateURL@ —or set @UsePreviousTemplate@ to true.
--
-- * 'ussAccounts' - [@Self-managed@ permissions] The accounts in which to update associated stack instances. If you specify accounts, you must also specify the Regions in which to update stack set instances. To update /all/ the stack instances associated with this stack set, do not specify the @Accounts@ or @Regions@ properties. If the stack set update includes changes to the template (that is, if the @TemplateBody@ or @TemplateURL@ properties are specified), or the @Parameters@ property, AWS CloudFormation marks all stack instances with a status of @OUTDATED@ prior to updating the stack instances in the specified accounts and Regions. If the stack set update does not include changes to the template or parameters, AWS CloudFormation updates the stack instances in the specified accounts and Regions, while leaving all other stack instances with their existing stack instance status.
--
-- * 'ussRegions' - The Regions in which to update associated stack instances. If you specify Regions, you must also specify accounts in which to update stack set instances. To update /all/ the stack instances associated with this stack set, do not specify the @Accounts@ or @Regions@ properties. If the stack set update includes changes to the template (that is, if the @TemplateBody@ or @TemplateURL@ properties are specified), or the @Parameters@ property, AWS CloudFormation marks all stack instances with a status of @OUTDATED@ prior to updating the stack instances in the specified accounts and Regions. If the stack set update does not include changes to the template or parameters, AWS CloudFormation updates the stack instances in the specified accounts and Regions, while leaving all other stack instances with their existing stack instance status.
--
-- * 'ussAutoDeployment' - [@Service-managed@ permissions] Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to a target organization or organizational unit (OU). If you specify @AutoDeployment@ , do not specify @DeploymentTargets@ or @Regions@ .
--
-- * 'ussPermissionModel' - Describes how the IAM roles required for stack set operations are created. You cannot modify @PermissionModel@ if there are stack instances associated with your stack set.     * With @self-managed@ permissions, you must create the administrator and execution roles required to deploy to target accounts. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions> .     * With @service-managed@ permissions, StackSets automatically creates the IAM roles required to deploy to accounts managed by AWS Organizations. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions> .
--
-- * 'ussParameters' - A list of input parameters for the stack set template.
--
-- * 'ussOperationPreferences' - Preferences for how AWS CloudFormation performs this stack set operation.
--
-- * 'ussOperationId' - The unique ID for this stack set operation.  The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You might retry stack set operation requests to ensure that AWS CloudFormation successfully received them. If you don't specify an operation ID, AWS CloudFormation generates one automatically. Repeating this stack set operation with a new operation ID retries all stack instances whose status is @OUTDATED@ .
--
-- * 'ussTemplateBody' - The structure that contains the template body, with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide. Conditional: You must specify only one of the following parameters: @TemplateBody@ or @TemplateURL@ —or set @UsePreviousTemplate@ to true.
--
-- * 'ussTemplateURL' - The location of the file that contains the template body. The URL must point to a template (maximum size: 460,800 bytes) that is located in an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide. Conditional: You must specify only one of the following parameters: @TemplateBody@ or @TemplateURL@ —or set @UsePreviousTemplate@ to true.
--
-- * 'ussDeploymentTargets' - [@Service-managed@ permissions] The AWS Organizations accounts in which to update associated stack instances. To update all the stack instances associated with this stack set, do not specify @DeploymentTargets@ or @Regions@ . If the stack set update includes changes to the template (that is, if @TemplateBody@ or @TemplateURL@ is specified), or the @Parameters@ , AWS CloudFormation marks all stack instances with a status of @OUTDATED@ prior to updating the stack instances in the specified accounts and Regions. If the stack set update does not include changes to the template or parameters, AWS CloudFormation updates the stack instances in the specified accounts and Regions, while leaving all other stack instances with their existing stack instance status.
--
-- * 'ussDescription' - A brief description of updates that you are making.
--
-- * 'ussCapabilities' - In some cases, you must explicitly acknowledge that your stack template contains certain capabilities in order for AWS CloudFormation to update the stack set and its associated stack instances.     * @CAPABILITY_IAM@ and @CAPABILITY_NAMED_IAM@  Some stack templates might include resources that can affect permissions in your AWS account; for example, by creating new AWS Identity and Access Management (IAM) users. For those stacks sets, you must explicitly acknowledge this by specifying one of these capabilities. The following IAM resources require you to specify either the @CAPABILITY_IAM@ or @CAPABILITY_NAMED_IAM@ capability.     * If you have IAM resources, you can specify either capability.      * If you have IAM resources with custom names, you /must/ specify @CAPABILITY_NAMED_IAM@ .      * If you don't specify either of these capabilities, AWS CloudFormation returns an @InsufficientCapabilities@ error. If your stack template contains these resources, we recommend that you review all permissions associated with them and edit their permissions if necessary.     * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-accesskey.html AWS::IAM::AccessKey>      * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-group.html AWS::IAM::Group>      * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-instanceprofile.html AWS::IAM::InstanceProfile>      * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-policy.html AWS::IAM::Policy>      * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-role.html AWS::IAM::Role>      * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-user.html AWS::IAM::User>      * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-addusertogroup.html AWS::IAM::UserToGroupAddition>  For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates> .     * @CAPABILITY_AUTO_EXPAND@  Some templates contain macros. If your stack template contains one or more macros, and you choose to update a stack directly from the processed template, without first reviewing the resulting changes in a change set, you must acknowledge this capability. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-macros.html Using AWS CloudFormation Macros to Perform Custom Processing on Templates> . /Important:/ Stack sets do not currently support macros in stack templates. (This includes the <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/create-reusable-transform-function-snippets-and-add-to-your-template-with-aws-include-transform.html AWS::Include> and <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/transform-aws-serverless.html AWS::Serverless> transforms, which are macros hosted by AWS CloudFormation.) Even if you specify this capability, if you include a macro in your template the stack set operation will fail.
--
-- * 'ussTags' - The key-value pairs to associate with this stack set and the stacks created from it. AWS CloudFormation also propagates these tags to supported resources that are created in the stacks. You can specify a maximum number of 50 tags. If you specify tags for this parameter, those tags replace any list of tags that are currently associated with this stack set. This means:     * If you don't specify this parameter, AWS CloudFormation doesn't modify the stack's tags.      * If you specify /any/ tags using this parameter, you must specify /all/ the tags that you want associated with this stack set, even tags you've specifed before (for example, when creating the stack set or during a previous update of the stack set.). Any tags that you don't include in the updated list of tags are removed from the stack set, and therefore from the stacks and resources as well.      * If you specify an empty value, AWS CloudFormation removes all currently associated tags. If you specify new tags as part of an @UpdateStackSet@ action, AWS CloudFormation checks to see if you have the required IAM permission to tag resources. If you omit tags that are currently associated with the stack set from the list of tags you specify, AWS CloudFormation assumes that you want to remove those tags from the stack set, and checks to see if you have permission to untag resources. If you don't have the necessary permission(s), the entire @UpdateStackSet@ action fails with an @access denied@ error, and the stack set is not updated.
--
-- * 'ussExecutionRoleName' - The name of the IAM execution role to use to update the stack set. If you do not specify an execution role, AWS CloudFormation uses the @AWSCloudFormationStackSetExecutionRole@ role for the stack set operation. Specify an IAM role only if you are using customized execution roles to control which stack resources users and groups can include in their stack sets.  If you specify a customized execution role, AWS CloudFormation uses that role to update the stack. If you do not specify a customized execution role, AWS CloudFormation performs the update using the role previously associated with the stack set, so long as you have permissions to perform operations on the stack set.
--
-- * 'ussStackSetName' - The name or unique ID of the stack set that you want to update.
updateStackSet ::
  -- | 'ussStackSetName'
  Text ->
  UpdateStackSet
updateStackSet pStackSetName_ =
  UpdateStackSet'
    { _ussAdministrationRoleARN = Nothing,
      _ussUsePreviousTemplate = Nothing,
      _ussAccounts = Nothing,
      _ussRegions = Nothing,
      _ussAutoDeployment = Nothing,
      _ussPermissionModel = Nothing,
      _ussParameters = Nothing,
      _ussOperationPreferences = Nothing,
      _ussOperationId = Nothing,
      _ussTemplateBody = Nothing,
      _ussTemplateURL = Nothing,
      _ussDeploymentTargets = Nothing,
      _ussDescription = Nothing,
      _ussCapabilities = Nothing,
      _ussTags = Nothing,
      _ussExecutionRoleName = Nothing,
      _ussStackSetName = pStackSetName_
    }

-- | The Amazon Resource Number (ARN) of the IAM role to use to update this stack set. Specify an IAM role only if you are using customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Granting Permissions for Stack Set Operations> in the /AWS CloudFormation User Guide/ . If you specified a customized administrator role when you created the stack set, you must specify a customized administrator role, even if it is the same customized administrator role used with this stack set previously.
ussAdministrationRoleARN :: Lens' UpdateStackSet (Maybe Text)
ussAdministrationRoleARN = lens _ussAdministrationRoleARN (\s a -> s {_ussAdministrationRoleARN = a})

-- | Use the existing template that's associated with the stack set that you're updating. Conditional: You must specify only one of the following parameters: @TemplateBody@ or @TemplateURL@ —or set @UsePreviousTemplate@ to true.
ussUsePreviousTemplate :: Lens' UpdateStackSet (Maybe Bool)
ussUsePreviousTemplate = lens _ussUsePreviousTemplate (\s a -> s {_ussUsePreviousTemplate = a})

-- | [@Self-managed@ permissions] The accounts in which to update associated stack instances. If you specify accounts, you must also specify the Regions in which to update stack set instances. To update /all/ the stack instances associated with this stack set, do not specify the @Accounts@ or @Regions@ properties. If the stack set update includes changes to the template (that is, if the @TemplateBody@ or @TemplateURL@ properties are specified), or the @Parameters@ property, AWS CloudFormation marks all stack instances with a status of @OUTDATED@ prior to updating the stack instances in the specified accounts and Regions. If the stack set update does not include changes to the template or parameters, AWS CloudFormation updates the stack instances in the specified accounts and Regions, while leaving all other stack instances with their existing stack instance status.
ussAccounts :: Lens' UpdateStackSet [Text]
ussAccounts = lens _ussAccounts (\s a -> s {_ussAccounts = a}) . _Default . _Coerce

-- | The Regions in which to update associated stack instances. If you specify Regions, you must also specify accounts in which to update stack set instances. To update /all/ the stack instances associated with this stack set, do not specify the @Accounts@ or @Regions@ properties. If the stack set update includes changes to the template (that is, if the @TemplateBody@ or @TemplateURL@ properties are specified), or the @Parameters@ property, AWS CloudFormation marks all stack instances with a status of @OUTDATED@ prior to updating the stack instances in the specified accounts and Regions. If the stack set update does not include changes to the template or parameters, AWS CloudFormation updates the stack instances in the specified accounts and Regions, while leaving all other stack instances with their existing stack instance status.
ussRegions :: Lens' UpdateStackSet [Text]
ussRegions = lens _ussRegions (\s a -> s {_ussRegions = a}) . _Default . _Coerce

-- | [@Service-managed@ permissions] Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to a target organization or organizational unit (OU). If you specify @AutoDeployment@ , do not specify @DeploymentTargets@ or @Regions@ .
ussAutoDeployment :: Lens' UpdateStackSet (Maybe AutoDeployment)
ussAutoDeployment = lens _ussAutoDeployment (\s a -> s {_ussAutoDeployment = a})

-- | Describes how the IAM roles required for stack set operations are created. You cannot modify @PermissionModel@ if there are stack instances associated with your stack set.     * With @self-managed@ permissions, you must create the administrator and execution roles required to deploy to target accounts. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions> .     * With @service-managed@ permissions, StackSets automatically creates the IAM roles required to deploy to accounts managed by AWS Organizations. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions> .
ussPermissionModel :: Lens' UpdateStackSet (Maybe PermissionModels)
ussPermissionModel = lens _ussPermissionModel (\s a -> s {_ussPermissionModel = a})

-- | A list of input parameters for the stack set template.
ussParameters :: Lens' UpdateStackSet [Parameter]
ussParameters = lens _ussParameters (\s a -> s {_ussParameters = a}) . _Default . _Coerce

-- | Preferences for how AWS CloudFormation performs this stack set operation.
ussOperationPreferences :: Lens' UpdateStackSet (Maybe StackSetOperationPreferences)
ussOperationPreferences = lens _ussOperationPreferences (\s a -> s {_ussOperationPreferences = a})

-- | The unique ID for this stack set operation.  The operation ID also functions as an idempotency token, to ensure that AWS CloudFormation performs the stack set operation only once, even if you retry the request multiple times. You might retry stack set operation requests to ensure that AWS CloudFormation successfully received them. If you don't specify an operation ID, AWS CloudFormation generates one automatically. Repeating this stack set operation with a new operation ID retries all stack instances whose status is @OUTDATED@ .
ussOperationId :: Lens' UpdateStackSet (Maybe Text)
ussOperationId = lens _ussOperationId (\s a -> s {_ussOperationId = a})

-- | The structure that contains the template body, with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide. Conditional: You must specify only one of the following parameters: @TemplateBody@ or @TemplateURL@ —or set @UsePreviousTemplate@ to true.
ussTemplateBody :: Lens' UpdateStackSet (Maybe Text)
ussTemplateBody = lens _ussTemplateBody (\s a -> s {_ussTemplateBody = a})

-- | The location of the file that contains the template body. The URL must point to a template (maximum size: 460,800 bytes) that is located in an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide. Conditional: You must specify only one of the following parameters: @TemplateBody@ or @TemplateURL@ —or set @UsePreviousTemplate@ to true.
ussTemplateURL :: Lens' UpdateStackSet (Maybe Text)
ussTemplateURL = lens _ussTemplateURL (\s a -> s {_ussTemplateURL = a})

-- | [@Service-managed@ permissions] The AWS Organizations accounts in which to update associated stack instances. To update all the stack instances associated with this stack set, do not specify @DeploymentTargets@ or @Regions@ . If the stack set update includes changes to the template (that is, if @TemplateBody@ or @TemplateURL@ is specified), or the @Parameters@ , AWS CloudFormation marks all stack instances with a status of @OUTDATED@ prior to updating the stack instances in the specified accounts and Regions. If the stack set update does not include changes to the template or parameters, AWS CloudFormation updates the stack instances in the specified accounts and Regions, while leaving all other stack instances with their existing stack instance status.
ussDeploymentTargets :: Lens' UpdateStackSet (Maybe DeploymentTargets)
ussDeploymentTargets = lens _ussDeploymentTargets (\s a -> s {_ussDeploymentTargets = a})

-- | A brief description of updates that you are making.
ussDescription :: Lens' UpdateStackSet (Maybe Text)
ussDescription = lens _ussDescription (\s a -> s {_ussDescription = a})

-- | In some cases, you must explicitly acknowledge that your stack template contains certain capabilities in order for AWS CloudFormation to update the stack set and its associated stack instances.     * @CAPABILITY_IAM@ and @CAPABILITY_NAMED_IAM@  Some stack templates might include resources that can affect permissions in your AWS account; for example, by creating new AWS Identity and Access Management (IAM) users. For those stacks sets, you must explicitly acknowledge this by specifying one of these capabilities. The following IAM resources require you to specify either the @CAPABILITY_IAM@ or @CAPABILITY_NAMED_IAM@ capability.     * If you have IAM resources, you can specify either capability.      * If you have IAM resources with custom names, you /must/ specify @CAPABILITY_NAMED_IAM@ .      * If you don't specify either of these capabilities, AWS CloudFormation returns an @InsufficientCapabilities@ error. If your stack template contains these resources, we recommend that you review all permissions associated with them and edit their permissions if necessary.     * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-accesskey.html AWS::IAM::AccessKey>      * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-group.html AWS::IAM::Group>      * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-instanceprofile.html AWS::IAM::InstanceProfile>      * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-policy.html AWS::IAM::Policy>      * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-role.html AWS::IAM::Role>      * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-user.html AWS::IAM::User>      * <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-addusertogroup.html AWS::IAM::UserToGroupAddition>  For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates> .     * @CAPABILITY_AUTO_EXPAND@  Some templates contain macros. If your stack template contains one or more macros, and you choose to update a stack directly from the processed template, without first reviewing the resulting changes in a change set, you must acknowledge this capability. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-macros.html Using AWS CloudFormation Macros to Perform Custom Processing on Templates> . /Important:/ Stack sets do not currently support macros in stack templates. (This includes the <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/create-reusable-transform-function-snippets-and-add-to-your-template-with-aws-include-transform.html AWS::Include> and <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/transform-aws-serverless.html AWS::Serverless> transforms, which are macros hosted by AWS CloudFormation.) Even if you specify this capability, if you include a macro in your template the stack set operation will fail.
ussCapabilities :: Lens' UpdateStackSet [Capability]
ussCapabilities = lens _ussCapabilities (\s a -> s {_ussCapabilities = a}) . _Default . _Coerce

-- | The key-value pairs to associate with this stack set and the stacks created from it. AWS CloudFormation also propagates these tags to supported resources that are created in the stacks. You can specify a maximum number of 50 tags. If you specify tags for this parameter, those tags replace any list of tags that are currently associated with this stack set. This means:     * If you don't specify this parameter, AWS CloudFormation doesn't modify the stack's tags.      * If you specify /any/ tags using this parameter, you must specify /all/ the tags that you want associated with this stack set, even tags you've specifed before (for example, when creating the stack set or during a previous update of the stack set.). Any tags that you don't include in the updated list of tags are removed from the stack set, and therefore from the stacks and resources as well.      * If you specify an empty value, AWS CloudFormation removes all currently associated tags. If you specify new tags as part of an @UpdateStackSet@ action, AWS CloudFormation checks to see if you have the required IAM permission to tag resources. If you omit tags that are currently associated with the stack set from the list of tags you specify, AWS CloudFormation assumes that you want to remove those tags from the stack set, and checks to see if you have permission to untag resources. If you don't have the necessary permission(s), the entire @UpdateStackSet@ action fails with an @access denied@ error, and the stack set is not updated.
ussTags :: Lens' UpdateStackSet [Tag]
ussTags = lens _ussTags (\s a -> s {_ussTags = a}) . _Default . _Coerce

-- | The name of the IAM execution role to use to update the stack set. If you do not specify an execution role, AWS CloudFormation uses the @AWSCloudFormationStackSetExecutionRole@ role for the stack set operation. Specify an IAM role only if you are using customized execution roles to control which stack resources users and groups can include in their stack sets.  If you specify a customized execution role, AWS CloudFormation uses that role to update the stack. If you do not specify a customized execution role, AWS CloudFormation performs the update using the role previously associated with the stack set, so long as you have permissions to perform operations on the stack set.
ussExecutionRoleName :: Lens' UpdateStackSet (Maybe Text)
ussExecutionRoleName = lens _ussExecutionRoleName (\s a -> s {_ussExecutionRoleName = a})

-- | The name or unique ID of the stack set that you want to update.
ussStackSetName :: Lens' UpdateStackSet Text
ussStackSetName = lens _ussStackSetName (\s a -> s {_ussStackSetName = a})

instance AWSRequest UpdateStackSet where
  type Rs UpdateStackSet = UpdateStackSetResponse
  request = postQuery cloudFormation
  response =
    receiveXMLWrapper
      "UpdateStackSetResult"
      ( \s h x ->
          UpdateStackSetResponse'
            <$> (x .@? "OperationId") <*> (pure (fromEnum s))
      )

instance Hashable UpdateStackSet

instance NFData UpdateStackSet

instance ToHeaders UpdateStackSet where
  toHeaders = const mempty

instance ToPath UpdateStackSet where
  toPath = const "/"

instance ToQuery UpdateStackSet where
  toQuery UpdateStackSet' {..} =
    mconcat
      [ "Action" =: ("UpdateStackSet" :: ByteString),
        "Version" =: ("2010-05-15" :: ByteString),
        "AdministrationRoleARN" =: _ussAdministrationRoleARN,
        "UsePreviousTemplate" =: _ussUsePreviousTemplate,
        "Accounts" =: toQuery (toQueryList "member" <$> _ussAccounts),
        "Regions" =: toQuery (toQueryList "member" <$> _ussRegions),
        "AutoDeployment" =: _ussAutoDeployment,
        "PermissionModel" =: _ussPermissionModel,
        "Parameters" =: toQuery (toQueryList "member" <$> _ussParameters),
        "OperationPreferences" =: _ussOperationPreferences,
        "OperationId" =: _ussOperationId,
        "TemplateBody" =: _ussTemplateBody,
        "TemplateURL" =: _ussTemplateURL,
        "DeploymentTargets" =: _ussDeploymentTargets,
        "Description" =: _ussDescription,
        "Capabilities"
          =: toQuery (toQueryList "member" <$> _ussCapabilities),
        "Tags" =: toQuery (toQueryList "member" <$> _ussTags),
        "ExecutionRoleName" =: _ussExecutionRoleName,
        "StackSetName" =: _ussStackSetName
      ]

-- | /See:/ 'updateStackSetResponse' smart constructor.
data UpdateStackSetResponse = UpdateStackSetResponse'
  { _ussrsOperationId ::
      !(Maybe Text),
    _ussrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateStackSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ussrsOperationId' - The unique ID for this stack set operation.
--
-- * 'ussrsResponseStatus' - -- | The response status code.
updateStackSetResponse ::
  -- | 'ussrsResponseStatus'
  Int ->
  UpdateStackSetResponse
updateStackSetResponse pResponseStatus_ =
  UpdateStackSetResponse'
    { _ussrsOperationId = Nothing,
      _ussrsResponseStatus = pResponseStatus_
    }

-- | The unique ID for this stack set operation.
ussrsOperationId :: Lens' UpdateStackSetResponse (Maybe Text)
ussrsOperationId = lens _ussrsOperationId (\s a -> s {_ussrsOperationId = a})

-- | -- | The response status code.
ussrsResponseStatus :: Lens' UpdateStackSetResponse Int
ussrsResponseStatus = lens _ussrsResponseStatus (\s a -> s {_ussrsResponseStatus = a})

instance NFData UpdateStackSetResponse
