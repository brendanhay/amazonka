{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.UpdateStack
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a stack as specified in the template. After the call completes successfully, the stack update starts. You can check the status of the stack via the 'DescribeStacks' action.
--
--
-- To get a copy of the template for an existing stack, you can use the 'GetTemplate' action.
--
-- For more information about creating an update template, updating a stack, and monitoring the progress of the update, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-updating-stacks.html Updating a Stack> .
--
module Network.AWS.CloudFormation.UpdateStack
    (
    -- * Creating a Request
      updateStack
    , UpdateStack
    -- * Request Lenses
    , usUsePreviousTemplate
    , usNotificationARNs
    , usStackPolicyBody
    , usStackPolicyDuringUpdateBody
    , usStackPolicyDuringUpdateURL
    , usParameters
    , usStackPolicyURL
    , usTemplateBody
    , usTemplateURL
    , usClientRequestToken
    , usCapabilities
    , usRollbackConfiguration
    , usResourceTypes
    , usTags
    , usRoleARN
    , usStackName

    -- * Destructuring the Response
    , updateStackResponse
    , UpdateStackResponse
    -- * Response Lenses
    , usrsStackId
    , usrsResponseStatus
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for an 'UpdateStack' action.
--
--
--
-- /See:/ 'updateStack' smart constructor.
data UpdateStack = UpdateStack'
  { _usUsePreviousTemplate         :: !(Maybe Bool)
  , _usNotificationARNs            :: !(Maybe [Text])
  , _usStackPolicyBody             :: !(Maybe Text)
  , _usStackPolicyDuringUpdateBody :: !(Maybe Text)
  , _usStackPolicyDuringUpdateURL  :: !(Maybe Text)
  , _usParameters                  :: !(Maybe [Parameter])
  , _usStackPolicyURL              :: !(Maybe Text)
  , _usTemplateBody                :: !(Maybe Text)
  , _usTemplateURL                 :: !(Maybe Text)
  , _usClientRequestToken          :: !(Maybe Text)
  , _usCapabilities                :: !(Maybe [Capability])
  , _usRollbackConfiguration       :: !(Maybe RollbackConfiguration)
  , _usResourceTypes               :: !(Maybe [Text])
  , _usTags                        :: !(Maybe [Tag])
  , _usRoleARN                     :: !(Maybe Text)
  , _usStackName                   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateStack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usUsePreviousTemplate' - Reuse the existing template that is associated with the stack that you are updating. Conditional: You must specify only one of the following parameters: @TemplateBody@ , @TemplateURL@ , or set the @UsePreviousTemplate@ to @true@ .
--
-- * 'usNotificationARNs' - Amazon Simple Notification Service topic Amazon Resource Names (ARNs) that AWS CloudFormation associates with the stack. Specify an empty list to remove all notification topics.
--
-- * 'usStackPolicyBody' - Structure containing a new stack policy body. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both. You might update the stack policy, for example, in order to protect a new resource that you created during a stack update. If you do not specify a stack policy, the current policy that is associated with the stack is unchanged.
--
-- * 'usStackPolicyDuringUpdateBody' - Structure containing the temporary overriding stack policy body. You can specify either the @StackPolicyDuringUpdateBody@ or the @StackPolicyDuringUpdateURL@ parameter, but not both. If you want to update protected resources, specify a temporary overriding stack policy during this update. If you do not specify a stack policy, the current policy that is associated with the stack will be used.
--
-- * 'usStackPolicyDuringUpdateURL' - Location of a file containing the temporary overriding stack policy. The URL must point to a policy (max size: 16KB) located in an S3 bucket in the same region as the stack. You can specify either the @StackPolicyDuringUpdateBody@ or the @StackPolicyDuringUpdateURL@ parameter, but not both. If you want to update protected resources, specify a temporary overriding stack policy during this update. If you do not specify a stack policy, the current policy that is associated with the stack will be used.
--
-- * 'usParameters' - A list of @Parameter@ structures that specify input parameters for the stack. For more information, see the <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter> data type.
--
-- * 'usStackPolicyURL' - Location of a file containing the updated stack policy. The URL must point to a policy (max size: 16KB) located in an S3 bucket in the same region as the stack. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both. You might update the stack policy, for example, in order to protect a new resource that you created during a stack update. If you do not specify a stack policy, the current policy that is associated with the stack is unchanged.
--
-- * 'usTemplateBody' - Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes. (For more information, go to <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.) Conditional: You must specify only one of the following parameters: @TemplateBody@ , @TemplateURL@ , or set the @UsePreviousTemplate@ to @true@ .
--
-- * 'usTemplateURL' - Location of file containing the template body. The URL must point to a template that is located in an Amazon S3 bucket. For more information, go to <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide. Conditional: You must specify only one of the following parameters: @TemplateBody@ , @TemplateURL@ , or set the @UsePreviousTemplate@ to @true@ .
--
-- * 'usClientRequestToken' - A unique identifier for this @UpdateStack@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to update a stack with the same name. You might retry @UpdateStack@ requests to ensure that AWS CloudFormation successfully received them. All events triggered by a given stack operation are assigned the same client request token, which you can use to track operations. For example, if you execute a @CreateStack@ operation with the token @token1@ , then all the @StackEvents@ generated by that operation will have @ClientRequestToken@ set as @token1@ . In the console, stack operations display the client request token on the Events tab. Stack operations that are initiated from the console use the token format /Console-StackOperation-ID/ , which helps you easily identify the stack operation . For example, if you create a stack using the console, each stack event would be assigned the same token in the following format: @Console-CreateStack-7f59c3cf-00d2-40c7-b2ff-e75db0987002@ .
--
-- * 'usCapabilities' - A list of values that you must specify before AWS CloudFormation can update certain stacks. Some stack templates might include resources that can affect permissions in your AWS account, for example, by creating new AWS Identity and Access Management (IAM) users. For those stacks, you must explicitly acknowledge their capabilities by specifying this parameter. The only valid values are @CAPABILITY_IAM@ and @CAPABILITY_NAMED_IAM@ . The following resources require you to specify this parameter: <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-accesskey.html AWS::IAM::AccessKey> , <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-group.html AWS::IAM::Group> , <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-instanceprofile.html AWS::IAM::InstanceProfile> , <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-policy.html AWS::IAM::Policy> , <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-role.html AWS::IAM::Role> , <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-user.html AWS::IAM::User> , and <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-addusertogroup.html AWS::IAM::UserToGroupAddition> . If your stack template contains these resources, we recommend that you review all permissions associated with them and edit their permissions if necessary. If you have IAM resources, you can specify either capability. If you have IAM resources with custom names, you must specify @CAPABILITY_NAMED_IAM@ . If you don't specify this parameter, this action returns an @InsufficientCapabilities@ error. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates> .
--
-- * 'usRollbackConfiguration' - The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
--
-- * 'usResourceTypes' - The template resource types that you have permissions to work with for this update stack action, such as @AWS::EC2::Instance@ , @AWS::EC2::*@ , or @Custom::MyCustomInstance@ . If the list of resource types doesn't include a resource that you're updating, the stack update fails. By default, AWS CloudFormation grants permissions to all resource types. AWS Identity and Access Management (IAM) uses this parameter for AWS CloudFormation-specific condition keys in IAM policies. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html Controlling Access with AWS Identity and Access Management> .
--
-- * 'usTags' - Key-value pairs to associate with this stack. AWS CloudFormation also propagates these tags to supported resources in the stack. You can specify a maximum number of 50 tags. If you don't specify this parameter, AWS CloudFormation doesn't modify the stack's tags. If you specify an empty value, AWS CloudFormation removes all associated tags.
--
-- * 'usRoleARN' - The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that AWS CloudFormation assumes to update the stack. AWS CloudFormation uses the role's credentials to make calls on your behalf. AWS CloudFormation always uses this role for all future operations on the stack. As long as users have permission to operate on the stack, AWS CloudFormation uses this role even if the users don't have permission to pass it. Ensure that the role grants least privilege. If you don't specify a value, AWS CloudFormation uses the role that was previously associated with the stack. If no role is available, AWS CloudFormation uses a temporary session that is generated from your user credentials.
--
-- * 'usStackName' - The name or unique stack ID of the stack to update.
updateStack
    :: Text -- ^ 'usStackName'
    -> UpdateStack
updateStack pStackName_ =
  UpdateStack'
    { _usUsePreviousTemplate = Nothing
    , _usNotificationARNs = Nothing
    , _usStackPolicyBody = Nothing
    , _usStackPolicyDuringUpdateBody = Nothing
    , _usStackPolicyDuringUpdateURL = Nothing
    , _usParameters = Nothing
    , _usStackPolicyURL = Nothing
    , _usTemplateBody = Nothing
    , _usTemplateURL = Nothing
    , _usClientRequestToken = Nothing
    , _usCapabilities = Nothing
    , _usRollbackConfiguration = Nothing
    , _usResourceTypes = Nothing
    , _usTags = Nothing
    , _usRoleARN = Nothing
    , _usStackName = pStackName_
    }


-- | Reuse the existing template that is associated with the stack that you are updating. Conditional: You must specify only one of the following parameters: @TemplateBody@ , @TemplateURL@ , or set the @UsePreviousTemplate@ to @true@ .
usUsePreviousTemplate :: Lens' UpdateStack (Maybe Bool)
usUsePreviousTemplate = lens _usUsePreviousTemplate (\ s a -> s{_usUsePreviousTemplate = a})

-- | Amazon Simple Notification Service topic Amazon Resource Names (ARNs) that AWS CloudFormation associates with the stack. Specify an empty list to remove all notification topics.
usNotificationARNs :: Lens' UpdateStack [Text]
usNotificationARNs = lens _usNotificationARNs (\ s a -> s{_usNotificationARNs = a}) . _Default . _Coerce

-- | Structure containing a new stack policy body. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both. You might update the stack policy, for example, in order to protect a new resource that you created during a stack update. If you do not specify a stack policy, the current policy that is associated with the stack is unchanged.
usStackPolicyBody :: Lens' UpdateStack (Maybe Text)
usStackPolicyBody = lens _usStackPolicyBody (\ s a -> s{_usStackPolicyBody = a})

-- | Structure containing the temporary overriding stack policy body. You can specify either the @StackPolicyDuringUpdateBody@ or the @StackPolicyDuringUpdateURL@ parameter, but not both. If you want to update protected resources, specify a temporary overriding stack policy during this update. If you do not specify a stack policy, the current policy that is associated with the stack will be used.
usStackPolicyDuringUpdateBody :: Lens' UpdateStack (Maybe Text)
usStackPolicyDuringUpdateBody = lens _usStackPolicyDuringUpdateBody (\ s a -> s{_usStackPolicyDuringUpdateBody = a})

-- | Location of a file containing the temporary overriding stack policy. The URL must point to a policy (max size: 16KB) located in an S3 bucket in the same region as the stack. You can specify either the @StackPolicyDuringUpdateBody@ or the @StackPolicyDuringUpdateURL@ parameter, but not both. If you want to update protected resources, specify a temporary overriding stack policy during this update. If you do not specify a stack policy, the current policy that is associated with the stack will be used.
usStackPolicyDuringUpdateURL :: Lens' UpdateStack (Maybe Text)
usStackPolicyDuringUpdateURL = lens _usStackPolicyDuringUpdateURL (\ s a -> s{_usStackPolicyDuringUpdateURL = a})

-- | A list of @Parameter@ structures that specify input parameters for the stack. For more information, see the <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter> data type.
usParameters :: Lens' UpdateStack [Parameter]
usParameters = lens _usParameters (\ s a -> s{_usParameters = a}) . _Default . _Coerce

-- | Location of a file containing the updated stack policy. The URL must point to a policy (max size: 16KB) located in an S3 bucket in the same region as the stack. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both. You might update the stack policy, for example, in order to protect a new resource that you created during a stack update. If you do not specify a stack policy, the current policy that is associated with the stack is unchanged.
usStackPolicyURL :: Lens' UpdateStack (Maybe Text)
usStackPolicyURL = lens _usStackPolicyURL (\ s a -> s{_usStackPolicyURL = a})

-- | Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes. (For more information, go to <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.) Conditional: You must specify only one of the following parameters: @TemplateBody@ , @TemplateURL@ , or set the @UsePreviousTemplate@ to @true@ .
usTemplateBody :: Lens' UpdateStack (Maybe Text)
usTemplateBody = lens _usTemplateBody (\ s a -> s{_usTemplateBody = a})

-- | Location of file containing the template body. The URL must point to a template that is located in an Amazon S3 bucket. For more information, go to <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide. Conditional: You must specify only one of the following parameters: @TemplateBody@ , @TemplateURL@ , or set the @UsePreviousTemplate@ to @true@ .
usTemplateURL :: Lens' UpdateStack (Maybe Text)
usTemplateURL = lens _usTemplateURL (\ s a -> s{_usTemplateURL = a})

-- | A unique identifier for this @UpdateStack@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to update a stack with the same name. You might retry @UpdateStack@ requests to ensure that AWS CloudFormation successfully received them. All events triggered by a given stack operation are assigned the same client request token, which you can use to track operations. For example, if you execute a @CreateStack@ operation with the token @token1@ , then all the @StackEvents@ generated by that operation will have @ClientRequestToken@ set as @token1@ . In the console, stack operations display the client request token on the Events tab. Stack operations that are initiated from the console use the token format /Console-StackOperation-ID/ , which helps you easily identify the stack operation . For example, if you create a stack using the console, each stack event would be assigned the same token in the following format: @Console-CreateStack-7f59c3cf-00d2-40c7-b2ff-e75db0987002@ .
usClientRequestToken :: Lens' UpdateStack (Maybe Text)
usClientRequestToken = lens _usClientRequestToken (\ s a -> s{_usClientRequestToken = a})

-- | A list of values that you must specify before AWS CloudFormation can update certain stacks. Some stack templates might include resources that can affect permissions in your AWS account, for example, by creating new AWS Identity and Access Management (IAM) users. For those stacks, you must explicitly acknowledge their capabilities by specifying this parameter. The only valid values are @CAPABILITY_IAM@ and @CAPABILITY_NAMED_IAM@ . The following resources require you to specify this parameter: <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-accesskey.html AWS::IAM::AccessKey> , <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-group.html AWS::IAM::Group> , <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-instanceprofile.html AWS::IAM::InstanceProfile> , <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-policy.html AWS::IAM::Policy> , <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-role.html AWS::IAM::Role> , <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-user.html AWS::IAM::User> , and <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-addusertogroup.html AWS::IAM::UserToGroupAddition> . If your stack template contains these resources, we recommend that you review all permissions associated with them and edit their permissions if necessary. If you have IAM resources, you can specify either capability. If you have IAM resources with custom names, you must specify @CAPABILITY_NAMED_IAM@ . If you don't specify this parameter, this action returns an @InsufficientCapabilities@ error. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates> .
usCapabilities :: Lens' UpdateStack [Capability]
usCapabilities = lens _usCapabilities (\ s a -> s{_usCapabilities = a}) . _Default . _Coerce

-- | The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
usRollbackConfiguration :: Lens' UpdateStack (Maybe RollbackConfiguration)
usRollbackConfiguration = lens _usRollbackConfiguration (\ s a -> s{_usRollbackConfiguration = a})

-- | The template resource types that you have permissions to work with for this update stack action, such as @AWS::EC2::Instance@ , @AWS::EC2::*@ , or @Custom::MyCustomInstance@ . If the list of resource types doesn't include a resource that you're updating, the stack update fails. By default, AWS CloudFormation grants permissions to all resource types. AWS Identity and Access Management (IAM) uses this parameter for AWS CloudFormation-specific condition keys in IAM policies. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html Controlling Access with AWS Identity and Access Management> .
usResourceTypes :: Lens' UpdateStack [Text]
usResourceTypes = lens _usResourceTypes (\ s a -> s{_usResourceTypes = a}) . _Default . _Coerce

-- | Key-value pairs to associate with this stack. AWS CloudFormation also propagates these tags to supported resources in the stack. You can specify a maximum number of 50 tags. If you don't specify this parameter, AWS CloudFormation doesn't modify the stack's tags. If you specify an empty value, AWS CloudFormation removes all associated tags.
usTags :: Lens' UpdateStack [Tag]
usTags = lens _usTags (\ s a -> s{_usTags = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that AWS CloudFormation assumes to update the stack. AWS CloudFormation uses the role's credentials to make calls on your behalf. AWS CloudFormation always uses this role for all future operations on the stack. As long as users have permission to operate on the stack, AWS CloudFormation uses this role even if the users don't have permission to pass it. Ensure that the role grants least privilege. If you don't specify a value, AWS CloudFormation uses the role that was previously associated with the stack. If no role is available, AWS CloudFormation uses a temporary session that is generated from your user credentials.
usRoleARN :: Lens' UpdateStack (Maybe Text)
usRoleARN = lens _usRoleARN (\ s a -> s{_usRoleARN = a})

-- | The name or unique stack ID of the stack to update.
usStackName :: Lens' UpdateStack Text
usStackName = lens _usStackName (\ s a -> s{_usStackName = a})

instance AWSRequest UpdateStack where
        type Rs UpdateStack = UpdateStackResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "UpdateStackResult"
              (\ s h x ->
                 UpdateStackResponse' <$>
                   (x .@? "StackId") <*> (pure (fromEnum s)))

instance Hashable UpdateStack where

instance NFData UpdateStack where

instance ToHeaders UpdateStack where
        toHeaders = const mempty

instance ToPath UpdateStack where
        toPath = const "/"

instance ToQuery UpdateStack where
        toQuery UpdateStack'{..}
          = mconcat
              ["Action" =: ("UpdateStack" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "UsePreviousTemplate" =: _usUsePreviousTemplate,
               "NotificationARNs" =:
                 toQuery
                   (toQueryList "member" <$> _usNotificationARNs),
               "StackPolicyBody" =: _usStackPolicyBody,
               "StackPolicyDuringUpdateBody" =:
                 _usStackPolicyDuringUpdateBody,
               "StackPolicyDuringUpdateURL" =:
                 _usStackPolicyDuringUpdateURL,
               "Parameters" =:
                 toQuery (toQueryList "member" <$> _usParameters),
               "StackPolicyURL" =: _usStackPolicyURL,
               "TemplateBody" =: _usTemplateBody,
               "TemplateURL" =: _usTemplateURL,
               "ClientRequestToken" =: _usClientRequestToken,
               "Capabilities" =:
                 toQuery (toQueryList "member" <$> _usCapabilities),
               "RollbackConfiguration" =: _usRollbackConfiguration,
               "ResourceTypes" =:
                 toQuery (toQueryList "member" <$> _usResourceTypes),
               "Tags" =: toQuery (toQueryList "member" <$> _usTags),
               "RoleARN" =: _usRoleARN, "StackName" =: _usStackName]

-- | The output for an 'UpdateStack' action.
--
--
--
-- /See:/ 'updateStackResponse' smart constructor.
data UpdateStackResponse = UpdateStackResponse'
  { _usrsStackId        :: !(Maybe Text)
  , _usrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateStackResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usrsStackId' - Unique identifier of the stack.
--
-- * 'usrsResponseStatus' - -- | The response status code.
updateStackResponse
    :: Int -- ^ 'usrsResponseStatus'
    -> UpdateStackResponse
updateStackResponse pResponseStatus_ =
  UpdateStackResponse'
    {_usrsStackId = Nothing, _usrsResponseStatus = pResponseStatus_}


-- | Unique identifier of the stack.
usrsStackId :: Lens' UpdateStackResponse (Maybe Text)
usrsStackId = lens _usrsStackId (\ s a -> s{_usrsStackId = a})

-- | -- | The response status code.
usrsResponseStatus :: Lens' UpdateStackResponse Int
usrsResponseStatus = lens _usrsResponseStatus (\ s a -> s{_usrsResponseStatus = a})

instance NFData UpdateStackResponse where
