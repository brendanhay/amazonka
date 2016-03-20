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
-- Module      : Network.AWS.CloudFormation.CreateStack
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a stack as specified in the template. After the call completes
-- successfully, the stack creation starts. You can check the status of the
-- stack via the < DescribeStacks> API.
module Network.AWS.CloudFormation.CreateStack
    (
    -- * Creating a Request
      createStack
    , CreateStack
    -- * Request Lenses
    , csDisableRollback
    , csNotificationARNs
    , csStackPolicyBody
    , csParameters
    , csStackPolicyURL
    , csTemplateBody
    , csTemplateURL
    , csCapabilities
    , csOnFailure
    , csResourceTypes
    , csTags
    , csTimeoutInMinutes
    , csStackName

    -- * Destructuring the Response
    , createStackResponse
    , CreateStackResponse
    -- * Response Lenses
    , csrsStackId
    , csrsResponseStatus
    ) where

import           Network.AWS.CloudFormation.Types
import           Network.AWS.CloudFormation.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for < CreateStack> action.
--
-- /See:/ 'createStack' smart constructor.
data CreateStack = CreateStack'
    { _csDisableRollback  :: !(Maybe Bool)
    , _csNotificationARNs :: !(Maybe [Text])
    , _csStackPolicyBody  :: !(Maybe Text)
    , _csParameters       :: !(Maybe [Parameter])
    , _csStackPolicyURL   :: !(Maybe Text)
    , _csTemplateBody     :: !(Maybe Text)
    , _csTemplateURL      :: !(Maybe Text)
    , _csCapabilities     :: !(Maybe [Capability])
    , _csOnFailure        :: !(Maybe OnFailure)
    , _csResourceTypes    :: !(Maybe [Text])
    , _csTags             :: !(Maybe [Tag])
    , _csTimeoutInMinutes :: !(Maybe Nat)
    , _csStackName        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateStack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csDisableRollback'
--
-- * 'csNotificationARNs'
--
-- * 'csStackPolicyBody'
--
-- * 'csParameters'
--
-- * 'csStackPolicyURL'
--
-- * 'csTemplateBody'
--
-- * 'csTemplateURL'
--
-- * 'csCapabilities'
--
-- * 'csOnFailure'
--
-- * 'csResourceTypes'
--
-- * 'csTags'
--
-- * 'csTimeoutInMinutes'
--
-- * 'csStackName'
createStack
    :: Text -- ^ 'csStackName'
    -> CreateStack
createStack pStackName_ =
    CreateStack'
    { _csDisableRollback = Nothing
    , _csNotificationARNs = Nothing
    , _csStackPolicyBody = Nothing
    , _csParameters = Nothing
    , _csStackPolicyURL = Nothing
    , _csTemplateBody = Nothing
    , _csTemplateURL = Nothing
    , _csCapabilities = Nothing
    , _csOnFailure = Nothing
    , _csResourceTypes = Nothing
    , _csTags = Nothing
    , _csTimeoutInMinutes = Nothing
    , _csStackName = pStackName_
    }

-- | Set to 'true' to disable rollback of the stack if stack creation failed.
-- You can specify either 'DisableRollback' or 'OnFailure', but not both.
--
-- Default: 'false'
csDisableRollback :: Lens' CreateStack (Maybe Bool)
csDisableRollback = lens _csDisableRollback (\ s a -> s{_csDisableRollback = a});

-- | The Simple Notification Service (SNS) topic ARNs to publish stack
-- related events. You can find your SNS topic ARNs using the
-- <http://console.aws.amazon.com/sns SNS console> or your Command Line
-- Interface (CLI).
csNotificationARNs :: Lens' CreateStack [Text]
csNotificationARNs = lens _csNotificationARNs (\ s a -> s{_csNotificationARNs = a}) . _Default . _Coerce;

-- | Structure containing the stack policy body. For more information, go to
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources>
-- in the AWS CloudFormation User Guide. You can specify either the
-- 'StackPolicyBody' or the 'StackPolicyURL' parameter, but not both.
csStackPolicyBody :: Lens' CreateStack (Maybe Text)
csStackPolicyBody = lens _csStackPolicyBody (\ s a -> s{_csStackPolicyBody = a});

-- | A list of 'Parameter' structures that specify input parameters for the
-- stack. For more information, see the
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter>
-- data type.
csParameters :: Lens' CreateStack [Parameter]
csParameters = lens _csParameters (\ s a -> s{_csParameters = a}) . _Default . _Coerce;

-- | Location of a file containing the stack policy. The URL must point to a
-- policy (max size: 16KB) located in an S3 bucket in the same region as
-- the stack. You can specify either the 'StackPolicyBody' or the
-- 'StackPolicyURL' parameter, but not both.
csStackPolicyURL :: Lens' CreateStack (Maybe Text)
csStackPolicyURL = lens _csStackPolicyURL (\ s a -> s{_csStackPolicyURL = a});

-- | Structure containing the template body with a minimum length of 1 byte
-- and a maximum length of 51,200 bytes. For more information, go to
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify either the 'TemplateBody' or the
-- 'TemplateURL' parameter, but not both.
csTemplateBody :: Lens' CreateStack (Maybe Text)
csTemplateBody = lens _csTemplateBody (\ s a -> s{_csTemplateBody = a});

-- | Location of file containing the template body. The URL must point to a
-- template (max size: 460,800 bytes) that is located in an Amazon S3
-- bucket. For more information, go to the
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify either the 'TemplateBody' or the
-- 'TemplateURL' parameter, but not both.
csTemplateURL :: Lens' CreateStack (Maybe Text)
csTemplateURL = lens _csTemplateURL (\ s a -> s{_csTemplateURL = a});

-- | A list of capabilities that you must specify before AWS CloudFormation
-- can create or update certain stacks. Some stack templates might include
-- resources that can affect permissions in your AWS account. For those
-- stacks, you must explicitly acknowledge their capabilities by specifying
-- this parameter.
--
-- Currently, the only valid value is 'CAPABILITY_IAM', which is required
-- for the following resources:
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-accesskey.html AWS::IAM::AccessKey>,
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-group.html AWS::IAM::Group>,
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-instanceprofile.html AWS::IAM::InstanceProfile>,
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-policy.html AWS::IAM::Policy>,
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-role.html AWS::IAM::Role>,
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-user.html AWS::IAM::User>,
-- and
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-addusertogroup.html AWS::IAM::UserToGroupAddition>.
-- If your stack template contains these resources, we recommend that you
-- review any permissions associated with them. If you don\'t specify this
-- parameter, this action returns an 'InsufficientCapabilities' error.
csCapabilities :: Lens' CreateStack [Capability]
csCapabilities = lens _csCapabilities (\ s a -> s{_csCapabilities = a}) . _Default . _Coerce;

-- | Determines what action will be taken if stack creation fails. This must
-- be one of: DO_NOTHING, ROLLBACK, or DELETE. You can specify either
-- 'OnFailure' or 'DisableRollback', but not both.
--
-- Default: 'ROLLBACK'
csOnFailure :: Lens' CreateStack (Maybe OnFailure)
csOnFailure = lens _csOnFailure (\ s a -> s{_csOnFailure = a});

-- | The template resource types that you have permissions to work with for
-- this create stack action, such as 'AWS::EC2::Instance', 'AWS::EC2::*',
-- or 'Custom::MyCustomInstance'. Use the following syntax to describe
-- template resource types: 'AWS::*' (for all AWS resource), 'Custom::*'
-- (for all custom resources), 'Custom::logical_ID' (for a specific custom
-- resource), 'AWS::service_name::*' (for all resources of a particular AWS
-- service), and 'AWS::service_name::resource_logical_ID' (for a specific
-- AWS resource).
--
-- If the list of resource types doesn\'t include a resource that you\'re
-- creating, the stack creation fails. By default, AWS CloudFormation
-- grants permissions to all resource types. AWS Identity and Access
-- Management (IAM) uses this parameter for AWS CloudFormation-specific
-- condition keys in IAM policies. For more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html Controlling Access with AWS Identity and Access Management>.
csResourceTypes :: Lens' CreateStack [Text]
csResourceTypes = lens _csResourceTypes (\ s a -> s{_csResourceTypes = a}) . _Default . _Coerce;

-- | Key-value pairs to associate with this stack. AWS CloudFormation also
-- propagates these tags to the resources created in the stack. A maximum
-- number of 10 tags can be specified.
csTags :: Lens' CreateStack [Tag]
csTags = lens _csTags (\ s a -> s{_csTags = a}) . _Default . _Coerce;

-- | The amount of time that can pass before the stack status becomes
-- CREATE_FAILED; if 'DisableRollback' is not set or is set to 'false', the
-- stack will be rolled back.
csTimeoutInMinutes :: Lens' CreateStack (Maybe Natural)
csTimeoutInMinutes = lens _csTimeoutInMinutes (\ s a -> s{_csTimeoutInMinutes = a}) . mapping _Nat;

-- | The name that is associated with the stack. The name must be unique in
-- the region in which you are creating the stack.
--
-- A stack name can contain only alphanumeric characters (case sensitive)
-- and hyphens. It must start with an alphabetic character and cannot be
-- longer than 128 characters.
csStackName :: Lens' CreateStack Text
csStackName = lens _csStackName (\ s a -> s{_csStackName = a});

instance AWSRequest CreateStack where
        type Rs CreateStack = CreateStackResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "CreateStackResult"
              (\ s h x ->
                 CreateStackResponse' <$>
                   (x .@? "StackId") <*> (pure (fromEnum s)))

instance Hashable CreateStack

instance ToHeaders CreateStack where
        toHeaders = const mempty

instance ToPath CreateStack where
        toPath = const "/"

instance ToQuery CreateStack where
        toQuery CreateStack'{..}
          = mconcat
              ["Action" =: ("CreateStack" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "DisableRollback" =: _csDisableRollback,
               "NotificationARNs" =:
                 toQuery
                   (toQueryList "member" <$> _csNotificationARNs),
               "StackPolicyBody" =: _csStackPolicyBody,
               "Parameters" =:
                 toQuery (toQueryList "member" <$> _csParameters),
               "StackPolicyURL" =: _csStackPolicyURL,
               "TemplateBody" =: _csTemplateBody,
               "TemplateURL" =: _csTemplateURL,
               "Capabilities" =:
                 toQuery (toQueryList "member" <$> _csCapabilities),
               "OnFailure" =: _csOnFailure,
               "ResourceTypes" =:
                 toQuery (toQueryList "member" <$> _csResourceTypes),
               "Tags" =: toQuery (toQueryList "member" <$> _csTags),
               "TimeoutInMinutes" =: _csTimeoutInMinutes,
               "StackName" =: _csStackName]

-- | The output for a < CreateStack> action.
--
-- /See:/ 'createStackResponse' smart constructor.
data CreateStackResponse = CreateStackResponse'
    { _csrsStackId        :: !(Maybe Text)
    , _csrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateStackResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csrsStackId'
--
-- * 'csrsResponseStatus'
createStackResponse
    :: Int -- ^ 'csrsResponseStatus'
    -> CreateStackResponse
createStackResponse pResponseStatus_ =
    CreateStackResponse'
    { _csrsStackId = Nothing
    , _csrsResponseStatus = pResponseStatus_
    }

-- | Unique identifier of the stack.
csrsStackId :: Lens' CreateStackResponse (Maybe Text)
csrsStackId = lens _csrsStackId (\ s a -> s{_csrsStackId = a});

-- | The response status code.
csrsResponseStatus :: Lens' CreateStackResponse Int
csrsResponseStatus = lens _csrsResponseStatus (\ s a -> s{_csrsResponseStatus = a});
