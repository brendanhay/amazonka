{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFormation.CreateStack
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a stack as specified in the template. After the call completes
-- successfully, the stack creation starts. You can check the status of the
-- stack via the DescribeStacks API.
--
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_CreateStack.html>
module Network.AWS.CloudFormation.CreateStack
    (
    -- * Request
      CreateStack
    -- ** Request constructor
    , createStack
    -- ** Request lenses
    , csDisableRollback
    , csNotificationARNs
    , csStackPolicyBody
    , csParameters
    , csStackPolicyURL
    , csTemplateBody
    , csTemplateURL
    , csCapabilities
    , csOnFailure
    , csTags
    , csTimeoutInMinutes
    , csStackName

    -- * Response
    , CreateStackResponse
    -- ** Response constructor
    , createStackResponse
    -- ** Response lenses
    , csrStackId
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createStack' smart constructor.
--
-- The fields accessible through corresponding lenses are:
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
-- * 'csTags'
--
-- * 'csTimeoutInMinutes'
--
-- * 'csStackName'
data CreateStack = CreateStack'{_csDisableRollback :: Maybe Bool, _csNotificationARNs :: Maybe [Text], _csStackPolicyBody :: Maybe Text, _csParameters :: Maybe [Parameter], _csStackPolicyURL :: Maybe Text, _csTemplateBody :: Maybe Text, _csTemplateURL :: Maybe Text, _csCapabilities :: Maybe [Capability], _csOnFailure :: Maybe OnFailure, _csTags :: Maybe [Tag], _csTimeoutInMinutes :: Maybe Nat, _csStackName :: Text} deriving (Eq, Read, Show)

-- | 'CreateStack' smart constructor.
createStack :: Text -> CreateStack
createStack pStackName = CreateStack'{_csDisableRollback = Nothing, _csNotificationARNs = Nothing, _csStackPolicyBody = Nothing, _csParameters = Nothing, _csStackPolicyURL = Nothing, _csTemplateBody = Nothing, _csTemplateURL = Nothing, _csCapabilities = Nothing, _csOnFailure = Nothing, _csTags = Nothing, _csTimeoutInMinutes = Nothing, _csStackName = pStackName};

-- | Set to @true@ to disable rollback of the stack if stack creation failed.
-- You can specify either @DisableRollback@ or @OnFailure@, but not both.
--
-- Default: @false@
csDisableRollback :: Lens' CreateStack (Maybe Bool)
csDisableRollback = lens _csDisableRollback (\ s a -> s{_csDisableRollback = a});

-- | The Simple Notification Service (SNS) topic ARNs to publish stack
-- related events. You can find your SNS topic ARNs using the
-- <http://console.aws.amazon.com/sns SNS console> or your Command Line
-- Interface (CLI).
csNotificationARNs :: Lens' CreateStack [Text]
csNotificationARNs = lens _csNotificationARNs (\ s a -> s{_csNotificationARNs = a}) . _Default;

-- | Structure containing the stack policy body. For more information, go to
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources>
-- in the AWS CloudFormation User Guide. You can specify either the
-- @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
csStackPolicyBody :: Lens' CreateStack (Maybe Text)
csStackPolicyBody = lens _csStackPolicyBody (\ s a -> s{_csStackPolicyBody = a});

-- | A list of @Parameter@ structures that specify input parameters for the
-- stack.
csParameters :: Lens' CreateStack [Parameter]
csParameters = lens _csParameters (\ s a -> s{_csParameters = a}) . _Default;

-- | Location of a file containing the stack policy. The URL must point to a
-- policy (max size: 16KB) located in an S3 bucket in the same region as
-- the stack. You can specify either the @StackPolicyBody@ or the
-- @StackPolicyURL@ parameter, but not both.
csStackPolicyURL :: Lens' CreateStack (Maybe Text)
csStackPolicyURL = lens _csStackPolicyURL (\ s a -> s{_csStackPolicyURL = a});

-- | Structure containing the template body with a minimum length of 1 byte
-- and a maximum length of 51,200 bytes. For more information, go to
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify either the @TemplateBody@ or the
-- @TemplateURL@ parameter, but not both.
csTemplateBody :: Lens' CreateStack (Maybe Text)
csTemplateBody = lens _csTemplateBody (\ s a -> s{_csTemplateBody = a});

-- | Location of file containing the template body. The URL must point to a
-- template (max size: 460,800 bytes) located in an S3 bucket in the same
-- region as the stack. For more information, go to the
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify either the @TemplateBody@ or the
-- @TemplateURL@ parameter, but not both.
csTemplateURL :: Lens' CreateStack (Maybe Text)
csTemplateURL = lens _csTemplateURL (\ s a -> s{_csTemplateURL = a});

-- | A list of capabilities that you must specify before AWS CloudFormation
-- can create or update certain stacks. Some stack templates might include
-- resources that can affect permissions in your AWS account. For those
-- stacks, you must explicitly acknowledge their capabilities by specifying
-- this parameter.
--
-- Currently, the only valid value is @CAPABILITY_IAM@, which is required
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
-- parameter, this action returns an @InsufficientCapabilities@ error.
csCapabilities :: Lens' CreateStack [Capability]
csCapabilities = lens _csCapabilities (\ s a -> s{_csCapabilities = a}) . _Default;

-- | Determines what action will be taken if stack creation fails. This must
-- be one of: DO_NOTHING, ROLLBACK, or DELETE. You can specify either
-- @OnFailure@ or @DisableRollback@, but not both.
--
-- Default: @ROLLBACK@
csOnFailure :: Lens' CreateStack (Maybe OnFailure)
csOnFailure = lens _csOnFailure (\ s a -> s{_csOnFailure = a});

-- | A set of user-defined @Tags@ to associate with this stack, represented
-- by key\/value pairs. Tags defined for the stack are propagated to EC2
-- resources that are created as part of the stack. A maximum number of 10
-- tags can be specified.
csTags :: Lens' CreateStack [Tag]
csTags = lens _csTags (\ s a -> s{_csTags = a}) . _Default;

-- | The amount of time that can pass before the stack status becomes
-- CREATE_FAILED; if @DisableRollback@ is not set or is set to @false@, the
-- stack will be rolled back.
csTimeoutInMinutes :: Lens' CreateStack (Maybe Natural)
csTimeoutInMinutes = lens _csTimeoutInMinutes (\ s a -> s{_csTimeoutInMinutes = a}) . mapping _Nat;

-- | The name that is associated with the stack. The name must be unique in
-- the region in which you are creating the stack.
--
-- A stack name can contain only alphanumeric characters (case sensitive)
-- and hyphens. It must start with an alphabetic character and cannot be
-- longer than 255 characters.
csStackName :: Lens' CreateStack Text
csStackName = lens _csStackName (\ s a -> s{_csStackName = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest CreateStack where
        type Sv CreateStack = CloudFormation
        type Rs CreateStack = CreateStackResponse
        request = post
        response
          = receiveXMLWrapper "CreateStackResult"
              (\ s h x ->
                 CreateStackResponse' <$> (x .@? "StackId"))

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
               "Tags" =: toQuery (toQueryList "member" <$> _csTags),
               "TimeoutInMinutes" =: _csTimeoutInMinutes,
               "StackName" =: _csStackName]

-- | /See:/ 'createStackResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csrStackId'
newtype CreateStackResponse = CreateStackResponse'{_csrStackId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'CreateStackResponse' smart constructor.
createStackResponse :: CreateStackResponse
createStackResponse = CreateStackResponse'{_csrStackId = Nothing};

-- | Unique identifier of the stack.
csrStackId :: Lens' CreateStackResponse (Maybe Text)
csrStackId = lens _csrStackId (\ s a -> s{_csrStackId = a});
