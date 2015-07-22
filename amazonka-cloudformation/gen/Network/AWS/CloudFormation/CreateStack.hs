{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.CreateStack
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a stack as specified in the template. After the call completes
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
    , csrqDisableRollback
    , csrqNotificationARNs
    , csrqStackPolicyBody
    , csrqParameters
    , csrqStackPolicyURL
    , csrqTemplateBody
    , csrqTemplateURL
    , csrqCapabilities
    , csrqOnFailure
    , csrqTags
    , csrqTimeoutInMinutes
    , csrqStackName

    -- * Response
    , CreateStackResponse
    -- ** Response constructor
    , createStackResponse
    -- ** Response lenses
    , csrsStackId
    , csrsStatus
    ) where

import           Network.AWS.CloudFormation.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for CreateStack action.
--
-- /See:/ 'createStack' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csrqDisableRollback'
--
-- * 'csrqNotificationARNs'
--
-- * 'csrqStackPolicyBody'
--
-- * 'csrqParameters'
--
-- * 'csrqStackPolicyURL'
--
-- * 'csrqTemplateBody'
--
-- * 'csrqTemplateURL'
--
-- * 'csrqCapabilities'
--
-- * 'csrqOnFailure'
--
-- * 'csrqTags'
--
-- * 'csrqTimeoutInMinutes'
--
-- * 'csrqStackName'
data CreateStack = CreateStack'
    { _csrqDisableRollback  :: !(Maybe Bool)
    , _csrqNotificationARNs :: !(Maybe [Text])
    , _csrqStackPolicyBody  :: !(Maybe Text)
    , _csrqParameters       :: !(Maybe [Parameter])
    , _csrqStackPolicyURL   :: !(Maybe Text)
    , _csrqTemplateBody     :: !(Maybe Text)
    , _csrqTemplateURL      :: !(Maybe Text)
    , _csrqCapabilities     :: !(Maybe [Capability])
    , _csrqOnFailure        :: !(Maybe OnFailure)
    , _csrqTags             :: !(Maybe [Tag])
    , _csrqTimeoutInMinutes :: !(Maybe Nat)
    , _csrqStackName        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateStack' smart constructor.
createStack :: Text -> CreateStack
createStack pStackName_ =
    CreateStack'
    { _csrqDisableRollback = Nothing
    , _csrqNotificationARNs = Nothing
    , _csrqStackPolicyBody = Nothing
    , _csrqParameters = Nothing
    , _csrqStackPolicyURL = Nothing
    , _csrqTemplateBody = Nothing
    , _csrqTemplateURL = Nothing
    , _csrqCapabilities = Nothing
    , _csrqOnFailure = Nothing
    , _csrqTags = Nothing
    , _csrqTimeoutInMinutes = Nothing
    , _csrqStackName = pStackName_
    }

-- | Set to @true@ to disable rollback of the stack if stack creation failed.
-- You can specify either @DisableRollback@ or @OnFailure@, but not both.
--
-- Default: @false@
csrqDisableRollback :: Lens' CreateStack (Maybe Bool)
csrqDisableRollback = lens _csrqDisableRollback (\ s a -> s{_csrqDisableRollback = a});

-- | The Simple Notification Service (SNS) topic ARNs to publish stack
-- related events. You can find your SNS topic ARNs using the
-- <http://console.aws.amazon.com/sns SNS console> or your Command Line
-- Interface (CLI).
csrqNotificationARNs :: Lens' CreateStack [Text]
csrqNotificationARNs = lens _csrqNotificationARNs (\ s a -> s{_csrqNotificationARNs = a}) . _Default;

-- | Structure containing the stack policy body. For more information, go to
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources>
-- in the AWS CloudFormation User Guide. You can specify either the
-- @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
csrqStackPolicyBody :: Lens' CreateStack (Maybe Text)
csrqStackPolicyBody = lens _csrqStackPolicyBody (\ s a -> s{_csrqStackPolicyBody = a});

-- | A list of @Parameter@ structures that specify input parameters for the
-- stack.
csrqParameters :: Lens' CreateStack [Parameter]
csrqParameters = lens _csrqParameters (\ s a -> s{_csrqParameters = a}) . _Default;

-- | Location of a file containing the stack policy. The URL must point to a
-- policy (max size: 16KB) located in an S3 bucket in the same region as
-- the stack. You can specify either the @StackPolicyBody@ or the
-- @StackPolicyURL@ parameter, but not both.
csrqStackPolicyURL :: Lens' CreateStack (Maybe Text)
csrqStackPolicyURL = lens _csrqStackPolicyURL (\ s a -> s{_csrqStackPolicyURL = a});

-- | Structure containing the template body with a minimum length of 1 byte
-- and a maximum length of 51,200 bytes. For more information, go to
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify either the @TemplateBody@ or the
-- @TemplateURL@ parameter, but not both.
csrqTemplateBody :: Lens' CreateStack (Maybe Text)
csrqTemplateBody = lens _csrqTemplateBody (\ s a -> s{_csrqTemplateBody = a});

-- | Location of file containing the template body. The URL must point to a
-- template (max size: 460,800 bytes) located in an S3 bucket in the same
-- region as the stack. For more information, go to the
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify either the @TemplateBody@ or the
-- @TemplateURL@ parameter, but not both.
csrqTemplateURL :: Lens' CreateStack (Maybe Text)
csrqTemplateURL = lens _csrqTemplateURL (\ s a -> s{_csrqTemplateURL = a});

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
csrqCapabilities :: Lens' CreateStack [Capability]
csrqCapabilities = lens _csrqCapabilities (\ s a -> s{_csrqCapabilities = a}) . _Default;

-- | Determines what action will be taken if stack creation fails. This must
-- be one of: DO_NOTHING, ROLLBACK, or DELETE. You can specify either
-- @OnFailure@ or @DisableRollback@, but not both.
--
-- Default: @ROLLBACK@
csrqOnFailure :: Lens' CreateStack (Maybe OnFailure)
csrqOnFailure = lens _csrqOnFailure (\ s a -> s{_csrqOnFailure = a});

-- | A set of user-defined @Tags@ to associate with this stack, represented
-- by key\/value pairs. Tags defined for the stack are propagated to EC2
-- resources that are created as part of the stack. A maximum number of 10
-- tags can be specified.
csrqTags :: Lens' CreateStack [Tag]
csrqTags = lens _csrqTags (\ s a -> s{_csrqTags = a}) . _Default;

-- | The amount of time that can pass before the stack status becomes
-- CREATE_FAILED; if @DisableRollback@ is not set or is set to @false@, the
-- stack will be rolled back.
csrqTimeoutInMinutes :: Lens' CreateStack (Maybe Natural)
csrqTimeoutInMinutes = lens _csrqTimeoutInMinutes (\ s a -> s{_csrqTimeoutInMinutes = a}) . mapping _Nat;

-- | The name that is associated with the stack. The name must be unique in
-- the region in which you are creating the stack.
--
-- A stack name can contain only alphanumeric characters (case sensitive)
-- and hyphens. It must start with an alphabetic character and cannot be
-- longer than 255 characters.
csrqStackName :: Lens' CreateStack Text
csrqStackName = lens _csrqStackName (\ s a -> s{_csrqStackName = a});

instance AWSRequest CreateStack where
        type Sv CreateStack = CloudFormation
        type Rs CreateStack = CreateStackResponse
        request = post
        response
          = receiveXMLWrapper "CreateStackResult"
              (\ s h x ->
                 CreateStackResponse' <$>
                   (x .@? "StackId") <*> (pure (fromEnum s)))

instance ToHeaders CreateStack where
        toHeaders = const mempty

instance ToPath CreateStack where
        toPath = const "/"

instance ToQuery CreateStack where
        toQuery CreateStack'{..}
          = mconcat
              ["Action" =: ("CreateStack" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "DisableRollback" =: _csrqDisableRollback,
               "NotificationARNs" =:
                 toQuery
                   (toQueryList "member" <$> _csrqNotificationARNs),
               "StackPolicyBody" =: _csrqStackPolicyBody,
               "Parameters" =:
                 toQuery (toQueryList "member" <$> _csrqParameters),
               "StackPolicyURL" =: _csrqStackPolicyURL,
               "TemplateBody" =: _csrqTemplateBody,
               "TemplateURL" =: _csrqTemplateURL,
               "Capabilities" =:
                 toQuery (toQueryList "member" <$> _csrqCapabilities),
               "OnFailure" =: _csrqOnFailure,
               "Tags" =:
                 toQuery (toQueryList "member" <$> _csrqTags),
               "TimeoutInMinutes" =: _csrqTimeoutInMinutes,
               "StackName" =: _csrqStackName]

-- | The output for a CreateStack action.
--
-- /See:/ 'createStackResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csrsStackId'
--
-- * 'csrsStatus'
data CreateStackResponse = CreateStackResponse'
    { _csrsStackId :: !(Maybe Text)
    , _csrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateStackResponse' smart constructor.
createStackResponse :: Int -> CreateStackResponse
createStackResponse pStatus_ =
    CreateStackResponse'
    { _csrsStackId = Nothing
    , _csrsStatus = pStatus_
    }

-- | Unique identifier of the stack.
csrsStackId :: Lens' CreateStackResponse (Maybe Text)
csrsStackId = lens _csrsStackId (\ s a -> s{_csrsStackId = a});

-- | FIXME: Undocumented member.
csrsStatus :: Lens' CreateStackResponse Int
csrsStatus = lens _csrsStatus (\ s a -> s{_csrsStatus = a});
