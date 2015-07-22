{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.UpdateStack
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Updates a stack as specified in the template. After the call completes
-- successfully, the stack update starts. You can check the status of the
-- stack via the DescribeStacks action.
--
-- To get a copy of the template for an existing stack, you can use the
-- GetTemplate action.
--
-- Tags that were associated with this stack during creation time will
-- still be associated with the stack after an @UpdateStack@ operation.
--
-- For more information about creating an update template, updating a
-- stack, and monitoring the progress of the update, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-updating-stacks.html Updating a Stack>.
--
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStack.html>
module Network.AWS.CloudFormation.UpdateStack
    (
    -- * Request
      UpdateStack
    -- ** Request constructor
    , updateStack
    -- ** Request lenses
    , usrqUsePreviousTemplate
    , usrqNotificationARNs
    , usrqStackPolicyBody
    , usrqStackPolicyDuringUpdateBody
    , usrqStackPolicyDuringUpdateURL
    , usrqParameters
    , usrqStackPolicyURL
    , usrqTemplateBody
    , usrqTemplateURL
    , usrqCapabilities
    , usrqStackName

    -- * Response
    , UpdateStackResponse
    -- ** Response constructor
    , updateStackResponse
    -- ** Response lenses
    , usrsStackId
    , usrsStatus
    ) where

import           Network.AWS.CloudFormation.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for UpdateStack action.
--
-- /See:/ 'updateStack' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usrqUsePreviousTemplate'
--
-- * 'usrqNotificationARNs'
--
-- * 'usrqStackPolicyBody'
--
-- * 'usrqStackPolicyDuringUpdateBody'
--
-- * 'usrqStackPolicyDuringUpdateURL'
--
-- * 'usrqParameters'
--
-- * 'usrqStackPolicyURL'
--
-- * 'usrqTemplateBody'
--
-- * 'usrqTemplateURL'
--
-- * 'usrqCapabilities'
--
-- * 'usrqStackName'
data UpdateStack = UpdateStack'
    { _usrqUsePreviousTemplate         :: !(Maybe Bool)
    , _usrqNotificationARNs            :: !(Maybe [Text])
    , _usrqStackPolicyBody             :: !(Maybe Text)
    , _usrqStackPolicyDuringUpdateBody :: !(Maybe Text)
    , _usrqStackPolicyDuringUpdateURL  :: !(Maybe Text)
    , _usrqParameters                  :: !(Maybe [Parameter])
    , _usrqStackPolicyURL              :: !(Maybe Text)
    , _usrqTemplateBody                :: !(Maybe Text)
    , _usrqTemplateURL                 :: !(Maybe Text)
    , _usrqCapabilities                :: !(Maybe [Capability])
    , _usrqStackName                   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateStack' smart constructor.
updateStack :: Text -> UpdateStack
updateStack pStackName =
    UpdateStack'
    { _usrqUsePreviousTemplate = Nothing
    , _usrqNotificationARNs = Nothing
    , _usrqStackPolicyBody = Nothing
    , _usrqStackPolicyDuringUpdateBody = Nothing
    , _usrqStackPolicyDuringUpdateURL = Nothing
    , _usrqParameters = Nothing
    , _usrqStackPolicyURL = Nothing
    , _usrqTemplateBody = Nothing
    , _usrqTemplateURL = Nothing
    , _usrqCapabilities = Nothing
    , _usrqStackName = pStackName
    }

-- | Reuse the existing template that is associated with the stack that you
-- are updating.
usrqUsePreviousTemplate :: Lens' UpdateStack (Maybe Bool)
usrqUsePreviousTemplate = lens _usrqUsePreviousTemplate (\ s a -> s{_usrqUsePreviousTemplate = a});

-- | Update the ARNs for the Amazon SNS topics that are associated with the
-- stack.
usrqNotificationARNs :: Lens' UpdateStack [Text]
usrqNotificationARNs = lens _usrqNotificationARNs (\ s a -> s{_usrqNotificationARNs = a}) . _Default;

-- | Structure containing a new stack policy body. You can specify either the
-- @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
--
-- You might update the stack policy, for example, in order to protect a
-- new resource that you created during a stack update. If you do not
-- specify a stack policy, the current policy that is associated with the
-- stack is unchanged.
usrqStackPolicyBody :: Lens' UpdateStack (Maybe Text)
usrqStackPolicyBody = lens _usrqStackPolicyBody (\ s a -> s{_usrqStackPolicyBody = a});

-- | Structure containing the temporary overriding stack policy body. You can
-- specify either the @StackPolicyDuringUpdateBody@ or the
-- @StackPolicyDuringUpdateURL@ parameter, but not both.
--
-- If you want to update protected resources, specify a temporary
-- overriding stack policy during this update. If you do not specify a
-- stack policy, the current policy that is associated with the stack will
-- be used.
usrqStackPolicyDuringUpdateBody :: Lens' UpdateStack (Maybe Text)
usrqStackPolicyDuringUpdateBody = lens _usrqStackPolicyDuringUpdateBody (\ s a -> s{_usrqStackPolicyDuringUpdateBody = a});

-- | Location of a file containing the temporary overriding stack policy. The
-- URL must point to a policy (max size: 16KB) located in an S3 bucket in
-- the same region as the stack. You can specify either the
-- @StackPolicyDuringUpdateBody@ or the @StackPolicyDuringUpdateURL@
-- parameter, but not both.
--
-- If you want to update protected resources, specify a temporary
-- overriding stack policy during this update. If you do not specify a
-- stack policy, the current policy that is associated with the stack will
-- be used.
usrqStackPolicyDuringUpdateURL :: Lens' UpdateStack (Maybe Text)
usrqStackPolicyDuringUpdateURL = lens _usrqStackPolicyDuringUpdateURL (\ s a -> s{_usrqStackPolicyDuringUpdateURL = a});

-- | A list of @Parameter@ structures that specify input parameters for the
-- stack. For more information, see the
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_Parameter.html Parameter>
-- data type.
usrqParameters :: Lens' UpdateStack [Parameter]
usrqParameters = lens _usrqParameters (\ s a -> s{_usrqParameters = a}) . _Default;

-- | Location of a file containing the updated stack policy. The URL must
-- point to a policy (max size: 16KB) located in an S3 bucket in the same
-- region as the stack. You can specify either the @StackPolicyBody@ or the
-- @StackPolicyURL@ parameter, but not both.
--
-- You might update the stack policy, for example, in order to protect a
-- new resource that you created during a stack update. If you do not
-- specify a stack policy, the current policy that is associated with the
-- stack is unchanged.
usrqStackPolicyURL :: Lens' UpdateStack (Maybe Text)
usrqStackPolicyURL = lens _usrqStackPolicyURL (\ s a -> s{_usrqStackPolicyURL = a});

-- | Structure containing the template body with a minimum length of 1 byte
-- and a maximum length of 51,200 bytes. (For more information, go to
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.)
--
-- Conditional: You must specify either the @TemplateBody@ or the
-- @TemplateURL@ parameter, but not both.
usrqTemplateBody :: Lens' UpdateStack (Maybe Text)
usrqTemplateBody = lens _usrqTemplateBody (\ s a -> s{_usrqTemplateBody = a});

-- | Location of file containing the template body. The URL must point to a
-- template located in an S3 bucket in the same region as the stack. For
-- more information, go to
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify either the @TemplateBody@ or the
-- @TemplateURL@ parameter, but not both.
usrqTemplateURL :: Lens' UpdateStack (Maybe Text)
usrqTemplateURL = lens _usrqTemplateURL (\ s a -> s{_usrqTemplateURL = a});

-- | A list of capabilities that you must specify before AWS CloudFormation
-- can create or update certain stacks. Some stack templates might include
-- resources that can affect permissions in your AWS account. For those
-- stacks, you must explicitly acknowledge their capabilities by specifying
-- this parameter. Currently, the only valid value is @CAPABILITY_IAM@,
-- which is required for the following resources:
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
-- parameter, this action returns an InsufficientCapabilities error.
usrqCapabilities :: Lens' UpdateStack [Capability]
usrqCapabilities = lens _usrqCapabilities (\ s a -> s{_usrqCapabilities = a}) . _Default;

-- | The name or unique stack ID of the stack to update.
usrqStackName :: Lens' UpdateStack Text
usrqStackName = lens _usrqStackName (\ s a -> s{_usrqStackName = a});

instance AWSRequest UpdateStack where
        type Sv UpdateStack = CloudFormation
        type Rs UpdateStack = UpdateStackResponse
        request = post
        response
          = receiveXMLWrapper "UpdateStackResult"
              (\ s h x ->
                 UpdateStackResponse' <$>
                   (x .@? "StackId") <*> (pure (fromEnum s)))

instance ToHeaders UpdateStack where
        toHeaders = const mempty

instance ToPath UpdateStack where
        toPath = const "/"

instance ToQuery UpdateStack where
        toQuery UpdateStack'{..}
          = mconcat
              ["Action" =: ("UpdateStack" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "UsePreviousTemplate" =: _usrqUsePreviousTemplate,
               "NotificationARNs" =:
                 toQuery
                   (toQueryList "member" <$> _usrqNotificationARNs),
               "StackPolicyBody" =: _usrqStackPolicyBody,
               "StackPolicyDuringUpdateBody" =:
                 _usrqStackPolicyDuringUpdateBody,
               "StackPolicyDuringUpdateURL" =:
                 _usrqStackPolicyDuringUpdateURL,
               "Parameters" =:
                 toQuery (toQueryList "member" <$> _usrqParameters),
               "StackPolicyURL" =: _usrqStackPolicyURL,
               "TemplateBody" =: _usrqTemplateBody,
               "TemplateURL" =: _usrqTemplateURL,
               "Capabilities" =:
                 toQuery (toQueryList "member" <$> _usrqCapabilities),
               "StackName" =: _usrqStackName]

-- | The output for a UpdateStack action.
--
-- /See:/ 'updateStackResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usrsStackId'
--
-- * 'usrsStatus'
data UpdateStackResponse = UpdateStackResponse'
    { _usrsStackId :: !(Maybe Text)
    , _usrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateStackResponse' smart constructor.
updateStackResponse :: Int -> UpdateStackResponse
updateStackResponse pStatus =
    UpdateStackResponse'
    { _usrsStackId = Nothing
    , _usrsStatus = pStatus
    }

-- | Unique identifier of the stack.
usrsStackId :: Lens' UpdateStackResponse (Maybe Text)
usrsStackId = lens _usrsStackId (\ s a -> s{_usrsStackId = a});

-- | FIXME: Undocumented member.
usrsStatus :: Lens' UpdateStackResponse Int
usrsStatus = lens _usrsStatus (\ s a -> s{_usrsStatus = a});
