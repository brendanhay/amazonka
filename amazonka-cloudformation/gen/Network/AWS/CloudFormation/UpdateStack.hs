{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation.UpdateStack
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates a stack as specified in the template. After the call completes
-- successfully, the stack update starts. You can check the status of the
-- stack via the DescribeStacks action. To get a copy of the template for an
-- existing stack, you can use the GetTemplate action. Tags that were
-- associated with this stack during creation time will still be associated
-- with the stack after an UpdateStack operation. For more information about
-- creating an update template, updating a stack, and monitoring the progress
-- of the update, see Updating a Stack.
--
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStack.html>
module Network.AWS.CloudFormation.UpdateStack
    (
    -- * Request
      UpdateStack
    -- ** Request constructor
    , updateStack
    -- ** Request lenses
    , usCapabilities
    , usNotificationARNs
    , usParameters
    , usStackName
    , usStackPolicyBody
    , usStackPolicyDuringUpdateBody
    , usStackPolicyDuringUpdateURL
    , usStackPolicyURL
    , usTemplateBody
    , usTemplateURL
    , usUsePreviousTemplate

    -- * Response
    , UpdateStackResponse
    -- ** Response constructor
    , updateStackResponse
    -- ** Response lenses
    , usrStackId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types
import qualified GHC.Exts

data UpdateStack = UpdateStack
    { _usCapabilities                :: [Text]
    , _usNotificationARNs            :: [Text]
    , _usParameters                  :: [Parameter]
    , _usStackName                   :: Text
    , _usStackPolicyBody             :: Maybe Text
    , _usStackPolicyDuringUpdateBody :: Maybe Text
    , _usStackPolicyDuringUpdateURL  :: Maybe Text
    , _usStackPolicyURL              :: Maybe Text
    , _usTemplateBody                :: Maybe Text
    , _usTemplateURL                 :: Maybe Text
    , _usUsePreviousTemplate         :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | 'UpdateStack' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usCapabilities' @::@ ['Text']
--
-- * 'usNotificationARNs' @::@ ['Text']
--
-- * 'usParameters' @::@ ['Parameter']
--
-- * 'usStackName' @::@ 'Text'
--
-- * 'usStackPolicyBody' @::@ 'Maybe' 'Text'
--
-- * 'usStackPolicyDuringUpdateBody' @::@ 'Maybe' 'Text'
--
-- * 'usStackPolicyDuringUpdateURL' @::@ 'Maybe' 'Text'
--
-- * 'usStackPolicyURL' @::@ 'Maybe' 'Text'
--
-- * 'usTemplateBody' @::@ 'Maybe' 'Text'
--
-- * 'usTemplateURL' @::@ 'Maybe' 'Text'
--
-- * 'usUsePreviousTemplate' @::@ 'Maybe' 'Bool'
--
updateStack :: Text -- ^ 'usStackName'
            -> UpdateStack
updateStack p1 = UpdateStack
    { _usStackName                   = p1
    , _usTemplateBody                = Nothing
    , _usTemplateURL                 = Nothing
    , _usUsePreviousTemplate         = Nothing
    , _usStackPolicyDuringUpdateBody = Nothing
    , _usStackPolicyDuringUpdateURL  = Nothing
    , _usParameters                  = mempty
    , _usCapabilities                = mempty
    , _usStackPolicyBody             = Nothing
    , _usStackPolicyURL              = Nothing
    , _usNotificationARNs            = mempty
    }

-- | A list of capabilities that you must specify before AWS CloudFormation
-- can create or update certain stacks. Some stack templates might include
-- resources that can affect permissions in your AWS account. For those
-- stacks, you must explicitly acknowledge their capabilities by specifying
-- this parameter. Currently, the only valid value is CAPABILITY_IAM, which
-- is required for the following resources: AWS::CloudFormation::Stack,
-- AWS::IAM::AccessKey, AWS::IAM::Group, AWS::IAM::InstanceProfile,
-- AWS::IAM::Policy, AWS::IAM::Role, AWS::IAM::User, and
-- AWS::IAM::UserToGroupAddition. If your stack template contains these
-- resources, we recommend that you review any permissions associated with
-- them. If you don't specify this parameter, this action returns an
-- InsufficientCapabilities error.
usCapabilities :: Lens' UpdateStack [Text]
usCapabilities = lens _usCapabilities (\s a -> s { _usCapabilities = a })

-- | Update the ARNs for the Amazon SNS topics that are associated with the
-- stack.
usNotificationARNs :: Lens' UpdateStack [Text]
usNotificationARNs =
    lens _usNotificationARNs (\s a -> s { _usNotificationARNs = a })

-- | A list of Parameter structures that specify input parameters for the
-- stack.
usParameters :: Lens' UpdateStack [Parameter]
usParameters = lens _usParameters (\s a -> s { _usParameters = a })

-- | The name or stack ID of the stack to update. Must contain only
-- alphanumeric characters (case sensitive) and start with an alpha
-- character. Maximum length of the name is 255 characters.
usStackName :: Lens' UpdateStack Text
usStackName = lens _usStackName (\s a -> s { _usStackName = a })

-- | Structure containing a new stack policy body. You can specify either the
-- StackPolicyBody or the StackPolicyURL parameter, but not both. You might
-- update the stack policy, for example, in order to protect a new resource
-- that you created during a stack update. If you do not specify a stack
-- policy, the current policy that is associated with the stack is
-- unchanged.
usStackPolicyBody :: Lens' UpdateStack (Maybe Text)
usStackPolicyBody =
    lens _usStackPolicyBody (\s a -> s { _usStackPolicyBody = a })

-- | Structure containing the temporary overriding stack policy body. You can
-- specify either the StackPolicyDuringUpdateBody or the
-- StackPolicyDuringUpdateURL parameter, but not both. If you want to update
-- protected resources, specify a temporary overriding stack policy during
-- this update. If you do not specify a stack policy, the current policy
-- that is associated with the stack will be used.
usStackPolicyDuringUpdateBody :: Lens' UpdateStack (Maybe Text)
usStackPolicyDuringUpdateBody =
    lens _usStackPolicyDuringUpdateBody
        (\s a -> s { _usStackPolicyDuringUpdateBody = a })

-- | Location of a file containing the temporary overriding stack policy. The
-- URL must point to a policy (max size: 16KB) located in an S3 bucket in
-- the same region as the stack. You can specify either the
-- StackPolicyDuringUpdateBody or the StackPolicyDuringUpdateURL parameter,
-- but not both. If you want to update protected resources, specify a
-- temporary overriding stack policy during this update. If you do not
-- specify a stack policy, the current policy that is associated with the
-- stack will be used.
usStackPolicyDuringUpdateURL :: Lens' UpdateStack (Maybe Text)
usStackPolicyDuringUpdateURL =
    lens _usStackPolicyDuringUpdateURL
        (\s a -> s { _usStackPolicyDuringUpdateURL = a })

-- | Location of a file containing the updated stack policy. The URL must
-- point to a policy (max size: 16KB) located in an S3 bucket in the same
-- region as the stack. You can specify either the StackPolicyBody or the
-- StackPolicyURL parameter, but not both. You might update the stack
-- policy, for example, in order to protect a new resource that you created
-- during a stack update. If you do not specify a stack policy, the current
-- policy that is associated with the stack is unchanged.
usStackPolicyURL :: Lens' UpdateStack (Maybe Text)
usStackPolicyURL = lens _usStackPolicyURL (\s a -> s { _usStackPolicyURL = a })

-- | Structure containing the template body with a minimum length of 1 byte
-- and a maximum length of 51,200 bytes. (For more information, go to
-- Template Anatomy in the AWS CloudFormation User Guide.) Conditional: You
-- must specify either the TemplateBody or the TemplateURL parameter, but
-- not both.
usTemplateBody :: Lens' UpdateStack (Maybe Text)
usTemplateBody = lens _usTemplateBody (\s a -> s { _usTemplateBody = a })

-- | Location of file containing the template body. The URL must point to a
-- template located in an S3 bucket in the same region as the stack. For
-- more information, go to Template Anatomy in the AWS CloudFormation User
-- Guide. Conditional: You must specify either the TemplateBody or the
-- TemplateURL parameter, but not both.
usTemplateURL :: Lens' UpdateStack (Maybe Text)
usTemplateURL = lens _usTemplateURL (\s a -> s { _usTemplateURL = a })

-- | Reuse the existing template that is associated with the stack that you
-- are updating.
usUsePreviousTemplate :: Lens' UpdateStack (Maybe Bool)
usUsePreviousTemplate =
    lens _usUsePreviousTemplate (\s a -> s { _usUsePreviousTemplate = a })

newtype UpdateStackResponse = UpdateStackResponse
    { _usrStackId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'UpdateStackResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usrStackId' @::@ 'Maybe' 'Text'
--
updateStackResponse :: UpdateStackResponse
updateStackResponse = UpdateStackResponse
    { _usrStackId = Nothing
    }

-- | Unique identifier of the stack.
usrStackId :: Lens' UpdateStackResponse (Maybe Text)
usrStackId = lens _usrStackId (\s a -> s { _usrStackId = a })

instance ToPath UpdateStack where
    toPath = const "/"

instance ToQuery UpdateStack

instance ToHeaders UpdateStack

instance AWSRequest UpdateStack where
    type Sv UpdateStack = CloudFormation
    type Rs UpdateStack = UpdateStackResponse

    request  = post "UpdateStack"
    response = xmlResponse

instance FromXML UpdateStackResponse where
    parseXML x = UpdateStackResponse
        <$> x .@? "StackId"
