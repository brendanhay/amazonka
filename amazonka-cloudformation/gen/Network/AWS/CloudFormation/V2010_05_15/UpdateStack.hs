{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation.V2010_05_15.UpdateStack
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
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=UpdateStack
-- &StackName=MyStack &TemplateBody=[Template Document]
-- &Parameters.member.1.ParameterKey=AvailabilityZone
-- &Parameters.member.1.ParameterValue=us-east-1a &Version=2010-05-15
-- &SignatureVersion=2 &Timestamp=2010-07-27T22%3A26%3A28.000Z
-- &AWSAccessKeyId=[AWS Access KeyID] &Signature=[Signature]
-- arn:aws:cloudformation:us-east-1:123456789:stack/MyStack/aaf549a0-a413-11df-adb3-5081b3858e83.
-- 
module Network.AWS.CloudFormation.V2010_05_15.UpdateStack
    (
    -- * Request
      UpdateStack
    -- ** Request constructor
    , mkUpdateStack
    -- ** Request lenses
    , usStackName
    , usTemplateBody
    , usTemplateURL
    , usUsePreviousTemplate
    , usStackPolicyDuringUpdateBody
    , usStackPolicyDuringUpdateURL
    , usParameters
    , usCapabilities
    , usStackPolicyBody
    , usStackPolicyURL
    , usNotificationARNs

    -- * Response
    , UpdateStackResponse
    -- ** Response lenses
    , usrsStackId
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudFormation.V2010_05_15.Types
import Network.AWS.Prelude

-- | The input for UpdateStack action.
data UpdateStack = UpdateStack
    { _usStackName :: Text
    , _usTemplateBody :: Maybe Text
    , _usTemplateURL :: Maybe Text
    , _usUsePreviousTemplate :: Maybe Bool
    , _usStackPolicyDuringUpdateBody :: Maybe Text
    , _usStackPolicyDuringUpdateURL :: Maybe Text
    , _usParameters :: [Parameter]
    , _usCapabilities :: [Capability]
    , _usStackPolicyBody :: Maybe Text
    , _usStackPolicyURL :: Maybe Text
    , _usNotificationARNs :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateStack' request.
mkUpdateStack :: Text -- ^ 'usStackName'
              -> UpdateStack
mkUpdateStack p1 = UpdateStack
    { _usStackName = p1
    , _usTemplateBody = Nothing
    , _usTemplateURL = Nothing
    , _usUsePreviousTemplate = Nothing
    , _usStackPolicyDuringUpdateBody = Nothing
    , _usStackPolicyDuringUpdateURL = Nothing
    , _usParameters = mempty
    , _usCapabilities = mempty
    , _usStackPolicyBody = Nothing
    , _usStackPolicyURL = Nothing
    , _usNotificationARNs = mempty
    }
{-# INLINE mkUpdateStack #-}

-- | The name or stack ID of the stack to update. Must contain only alphanumeric
-- characters (case sensitive) and start with an alpha character. Maximum
-- length of the name is 255 characters.
usStackName :: Lens' UpdateStack Text
usStackName = lens _usStackName (\s a -> s { _usStackName = a })
{-# INLINE usStackName #-}

-- | Structure containing the template body with a minimum length of 1 byte and
-- a maximum length of 51,200 bytes. (For more information, go to Template
-- Anatomy in the AWS CloudFormation User Guide.) Conditional: You must
-- specify either the TemplateBody or the TemplateURL parameter, but not both.
usTemplateBody :: Lens' UpdateStack (Maybe Text)
usTemplateBody = lens _usTemplateBody (\s a -> s { _usTemplateBody = a })
{-# INLINE usTemplateBody #-}

-- | Location of file containing the template body. The URL must point to a
-- template located in an S3 bucket in the same region as the stack. For more
-- information, go to Template Anatomy in the AWS CloudFormation User Guide.
-- Conditional: You must specify either the TemplateBody or the TemplateURL
-- parameter, but not both.
usTemplateURL :: Lens' UpdateStack (Maybe Text)
usTemplateURL = lens _usTemplateURL (\s a -> s { _usTemplateURL = a })
{-# INLINE usTemplateURL #-}

-- | Reuse the existing template that is associated with the stack that you are
-- updating.
usUsePreviousTemplate :: Lens' UpdateStack (Maybe Bool)
usUsePreviousTemplate =
    lens _usUsePreviousTemplate (\s a -> s { _usUsePreviousTemplate = a })
{-# INLINE usUsePreviousTemplate #-}

-- | Structure containing the temporary overriding stack policy body. You can
-- specify either the StackPolicyDuringUpdateBody or the
-- StackPolicyDuringUpdateURL parameter, but not both. If you want to update
-- protected resources, specify a temporary overriding stack policy during
-- this update. If you do not specify a stack policy, the current policy that
-- is associated with the stack will be used.
usStackPolicyDuringUpdateBody :: Lens' UpdateStack (Maybe Text)
usStackPolicyDuringUpdateBody =
    lens _usStackPolicyDuringUpdateBody
         (\s a -> s { _usStackPolicyDuringUpdateBody = a })
{-# INLINE usStackPolicyDuringUpdateBody #-}

-- | Location of a file containing the temporary overriding stack policy. The
-- URL must point to a policy (max size: 16KB) located in an S3 bucket in the
-- same region as the stack. You can specify either the
-- StackPolicyDuringUpdateBody or the StackPolicyDuringUpdateURL parameter,
-- but not both. If you want to update protected resources, specify a
-- temporary overriding stack policy during this update. If you do not specify
-- a stack policy, the current policy that is associated with the stack will
-- be used.
usStackPolicyDuringUpdateURL :: Lens' UpdateStack (Maybe Text)
usStackPolicyDuringUpdateURL =
    lens _usStackPolicyDuringUpdateURL
         (\s a -> s { _usStackPolicyDuringUpdateURL = a })
{-# INLINE usStackPolicyDuringUpdateURL #-}

-- | A list of Parameter structures that specify input parameters for the stack.
usParameters :: Lens' UpdateStack [Parameter]
usParameters = lens _usParameters (\s a -> s { _usParameters = a })
{-# INLINE usParameters #-}

-- | The list of capabilities that you want to allow in the stack. If your stack
-- contains IAM resources, you must specify the CAPABILITY_IAM value for this
-- parameter; otherwise, this action returns an InsufficientCapabilities
-- error. IAM resources are the following: AWS::IAM::AccessKey,
-- AWS::IAM::Group, AWS::IAM::Policy, AWS::IAM::User, and
-- AWS::IAM::UserToGroupAddition.
usCapabilities :: Lens' UpdateStack [Capability]
usCapabilities = lens _usCapabilities (\s a -> s { _usCapabilities = a })
{-# INLINE usCapabilities #-}

-- | Structure containing a new stack policy body. You can specify either the
-- StackPolicyBody or the StackPolicyURL parameter, but not both. You might
-- update the stack policy, for example, in order to protect a new resource
-- that you created during a stack update. If you do not specify a stack
-- policy, the current policy that is associated with the stack is unchanged.
usStackPolicyBody :: Lens' UpdateStack (Maybe Text)
usStackPolicyBody =
    lens _usStackPolicyBody (\s a -> s { _usStackPolicyBody = a })
{-# INLINE usStackPolicyBody #-}

-- | Location of a file containing the updated stack policy. The URL must point
-- to a policy (max size: 16KB) located in an S3 bucket in the same region as
-- the stack. You can specify either the StackPolicyBody or the StackPolicyURL
-- parameter, but not both. You might update the stack policy, for example, in
-- order to protect a new resource that you created during a stack update. If
-- you do not specify a stack policy, the current policy that is associated
-- with the stack is unchanged.
usStackPolicyURL :: Lens' UpdateStack (Maybe Text)
usStackPolicyURL =
    lens _usStackPolicyURL (\s a -> s { _usStackPolicyURL = a })
{-# INLINE usStackPolicyURL #-}

-- | Update the ARNs for the Amazon SNS topics that are associated with the
-- stack.
usNotificationARNs :: Lens' UpdateStack [Text]
usNotificationARNs =
    lens _usNotificationARNs (\s a -> s { _usNotificationARNs = a })
{-# INLINE usNotificationARNs #-}

instance ToQuery UpdateStack where
    toQuery = genericQuery def

-- | The output for a UpdateStack action.
newtype UpdateStackResponse = UpdateStackResponse
    { _usrsStackId :: Maybe Text
    } deriving (Show, Generic)

-- | Unique identifier of the stack.
usrsStackId :: Lens' UpdateStackResponse (Maybe Text)
usrsStackId = lens _usrsStackId (\s a -> s { _usrsStackId = a })
{-# INLINE usrsStackId #-}

instance FromXML UpdateStackResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest UpdateStack where
    type Sv UpdateStack = CloudFormation
    type Rs UpdateStack = UpdateStackResponse

    request = post "UpdateStack"
    response _ = xmlResponse
