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
    , mkUpdateStackInput
    -- ** Request lenses
    , usiStackName
    , usiTemplateBody
    , usiTemplateURL
    , usiUsePreviousTemplate
    , usiStackPolicyDuringUpdateBody
    , usiStackPolicyDuringUpdateURL
    , usiParameters
    , usiCapabilities
    , usiStackPolicyBody
    , usiStackPolicyURL
    , usiNotificationARNs

    -- * Response
    , UpdateStackResponse
    -- ** Response lenses
    , usoStackId
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudFormation.V2010_05_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateStack' request.
mkUpdateStackInput :: Text -- ^ 'usiStackName'
                   -> UpdateStack
mkUpdateStackInput p1 = UpdateStack
    { _usiStackName = p1
    , _usiTemplateBody = Nothing
    , _usiTemplateURL = Nothing
    , _usiUsePreviousTemplate = Nothing
    , _usiStackPolicyDuringUpdateBody = Nothing
    , _usiStackPolicyDuringUpdateURL = Nothing
    , _usiParameters = mempty
    , _usiCapabilities = mempty
    , _usiStackPolicyBody = Nothing
    , _usiStackPolicyURL = Nothing
    , _usiNotificationARNs = mempty
    }
{-# INLINE mkUpdateStackInput #-}

data UpdateStack = UpdateStack
    { _usiStackName :: Text
      -- ^ The name or stack ID of the stack to update. Must contain only
      -- alphanumeric characters (case sensitive) and start with an alpha
      -- character. Maximum length of the name is 255 characters.
    , _usiTemplateBody :: Maybe Text
      -- ^ Structure containing the template body with a minimum length of 1
      -- byte and a maximum length of 51,200 bytes. (For more information,
      -- go to Template Anatomy in the AWS CloudFormation User Guide.)
      -- Conditional: You must specify either the TemplateBody or the
      -- TemplateURL parameter, but not both.
    , _usiTemplateURL :: Maybe Text
      -- ^ Location of file containing the template body. The URL must point
      -- to a template located in an S3 bucket in the same region as the
      -- stack. For more information, go to Template Anatomy in the AWS
      -- CloudFormation User Guide. Conditional: You must specify either
      -- the TemplateBody or the TemplateURL parameter, but not both.
    , _usiUsePreviousTemplate :: Maybe Bool
      -- ^ Reuse the existing template that is associated with the stack
      -- that you are updating.
    , _usiStackPolicyDuringUpdateBody :: Maybe Text
      -- ^ Structure containing the temporary overriding stack policy body.
      -- You can specify either the StackPolicyDuringUpdateBody or the
      -- StackPolicyDuringUpdateURL parameter, but not both. If you want
      -- to update protected resources, specify a temporary overriding
      -- stack policy during this update. If you do not specify a stack
      -- policy, the current policy that is associated with the stack will
      -- be used.
    , _usiStackPolicyDuringUpdateURL :: Maybe Text
      -- ^ Location of a file containing the temporary overriding stack
      -- policy. The URL must point to a policy (max size: 16KB) located
      -- in an S3 bucket in the same region as the stack. You can specify
      -- either the StackPolicyDuringUpdateBody or the
      -- StackPolicyDuringUpdateURL parameter, but not both. If you want
      -- to update protected resources, specify a temporary overriding
      -- stack policy during this update. If you do not specify a stack
      -- policy, the current policy that is associated with the stack will
      -- be used.
    , _usiParameters :: [Parameter]
      -- ^ A list of Parameter structures that specify input parameters for
      -- the stack.
    , _usiCapabilities :: [Capability]
      -- ^ The list of capabilities that you want to allow in the stack. If
      -- your stack contains IAM resources, you must specify the
      -- CAPABILITY_IAM value for this parameter; otherwise, this action
      -- returns an InsufficientCapabilities error. IAM resources are the
      -- following: AWS::IAM::AccessKey, AWS::IAM::Group,
      -- AWS::IAM::Policy, AWS::IAM::User, and
      -- AWS::IAM::UserToGroupAddition.
    , _usiStackPolicyBody :: Maybe Text
      -- ^ Structure containing a new stack policy body. You can specify
      -- either the StackPolicyBody or the StackPolicyURL parameter, but
      -- not both. You might update the stack policy, for example, in
      -- order to protect a new resource that you created during a stack
      -- update. If you do not specify a stack policy, the current policy
      -- that is associated with the stack is unchanged.
    , _usiStackPolicyURL :: Maybe Text
      -- ^ Location of a file containing the updated stack policy. The URL
      -- must point to a policy (max size: 16KB) located in an S3 bucket
      -- in the same region as the stack. You can specify either the
      -- StackPolicyBody or the StackPolicyURL parameter, but not both.
      -- You might update the stack policy, for example, in order to
      -- protect a new resource that you created during a stack update. If
      -- you do not specify a stack policy, the current policy that is
      -- associated with the stack is unchanged.
    , _usiNotificationARNs :: [Text]
      -- ^ Update the ARNs for the Amazon SNS topics that are associated
      -- with the stack.
    } deriving (Show, Generic)

-- | The name or stack ID of the stack to update. Must contain only alphanumeric
-- characters (case sensitive) and start with an alpha character. Maximum
-- length of the name is 255 characters.
usiStackName :: Lens' UpdateStack (Text)
usiStackName = lens _usiStackName (\s a -> s { _usiStackName = a })
{-# INLINE usiStackName #-}

-- | Structure containing the template body with a minimum length of 1 byte and
-- a maximum length of 51,200 bytes. (For more information, go to Template
-- Anatomy in the AWS CloudFormation User Guide.) Conditional: You must
-- specify either the TemplateBody or the TemplateURL parameter, but not both.
usiTemplateBody :: Lens' UpdateStack (Maybe Text)
usiTemplateBody = lens _usiTemplateBody (\s a -> s { _usiTemplateBody = a })
{-# INLINE usiTemplateBody #-}

-- | Location of file containing the template body. The URL must point to a
-- template located in an S3 bucket in the same region as the stack. For more
-- information, go to Template Anatomy in the AWS CloudFormation User Guide.
-- Conditional: You must specify either the TemplateBody or the TemplateURL
-- parameter, but not both.
usiTemplateURL :: Lens' UpdateStack (Maybe Text)
usiTemplateURL = lens _usiTemplateURL (\s a -> s { _usiTemplateURL = a })
{-# INLINE usiTemplateURL #-}

-- | Reuse the existing template that is associated with the stack that you are
-- updating.
usiUsePreviousTemplate :: Lens' UpdateStack (Maybe Bool)
usiUsePreviousTemplate = lens _usiUsePreviousTemplate (\s a -> s { _usiUsePreviousTemplate = a })
{-# INLINE usiUsePreviousTemplate #-}

-- | Structure containing the temporary overriding stack policy body. You can
-- specify either the StackPolicyDuringUpdateBody or the
-- StackPolicyDuringUpdateURL parameter, but not both. If you want to update
-- protected resources, specify a temporary overriding stack policy during
-- this update. If you do not specify a stack policy, the current policy that
-- is associated with the stack will be used.
usiStackPolicyDuringUpdateBody :: Lens' UpdateStack (Maybe Text)
usiStackPolicyDuringUpdateBody = lens _usiStackPolicyDuringUpdateBody (\s a -> s { _usiStackPolicyDuringUpdateBody = a })
{-# INLINE usiStackPolicyDuringUpdateBody #-}

-- | Location of a file containing the temporary overriding stack policy. The
-- URL must point to a policy (max size: 16KB) located in an S3 bucket in the
-- same region as the stack. You can specify either the
-- StackPolicyDuringUpdateBody or the StackPolicyDuringUpdateURL parameter,
-- but not both. If you want to update protected resources, specify a
-- temporary overriding stack policy during this update. If you do not specify
-- a stack policy, the current policy that is associated with the stack will
-- be used.
usiStackPolicyDuringUpdateURL :: Lens' UpdateStack (Maybe Text)
usiStackPolicyDuringUpdateURL = lens _usiStackPolicyDuringUpdateURL (\s a -> s { _usiStackPolicyDuringUpdateURL = a })
{-# INLINE usiStackPolicyDuringUpdateURL #-}

-- | A list of Parameter structures that specify input parameters for the stack.
usiParameters :: Lens' UpdateStack ([Parameter])
usiParameters = lens _usiParameters (\s a -> s { _usiParameters = a })
{-# INLINE usiParameters #-}

-- | The list of capabilities that you want to allow in the stack. If your stack
-- contains IAM resources, you must specify the CAPABILITY_IAM value for this
-- parameter; otherwise, this action returns an InsufficientCapabilities
-- error. IAM resources are the following: AWS::IAM::AccessKey,
-- AWS::IAM::Group, AWS::IAM::Policy, AWS::IAM::User, and
-- AWS::IAM::UserToGroupAddition.
usiCapabilities :: Lens' UpdateStack ([Capability])
usiCapabilities = lens _usiCapabilities (\s a -> s { _usiCapabilities = a })
{-# INLINE usiCapabilities #-}

-- | Structure containing a new stack policy body. You can specify either the
-- StackPolicyBody or the StackPolicyURL parameter, but not both. You might
-- update the stack policy, for example, in order to protect a new resource
-- that you created during a stack update. If you do not specify a stack
-- policy, the current policy that is associated with the stack is unchanged.
usiStackPolicyBody :: Lens' UpdateStack (Maybe Text)
usiStackPolicyBody = lens _usiStackPolicyBody (\s a -> s { _usiStackPolicyBody = a })
{-# INLINE usiStackPolicyBody #-}

-- | Location of a file containing the updated stack policy. The URL must point
-- to a policy (max size: 16KB) located in an S3 bucket in the same region as
-- the stack. You can specify either the StackPolicyBody or the StackPolicyURL
-- parameter, but not both. You might update the stack policy, for example, in
-- order to protect a new resource that you created during a stack update. If
-- you do not specify a stack policy, the current policy that is associated
-- with the stack is unchanged.
usiStackPolicyURL :: Lens' UpdateStack (Maybe Text)
usiStackPolicyURL = lens _usiStackPolicyURL (\s a -> s { _usiStackPolicyURL = a })
{-# INLINE usiStackPolicyURL #-}

-- | Update the ARNs for the Amazon SNS topics that are associated with the
-- stack.
usiNotificationARNs :: Lens' UpdateStack ([Text])
usiNotificationARNs = lens _usiNotificationARNs (\s a -> s { _usiNotificationARNs = a })
{-# INLINE usiNotificationARNs #-}

instance ToQuery UpdateStack where
    toQuery = genericQuery def

newtype UpdateStackResponse = UpdateStackResponse
    { _usoStackId :: Maybe Text
      -- ^ Unique identifier of the stack.
    } deriving (Show, Generic)

-- | Unique identifier of the stack.
usoStackId :: Lens' UpdateStackResponse (Maybe Text)
usoStackId = lens _usoStackId (\s a -> s { _usoStackId = a })
{-# INLINE usoStackId #-}

instance FromXML UpdateStackResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest UpdateStack where
    type Sv UpdateStack = CloudFormation
    type Rs UpdateStack = UpdateStackResponse

    request = post "UpdateStack"
    response _ = xmlResponse
