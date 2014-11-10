{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
module Network.AWS.CloudFormation.UpdateStack
    (
    -- * Request
      UpdateStackInput
    -- ** Request constructor
    , updateStack
    -- ** Request lenses
    , usiCapabilities
    , usiNotificationARNs
    , usiParameters
    , usiStackName
    , usiStackPolicyBody
    , usiStackPolicyDuringUpdateBody
    , usiStackPolicyDuringUpdateURL
    , usiStackPolicyURL
    , usiTemplateBody
    , usiTemplateURL
    , usiUsePreviousTemplate

    -- * Response
    , UpdateStackOutput
    -- ** Response constructor
    , updateStackResponse
    -- ** Response lenses
    , usoStackId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types

data UpdateStackInput = UpdateStackInput
    { _usiCapabilities                :: [Text]
    , _usiNotificationARNs            :: [Text]
    , _usiParameters                  :: [Parameter]
    , _usiStackName                   :: Text
    , _usiStackPolicyBody             :: Maybe Text
    , _usiStackPolicyDuringUpdateBody :: Maybe Text
    , _usiStackPolicyDuringUpdateURL  :: Maybe Text
    , _usiStackPolicyURL              :: Maybe Text
    , _usiTemplateBody                :: Maybe Text
    , _usiTemplateURL                 :: Maybe Text
    , _usiUsePreviousTemplate         :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | 'UpdateStackInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usiCapabilities' @::@ ['Text']
--
-- * 'usiNotificationARNs' @::@ ['Text']
--
-- * 'usiParameters' @::@ ['Parameter']
--
-- * 'usiStackName' @::@ 'Text'
--
-- * 'usiStackPolicyBody' @::@ 'Maybe' 'Text'
--
-- * 'usiStackPolicyDuringUpdateBody' @::@ 'Maybe' 'Text'
--
-- * 'usiStackPolicyDuringUpdateURL' @::@ 'Maybe' 'Text'
--
-- * 'usiStackPolicyURL' @::@ 'Maybe' 'Text'
--
-- * 'usiTemplateBody' @::@ 'Maybe' 'Text'
--
-- * 'usiTemplateURL' @::@ 'Maybe' 'Text'
--
-- * 'usiUsePreviousTemplate' @::@ 'Maybe' 'Bool'
--
updateStack :: Text -- ^ 'usiStackName'
            -> UpdateStackInput
updateStack p1 = UpdateStackInput
    { _usiStackName                   = p1
    , _usiTemplateBody                = Nothing
    , _usiTemplateURL                 = Nothing
    , _usiUsePreviousTemplate         = Nothing
    , _usiStackPolicyDuringUpdateBody = Nothing
    , _usiStackPolicyDuringUpdateURL  = Nothing
    , _usiParameters                  = mempty
    , _usiCapabilities                = mempty
    , _usiStackPolicyBody             = Nothing
    , _usiStackPolicyURL              = Nothing
    , _usiNotificationARNs            = mempty
    }

-- | The list of capabilities that you want to allow in the stack. If your
-- stack contains IAM resources, you must specify the CAPABILITY_IAM value
-- for this parameter; otherwise, this action returns an
-- InsufficientCapabilities error. IAM resources are the following:
-- AWS::IAM::AccessKey, AWS::IAM::Group, AWS::IAM::Policy, AWS::IAM::User,
-- and AWS::IAM::UserToGroupAddition.
usiCapabilities :: Lens' UpdateStackInput [Text]
usiCapabilities = lens _usiCapabilities (\s a -> s { _usiCapabilities = a })

-- | Update the ARNs for the Amazon SNS topics that are associated with the
-- stack.
usiNotificationARNs :: Lens' UpdateStackInput [Text]
usiNotificationARNs =
    lens _usiNotificationARNs (\s a -> s { _usiNotificationARNs = a })

-- | A list of Parameter structures that specify input parameters for the
-- stack.
usiParameters :: Lens' UpdateStackInput [Parameter]
usiParameters = lens _usiParameters (\s a -> s { _usiParameters = a })

-- | The name or stack ID of the stack to update.
usiStackName :: Lens' UpdateStackInput Text
usiStackName = lens _usiStackName (\s a -> s { _usiStackName = a })

-- | Structure containing a new stack policy body. You can specify either the
-- StackPolicyBody or the StackPolicyURL parameter, but not both. You might
-- update the stack policy, for example, in order to protect a new resource
-- that you created during a stack update. If you do not specify a stack
-- policy, the current policy that is associated with the stack is
-- unchanged.
usiStackPolicyBody :: Lens' UpdateStackInput (Maybe Text)
usiStackPolicyBody =
    lens _usiStackPolicyBody (\s a -> s { _usiStackPolicyBody = a })

-- | Structure containing the temporary overriding stack policy body. You can
-- specify either the StackPolicyDuringUpdateBody or the
-- StackPolicyDuringUpdateURL parameter, but not both. If you want to update
-- protected resources, specify a temporary overriding stack policy during
-- this update. If you do not specify a stack policy, the current policy
-- that is associated with the stack will be used.
usiStackPolicyDuringUpdateBody :: Lens' UpdateStackInput (Maybe Text)
usiStackPolicyDuringUpdateBody =
    lens _usiStackPolicyDuringUpdateBody
        (\s a -> s { _usiStackPolicyDuringUpdateBody = a })

-- | Location of a file containing the temporary overriding stack policy. The
-- URL must point to a policy (max size: 16KB) located in an S3 bucket in
-- the same region as the stack. You can specify either the
-- StackPolicyDuringUpdateBody or the StackPolicyDuringUpdateURL parameter,
-- but not both. If you want to update protected resources, specify a
-- temporary overriding stack policy during this update. If you do not
-- specify a stack policy, the current policy that is associated with the
-- stack will be used.
usiStackPolicyDuringUpdateURL :: Lens' UpdateStackInput (Maybe Text)
usiStackPolicyDuringUpdateURL =
    lens _usiStackPolicyDuringUpdateURL
        (\s a -> s { _usiStackPolicyDuringUpdateURL = a })

-- | Location of a file containing the updated stack policy. The URL must
-- point to a policy (max size: 16KB) located in an S3 bucket in the same
-- region as the stack. You can specify either the StackPolicyBody or the
-- StackPolicyURL parameter, but not both. You might update the stack
-- policy, for example, in order to protect a new resource that you created
-- during a stack update. If you do not specify a stack policy, the current
-- policy that is associated with the stack is unchanged.
usiStackPolicyURL :: Lens' UpdateStackInput (Maybe Text)
usiStackPolicyURL =
    lens _usiStackPolicyURL (\s a -> s { _usiStackPolicyURL = a })

-- | Structure containing the template body with a minimum length of 1 byte
-- and a maximum length of 51,200 bytes. (For more information, go to
-- Template Anatomy in the AWS CloudFormation User Guide.) Conditional: You
-- must specify either the TemplateBody or the TemplateURL parameter, but
-- not both.
usiTemplateBody :: Lens' UpdateStackInput (Maybe Text)
usiTemplateBody = lens _usiTemplateBody (\s a -> s { _usiTemplateBody = a })

-- | Location of file containing the template body. The URL must point to a
-- template located in an S3 bucket in the same region as the stack. For
-- more information, go to Template Anatomy in the AWS CloudFormation User
-- Guide. Conditional: You must specify either the TemplateBody or the
-- TemplateURL parameter, but not both.
usiTemplateURL :: Lens' UpdateStackInput (Maybe Text)
usiTemplateURL = lens _usiTemplateURL (\s a -> s { _usiTemplateURL = a })

-- | Reuse the existing template that is associated with the stack that you
-- are updating.
usiUsePreviousTemplate :: Lens' UpdateStackInput (Maybe Bool)
usiUsePreviousTemplate =
    lens _usiUsePreviousTemplate (\s a -> s { _usiUsePreviousTemplate = a })

instance ToPath UpdateStackInput where
    toPath = const "/"

instance ToQuery UpdateStackInput

newtype UpdateStackOutput = UpdateStackOutput
    { _usoStackId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'UpdateStackOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usoStackId' @::@ 'Maybe' 'Text'
--
updateStackResponse :: UpdateStackOutput
updateStackResponse = UpdateStackOutput
    { _usoStackId = Nothing
    }

-- | Unique identifier of the stack.
usoStackId :: Lens' UpdateStackOutput (Maybe Text)
usoStackId = lens _usoStackId (\s a -> s { _usoStackId = a })

instance AWSRequest UpdateStackInput where
    type Sv UpdateStackInput = CloudFormation
    type Rs UpdateStackInput = UpdateStackOutput

    request  = post "UpdateStack"
    response = xmlResponse $ \h x -> UpdateStackOutput
        <$> x %| "StackId"
