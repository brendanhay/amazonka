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

-- Module      : Network.AWS.CloudFormation.CreateStack
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a stack as specified in the template. After the call completes
-- successfully, the stack creation starts. You can check the status of the
-- stack via the DescribeStacks API.
module Network.AWS.CloudFormation.CreateStack
    (
    -- * Request
      CreateStackInput
    -- ** Request constructor
    , createStackInput
    -- ** Request lenses
    , csiCapabilities
    , csiDisableRollback
    , csiNotificationARNs
    , csiOnFailure
    , csiParameters
    , csiStackName
    , csiStackPolicyBody
    , csiStackPolicyURL
    , csiTags
    , csiTemplateBody
    , csiTemplateURL
    , csiTimeoutInMinutes

    -- * Response
    , CreateStackOutput
    -- ** Response constructor
    , createStackOutput
    -- ** Response lenses
    , csoStackId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types

data CreateStackInput = CreateStackInput
    { _csiCapabilities     :: [Text]
    , _csiDisableRollback  :: Maybe Bool
    , _csiNotificationARNs :: [Text]
    , _csiOnFailure        :: Maybe Text
    , _csiParameters       :: [Parameter]
    , _csiStackName        :: Text
    , _csiStackPolicyBody  :: Maybe Text
    , _csiStackPolicyURL   :: Maybe Text
    , _csiTags             :: [Tag]
    , _csiTemplateBody     :: Maybe Text
    , _csiTemplateURL      :: Maybe Text
    , _csiTimeoutInMinutes :: Maybe Natural
    } (Eq, Show, Generic)

-- | 'CreateStackInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csiCapabilities' @::@ ['Text']
--
-- * 'csiDisableRollback' @::@ 'Maybe' 'Bool'
--
-- * 'csiNotificationARNs' @::@ ['Text']
--
-- * 'csiOnFailure' @::@ 'Maybe' 'Text'
--
-- * 'csiParameters' @::@ ['Parameter']
--
-- * 'csiStackName' @::@ 'Text'
--
-- * 'csiStackPolicyBody' @::@ 'Maybe' 'Text'
--
-- * 'csiStackPolicyURL' @::@ 'Maybe' 'Text'
--
-- * 'csiTags' @::@ ['Tag']
--
-- * 'csiTemplateBody' @::@ 'Maybe' 'Text'
--
-- * 'csiTemplateURL' @::@ 'Maybe' 'Text'
--
-- * 'csiTimeoutInMinutes' @::@ 'Maybe' 'Natural'
--
createStackInput :: Text -- ^ 'csiStackName'
                 -> CreateStackInput
createStackInput p1 = CreateStackInput
    { _csiStackName        = p1
    , _csiTemplateBody     = Nothing
    , _csiTemplateURL      = Nothing
    , _csiParameters       = mempty
    , _csiDisableRollback  = Nothing
    , _csiTimeoutInMinutes = Nothing
    , _csiNotificationARNs = mempty
    , _csiCapabilities     = mempty
    , _csiOnFailure        = Nothing
    , _csiStackPolicyBody  = Nothing
    , _csiStackPolicyURL   = Nothing
    , _csiTags             = mempty
    }

-- | The list of capabilities that you want to allow in the stack. If your
-- template contains certain resources, you must specify the CAPABILITY_IAM
-- value for this parameter; otherwise, this action returns an
-- InsufficientCapabilities error. The following resources require you to
-- specify the capabilities parameter: AWS::CloudFormation::Stack,
-- AWS::IAM::AccessKey, AWS::IAM::Group, AWS::IAM::InstanceProfile,
-- AWS::IAM::Policy, AWS::IAM::Role, AWS::IAM::User, and
-- AWS::IAM::UserToGroupAddition.
csiCapabilities :: Lens' CreateStackInput [Text]
csiCapabilities = lens _csiCapabilities (\s a -> s { _csiCapabilities = a })

-- | Set to true to disable rollback of the stack if stack creation failed.
-- You can specify either DisableRollback or OnFailure, but not both.
-- Default: false.
csiDisableRollback :: Lens' CreateStackInput (Maybe Bool)
csiDisableRollback =
    lens _csiDisableRollback (\s a -> s { _csiDisableRollback = a })

-- | The Simple Notification Service (SNS) topic ARNs to publish stack related
-- events. You can find your SNS topic ARNs using the SNS console or your
-- Command Line Interface (CLI).
csiNotificationARNs :: Lens' CreateStackInput [Text]
csiNotificationARNs =
    lens _csiNotificationARNs (\s a -> s { _csiNotificationARNs = a })

-- | Determines what action will be taken if stack creation fails. This must
-- be one of: DO_NOTHING, ROLLBACK, or DELETE. You can specify either
-- OnFailure or DisableRollback, but not both. Default: ROLLBACK.
csiOnFailure :: Lens' CreateStackInput (Maybe Text)
csiOnFailure = lens _csiOnFailure (\s a -> s { _csiOnFailure = a })

-- | A list of Parameter structures that specify input parameters for the
-- stack.
csiParameters :: Lens' CreateStackInput [Parameter]
csiParameters = lens _csiParameters (\s a -> s { _csiParameters = a })

-- | The name associated with the stack. The name must be unique within your
-- AWS account.
csiStackName :: Lens' CreateStackInput Text
csiStackName = lens _csiStackName (\s a -> s { _csiStackName = a })

-- | Structure containing the stack policy body. For more information, go to
-- Prevent Updates to Stack Resources in the AWS CloudFormation User Guide.
-- You can specify either the StackPolicyBody or the StackPolicyURL
-- parameter, but not both.
csiStackPolicyBody :: Lens' CreateStackInput (Maybe Text)
csiStackPolicyBody =
    lens _csiStackPolicyBody (\s a -> s { _csiStackPolicyBody = a })

-- | Location of a file containing the stack policy. The URL must point to a
-- policy (max size: 16KB) located in an S3 bucket in the same region as the
-- stack. You can specify either the StackPolicyBody or the StackPolicyURL
-- parameter, but not both.
csiStackPolicyURL :: Lens' CreateStackInput (Maybe Text)
csiStackPolicyURL =
    lens _csiStackPolicyURL (\s a -> s { _csiStackPolicyURL = a })

-- | A set of user-defined Tags to associate with this stack, represented by
-- key/value pairs. Tags defined for the stack are propagated to EC2
-- resources that are created as part of the stack. A maximum number of 10
-- tags can be specified.
csiTags :: Lens' CreateStackInput [Tag]
csiTags = lens _csiTags (\s a -> s { _csiTags = a })

-- | Structure containing the template body with a minimum length of 1 byte
-- and a maximum length of 51,200 bytes. For more information, go to
-- Template Anatomy in the AWS CloudFormation User Guide. Conditional: You
-- must specify either the TemplateBody or the TemplateURL parameter, but
-- not both.
csiTemplateBody :: Lens' CreateStackInput (Maybe Text)
csiTemplateBody = lens _csiTemplateBody (\s a -> s { _csiTemplateBody = a })

-- | Location of file containing the template body. The URL must point to a
-- template (max size: 307,200 bytes) located in an S3 bucket in the same
-- region as the stack. For more information, go to the Template Anatomy in
-- the AWS CloudFormation User Guide. Conditional: You must specify either
-- the TemplateBody or the TemplateURL parameter, but not both.
csiTemplateURL :: Lens' CreateStackInput (Maybe Text)
csiTemplateURL = lens _csiTemplateURL (\s a -> s { _csiTemplateURL = a })

-- | The amount of time that can pass before the stack status becomes
-- CREATE_FAILED; if DisableRollback is not set or is set to false, the
-- stack will be rolled back.
csiTimeoutInMinutes :: Lens' CreateStackInput (Maybe Natural)
csiTimeoutInMinutes =
    lens _csiTimeoutInMinutes (\s a -> s { _csiTimeoutInMinutes = a })
instance ToQuery CreateStackInput

instance ToPath CreateStackInput where
    toPath = const "/"

newtype CreateStackOutput = CreateStackOutput
    { _csoStackId :: Maybe Text
    } (Eq, Ord, Show, Generic, Monoid)

-- | 'CreateStackOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csoStackId' @::@ 'Maybe' 'Text'
--
createStackOutput :: CreateStackOutput
createStackOutput = CreateStackOutput
    { _csoStackId = Nothing
    }

-- | Unique identifier of the stack.
csoStackId :: Lens' CreateStackOutput (Maybe Text)
csoStackId = lens _csoStackId (\s a -> s { _csoStackId = a })

instance FromXML CreateStackOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateStackOutput"

instance AWSRequest CreateStackInput where
    type Sv CreateStackInput = CloudFormation
    type Rs CreateStackInput = CreateStackOutput

    request  = post "CreateStack"
    response = xmlResponse $ \h x -> CreateStackOutput
        <$> x %| "StackId"
