{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudFormation.V2010_05_15.CreateStack
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
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=CreateStack
-- &StackName=MyStack &TemplateBody=[Template Document]
-- &NotificationARNs.member.1=arn:aws:sns:us-east-1:1234567890:my-topic
-- &Parameters.member.1.ParameterKey=AvailabilityZone
-- &Parameters.member.1.ParameterValue=us-east-1a &Version=2010-05-15
-- &SignatureVersion=2 &Timestamp=2010-07-27T22%3A26%3A28.000Z
-- &AWSAccessKeyId=[AWS Access KeyID] &Signature=[Signature]
-- arn:aws:cloudformation:us-east-1:123456789:stack/MyStack/aaf549a0-a413-11df-adb3-5081b3858e83.
-- 
module Network.AWS.CloudFormation.V2010_05_15.CreateStack where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.V2010_05_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateStack' request.
createStack :: Text -- ^ '_csiStackName'
            -> CreateStack
createStack p1 = CreateStack
    { _csiStackName = p1
    , _csiCapabilities = mempty
    , _csiDisableRollback = Nothing
    , _csiNotificationARNs = mempty
    , _csiOnFailure = Nothing
    , _csiParameters = mempty
    , _csiStackPolicyBody = Nothing
    , _csiStackPolicyURL = Nothing
    , _csiTags = mempty
    , _csiTemplateBody = Nothing
    , _csiTemplateURL = Nothing
    , _csiTimeoutInMinutes = Nothing
    }

data CreateStack = CreateStack
    { _csiStackName :: Text
      -- ^ The name associated with the stack. The name must be unique
      -- within your AWS account. Must contain only alphanumeric
      -- characters (case sensitive) and start with an alpha character.
      -- Maximum length of the name is 255 characters.
    , _csiCapabilities :: [Capability]
      -- ^ The list of capabilities that you want to allow in the stack. If
      -- your template contains certain resources, you must specify the
      -- CAPABILITY_IAM value for this parameter; otherwise, this action
      -- returns an InsufficientCapabilities error. The following
      -- resources require you to specify the capabilities parameter:
      -- AWS::CloudFormation::Stack, AWS::IAM::AccessKey, AWS::IAM::Group,
      -- AWS::IAM::InstanceProfile, AWS::IAM::Policy, AWS::IAM::Role,
      -- AWS::IAM::User, and AWS::IAM::UserToGroupAddition.
    , _csiDisableRollback :: Maybe Bool
      -- ^ Set to true to disable rollback of the stack if stack creation
      -- failed. You can specify either DisableRollback or OnFailure, but
      -- not both. Default: false.
    , _csiNotificationARNs :: [Text]
      -- ^ The Simple Notification Service (SNS) topic ARNs to publish stack
      -- related events. You can find your SNS topic ARNs using the SNS
      -- console or your Command Line Interface (CLI).
    , _csiOnFailure :: Maybe OnFailure
      -- ^ Determines what action will be taken if stack creation fails.
      -- This must be one of: DO_NOTHING, ROLLBACK, or DELETE. You can
      -- specify either OnFailure or DisableRollback, but not both.
      -- Default: ROLLBACK.
    , _csiParameters :: [Parameter]
      -- ^ A list of Parameter structures that specify input parameters for
      -- the stack.
    , _csiStackPolicyBody :: Maybe Text
      -- ^ Structure containing the stack policy body. For more information,
      -- go to Prevent Updates to Stack Resources in the AWS
      -- CloudFormation User Guide. You can specify either the
      -- StackPolicyBody or the StackPolicyURL parameter, but not both.
    , _csiStackPolicyURL :: Maybe Text
      -- ^ Location of a file containing the stack policy. The URL must
      -- point to a policy (max size: 16KB) located in an S3 bucket in the
      -- same region as the stack. You can specify either the
      -- StackPolicyBody or the StackPolicyURL parameter, but not both.
    , _csiTags :: [Tag]
      -- ^ A set of user-defined Tags to associate with this stack,
      -- represented by key/value pairs. Tags defined for the stack are
      -- propagated to EC2 resources that are created as part of the
      -- stack. A maximum number of 10 tags can be specified.
    , _csiTemplateBody :: Maybe Text
      -- ^ Structure containing the template body with a minimum length of 1
      -- byte and a maximum length of 51,200 bytes. For more information,
      -- go to Template Anatomy in the AWS CloudFormation User Guide.
      -- Conditional: You must specify either the TemplateBody or the
      -- TemplateURL parameter, but not both.
    , _csiTemplateURL :: Maybe Text
      -- ^ Location of file containing the template body. The URL must point
      -- to a template (max size: 307,200 bytes) located in an S3 bucket
      -- in the same region as the stack. For more information, go to the
      -- Template Anatomy in the AWS CloudFormation User Guide.
      -- Conditional: You must specify either the TemplateBody or the
      -- TemplateURL parameter, but not both.
    , _csiTimeoutInMinutes :: Maybe Integer
      -- ^ The amount of time that can pass before the stack status becomes
      -- CREATE_FAILED; if DisableRollback is not set or is set to false,
      -- the stack will be rolled back.
    } deriving (Show, Generic)

makeLenses ''CreateStack

instance ToQuery CreateStack where
    toQuery = genericToQuery def

data CreateStackResponse = CreateStackResponse
    { _csoStackId :: Maybe Text
      -- ^ Unique identifier of the stack.
    } deriving (Show, Generic)

makeLenses ''CreateStackResponse

instance AWSRequest CreateStack where
    type Sv CreateStack = CloudFormation
    type Rs CreateStack = CreateStackResponse

    request = post "CreateStack"
    response _ = cursorResponse $ \hs xml ->
        pure CreateStackResponse
            <*> xml %|? "StackId"
