{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.CloudFormation.V2010_05_15.CreateStack
    (
    -- * Request
      CreateStack
    -- ** Request constructor
    , mkCreateStackInput
    -- ** Request lenses
    , csiStackName
    , csiTemplateBody
    , csiTemplateURL
    , csiParameters
    , csiDisableRollback
    , csiTimeoutInMinutes
    , csiNotificationARNs
    , csiCapabilities
    , csiOnFailure
    , csiStackPolicyBody
    , csiStackPolicyURL
    , csiTags

    -- * Response
    , CreateStackResponse
    -- ** Response lenses
    , csoStackId
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudFormation.V2010_05_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateStack' request.
mkCreateStackInput :: Text -- ^ 'csiStackName'
                   -> CreateStack
mkCreateStackInput p1 = CreateStack
    { _csiStackName = p1
    , _csiTemplateBody = Nothing
    , _csiTemplateURL = Nothing
    , _csiParameters = mempty
    , _csiDisableRollback = Nothing
    , _csiTimeoutInMinutes = Nothing
    , _csiNotificationARNs = mempty
    , _csiCapabilities = mempty
    , _csiOnFailure = Nothing
    , _csiStackPolicyBody = Nothing
    , _csiStackPolicyURL = Nothing
    , _csiTags = mempty
    }
{-# INLINE mkCreateStackInput #-}

data CreateStack = CreateStack
    { _csiStackName :: Text
      -- ^ The name associated with the stack. The name must be unique
      -- within your AWS account. Must contain only alphanumeric
      -- characters (case sensitive) and start with an alpha character.
      -- Maximum length of the name is 255 characters.
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
    , _csiParameters :: [Parameter]
      -- ^ A list of Parameter structures that specify input parameters for
      -- the stack.
    , _csiDisableRollback :: Maybe Bool
      -- ^ Set to true to disable rollback of the stack if stack creation
      -- failed. You can specify either DisableRollback or OnFailure, but
      -- not both. Default: false.
    , _csiTimeoutInMinutes :: Maybe Integer
      -- ^ The amount of time that can pass before the stack status becomes
      -- CREATE_FAILED; if DisableRollback is not set or is set to false,
      -- the stack will be rolled back.
    , _csiNotificationARNs :: [Text]
      -- ^ The Simple Notification Service (SNS) topic ARNs to publish stack
      -- related events. You can find your SNS topic ARNs using the SNS
      -- console or your Command Line Interface (CLI).
    , _csiCapabilities :: [Capability]
      -- ^ The list of capabilities that you want to allow in the stack. If
      -- your template contains certain resources, you must specify the
      -- CAPABILITY_IAM value for this parameter; otherwise, this action
      -- returns an InsufficientCapabilities error. The following
      -- resources require you to specify the capabilities parameter:
      -- AWS::CloudFormation::Stack, AWS::IAM::AccessKey, AWS::IAM::Group,
      -- AWS::IAM::InstanceProfile, AWS::IAM::Policy, AWS::IAM::Role,
      -- AWS::IAM::User, and AWS::IAM::UserToGroupAddition.
    , _csiOnFailure :: Maybe OnFailure
      -- ^ Determines what action will be taken if stack creation fails.
      -- This must be one of: DO_NOTHING, ROLLBACK, or DELETE. You can
      -- specify either OnFailure or DisableRollback, but not both.
      -- Default: ROLLBACK.
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
    } deriving (Show, Generic)

-- | The name associated with the stack. The name must be unique within your AWS
-- account. Must contain only alphanumeric characters (case sensitive) and
-- start with an alpha character. Maximum length of the name is 255
-- characters.
csiStackName :: Lens' CreateStack (Text)
csiStackName = lens _csiStackName (\s a -> s { _csiStackName = a })
{-# INLINE csiStackName #-}

-- | Structure containing the template body with a minimum length of 1 byte and
-- a maximum length of 51,200 bytes. For more information, go to Template
-- Anatomy in the AWS CloudFormation User Guide. Conditional: You must specify
-- either the TemplateBody or the TemplateURL parameter, but not both.
csiTemplateBody :: Lens' CreateStack (Maybe Text)
csiTemplateBody = lens _csiTemplateBody (\s a -> s { _csiTemplateBody = a })
{-# INLINE csiTemplateBody #-}

-- | Location of file containing the template body. The URL must point to a
-- template (max size: 307,200 bytes) located in an S3 bucket in the same
-- region as the stack. For more information, go to the Template Anatomy in
-- the AWS CloudFormation User Guide. Conditional: You must specify either the
-- TemplateBody or the TemplateURL parameter, but not both.
csiTemplateURL :: Lens' CreateStack (Maybe Text)
csiTemplateURL = lens _csiTemplateURL (\s a -> s { _csiTemplateURL = a })
{-# INLINE csiTemplateURL #-}

-- | A list of Parameter structures that specify input parameters for the stack.
csiParameters :: Lens' CreateStack ([Parameter])
csiParameters = lens _csiParameters (\s a -> s { _csiParameters = a })
{-# INLINE csiParameters #-}

-- | Set to true to disable rollback of the stack if stack creation failed. You
-- can specify either DisableRollback or OnFailure, but not both. Default:
-- false.
csiDisableRollback :: Lens' CreateStack (Maybe Bool)
csiDisableRollback = lens _csiDisableRollback (\s a -> s { _csiDisableRollback = a })
{-# INLINE csiDisableRollback #-}

-- | The amount of time that can pass before the stack status becomes
-- CREATE_FAILED; if DisableRollback is not set or is set to false, the stack
-- will be rolled back.
csiTimeoutInMinutes :: Lens' CreateStack (Maybe Integer)
csiTimeoutInMinutes = lens _csiTimeoutInMinutes (\s a -> s { _csiTimeoutInMinutes = a })
{-# INLINE csiTimeoutInMinutes #-}

-- | The Simple Notification Service (SNS) topic ARNs to publish stack related
-- events. You can find your SNS topic ARNs using the SNS console or your
-- Command Line Interface (CLI).
csiNotificationARNs :: Lens' CreateStack ([Text])
csiNotificationARNs = lens _csiNotificationARNs (\s a -> s { _csiNotificationARNs = a })
{-# INLINE csiNotificationARNs #-}

-- | The list of capabilities that you want to allow in the stack. If your
-- template contains certain resources, you must specify the CAPABILITY_IAM
-- value for this parameter; otherwise, this action returns an
-- InsufficientCapabilities error. The following resources require you to
-- specify the capabilities parameter: AWS::CloudFormation::Stack,
-- AWS::IAM::AccessKey, AWS::IAM::Group, AWS::IAM::InstanceProfile,
-- AWS::IAM::Policy, AWS::IAM::Role, AWS::IAM::User, and
-- AWS::IAM::UserToGroupAddition.
csiCapabilities :: Lens' CreateStack ([Capability])
csiCapabilities = lens _csiCapabilities (\s a -> s { _csiCapabilities = a })
{-# INLINE csiCapabilities #-}

-- | Determines what action will be taken if stack creation fails. This must be
-- one of: DO_NOTHING, ROLLBACK, or DELETE. You can specify either OnFailure
-- or DisableRollback, but not both. Default: ROLLBACK.
csiOnFailure :: Lens' CreateStack (Maybe OnFailure)
csiOnFailure = lens _csiOnFailure (\s a -> s { _csiOnFailure = a })
{-# INLINE csiOnFailure #-}

-- | Structure containing the stack policy body. For more information, go to
-- Prevent Updates to Stack Resources in the AWS CloudFormation User Guide.
-- You can specify either the StackPolicyBody or the StackPolicyURL parameter,
-- but not both.
csiStackPolicyBody :: Lens' CreateStack (Maybe Text)
csiStackPolicyBody = lens _csiStackPolicyBody (\s a -> s { _csiStackPolicyBody = a })
{-# INLINE csiStackPolicyBody #-}

-- | Location of a file containing the stack policy. The URL must point to a
-- policy (max size: 16KB) located in an S3 bucket in the same region as the
-- stack. You can specify either the StackPolicyBody or the StackPolicyURL
-- parameter, but not both.
csiStackPolicyURL :: Lens' CreateStack (Maybe Text)
csiStackPolicyURL = lens _csiStackPolicyURL (\s a -> s { _csiStackPolicyURL = a })
{-# INLINE csiStackPolicyURL #-}

-- | A set of user-defined Tags to associate with this stack, represented by
-- key/value pairs. Tags defined for the stack are propagated to EC2 resources
-- that are created as part of the stack. A maximum number of 10 tags can be
-- specified.
csiTags :: Lens' CreateStack ([Tag])
csiTags = lens _csiTags (\s a -> s { _csiTags = a })
{-# INLINE csiTags #-}

instance ToQuery CreateStack where
    toQuery = genericQuery def

newtype CreateStackResponse = CreateStackResponse
    { _csoStackId :: Maybe Text
      -- ^ Unique identifier of the stack.
    } deriving (Show, Generic)

-- | Unique identifier of the stack.
csoStackId :: Lens' CreateStackResponse (Maybe Text)
csoStackId = lens _csoStackId (\s a -> s { _csoStackId = a })
{-# INLINE csoStackId #-}

instance FromXML CreateStackResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateStack where
    type Sv CreateStack = CloudFormation
    type Rs CreateStack = CreateStackResponse

    request = post "CreateStack"
    response _ = xmlResponse
