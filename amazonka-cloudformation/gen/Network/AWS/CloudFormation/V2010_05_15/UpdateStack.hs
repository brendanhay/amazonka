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
    , updateStack
    -- ** Request lenses
    , usiStackName
    , usiCapabilities
    , usiNotificationARNs
    , usiParameters
    , usiStackPolicyBody
    , usiStackPolicyDuringUpdateBody
    , usiStackPolicyDuringUpdateURL
    , usiStackPolicyURL
    , usiTemplateBody
    , usiTemplateURL
    , usiUsePreviousTemplate

    -- * Response
    , UpdateStackResponse
    -- ** Response lenses
    , usoStackId
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudFormation.V2010_05_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'UpdateStack' request.
updateStack :: Text -- ^ 'usiStackName'
            -> UpdateStack
updateStack p1 = UpdateStack
    { _usiStackName = p1
    , _usiCapabilities = mempty
    , _usiNotificationARNs = mempty
    , _usiParameters = mempty
    , _usiStackPolicyBody = Nothing
    , _usiStackPolicyDuringUpdateBody = Nothing
    , _usiStackPolicyDuringUpdateURL = Nothing
    , _usiStackPolicyURL = Nothing
    , _usiTemplateBody = Nothing
    , _usiTemplateURL = Nothing
    , _usiUsePreviousTemplate = Nothing
    }

data UpdateStack = UpdateStack
    { _usiStackName :: Text
      -- ^ The name or stack ID of the stack to update. Must contain only
      -- alphanumeric characters (case sensitive) and start with an alpha
      -- character. Maximum length of the name is 255 characters.
    , _usiCapabilities :: [Capability]
      -- ^ The list of capabilities that you want to allow in the stack. If
      -- your stack contains IAM resources, you must specify the
      -- CAPABILITY_IAM value for this parameter; otherwise, this action
      -- returns an InsufficientCapabilities error. IAM resources are the
      -- following: AWS::IAM::AccessKey, AWS::IAM::Group,
      -- AWS::IAM::Policy, AWS::IAM::User, and
      -- AWS::IAM::UserToGroupAddition.
    , _usiNotificationARNs :: [Text]
      -- ^ Update the ARNs for the Amazon SNS topics that are associated
      -- with the stack.
    , _usiParameters :: [Parameter]
      -- ^ A list of Parameter structures that specify input parameters for
      -- the stack.
    , _usiStackPolicyBody :: Maybe Text
      -- ^ Structure containing a new stack policy body. You can specify
      -- either the StackPolicyBody or the StackPolicyURL parameter, but
      -- not both. You might update the stack policy, for example, in
      -- order to protect a new resource that you created during a stack
      -- update. If you do not specify a stack policy, the current policy
      -- that is associated with the stack is unchanged.
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
    , _usiStackPolicyURL :: Maybe Text
      -- ^ Location of a file containing the updated stack policy. The URL
      -- must point to a policy (max size: 16KB) located in an S3 bucket
      -- in the same region as the stack. You can specify either the
      -- StackPolicyBody or the StackPolicyURL parameter, but not both.
      -- You might update the stack policy, for example, in order to
      -- protect a new resource that you created during a stack update. If
      -- you do not specify a stack policy, the current policy that is
      -- associated with the stack is unchanged.
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
    } deriving (Show, Generic)

-- | The name or stack ID of the stack to update. Must contain only alphanumeric
-- characters (case sensitive) and start with an alpha character. Maximum
-- length of the name is 255 characters.
usiStackName
    :: Functor f
    => (Text
    -> f (Text))
    -> UpdateStack
    -> f UpdateStack
usiStackName f x =
    (\y -> x { _usiStackName = y })
       <$> f (_usiStackName x)
{-# INLINE usiStackName #-}

-- | The list of capabilities that you want to allow in the stack. If your stack
-- contains IAM resources, you must specify the CAPABILITY_IAM value for this
-- parameter; otherwise, this action returns an InsufficientCapabilities
-- error. IAM resources are the following: AWS::IAM::AccessKey,
-- AWS::IAM::Group, AWS::IAM::Policy, AWS::IAM::User, and
-- AWS::IAM::UserToGroupAddition.
usiCapabilities
    :: Functor f
    => ([Capability]
    -> f ([Capability]))
    -> UpdateStack
    -> f UpdateStack
usiCapabilities f x =
    (\y -> x { _usiCapabilities = y })
       <$> f (_usiCapabilities x)
{-# INLINE usiCapabilities #-}

-- | Update the ARNs for the Amazon SNS topics that are associated with the
-- stack.
usiNotificationARNs
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> UpdateStack
    -> f UpdateStack
usiNotificationARNs f x =
    (\y -> x { _usiNotificationARNs = y })
       <$> f (_usiNotificationARNs x)
{-# INLINE usiNotificationARNs #-}

-- | A list of Parameter structures that specify input parameters for the stack.
usiParameters
    :: Functor f
    => ([Parameter]
    -> f ([Parameter]))
    -> UpdateStack
    -> f UpdateStack
usiParameters f x =
    (\y -> x { _usiParameters = y })
       <$> f (_usiParameters x)
{-# INLINE usiParameters #-}

-- | Structure containing a new stack policy body. You can specify either the
-- StackPolicyBody or the StackPolicyURL parameter, but not both. You might
-- update the stack policy, for example, in order to protect a new resource
-- that you created during a stack update. If you do not specify a stack
-- policy, the current policy that is associated with the stack is unchanged.
usiStackPolicyBody
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateStack
    -> f UpdateStack
usiStackPolicyBody f x =
    (\y -> x { _usiStackPolicyBody = y })
       <$> f (_usiStackPolicyBody x)
{-# INLINE usiStackPolicyBody #-}

-- | Structure containing the temporary overriding stack policy body. You can
-- specify either the StackPolicyDuringUpdateBody or the
-- StackPolicyDuringUpdateURL parameter, but not both. If you want to update
-- protected resources, specify a temporary overriding stack policy during
-- this update. If you do not specify a stack policy, the current policy that
-- is associated with the stack will be used.
usiStackPolicyDuringUpdateBody
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateStack
    -> f UpdateStack
usiStackPolicyDuringUpdateBody f x =
    (\y -> x { _usiStackPolicyDuringUpdateBody = y })
       <$> f (_usiStackPolicyDuringUpdateBody x)
{-# INLINE usiStackPolicyDuringUpdateBody #-}

-- | Location of a file containing the temporary overriding stack policy. The
-- URL must point to a policy (max size: 16KB) located in an S3 bucket in the
-- same region as the stack. You can specify either the
-- StackPolicyDuringUpdateBody or the StackPolicyDuringUpdateURL parameter,
-- but not both. If you want to update protected resources, specify a
-- temporary overriding stack policy during this update. If you do not specify
-- a stack policy, the current policy that is associated with the stack will
-- be used.
usiStackPolicyDuringUpdateURL
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateStack
    -> f UpdateStack
usiStackPolicyDuringUpdateURL f x =
    (\y -> x { _usiStackPolicyDuringUpdateURL = y })
       <$> f (_usiStackPolicyDuringUpdateURL x)
{-# INLINE usiStackPolicyDuringUpdateURL #-}

-- | Location of a file containing the updated stack policy. The URL must point
-- to a policy (max size: 16KB) located in an S3 bucket in the same region as
-- the stack. You can specify either the StackPolicyBody or the StackPolicyURL
-- parameter, but not both. You might update the stack policy, for example, in
-- order to protect a new resource that you created during a stack update. If
-- you do not specify a stack policy, the current policy that is associated
-- with the stack is unchanged.
usiStackPolicyURL
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateStack
    -> f UpdateStack
usiStackPolicyURL f x =
    (\y -> x { _usiStackPolicyURL = y })
       <$> f (_usiStackPolicyURL x)
{-# INLINE usiStackPolicyURL #-}

-- | Structure containing the template body with a minimum length of 1 byte and
-- a maximum length of 51,200 bytes. (For more information, go to Template
-- Anatomy in the AWS CloudFormation User Guide.) Conditional: You must
-- specify either the TemplateBody or the TemplateURL parameter, but not both.
usiTemplateBody
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateStack
    -> f UpdateStack
usiTemplateBody f x =
    (\y -> x { _usiTemplateBody = y })
       <$> f (_usiTemplateBody x)
{-# INLINE usiTemplateBody #-}

-- | Location of file containing the template body. The URL must point to a
-- template located in an S3 bucket in the same region as the stack. For more
-- information, go to Template Anatomy in the AWS CloudFormation User Guide.
-- Conditional: You must specify either the TemplateBody or the TemplateURL
-- parameter, but not both.
usiTemplateURL
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateStack
    -> f UpdateStack
usiTemplateURL f x =
    (\y -> x { _usiTemplateURL = y })
       <$> f (_usiTemplateURL x)
{-# INLINE usiTemplateURL #-}

-- | Reuse the existing template that is associated with the stack that you are
-- updating.
usiUsePreviousTemplate
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> UpdateStack
    -> f UpdateStack
usiUsePreviousTemplate f x =
    (\y -> x { _usiUsePreviousTemplate = y })
       <$> f (_usiUsePreviousTemplate x)
{-# INLINE usiUsePreviousTemplate #-}

instance ToQuery UpdateStack where
    toQuery = genericQuery def

data UpdateStackResponse = UpdateStackResponse
    { _usoStackId :: Maybe Text
      -- ^ Unique identifier of the stack.
    } deriving (Show, Generic)

-- | Unique identifier of the stack.
usoStackId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateStackResponse
    -> f UpdateStackResponse
usoStackId f x =
    (\y -> x { _usoStackId = y })
       <$> f (_usoStackId x)
{-# INLINE usoStackId #-}

instance FromXML UpdateStackResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest UpdateStack where
    type Sv UpdateStack = CloudFormation
    type Rs UpdateStack = UpdateStackResponse

    request = post "UpdateStack"
    response _ = xmlResponse
