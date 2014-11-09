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

-- Module      : Network.AWS.CloudFormation.DescribeStackResources
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns AWS resource descriptions for running and deleted stacks. If
-- StackName is specified, all the associated resources that are part of the
-- stack are returned. If PhysicalResourceId is specified, the associated
-- resources of the stack that the resource belongs to are returned. For
-- deleted stacks, DescribeStackResources returns resource information for up
-- to 90 days after the stack has been deleted. You must specify either
-- StackName or PhysicalResourceId, but not both. In addition, you can specify
-- LogicalResourceId to filter the returned result. For more information about
-- resources, the LogicalResourceId and PhysicalResourceId, go to the AWS
-- CloudFormation User Guide.
module Network.AWS.CloudFormation.DescribeStackResources
    (
    -- * Request
      DescribeStackResourcesInput
    -- ** Request constructor
    , describeStackResourcesInput
    -- ** Request lenses
    , dsriLogicalResourceId
    , dsriPhysicalResourceId
    , dsriStackName

    -- * Response
    , DescribeStackResourcesOutput
    -- ** Response constructor
    , describeStackResourcesOutput
    -- ** Response lenses
    , dsroStackResources
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types

data DescribeStackResourcesInput = DescribeStackResourcesInput
    { _dsriLogicalResourceId  :: Maybe Text
    , _dsriPhysicalResourceId :: Maybe Text
    , _dsriStackName          :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeStackResourcesInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsriLogicalResourceId' @::@ 'Maybe' 'Text'
--
-- * 'dsriPhysicalResourceId' @::@ 'Maybe' 'Text'
--
-- * 'dsriStackName' @::@ 'Maybe' 'Text'
--
describeStackResourcesInput :: DescribeStackResourcesInput
describeStackResourcesInput = DescribeStackResourcesInput
    { _dsriStackName          = Nothing
    , _dsriLogicalResourceId  = Nothing
    , _dsriPhysicalResourceId = Nothing
    }

-- | The logical name of the resource as specified in the template. Default:
-- There is no default value.
dsriLogicalResourceId :: Lens' DescribeStackResourcesInput (Maybe Text)
dsriLogicalResourceId =
    lens _dsriLogicalResourceId (\s a -> s { _dsriLogicalResourceId = a })

-- | The name or unique identifier that corresponds to a physical instance ID
-- of a resource supported by AWS CloudFormation. For example, for an Amazon
-- Elastic Compute Cloud (EC2) instance, PhysicalResourceId corresponds to
-- the InstanceId. You can pass the EC2 InstanceId to DescribeStackResources
-- to find which stack the instance belongs to and what other resources are
-- part of the stack. Required: Conditional. If you do not specify
-- PhysicalResourceId, you must specify StackName. Default: There is no
-- default value.
dsriPhysicalResourceId :: Lens' DescribeStackResourcesInput (Maybe Text)
dsriPhysicalResourceId =
    lens _dsriPhysicalResourceId (\s a -> s { _dsriPhysicalResourceId = a })

-- | The name or the unique identifier associated with the stack, which are
-- not always interchangeable: Running stacks: You can specify either the
-- stack's name or its unique stack ID. Deleted stacks: You must specify the
-- unique stack ID. Default: There is no default value. Required:
-- Conditional. If you do not specify StackName, you must specify
-- PhysicalResourceId.
dsriStackName :: Lens' DescribeStackResourcesInput (Maybe Text)
dsriStackName = lens _dsriStackName (\s a -> s { _dsriStackName = a })

instance ToPath DescribeStackResourcesInput where
    toPath = const "/"

instance ToQuery DescribeStackResourcesInput

newtype DescribeStackResourcesOutput = DescribeStackResourcesOutput
    { _dsroStackResources :: [StackResource]
    } deriving (Eq, Show, Generic, Monoid)

-- | 'DescribeStackResourcesOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsroStackResources' @::@ ['StackResource']
--
describeStackResourcesOutput :: DescribeStackResourcesOutput
describeStackResourcesOutput = DescribeStackResourcesOutput
    { _dsroStackResources = mempty
    }

-- | A list of StackResource structures.
dsroStackResources :: Lens' DescribeStackResourcesOutput [StackResource]
dsroStackResources =
    lens _dsroStackResources (\s a -> s { _dsroStackResources = a })

instance AWSRequest DescribeStackResourcesInput where
    type Sv DescribeStackResourcesInput = CloudFormation
    type Rs DescribeStackResourcesInput = DescribeStackResourcesOutput

    request  = post "DescribeStackResources"
    response = const . xmlResponse $ \h x -> DescribeStackResourcesOutput
newtype
