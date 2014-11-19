{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
-- resources of the stack that the resource belongs to are returned. Only the
-- first 100 resources will be returned. If your stack has more resources than
-- this, you should use ListStackResources instead. For deleted stacks,
-- DescribeStackResources returns resource information for up to 90 days after
-- the stack has been deleted. You must specify either StackName or
-- PhysicalResourceId, but not both. In addition, you can specify
-- LogicalResourceId to filter the returned result. For more information about
-- resources, the LogicalResourceId and PhysicalResourceId, go to the AWS
-- CloudFormation User Guide. A ValidationError is returned if you specify
-- both StackName and PhysicalResourceId in the same request.
--
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DescribeStackResources.html>
module Network.AWS.CloudFormation.DescribeStackResources
    (
    -- * Request
      DescribeStackResources
    -- ** Request constructor
    , describeStackResources
    -- ** Request lenses
    , dsrLogicalResourceId
    , dsrPhysicalResourceId
    , dsrStackName

    -- * Response
    , DescribeStackResourcesResponse
    -- ** Response constructor
    , describeStackResourcesResponse
    -- ** Response lenses
    , dsrrStackResources
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types
import qualified GHC.Exts

data DescribeStackResources = DescribeStackResources
    { _dsrLogicalResourceId  :: Maybe Text
    , _dsrPhysicalResourceId :: Maybe Text
    , _dsrStackName          :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeStackResources' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrLogicalResourceId' @::@ 'Maybe' 'Text'
--
-- * 'dsrPhysicalResourceId' @::@ 'Maybe' 'Text'
--
-- * 'dsrStackName' @::@ 'Maybe' 'Text'
--
describeStackResources :: DescribeStackResources
describeStackResources = DescribeStackResources
    { _dsrStackName          = Nothing
    , _dsrLogicalResourceId  = Nothing
    , _dsrPhysicalResourceId = Nothing
    }

-- | The logical name of the resource as specified in the template. Default:
-- There is no default value.
dsrLogicalResourceId :: Lens' DescribeStackResources (Maybe Text)
dsrLogicalResourceId =
    lens _dsrLogicalResourceId (\s a -> s { _dsrLogicalResourceId = a })

-- | The name or unique identifier that corresponds to a physical instance ID
-- of a resource supported by AWS CloudFormation. For example, for an Amazon
-- Elastic Compute Cloud (EC2) instance, PhysicalResourceId corresponds to
-- the InstanceId. You can pass the EC2 InstanceId to DescribeStackResources
-- to find which stack the instance belongs to and what other resources are
-- part of the stack. Required: Conditional. If you do not specify
-- PhysicalResourceId, you must specify StackName. Default: There is no
-- default value.
dsrPhysicalResourceId :: Lens' DescribeStackResources (Maybe Text)
dsrPhysicalResourceId =
    lens _dsrPhysicalResourceId (\s a -> s { _dsrPhysicalResourceId = a })

-- | The name or the unique identifier associated with the stack, which are
-- not always interchangeable: Running stacks: You can specify either the
-- stack's name or its unique stack ID. Deleted stacks: You must specify the
-- unique stack ID. Default: There is no default value. Required:
-- Conditional. If you do not specify StackName, you must specify
-- PhysicalResourceId.
dsrStackName :: Lens' DescribeStackResources (Maybe Text)
dsrStackName = lens _dsrStackName (\s a -> s { _dsrStackName = a })

newtype DescribeStackResourcesResponse = DescribeStackResourcesResponse
    { _dsrrStackResources :: [StackResource]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeStackResourcesResponse where
    type Item DescribeStackResourcesResponse = StackResource

    fromList = DescribeStackResourcesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dsrrStackResources

-- | 'DescribeStackResourcesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrrStackResources' @::@ ['StackResource']
--
describeStackResourcesResponse :: DescribeStackResourcesResponse
describeStackResourcesResponse = DescribeStackResourcesResponse
    { _dsrrStackResources = mempty
    }

-- | A list of StackResource structures.
dsrrStackResources :: Lens' DescribeStackResourcesResponse [StackResource]
dsrrStackResources =
    lens _dsrrStackResources (\s a -> s { _dsrrStackResources = a })

instance ToPath DescribeStackResources where
    toPath = const "/"

instance ToQuery DescribeStackResources

instance ToHeaders DescribeStackResources

instance AWSRequest DescribeStackResources where
    type Sv DescribeStackResources = CloudFormation
    type Rs DescribeStackResources = DescribeStackResourcesResponse

    request  = post "DescribeStackResources"
    response = xmlResponse

instance FromXML DescribeStackResourcesResponse where
    parseXML = withElement "DescribeStackResourcesResult" $ \x ->
            <$> x .@ "StackResources"
