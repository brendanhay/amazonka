{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation.V2010_05_15.ListStackResources
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns descriptions of all resources of the specified stack. For deleted
-- stacks, ListStackResources returns resource information for up to 90 days
-- after the stack has been deleted.
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=ListStackResources
-- &StackName=MyStack &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2011-07-08T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature] CREATE_COMPLETE DBSecurityGroup 2011-06-21T20:15:58Z
-- gmarcteststack-dbsecuritygroup-1s5m0ez5lkk6w AWS::RDS::DBSecurityGroup
-- CREATE_COMPLETE SampleDB 2011-06-21T20:25:57Z MyStack-sampledb-ycwhk1v830lx
-- AWS::RDS::DBInstance CREATE_COMPLETE SampleApplication 2011-06-21T20:26:12Z
-- MyStack-SampleApplication-1MKNASYR3RBQL AWS::ElasticBeanstalk::Application
-- CREATE_COMPLETE SampleEnvironment 2011-06-21T20:28:48Z
-- myst-Samp-1AGU6ERZX6M3Q AWS::ElasticBeanstalk::Environment CREATE_COMPLETE
-- AlarmTopic 2011-06-21T20:29:06Z
-- arn:aws:sns:us-east-1:803981987763:MyStack-AlarmTopic-SW4IQELG7RPJ
-- AWS::SNS::Topic CREATE_COMPLETE CPUAlarmHigh 2011-06-21T20:29:23Z
-- MyStack-CPUAlarmHigh-POBWQPDJA81F AWS::CloudWatch::Alarm
-- 2d06e36c-ac1d-11e0-a958-f9382b6eb86b.
module Network.AWS.CloudFormation.V2010_05_15.ListStackResources
    (
    -- * Request
      ListStackResources
    -- ** Request constructor
    , mkListStackResources
    -- ** Request lenses
    , lsrStackName
    , lsrNextToken

    -- * Response
    , ListStackResourcesResponse
    -- ** Response constructor
    , mkListStackResourcesResponse
    -- ** Response lenses
    , lsrrStackResourceSummaries
    , lsrrNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudFormation.V2010_05_15.Types
import Network.AWS.Prelude

-- | The input for ListStackResource action.
data ListStackResources = ListStackResources
    { _lsrStackName :: Text
    , _lsrNextToken :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListStackResources' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StackName ::@ @Text@
--
-- * @NextToken ::@ @Maybe Text@
--
mkListStackResources :: Text -- ^ 'lsrStackName'
                     -> ListStackResources
mkListStackResources p1 = ListStackResources
    { _lsrStackName = p1
    , _lsrNextToken = Nothing
    }

-- | The name or the unique identifier associated with the stack, which are not
-- always interchangeable: Running stacks: You can specify either the stack's
-- name or its unique stack ID. Deleted stacks: You must specify the unique
-- stack ID. Default: There is no default value.
lsrStackName :: Lens' ListStackResources Text
lsrStackName = lens _lsrStackName (\s a -> s { _lsrStackName = a })

-- | String that identifies the start of the next list of stack resource
-- summaries, if there is one. Default: There is no default value.
lsrNextToken :: Lens' ListStackResources (Maybe Text)
lsrNextToken = lens _lsrNextToken (\s a -> s { _lsrNextToken = a })

instance ToQuery ListStackResources where
    toQuery = genericQuery def

-- | The output for a ListStackResources action.
data ListStackResourcesResponse = ListStackResourcesResponse
    { _lsrrStackResourceSummaries :: [StackResourceSummary]
    , _lsrrNextToken :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListStackResourcesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StackResourceSummaries ::@ @[StackResourceSummary]@
--
-- * @NextToken ::@ @Maybe Text@
--
mkListStackResourcesResponse :: ListStackResourcesResponse
mkListStackResourcesResponse = ListStackResourcesResponse
    { _lsrrStackResourceSummaries = mempty
    , _lsrrNextToken = Nothing
    }

-- | A list of StackResourceSummary structures.
lsrrStackResourceSummaries :: Lens' ListStackResourcesResponse [StackResourceSummary]
lsrrStackResourceSummaries =
    lens _lsrrStackResourceSummaries
         (\s a -> s { _lsrrStackResourceSummaries = a })

-- | String that identifies the start of the next list of stack resources, if
-- there is one.
lsrrNextToken :: Lens' ListStackResourcesResponse (Maybe Text)
lsrrNextToken = lens _lsrrNextToken (\s a -> s { _lsrrNextToken = a })

instance FromXML ListStackResourcesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListStackResources where
    type Sv ListStackResources = CloudFormation
    type Rs ListStackResources = ListStackResourcesResponse

    request = post "ListStackResources"
    response _ = xmlResponse

instance AWSPager ListStackResources where
    next rq rs = (\x -> rq & lsrNextToken ?~ x)
        <$> (rs ^. lsrrNextToken)
