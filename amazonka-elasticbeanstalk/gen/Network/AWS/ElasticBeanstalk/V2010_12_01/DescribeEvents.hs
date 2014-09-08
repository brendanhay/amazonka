{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeEvents
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns list of event descriptions matching criteria up to the last 6
-- weeks. This action returns the most recent 1,000 events from the specified
-- NextToken.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &Severity=TRACE &StartTime=2010-11-17T10%3A26%3A40Z
-- &Operation=DescribeEvents &AuthParams Successfully completed
-- createEnvironment activity. 2010-11-17T20:25:35.191Z New Version
-- bb01fa74-f287-11df-8a78-9f77047e0d0c SampleApp SampleAppVersion INFO
-- Launching a new EC2 instance: i-04a8c569 2010-11-17T20:21:30Z New Version
-- SampleApp SampleAppVersion DEBUG At least one EC2 instance has entered the
-- InService lifecycle state. 2010-11-17T20:20:32.008Z New Version
-- bb01fa74-f287-11df-8a78-9f77047e0d0c SampleApp SampleAppVersion INFO
-- Elastic Load Balancer elasticbeanstalk-SampleAppVersion has failed 0
-- healthy instances - Environment may not be available. 2010-11-17T20:19:28Z
-- New Version SampleApp SampleAppVersion WARN
-- f10d02dd-f288-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeEvents
    (
    -- * Request
      DescribeEvents
    -- ** Request constructor
    , mkDescribeEvents
    -- ** Request lenses
    , de1ApplicationName
    , de1VersionLabel
    , de1TemplateName
    , de1EnvironmentId
    , de1EnvironmentName
    , de1RequestId
    , de1Severity
    , de1StartTime
    , de1EndTime
    , de1MaxRecords
    , de1NextToken

    -- * Response
    , DescribeEventsResponse
    -- ** Response constructor
    , mkDescribeEventsResponse
    -- ** Response lenses
    , der1rEvents
    , der1rNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | This documentation target is not reported in the API reference.
data DescribeEvents = DescribeEvents
    { _de1ApplicationName :: Maybe Text
    , _de1VersionLabel :: Maybe Text
    , _de1TemplateName :: Maybe Text
    , _de1EnvironmentId :: Maybe Text
    , _de1EnvironmentName :: Maybe Text
    , _de1RequestId :: Maybe Text
    , _de1Severity :: Maybe EventSeverity
    , _de1StartTime :: Maybe ISO8601
    , _de1EndTime :: Maybe ISO8601
    , _de1MaxRecords :: Maybe Integer
    , _de1NextToken :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeEvents' request.
mkDescribeEvents :: DescribeEvents
mkDescribeEvents = DescribeEvents
    { _de1ApplicationName = Nothing
    , _de1VersionLabel = Nothing
    , _de1TemplateName = Nothing
    , _de1EnvironmentId = Nothing
    , _de1EnvironmentName = Nothing
    , _de1RequestId = Nothing
    , _de1Severity = Nothing
    , _de1StartTime = Nothing
    , _de1EndTime = Nothing
    , _de1MaxRecords = Nothing
    , _de1NextToken = Nothing
    }

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- include only those associated with this application.
de1ApplicationName :: Lens' DescribeEvents (Maybe Text)
de1ApplicationName =
    lens _de1ApplicationName (\s a -> s { _de1ApplicationName = a })

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- those associated with this application version.
de1VersionLabel :: Lens' DescribeEvents (Maybe Text)
de1VersionLabel = lens _de1VersionLabel (\s a -> s { _de1VersionLabel = a })

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- those that are associated with this environment configuration.
de1TemplateName :: Lens' DescribeEvents (Maybe Text)
de1TemplateName = lens _de1TemplateName (\s a -> s { _de1TemplateName = a })

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- those associated with this environment.
de1EnvironmentId :: Lens' DescribeEvents (Maybe Text)
de1EnvironmentId =
    lens _de1EnvironmentId (\s a -> s { _de1EnvironmentId = a })

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- those associated with this environment.
de1EnvironmentName :: Lens' DescribeEvents (Maybe Text)
de1EnvironmentName =
    lens _de1EnvironmentName (\s a -> s { _de1EnvironmentName = a })

-- | If specified, AWS Elastic Beanstalk restricts the described events to
-- include only those associated with this request ID.
de1RequestId :: Lens' DescribeEvents (Maybe Text)
de1RequestId = lens _de1RequestId (\s a -> s { _de1RequestId = a })

-- | If specified, limits the events returned from this call to include only
-- those with the specified severity or higher.
de1Severity :: Lens' DescribeEvents (Maybe EventSeverity)
de1Severity = lens _de1Severity (\s a -> s { _de1Severity = a })

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- those that occur on or after this time.
de1StartTime :: Lens' DescribeEvents (Maybe ISO8601)
de1StartTime = lens _de1StartTime (\s a -> s { _de1StartTime = a })

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- those that occur up to, but not including, the EndTime.
de1EndTime :: Lens' DescribeEvents (Maybe ISO8601)
de1EndTime = lens _de1EndTime (\s a -> s { _de1EndTime = a })

-- | Specifies the maximum number of events that can be returned, beginning with
-- the most recent event.
de1MaxRecords :: Lens' DescribeEvents (Maybe Integer)
de1MaxRecords = lens _de1MaxRecords (\s a -> s { _de1MaxRecords = a })

-- | Pagination token. If specified, the events return the next batch of
-- results.
de1NextToken :: Lens' DescribeEvents (Maybe Text)
de1NextToken = lens _de1NextToken (\s a -> s { _de1NextToken = a })

instance ToQuery DescribeEvents where
    toQuery = genericQuery def

-- | Result message wrapping a list of event descriptions.
data DescribeEventsResponse = DescribeEventsResponse
    { _der1rEvents :: [EventDescription]
    , _der1rNextToken :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeEventsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDescribeEventsResponse :: DescribeEventsResponse
mkDescribeEventsResponse = DescribeEventsResponse
    { _der1rEvents = mempty
    , _der1rNextToken = Nothing
    }

-- | A list of EventDescription.
der1rEvents :: Lens' DescribeEventsResponse [EventDescription]
der1rEvents = lens _der1rEvents (\s a -> s { _der1rEvents = a })

-- | If returned, this indicates that there are more results to obtain. Use this
-- token in the next DescribeEvents call to get the next batch of events.
der1rNextToken :: Lens' DescribeEventsResponse (Maybe Text)
der1rNextToken = lens _der1rNextToken (\s a -> s { _der1rNextToken = a })

instance FromXML DescribeEventsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeEvents where
    type Sv DescribeEvents = ElasticBeanstalk
    type Rs DescribeEvents = DescribeEventsResponse

    request = post "DescribeEvents"
    response _ = xmlResponse

instance AWSPager DescribeEvents where
    next rq rs = (\x -> rq & de1NextToken ?~ x)
        <$> (rs ^. der1rNextToken)
