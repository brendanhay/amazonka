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
    , mkDescribeEventsMessage
    -- ** Request lenses
    , denApplicationName
    , denVersionLabel
    , denTemplateName
    , denEnvironmentId
    , denEnvironmentName
    , denRequestId
    , denSeverity
    , denStartTime
    , denEndTime
    , denMaxRecords
    , denNextToken

    -- * Response
    , DescribeEventsResponse
    -- ** Response lenses
    , ednEvents
    , ednNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeEvents' request.
mkDescribeEventsMessage :: DescribeEvents
mkDescribeEventsMessage = DescribeEvents
    { _denApplicationName = Nothing
    , _denVersionLabel = Nothing
    , _denTemplateName = Nothing
    , _denEnvironmentId = Nothing
    , _denEnvironmentName = Nothing
    , _denRequestId = Nothing
    , _denSeverity = Nothing
    , _denStartTime = Nothing
    , _denEndTime = Nothing
    , _denMaxRecords = Nothing
    , _denNextToken = Nothing
    }
{-# INLINE mkDescribeEventsMessage #-}

data DescribeEvents = DescribeEvents
    { _denApplicationName :: Maybe Text
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned
      -- descriptions to include only those associated with this
      -- application.
    , _denVersionLabel :: Maybe Text
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned
      -- descriptions to those associated with this application version.
    , _denTemplateName :: Maybe Text
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned
      -- descriptions to those that are associated with this environment
      -- configuration.
    , _denEnvironmentId :: Maybe Text
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned
      -- descriptions to those associated with this environment.
    , _denEnvironmentName :: Maybe Text
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned
      -- descriptions to those associated with this environment.
    , _denRequestId :: Maybe Text
      -- ^ If specified, AWS Elastic Beanstalk restricts the described
      -- events to include only those associated with this request ID.
    , _denSeverity :: Maybe EventSeverity
      -- ^ If specified, limits the events returned from this call to
      -- include only those with the specified severity or higher.
    , _denStartTime :: Maybe ISO8601
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned
      -- descriptions to those that occur on or after this time.
    , _denEndTime :: Maybe ISO8601
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned
      -- descriptions to those that occur up to, but not including, the
      -- EndTime.
    , _denMaxRecords :: Maybe Integer
      -- ^ Specifies the maximum number of events that can be returned,
      -- beginning with the most recent event.
    , _denNextToken :: Maybe Text
      -- ^ Pagination token. If specified, the events return the next batch
      -- of results.
    } deriving (Show, Generic)

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- include only those associated with this application.
denApplicationName :: Lens' DescribeEvents (Maybe Text)
denApplicationName = lens _denApplicationName (\s a -> s { _denApplicationName = a })
{-# INLINE denApplicationName #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- those associated with this application version.
denVersionLabel :: Lens' DescribeEvents (Maybe Text)
denVersionLabel = lens _denVersionLabel (\s a -> s { _denVersionLabel = a })
{-# INLINE denVersionLabel #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- those that are associated with this environment configuration.
denTemplateName :: Lens' DescribeEvents (Maybe Text)
denTemplateName = lens _denTemplateName (\s a -> s { _denTemplateName = a })
{-# INLINE denTemplateName #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- those associated with this environment.
denEnvironmentId :: Lens' DescribeEvents (Maybe Text)
denEnvironmentId = lens _denEnvironmentId (\s a -> s { _denEnvironmentId = a })
{-# INLINE denEnvironmentId #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- those associated with this environment.
denEnvironmentName :: Lens' DescribeEvents (Maybe Text)
denEnvironmentName = lens _denEnvironmentName (\s a -> s { _denEnvironmentName = a })
{-# INLINE denEnvironmentName #-}

-- | If specified, AWS Elastic Beanstalk restricts the described events to
-- include only those associated with this request ID.
denRequestId :: Lens' DescribeEvents (Maybe Text)
denRequestId = lens _denRequestId (\s a -> s { _denRequestId = a })
{-# INLINE denRequestId #-}

-- | If specified, limits the events returned from this call to include only
-- those with the specified severity or higher.
denSeverity :: Lens' DescribeEvents (Maybe EventSeverity)
denSeverity = lens _denSeverity (\s a -> s { _denSeverity = a })
{-# INLINE denSeverity #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- those that occur on or after this time.
denStartTime :: Lens' DescribeEvents (Maybe ISO8601)
denStartTime = lens _denStartTime (\s a -> s { _denStartTime = a })
{-# INLINE denStartTime #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- those that occur up to, but not including, the EndTime.
denEndTime :: Lens' DescribeEvents (Maybe ISO8601)
denEndTime = lens _denEndTime (\s a -> s { _denEndTime = a })
{-# INLINE denEndTime #-}

-- | Specifies the maximum number of events that can be returned, beginning with
-- the most recent event.
denMaxRecords :: Lens' DescribeEvents (Maybe Integer)
denMaxRecords = lens _denMaxRecords (\s a -> s { _denMaxRecords = a })
{-# INLINE denMaxRecords #-}

-- | Pagination token. If specified, the events return the next batch of
-- results.
denNextToken :: Lens' DescribeEvents (Maybe Text)
denNextToken = lens _denNextToken (\s a -> s { _denNextToken = a })
{-# INLINE denNextToken #-}

instance ToQuery DescribeEvents where
    toQuery = genericQuery def

data DescribeEventsResponse = DescribeEventsResponse
    { _ednEvents :: [EventDescription]
      -- ^ A list of EventDescription.
    , _ednNextToken :: Maybe Text
      -- ^ If returned, this indicates that there are more results to
      -- obtain. Use this token in the next DescribeEvents call to get the
      -- next batch of events.
    } deriving (Show, Generic)

-- | A list of EventDescription.
ednEvents :: Lens' DescribeEventsResponse ([EventDescription])
ednEvents = lens _ednEvents (\s a -> s { _ednEvents = a })
{-# INLINE ednEvents #-}

-- | If returned, this indicates that there are more results to obtain. Use this
-- token in the next DescribeEvents call to get the next batch of events.
ednNextToken :: Lens' DescribeEventsResponse (Maybe Text)
ednNextToken = lens _ednNextToken (\s a -> s { _ednNextToken = a })
{-# INLINE ednNextToken #-}

instance FromXML DescribeEventsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeEvents where
    type Sv DescribeEvents = ElasticBeanstalk
    type Rs DescribeEvents = DescribeEventsResponse

    request = post "DescribeEvents"
    response _ = xmlResponse

instance AWSPager DescribeEvents where
    next rq rs = (\x -> rq { _denNextToken = Just x })
        <$> (_ednNextToken rs)
