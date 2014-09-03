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
    , describeEvents
    -- ** Request lenses
    , denApplicationName
    , denTemplateName
    , denEnvironmentId
    , denEnvironmentName
    , denSeverity
    , denMaxRecords
    , denRequestId
    , denEndTime
    , denStartTime
    , denNextToken
    , denVersionLabel

    -- * Response
    , DescribeEventsResponse
    -- ** Response lenses
    , ednEvents
    , ednNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeEvents' request.
describeEvents :: DescribeEvents
describeEvents = DescribeEvents
    { _denApplicationName = Nothing
    , _denTemplateName = Nothing
    , _denEnvironmentId = Nothing
    , _denEnvironmentName = Nothing
    , _denSeverity = Nothing
    , _denMaxRecords = Nothing
    , _denRequestId = Nothing
    , _denEndTime = Nothing
    , _denStartTime = Nothing
    , _denNextToken = Nothing
    , _denVersionLabel = Nothing
    }

data DescribeEvents = DescribeEvents
    { _denApplicationName :: Maybe Text
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned
      -- descriptions to include only those associated with this
      -- application.
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
    , _denSeverity :: Maybe EventSeverity
      -- ^ If specified, limits the events returned from this call to
      -- include only those with the specified severity or higher.
    , _denMaxRecords :: Maybe Integer
      -- ^ Specifies the maximum number of events that can be returned,
      -- beginning with the most recent event.
    , _denRequestId :: Maybe Text
      -- ^ If specified, AWS Elastic Beanstalk restricts the described
      -- events to include only those associated with this request ID.
    , _denEndTime :: Maybe ISO8601
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned
      -- descriptions to those that occur up to, but not including, the
      -- EndTime.
    , _denStartTime :: Maybe ISO8601
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned
      -- descriptions to those that occur on or after this time.
    , _denNextToken :: Maybe Text
      -- ^ Pagination token. If specified, the events return the next batch
      -- of results.
    , _denVersionLabel :: Maybe Text
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned
      -- descriptions to those associated with this application version.
    } deriving (Show, Generic)

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- include only those associated with this application.
denApplicationName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeEvents
    -> f DescribeEvents
denApplicationName f x =
    (\y -> x { _denApplicationName = y })
       <$> f (_denApplicationName x)
{-# INLINE denApplicationName #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- those that are associated with this environment configuration.
denTemplateName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeEvents
    -> f DescribeEvents
denTemplateName f x =
    (\y -> x { _denTemplateName = y })
       <$> f (_denTemplateName x)
{-# INLINE denTemplateName #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- those associated with this environment.
denEnvironmentId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeEvents
    -> f DescribeEvents
denEnvironmentId f x =
    (\y -> x { _denEnvironmentId = y })
       <$> f (_denEnvironmentId x)
{-# INLINE denEnvironmentId #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- those associated with this environment.
denEnvironmentName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeEvents
    -> f DescribeEvents
denEnvironmentName f x =
    (\y -> x { _denEnvironmentName = y })
       <$> f (_denEnvironmentName x)
{-# INLINE denEnvironmentName #-}

-- | If specified, limits the events returned from this call to include only
-- those with the specified severity or higher.
denSeverity
    :: Functor f
    => (Maybe EventSeverity
    -> f (Maybe EventSeverity))
    -> DescribeEvents
    -> f DescribeEvents
denSeverity f x =
    (\y -> x { _denSeverity = y })
       <$> f (_denSeverity x)
{-# INLINE denSeverity #-}

-- | Specifies the maximum number of events that can be returned, beginning with
-- the most recent event.
denMaxRecords
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeEvents
    -> f DescribeEvents
denMaxRecords f x =
    (\y -> x { _denMaxRecords = y })
       <$> f (_denMaxRecords x)
{-# INLINE denMaxRecords #-}

-- | If specified, AWS Elastic Beanstalk restricts the described events to
-- include only those associated with this request ID.
denRequestId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeEvents
    -> f DescribeEvents
denRequestId f x =
    (\y -> x { _denRequestId = y })
       <$> f (_denRequestId x)
{-# INLINE denRequestId #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- those that occur up to, but not including, the EndTime.
denEndTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> DescribeEvents
    -> f DescribeEvents
denEndTime f x =
    (\y -> x { _denEndTime = y })
       <$> f (_denEndTime x)
{-# INLINE denEndTime #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- those that occur on or after this time.
denStartTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> DescribeEvents
    -> f DescribeEvents
denStartTime f x =
    (\y -> x { _denStartTime = y })
       <$> f (_denStartTime x)
{-# INLINE denStartTime #-}

-- | Pagination token. If specified, the events return the next batch of
-- results.
denNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeEvents
    -> f DescribeEvents
denNextToken f x =
    (\y -> x { _denNextToken = y })
       <$> f (_denNextToken x)
{-# INLINE denNextToken #-}

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- those associated with this application version.
denVersionLabel
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeEvents
    -> f DescribeEvents
denVersionLabel f x =
    (\y -> x { _denVersionLabel = y })
       <$> f (_denVersionLabel x)
{-# INLINE denVersionLabel #-}

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
ednEvents
    :: Functor f
    => ([EventDescription]
    -> f ([EventDescription]))
    -> DescribeEventsResponse
    -> f DescribeEventsResponse
ednEvents f x =
    (\y -> x { _ednEvents = y })
       <$> f (_ednEvents x)
{-# INLINE ednEvents #-}

-- | If returned, this indicates that there are more results to obtain. Use this
-- token in the next DescribeEvents call to get the next batch of events.
ednNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeEventsResponse
    -> f DescribeEventsResponse
ednNextToken f x =
    (\y -> x { _ednNextToken = y })
       <$> f (_ednNextToken x)
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
