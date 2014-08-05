{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeEvents where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeEvents' request.
describeEvents :: DescribeEvents
describeEvents = DescribeEvents
    { _demApplicationName = Nothing
    , _demTemplateName = Nothing
    , _demEnvironmentId = Nothing
    , _demEnvironmentName = Nothing
    , _demSeverity = Nothing
    , _demMaxRecords = Nothing
    , _demRequestId = Nothing
    , _demEndTime = Nothing
    , _demStartTime = Nothing
    , _demNextToken = Nothing
    , _demVersionLabel = Nothing
    }

data DescribeEvents = DescribeEvents
    { _demApplicationName :: Maybe Text
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned
      -- descriptions to include only those associated with this
      -- application.
    , _demTemplateName :: Maybe Text
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned
      -- descriptions to those that are associated with this environment
      -- configuration.
    , _demEnvironmentId :: Maybe Text
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned
      -- descriptions to those associated with this environment.
    , _demEnvironmentName :: Maybe Text
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned
      -- descriptions to those associated with this environment.
    , _demSeverity :: Maybe EventSeverity
      -- ^ If specified, limits the events returned from this call to
      -- include only those with the specified severity or higher.
    , _demMaxRecords :: Maybe Integer
      -- ^ Specifies the maximum number of events that can be returned,
      -- beginning with the most recent event.
    , _demRequestId :: Maybe Text
      -- ^ If specified, AWS Elastic Beanstalk restricts the described
      -- events to include only those associated with this request ID.
    , _demEndTime :: Maybe ISO8601
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned
      -- descriptions to those that occur up to, but not including, the
      -- EndTime.
    , _demStartTime :: Maybe ISO8601
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned
      -- descriptions to those that occur on or after this time.
    , _demNextToken :: Maybe Text
      -- ^ Pagination token. If specified, the events return the next batch
      -- of results.
    , _demVersionLabel :: Maybe Text
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned
      -- descriptions to those associated with this application version.
    } deriving (Show, Generic)

makeLenses ''DescribeEvents

instance ToQuery DescribeEvents where
    toQuery = genericToQuery def

data DescribeEventsResponse = DescribeEventsResponse
    { _edmEvents :: [EventDescription]
      -- ^ A list of EventDescription.
    , _edmNextToken :: Maybe Text
      -- ^ If returned, this indicates that there are more results to
      -- obtain. Use this token in the next DescribeEvents call to get the
      -- next batch of events.
    } deriving (Show, Generic)

makeLenses ''DescribeEventsResponse

instance AWSRequest DescribeEvents where
    type Sv DescribeEvents = ElasticBeanstalk
    type Rs DescribeEvents = DescribeEventsResponse

    request = post "DescribeEvents"
    response _ = cursorResponse $ \hs xml ->
        pure DescribeEventsResponse
            <*> xml %| "EventDescriptionList"
            <*> xml %|? "Token"

instance AWSPager DescribeEvents where
    next rq rs = (\x -> rq { _demNextToken = Just x })
        <$> (_edmNextToken rs)
