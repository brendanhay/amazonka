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

-- Module      : Network.AWS.ElasticBeanstalk.DescribeEvents
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns list of event descriptions matching criteria up to the last 6
-- weeks.
module Network.AWS.ElasticBeanstalk.DescribeEvents
    (
    -- * Request
      DescribeEventsMessage
    -- ** Request constructor
    , describeEventsMessage
    -- ** Request lenses
    , dem1ApplicationName
    , dem1EndTime
    , dem1EnvironmentId
    , dem1EnvironmentName
    , dem1MaxRecords
    , dem1NextToken
    , dem1RequestId
    , dem1Severity
    , dem1StartTime
    , dem1TemplateName
    , dem1VersionLabel

    -- * Response
    , EventDescriptionsMessage
    -- ** Response constructor
    , eventDescriptionsMessage
    -- ** Response lenses
    , edmEvents
    , edmNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types

data DescribeEventsMessage = DescribeEventsMessage
    { _dem1ApplicationName :: Maybe Text
    , _dem1EndTime         :: Maybe RFC822
    , _dem1EnvironmentId   :: Maybe Text
    , _dem1EnvironmentName :: Maybe Text
    , _dem1MaxRecords      :: Maybe Natural
    , _dem1NextToken       :: Maybe Text
    , _dem1RequestId       :: Maybe Text
    , _dem1Severity        :: Maybe Text
    , _dem1StartTime       :: Maybe RFC822
    , _dem1TemplateName    :: Maybe Text
    , _dem1VersionLabel    :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeEventsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dem1ApplicationName' @::@ 'Maybe' 'Text'
--
-- * 'dem1EndTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'dem1EnvironmentId' @::@ 'Maybe' 'Text'
--
-- * 'dem1EnvironmentName' @::@ 'Maybe' 'Text'
--
-- * 'dem1MaxRecords' @::@ 'Maybe' 'Natural'
--
-- * 'dem1NextToken' @::@ 'Maybe' 'Text'
--
-- * 'dem1RequestId' @::@ 'Maybe' 'Text'
--
-- * 'dem1Severity' @::@ 'Maybe' 'Text'
--
-- * 'dem1StartTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'dem1TemplateName' @::@ 'Maybe' 'Text'
--
-- * 'dem1VersionLabel' @::@ 'Maybe' 'Text'
--
describeEventsMessage :: DescribeEventsMessage
describeEventsMessage = DescribeEventsMessage
    { _dem1ApplicationName = Nothing
    , _dem1VersionLabel    = Nothing
    , _dem1TemplateName    = Nothing
    , _dem1EnvironmentId   = Nothing
    , _dem1EnvironmentName = Nothing
    , _dem1RequestId       = Nothing
    , _dem1Severity        = Nothing
    , _dem1StartTime       = Nothing
    , _dem1EndTime         = Nothing
    , _dem1MaxRecords      = Nothing
    , _dem1NextToken       = Nothing
    }

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those associated with this application.
dem1ApplicationName :: Lens' DescribeEventsMessage (Maybe Text)
dem1ApplicationName =
    lens _dem1ApplicationName (\s a -> s { _dem1ApplicationName = a })

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those that occur up to, but not including, the EndTime.
dem1EndTime :: Lens' DescribeEventsMessage (Maybe UTCTime)
dem1EndTime = lens _dem1EndTime (\s a -> s { _dem1EndTime = a })
    . mapping _Time

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those associated with this environment.
dem1EnvironmentId :: Lens' DescribeEventsMessage (Maybe Text)
dem1EnvironmentId =
    lens _dem1EnvironmentId (\s a -> s { _dem1EnvironmentId = a })

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those associated with this environment.
dem1EnvironmentName :: Lens' DescribeEventsMessage (Maybe Text)
dem1EnvironmentName =
    lens _dem1EnvironmentName (\s a -> s { _dem1EnvironmentName = a })

-- | Specifies the maximum number of events that can be returned, beginning
-- with the most recent event.
dem1MaxRecords :: Lens' DescribeEventsMessage (Maybe Natural)
dem1MaxRecords = lens _dem1MaxRecords (\s a -> s { _dem1MaxRecords = a })

-- | Pagination token. If specified, the events return the next batch of
-- results.
dem1NextToken :: Lens' DescribeEventsMessage (Maybe Text)
dem1NextToken = lens _dem1NextToken (\s a -> s { _dem1NextToken = a })

-- | If specified, AWS Elastic Beanstalk restricts the described events to
-- include only those associated with this request ID.
dem1RequestId :: Lens' DescribeEventsMessage (Maybe Text)
dem1RequestId = lens _dem1RequestId (\s a -> s { _dem1RequestId = a })

-- | If specified, limits the events returned from this call to include only
-- those with the specified severity or higher.
dem1Severity :: Lens' DescribeEventsMessage (Maybe Text)
dem1Severity = lens _dem1Severity (\s a -> s { _dem1Severity = a })

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those that occur on or after this time.
dem1StartTime :: Lens' DescribeEventsMessage (Maybe UTCTime)
dem1StartTime = lens _dem1StartTime (\s a -> s { _dem1StartTime = a })
    . mapping _Time

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those that are associated with this environment configuration.
dem1TemplateName :: Lens' DescribeEventsMessage (Maybe Text)
dem1TemplateName = lens _dem1TemplateName (\s a -> s { _dem1TemplateName = a })

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those associated with this application version.
dem1VersionLabel :: Lens' DescribeEventsMessage (Maybe Text)
dem1VersionLabel = lens _dem1VersionLabel (\s a -> s { _dem1VersionLabel = a })
instance ToQuery DescribeEventsMessage

instance ToPath DescribeEventsMessage where
    toPath = const "/"

data EventDescriptionsMessage = EventDescriptionsMessage
    { _edmEvents    :: [EventDescription]
    , _edmNextToken :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'EventDescriptionsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'edmEvents' @::@ ['EventDescription']
--
-- * 'edmNextToken' @::@ 'Maybe' 'Text'
--
eventDescriptionsMessage :: EventDescriptionsMessage
eventDescriptionsMessage = EventDescriptionsMessage
    { _edmEvents    = mempty
    , _edmNextToken = Nothing
    }

-- | A list of EventDescription.
edmEvents :: Lens' EventDescriptionsMessage [EventDescription]
edmEvents = lens _edmEvents (\s a -> s { _edmEvents = a })

-- | If returned, this indicates that there are more results to obtain. Use
-- this token in the next DescribeEvents call to get the next batch of
-- events.
edmNextToken :: Lens' EventDescriptionsMessage (Maybe Text)
edmNextToken = lens _edmNextToken (\s a -> s { _edmNextToken = a })
instance FromXML EventDescriptionsMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EventDescriptionsMessage"

instance AWSRequest DescribeEventsMessage where
    type Sv DescribeEventsMessage = ElasticBeanstalk
    type Rs DescribeEventsMessage = EventDescriptionsMessage

    request  = post "DescribeEvents"
    response = xmlResponse $ \h x -> EventDescriptionsMessage
        <$> x %| "Events"
        <*> x %| "NextToken"
