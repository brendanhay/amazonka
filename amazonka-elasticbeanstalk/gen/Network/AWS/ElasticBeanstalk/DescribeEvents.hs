{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_DescribeEvents.html>
module Network.AWS.ElasticBeanstalk.DescribeEvents
    (
    -- * Request
      DescribeEvents
    -- ** Request constructor
    , describeEvents
    -- ** Request lenses
    , deApplicationName
    , deEndTime
    , deEnvironmentId
    , deEnvironmentName
    , deMaxRecords
    , deNextToken
    , deRequestId
    , deSeverity
    , deStartTime
    , deTemplateName
    , deVersionLabel

    -- * Response
    , DescribeEventsResponse
    -- ** Response constructor
    , describeEventsResponse
    -- ** Response lenses
    , derEvents
    , derNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import qualified GHC.Exts

data DescribeEvents = DescribeEvents
    { _deApplicationName :: Maybe Text
    , _deEndTime         :: Maybe RFC822
    , _deEnvironmentId   :: Maybe Text
    , _deEnvironmentName :: Maybe Text
    , _deMaxRecords      :: Maybe Nat
    , _deNextToken       :: Maybe Text
    , _deRequestId       :: Maybe Text
    , _deSeverity        :: Maybe Text
    , _deStartTime       :: Maybe RFC822
    , _deTemplateName    :: Maybe Text
    , _deVersionLabel    :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeEvents' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'deApplicationName' @::@ 'Maybe' 'Text'
--
-- * 'deEndTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'deEnvironmentId' @::@ 'Maybe' 'Text'
--
-- * 'deEnvironmentName' @::@ 'Maybe' 'Text'
--
-- * 'deMaxRecords' @::@ 'Maybe' 'Natural'
--
-- * 'deNextToken' @::@ 'Maybe' 'Text'
--
-- * 'deRequestId' @::@ 'Maybe' 'Text'
--
-- * 'deSeverity' @::@ 'Maybe' 'Text'
--
-- * 'deStartTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'deTemplateName' @::@ 'Maybe' 'Text'
--
-- * 'deVersionLabel' @::@ 'Maybe' 'Text'
--
describeEvents :: DescribeEvents
describeEvents = DescribeEvents
    { _deApplicationName = Nothing
    , _deVersionLabel    = Nothing
    , _deTemplateName    = Nothing
    , _deEnvironmentId   = Nothing
    , _deEnvironmentName = Nothing
    , _deRequestId       = Nothing
    , _deSeverity        = Nothing
    , _deStartTime       = Nothing
    , _deEndTime         = Nothing
    , _deMaxRecords      = Nothing
    , _deNextToken       = Nothing
    }

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those associated with this application.
deApplicationName :: Lens' DescribeEvents (Maybe Text)
deApplicationName =
    lens _deApplicationName (\s a -> s { _deApplicationName = a })

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those that occur up to, but not including, the EndTime.
deEndTime :: Lens' DescribeEvents (Maybe UTCTime)
deEndTime = lens _deEndTime (\s a -> s { _deEndTime = a })
    . mapping _Time

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those associated with this environment.
deEnvironmentId :: Lens' DescribeEvents (Maybe Text)
deEnvironmentId = lens _deEnvironmentId (\s a -> s { _deEnvironmentId = a })

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those associated with this environment.
deEnvironmentName :: Lens' DescribeEvents (Maybe Text)
deEnvironmentName =
    lens _deEnvironmentName (\s a -> s { _deEnvironmentName = a })

-- | Specifies the maximum number of events that can be returned, beginning
-- with the most recent event.
deMaxRecords :: Lens' DescribeEvents (Maybe Natural)
deMaxRecords = lens _deMaxRecords (\s a -> s { _deMaxRecords = a })
    . mapping _Nat

-- | Pagination token. If specified, the events return the next batch of
-- results.
deNextToken :: Lens' DescribeEvents (Maybe Text)
deNextToken = lens _deNextToken (\s a -> s { _deNextToken = a })

-- | If specified, AWS Elastic Beanstalk restricts the described events to
-- include only those associated with this request ID.
deRequestId :: Lens' DescribeEvents (Maybe Text)
deRequestId = lens _deRequestId (\s a -> s { _deRequestId = a })

-- | If specified, limits the events returned from this call to include only
-- those with the specified severity or higher.
deSeverity :: Lens' DescribeEvents (Maybe Text)
deSeverity = lens _deSeverity (\s a -> s { _deSeverity = a })

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those that occur on or after this time.
deStartTime :: Lens' DescribeEvents (Maybe UTCTime)
deStartTime = lens _deStartTime (\s a -> s { _deStartTime = a })
    . mapping _Time

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those that are associated with this environment configuration.
deTemplateName :: Lens' DescribeEvents (Maybe Text)
deTemplateName = lens _deTemplateName (\s a -> s { _deTemplateName = a })

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those associated with this application version.
deVersionLabel :: Lens' DescribeEvents (Maybe Text)
deVersionLabel = lens _deVersionLabel (\s a -> s { _deVersionLabel = a })

data DescribeEventsResponse = DescribeEventsResponse
    { _derEvents    :: [EventDescription]
    , _derNextToken :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeEventsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'derEvents' @::@ ['EventDescription']
--
-- * 'derNextToken' @::@ 'Maybe' 'Text'
--
describeEventsResponse :: DescribeEventsResponse
describeEventsResponse = DescribeEventsResponse
    { _derEvents    = mempty
    , _derNextToken = Nothing
    }

-- | A list of EventDescription.
derEvents :: Lens' DescribeEventsResponse [EventDescription]
derEvents = lens _derEvents (\s a -> s { _derEvents = a })

-- | If returned, this indicates that there are more results to obtain. Use
-- this token in the next DescribeEvents call to get the next batch of
-- events.
derNextToken :: Lens' DescribeEventsResponse (Maybe Text)
derNextToken = lens _derNextToken (\s a -> s { _derNextToken = a })

instance AWSRequest DescribeEvents where
    type Sv DescribeEvents = ElasticBeanstalk
    type Rs DescribeEvents = DescribeEventsResponse

    request  = post "DescribeEvents"
    response = xmlResponse

instance FromXML DescribeEventsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeEventsResponse"

instance ToPath DescribeEvents where
    toPath = const "/"

instance ToHeaders DescribeEvents

instance ToQuery DescribeEvents
