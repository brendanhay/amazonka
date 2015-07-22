{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DescribeEvents
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns list of event descriptions matching criteria up to the last 6
-- weeks.
--
-- This action returns the most recent 1,000 events from the specified
-- @NextToken@.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_DescribeEvents.html>
module Network.AWS.ElasticBeanstalk.DescribeEvents
    (
    -- * Request
      DescribeEvents
    -- ** Request constructor
    , describeEvents
    -- ** Request lenses
    , derqRequestId
    , derqTemplateName
    , derqStartTime
    , derqSeverity
    , derqNextToken
    , derqVersionLabel
    , derqMaxRecords
    , derqEnvironmentName
    , derqEndTime
    , derqApplicationName
    , derqEnvironmentId

    -- * Response
    , DescribeEventsResponse
    -- ** Response constructor
    , describeEventsResponse
    -- ** Response lenses
    , dersNextToken
    , dersEvents
    , dersStatus
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | This documentation target is not reported in the API reference.
--
-- /See:/ 'describeEvents' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'derqRequestId'
--
-- * 'derqTemplateName'
--
-- * 'derqStartTime'
--
-- * 'derqSeverity'
--
-- * 'derqNextToken'
--
-- * 'derqVersionLabel'
--
-- * 'derqMaxRecords'
--
-- * 'derqEnvironmentName'
--
-- * 'derqEndTime'
--
-- * 'derqApplicationName'
--
-- * 'derqEnvironmentId'
data DescribeEvents = DescribeEvents'
    { _derqRequestId       :: !(Maybe Text)
    , _derqTemplateName    :: !(Maybe Text)
    , _derqStartTime       :: !(Maybe ISO8601)
    , _derqSeverity        :: !(Maybe EventSeverity)
    , _derqNextToken       :: !(Maybe Text)
    , _derqVersionLabel    :: !(Maybe Text)
    , _derqMaxRecords      :: !(Maybe Nat)
    , _derqEnvironmentName :: !(Maybe Text)
    , _derqEndTime         :: !(Maybe ISO8601)
    , _derqApplicationName :: !(Maybe Text)
    , _derqEnvironmentId   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeEvents' smart constructor.
describeEvents :: DescribeEvents
describeEvents =
    DescribeEvents'
    { _derqRequestId = Nothing
    , _derqTemplateName = Nothing
    , _derqStartTime = Nothing
    , _derqSeverity = Nothing
    , _derqNextToken = Nothing
    , _derqVersionLabel = Nothing
    , _derqMaxRecords = Nothing
    , _derqEnvironmentName = Nothing
    , _derqEndTime = Nothing
    , _derqApplicationName = Nothing
    , _derqEnvironmentId = Nothing
    }

-- | If specified, AWS Elastic Beanstalk restricts the described events to
-- include only those associated with this request ID.
derqRequestId :: Lens' DescribeEvents (Maybe Text)
derqRequestId = lens _derqRequestId (\ s a -> s{_derqRequestId = a});

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those that are associated with this environment configuration.
derqTemplateName :: Lens' DescribeEvents (Maybe Text)
derqTemplateName = lens _derqTemplateName (\ s a -> s{_derqTemplateName = a});

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those that occur on or after this time.
derqStartTime :: Lens' DescribeEvents (Maybe UTCTime)
derqStartTime = lens _derqStartTime (\ s a -> s{_derqStartTime = a}) . mapping _Time;

-- | If specified, limits the events returned from this call to include only
-- those with the specified severity or higher.
derqSeverity :: Lens' DescribeEvents (Maybe EventSeverity)
derqSeverity = lens _derqSeverity (\ s a -> s{_derqSeverity = a});

-- | Pagination token. If specified, the events return the next batch of
-- results.
derqNextToken :: Lens' DescribeEvents (Maybe Text)
derqNextToken = lens _derqNextToken (\ s a -> s{_derqNextToken = a});

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those associated with this application version.
derqVersionLabel :: Lens' DescribeEvents (Maybe Text)
derqVersionLabel = lens _derqVersionLabel (\ s a -> s{_derqVersionLabel = a});

-- | Specifies the maximum number of events that can be returned, beginning
-- with the most recent event.
derqMaxRecords :: Lens' DescribeEvents (Maybe Natural)
derqMaxRecords = lens _derqMaxRecords (\ s a -> s{_derqMaxRecords = a}) . mapping _Nat;

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those associated with this environment.
derqEnvironmentName :: Lens' DescribeEvents (Maybe Text)
derqEnvironmentName = lens _derqEnvironmentName (\ s a -> s{_derqEnvironmentName = a});

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those that occur up to, but not including, the @EndTime@.
derqEndTime :: Lens' DescribeEvents (Maybe UTCTime)
derqEndTime = lens _derqEndTime (\ s a -> s{_derqEndTime = a}) . mapping _Time;

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to include only those associated with this application.
derqApplicationName :: Lens' DescribeEvents (Maybe Text)
derqApplicationName = lens _derqApplicationName (\ s a -> s{_derqApplicationName = a});

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to those associated with this environment.
derqEnvironmentId :: Lens' DescribeEvents (Maybe Text)
derqEnvironmentId = lens _derqEnvironmentId (\ s a -> s{_derqEnvironmentId = a});

instance AWSPager DescribeEvents where
        page rq rs
          | stop (rs ^. dersNextToken) = Nothing
          | stop (rs ^. dersEvents) = Nothing
          | otherwise =
            Just $ rq & derqNextToken .~ rs ^. dersNextToken

instance AWSRequest DescribeEvents where
        type Sv DescribeEvents = ElasticBeanstalk
        type Rs DescribeEvents = DescribeEventsResponse
        request = post
        response
          = receiveXMLWrapper "DescribeEventsResult"
              (\ s h x ->
                 DescribeEventsResponse' <$>
                   (x .@? "NextToken") <*>
                     (x .@? "Events" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeEvents where
        toHeaders = const mempty

instance ToPath DescribeEvents where
        toPath = const "/"

instance ToQuery DescribeEvents where
        toQuery DescribeEvents'{..}
          = mconcat
              ["Action" =: ("DescribeEvents" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "RequestId" =: _derqRequestId,
               "TemplateName" =: _derqTemplateName,
               "StartTime" =: _derqStartTime,
               "Severity" =: _derqSeverity,
               "NextToken" =: _derqNextToken,
               "VersionLabel" =: _derqVersionLabel,
               "MaxRecords" =: _derqMaxRecords,
               "EnvironmentName" =: _derqEnvironmentName,
               "EndTime" =: _derqEndTime,
               "ApplicationName" =: _derqApplicationName,
               "EnvironmentId" =: _derqEnvironmentId]

-- | Result message wrapping a list of event descriptions.
--
-- /See:/ 'describeEventsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dersNextToken'
--
-- * 'dersEvents'
--
-- * 'dersStatus'
data DescribeEventsResponse = DescribeEventsResponse'
    { _dersNextToken :: !(Maybe Text)
    , _dersEvents    :: !(Maybe [EventDescription])
    , _dersStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeEventsResponse' smart constructor.
describeEventsResponse :: Int -> DescribeEventsResponse
describeEventsResponse pStatus =
    DescribeEventsResponse'
    { _dersNextToken = Nothing
    , _dersEvents = Nothing
    , _dersStatus = pStatus
    }

-- | If returned, this indicates that there are more results to obtain. Use
-- this token in the next DescribeEvents call to get the next batch of
-- events.
dersNextToken :: Lens' DescribeEventsResponse (Maybe Text)
dersNextToken = lens _dersNextToken (\ s a -> s{_dersNextToken = a});

-- | A list of EventDescription.
dersEvents :: Lens' DescribeEventsResponse [EventDescription]
dersEvents = lens _dersEvents (\ s a -> s{_dersEvents = a}) . _Default;

-- | FIXME: Undocumented member.
dersStatus :: Lens' DescribeEventsResponse Int
dersStatus = lens _dersStatus (\ s a -> s{_dersStatus = a});
