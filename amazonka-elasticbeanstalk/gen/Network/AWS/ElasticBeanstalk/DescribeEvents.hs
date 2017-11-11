{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DescribeEvents
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns list of event descriptions matching criteria up to the last 6 weeks.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ElasticBeanstalk.DescribeEvents
    (
    -- * Creating a Request
      describeEvents
    , DescribeEvents
    -- * Request Lenses
    , deRequestId
    , deTemplateName
    , deStartTime
    , deSeverity
    , deNextToken
    , deVersionLabel
    , dePlatformARN
    , deEnvironmentName
    , deMaxRecords
    , deEndTime
    , deApplicationName
    , deEnvironmentId

    -- * Destructuring the Response
    , describeEventsResponse
    , DescribeEventsResponse
    -- * Response Lenses
    , dersNextToken
    , dersEvents
    , dersResponseStatus
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to retrieve a list of events for an environment.
--
--
--
-- /See:/ 'describeEvents' smart constructor.
data DescribeEvents = DescribeEvents'
  { _deRequestId       :: {-# NOUNPACK #-}!(Maybe Text)
  , _deTemplateName    :: {-# NOUNPACK #-}!(Maybe Text)
  , _deStartTime       :: {-# NOUNPACK #-}!(Maybe ISO8601)
  , _deSeverity        :: {-# NOUNPACK #-}!(Maybe EventSeverity)
  , _deNextToken       :: {-# NOUNPACK #-}!(Maybe Text)
  , _deVersionLabel    :: {-# NOUNPACK #-}!(Maybe Text)
  , _dePlatformARN     :: {-# NOUNPACK #-}!(Maybe Text)
  , _deEnvironmentName :: {-# NOUNPACK #-}!(Maybe Text)
  , _deMaxRecords      :: {-# NOUNPACK #-}!(Maybe Nat)
  , _deEndTime         :: {-# NOUNPACK #-}!(Maybe ISO8601)
  , _deApplicationName :: {-# NOUNPACK #-}!(Maybe Text)
  , _deEnvironmentId   :: {-# NOUNPACK #-}!(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEvents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deRequestId' - If specified, AWS Elastic Beanstalk restricts the described events to include only those associated with this request ID.
--
-- * 'deTemplateName' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to those that are associated with this environment configuration.
--
-- * 'deStartTime' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to those that occur on or after this time.
--
-- * 'deSeverity' - If specified, limits the events returned from this call to include only those with the specified severity or higher.
--
-- * 'deNextToken' - Pagination token. If specified, the events return the next batch of results.
--
-- * 'deVersionLabel' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this application version.
--
-- * 'dePlatformARN' - The ARN of the version of the custom platform.
--
-- * 'deEnvironmentName' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this environment.
--
-- * 'deMaxRecords' - Specifies the maximum number of events that can be returned, beginning with the most recent event.
--
-- * 'deEndTime' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to those that occur up to, but not including, the @EndTime@ .
--
-- * 'deApplicationName' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those associated with this application.
--
-- * 'deEnvironmentId' - If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this environment.
describeEvents
    :: DescribeEvents
describeEvents =
  DescribeEvents'
  { _deRequestId = Nothing
  , _deTemplateName = Nothing
  , _deStartTime = Nothing
  , _deSeverity = Nothing
  , _deNextToken = Nothing
  , _deVersionLabel = Nothing
  , _dePlatformARN = Nothing
  , _deEnvironmentName = Nothing
  , _deMaxRecords = Nothing
  , _deEndTime = Nothing
  , _deApplicationName = Nothing
  , _deEnvironmentId = Nothing
  }


-- | If specified, AWS Elastic Beanstalk restricts the described events to include only those associated with this request ID.
deRequestId :: Lens' DescribeEvents (Maybe Text)
deRequestId = lens _deRequestId (\ s a -> s{_deRequestId = a});

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those that are associated with this environment configuration.
deTemplateName :: Lens' DescribeEvents (Maybe Text)
deTemplateName = lens _deTemplateName (\ s a -> s{_deTemplateName = a});

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those that occur on or after this time.
deStartTime :: Lens' DescribeEvents (Maybe UTCTime)
deStartTime = lens _deStartTime (\ s a -> s{_deStartTime = a}) . mapping _Time;

-- | If specified, limits the events returned from this call to include only those with the specified severity or higher.
deSeverity :: Lens' DescribeEvents (Maybe EventSeverity)
deSeverity = lens _deSeverity (\ s a -> s{_deSeverity = a});

-- | Pagination token. If specified, the events return the next batch of results.
deNextToken :: Lens' DescribeEvents (Maybe Text)
deNextToken = lens _deNextToken (\ s a -> s{_deNextToken = a});

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this application version.
deVersionLabel :: Lens' DescribeEvents (Maybe Text)
deVersionLabel = lens _deVersionLabel (\ s a -> s{_deVersionLabel = a});

-- | The ARN of the version of the custom platform.
dePlatformARN :: Lens' DescribeEvents (Maybe Text)
dePlatformARN = lens _dePlatformARN (\ s a -> s{_dePlatformARN = a});

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this environment.
deEnvironmentName :: Lens' DescribeEvents (Maybe Text)
deEnvironmentName = lens _deEnvironmentName (\ s a -> s{_deEnvironmentName = a});

-- | Specifies the maximum number of events that can be returned, beginning with the most recent event.
deMaxRecords :: Lens' DescribeEvents (Maybe Natural)
deMaxRecords = lens _deMaxRecords (\ s a -> s{_deMaxRecords = a}) . mapping _Nat;

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those that occur up to, but not including, the @EndTime@ .
deEndTime :: Lens' DescribeEvents (Maybe UTCTime)
deEndTime = lens _deEndTime (\ s a -> s{_deEndTime = a}) . mapping _Time;

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to include only those associated with this application.
deApplicationName :: Lens' DescribeEvents (Maybe Text)
deApplicationName = lens _deApplicationName (\ s a -> s{_deApplicationName = a});

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to those associated with this environment.
deEnvironmentId :: Lens' DescribeEvents (Maybe Text)
deEnvironmentId = lens _deEnvironmentId (\ s a -> s{_deEnvironmentId = a});

instance AWSPager DescribeEvents where
        page rq rs
          | stop (rs ^. dersNextToken) = Nothing
          | stop (rs ^. dersEvents) = Nothing
          | otherwise =
            Just $ rq & deNextToken .~ rs ^. dersNextToken

instance AWSRequest DescribeEvents where
        type Rs DescribeEvents = DescribeEventsResponse
        request = postQuery elasticBeanstalk
        response
          = receiveXMLWrapper "DescribeEventsResult"
              (\ s h x ->
                 DescribeEventsResponse' <$>
                   (x .@? "NextToken") <*>
                     (x .@? "Events" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeEvents where

instance NFData DescribeEvents where

instance ToHeaders DescribeEvents where
        toHeaders = const mempty

instance ToPath DescribeEvents where
        toPath = const "/"

instance ToQuery DescribeEvents where
        toQuery DescribeEvents'{..}
          = mconcat
              ["Action" =: ("DescribeEvents" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "RequestId" =: _deRequestId,
               "TemplateName" =: _deTemplateName,
               "StartTime" =: _deStartTime,
               "Severity" =: _deSeverity,
               "NextToken" =: _deNextToken,
               "VersionLabel" =: _deVersionLabel,
               "PlatformArn" =: _dePlatformARN,
               "EnvironmentName" =: _deEnvironmentName,
               "MaxRecords" =: _deMaxRecords,
               "EndTime" =: _deEndTime,
               "ApplicationName" =: _deApplicationName,
               "EnvironmentId" =: _deEnvironmentId]

-- | Result message wrapping a list of event descriptions.
--
--
--
-- /See:/ 'describeEventsResponse' smart constructor.
data DescribeEventsResponse = DescribeEventsResponse'
  { _dersNextToken      :: {-# NOUNPACK #-}!(Maybe Text)
  , _dersEvents         :: {-# NOUNPACK #-}!(Maybe [EventDescription])
  , _dersResponseStatus :: {-# NOUNPACK #-}!Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEventsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dersNextToken' - If returned, this indicates that there are more results to obtain. Use this token in the next 'DescribeEvents' call to get the next batch of events.
--
-- * 'dersEvents' - A list of 'EventDescription' .
--
-- * 'dersResponseStatus' - -- | The response status code.
describeEventsResponse
    :: Int -- ^ 'dersResponseStatus'
    -> DescribeEventsResponse
describeEventsResponse pResponseStatus_ =
  DescribeEventsResponse'
  { _dersNextToken = Nothing
  , _dersEvents = Nothing
  , _dersResponseStatus = pResponseStatus_
  }


-- | If returned, this indicates that there are more results to obtain. Use this token in the next 'DescribeEvents' call to get the next batch of events.
dersNextToken :: Lens' DescribeEventsResponse (Maybe Text)
dersNextToken = lens _dersNextToken (\ s a -> s{_dersNextToken = a});

-- | A list of 'EventDescription' .
dersEvents :: Lens' DescribeEventsResponse [EventDescription]
dersEvents = lens _dersEvents (\ s a -> s{_dersEvents = a}) . _Default . _Coerce;

-- | -- | The response status code.
dersResponseStatus :: Lens' DescribeEventsResponse Int
dersResponseStatus = lens _dersResponseStatus (\ s a -> s{_dersResponseStatus = a});

instance NFData DescribeEventsResponse where
