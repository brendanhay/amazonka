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
-- Module      : Network.AWS.WorkDocs.DescribeActivities
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the user activities in a specified time period.
--
--
module Network.AWS.WorkDocs.DescribeActivities
    (
    -- * Creating a Request
      describeActivities
    , DescribeActivities
    -- * Request Lenses
    , daStartTime
    , daAuthenticationToken
    , daUserId
    , daMarker
    , daEndTime
    , daLimit
    , daOrganizationId

    -- * Destructuring the Response
    , describeActivitiesResponse
    , DescribeActivitiesResponse
    -- * Response Lenses
    , darsUserActivities
    , darsMarker
    , darsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'describeActivities' smart constructor.
data DescribeActivities = DescribeActivities'
  { _daStartTime           :: !(Maybe POSIX)
  , _daAuthenticationToken :: !(Maybe (Sensitive Text))
  , _daUserId              :: !(Maybe Text)
  , _daMarker              :: !(Maybe Text)
  , _daEndTime             :: !(Maybe POSIX)
  , _daLimit               :: !(Maybe Nat)
  , _daOrganizationId      :: !(Maybe Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeActivities' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daStartTime' - The timestamp that determines the starting time of the activities. The response includes the activities performed after the specified timestamp.
--
-- * 'daAuthenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
--
-- * 'daUserId' - The ID of the user who performed the action. The response includes activities pertaining to this user. This is an optional parameter and is only applicable for administrative API (SigV4) requests.
--
-- * 'daMarker' - The marker for the next set of results.
--
-- * 'daEndTime' - The timestamp that determines the end time of the activities. The response includes the activities performed before the specified timestamp.
--
-- * 'daLimit' - The maximum number of items to return.
--
-- * 'daOrganizationId' - The ID of the organization. This is a mandatory parameter when using administrative API (SigV4) requests.
describeActivities
    :: DescribeActivities
describeActivities =
  DescribeActivities'
    { _daStartTime = Nothing
    , _daAuthenticationToken = Nothing
    , _daUserId = Nothing
    , _daMarker = Nothing
    , _daEndTime = Nothing
    , _daLimit = Nothing
    , _daOrganizationId = Nothing
    }


-- | The timestamp that determines the starting time of the activities. The response includes the activities performed after the specified timestamp.
daStartTime :: Lens' DescribeActivities (Maybe UTCTime)
daStartTime = lens _daStartTime (\ s a -> s{_daStartTime = a}) . mapping _Time

-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
daAuthenticationToken :: Lens' DescribeActivities (Maybe Text)
daAuthenticationToken = lens _daAuthenticationToken (\ s a -> s{_daAuthenticationToken = a}) . mapping _Sensitive

-- | The ID of the user who performed the action. The response includes activities pertaining to this user. This is an optional parameter and is only applicable for administrative API (SigV4) requests.
daUserId :: Lens' DescribeActivities (Maybe Text)
daUserId = lens _daUserId (\ s a -> s{_daUserId = a})

-- | The marker for the next set of results.
daMarker :: Lens' DescribeActivities (Maybe Text)
daMarker = lens _daMarker (\ s a -> s{_daMarker = a})

-- | The timestamp that determines the end time of the activities. The response includes the activities performed before the specified timestamp.
daEndTime :: Lens' DescribeActivities (Maybe UTCTime)
daEndTime = lens _daEndTime (\ s a -> s{_daEndTime = a}) . mapping _Time

-- | The maximum number of items to return.
daLimit :: Lens' DescribeActivities (Maybe Natural)
daLimit = lens _daLimit (\ s a -> s{_daLimit = a}) . mapping _Nat

-- | The ID of the organization. This is a mandatory parameter when using administrative API (SigV4) requests.
daOrganizationId :: Lens' DescribeActivities (Maybe Text)
daOrganizationId = lens _daOrganizationId (\ s a -> s{_daOrganizationId = a})

instance AWSRequest DescribeActivities where
        type Rs DescribeActivities =
             DescribeActivitiesResponse
        request = get workDocs
        response
          = receiveJSON
              (\ s h x ->
                 DescribeActivitiesResponse' <$>
                   (x .?> "UserActivities" .!@ mempty) <*>
                     (x .?> "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeActivities where

instance NFData DescribeActivities where

instance ToHeaders DescribeActivities where
        toHeaders DescribeActivities'{..}
          = mconcat
              ["Authentication" =# _daAuthenticationToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToPath DescribeActivities where
        toPath = const "/api/v1/activities"

instance ToQuery DescribeActivities where
        toQuery DescribeActivities'{..}
          = mconcat
              ["startTime" =: _daStartTime, "userId" =: _daUserId,
               "marker" =: _daMarker, "endTime" =: _daEndTime,
               "limit" =: _daLimit,
               "organizationId" =: _daOrganizationId]

-- | /See:/ 'describeActivitiesResponse' smart constructor.
data DescribeActivitiesResponse = DescribeActivitiesResponse'
  { _darsUserActivities :: !(Maybe [Activity])
  , _darsMarker         :: !(Maybe Text)
  , _darsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeActivitiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darsUserActivities' - The list of activities for the specified user and time period.
--
-- * 'darsMarker' - The marker for the next set of results.
--
-- * 'darsResponseStatus' - -- | The response status code.
describeActivitiesResponse
    :: Int -- ^ 'darsResponseStatus'
    -> DescribeActivitiesResponse
describeActivitiesResponse pResponseStatus_ =
  DescribeActivitiesResponse'
    { _darsUserActivities = Nothing
    , _darsMarker = Nothing
    , _darsResponseStatus = pResponseStatus_
    }


-- | The list of activities for the specified user and time period.
darsUserActivities :: Lens' DescribeActivitiesResponse [Activity]
darsUserActivities = lens _darsUserActivities (\ s a -> s{_darsUserActivities = a}) . _Default . _Coerce

-- | The marker for the next set of results.
darsMarker :: Lens' DescribeActivitiesResponse (Maybe Text)
darsMarker = lens _darsMarker (\ s a -> s{_darsMarker = a})

-- | -- | The response status code.
darsResponseStatus :: Lens' DescribeActivitiesResponse Int
darsResponseStatus = lens _darsResponseStatus (\ s a -> s{_darsResponseStatus = a})

instance NFData DescribeActivitiesResponse where
