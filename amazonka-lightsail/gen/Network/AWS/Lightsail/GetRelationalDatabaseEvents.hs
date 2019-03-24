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
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseEvents
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of events for a specific database in Amazon Lightsail.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetRelationalDatabaseEvents
    (
    -- * Creating a Request
      getRelationalDatabaseEvents
    , GetRelationalDatabaseEvents
    -- * Request Lenses
    , grdeDurationInMinutes
    , grdePageToken
    , grdeRelationalDatabaseName

    -- * Destructuring the Response
    , getRelationalDatabaseEventsResponse
    , GetRelationalDatabaseEventsResponse
    -- * Response Lenses
    , grdersNextPageToken
    , grdersRelationalDatabaseEvents
    , grdersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getRelationalDatabaseEvents' smart constructor.
data GetRelationalDatabaseEvents = GetRelationalDatabaseEvents'
  { _grdeDurationInMinutes      :: !(Maybe Int)
  , _grdePageToken              :: !(Maybe Text)
  , _grdeRelationalDatabaseName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRelationalDatabaseEvents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grdeDurationInMinutes' - The number of minutes in the past from which to retrieve events. For example, to get all events from the past 2 hours, enter 120. Default: @60@  The minimum is 1 and the maximum is 14 days (20160 minutes).
--
-- * 'grdePageToken' - A token used for advancing to a specific page of results from for get relational database events request.
--
-- * 'grdeRelationalDatabaseName' - The name of the database from which to get events.
getRelationalDatabaseEvents
    :: Text -- ^ 'grdeRelationalDatabaseName'
    -> GetRelationalDatabaseEvents
getRelationalDatabaseEvents pRelationalDatabaseName_ =
  GetRelationalDatabaseEvents'
    { _grdeDurationInMinutes = Nothing
    , _grdePageToken = Nothing
    , _grdeRelationalDatabaseName = pRelationalDatabaseName_
    }


-- | The number of minutes in the past from which to retrieve events. For example, to get all events from the past 2 hours, enter 120. Default: @60@  The minimum is 1 and the maximum is 14 days (20160 minutes).
grdeDurationInMinutes :: Lens' GetRelationalDatabaseEvents (Maybe Int)
grdeDurationInMinutes = lens _grdeDurationInMinutes (\ s a -> s{_grdeDurationInMinutes = a})

-- | A token used for advancing to a specific page of results from for get relational database events request.
grdePageToken :: Lens' GetRelationalDatabaseEvents (Maybe Text)
grdePageToken = lens _grdePageToken (\ s a -> s{_grdePageToken = a})

-- | The name of the database from which to get events.
grdeRelationalDatabaseName :: Lens' GetRelationalDatabaseEvents Text
grdeRelationalDatabaseName = lens _grdeRelationalDatabaseName (\ s a -> s{_grdeRelationalDatabaseName = a})

instance AWSPager GetRelationalDatabaseEvents where
        page rq rs
          | stop (rs ^. grdersNextPageToken) = Nothing
          | stop (rs ^. grdersRelationalDatabaseEvents) =
            Nothing
          | otherwise =
            Just $ rq &
              grdePageToken .~ rs ^. grdersNextPageToken

instance AWSRequest GetRelationalDatabaseEvents where
        type Rs GetRelationalDatabaseEvents =
             GetRelationalDatabaseEventsResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetRelationalDatabaseEventsResponse' <$>
                   (x .?> "nextPageToken") <*>
                     (x .?> "relationalDatabaseEvents" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetRelationalDatabaseEvents where

instance NFData GetRelationalDatabaseEvents where

instance ToHeaders GetRelationalDatabaseEvents where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetRelationalDatabaseEvents" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetRelationalDatabaseEvents where
        toJSON GetRelationalDatabaseEvents'{..}
          = object
              (catMaybes
                 [("durationInMinutes" .=) <$> _grdeDurationInMinutes,
                  ("pageToken" .=) <$> _grdePageToken,
                  Just
                    ("relationalDatabaseName" .=
                       _grdeRelationalDatabaseName)])

instance ToPath GetRelationalDatabaseEvents where
        toPath = const "/"

instance ToQuery GetRelationalDatabaseEvents where
        toQuery = const mempty

-- | /See:/ 'getRelationalDatabaseEventsResponse' smart constructor.
data GetRelationalDatabaseEventsResponse = GetRelationalDatabaseEventsResponse'
  { _grdersNextPageToken            :: !(Maybe Text)
  , _grdersRelationalDatabaseEvents :: !(Maybe [RelationalDatabaseEvent])
  , _grdersResponseStatus           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRelationalDatabaseEventsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grdersNextPageToken' - A token used for advancing to the next page of results from your get relational database events request.
--
-- * 'grdersRelationalDatabaseEvents' - An object describing the result of your get relational database events request.
--
-- * 'grdersResponseStatus' - -- | The response status code.
getRelationalDatabaseEventsResponse
    :: Int -- ^ 'grdersResponseStatus'
    -> GetRelationalDatabaseEventsResponse
getRelationalDatabaseEventsResponse pResponseStatus_ =
  GetRelationalDatabaseEventsResponse'
    { _grdersNextPageToken = Nothing
    , _grdersRelationalDatabaseEvents = Nothing
    , _grdersResponseStatus = pResponseStatus_
    }


-- | A token used for advancing to the next page of results from your get relational database events request.
grdersNextPageToken :: Lens' GetRelationalDatabaseEventsResponse (Maybe Text)
grdersNextPageToken = lens _grdersNextPageToken (\ s a -> s{_grdersNextPageToken = a})

-- | An object describing the result of your get relational database events request.
grdersRelationalDatabaseEvents :: Lens' GetRelationalDatabaseEventsResponse [RelationalDatabaseEvent]
grdersRelationalDatabaseEvents = lens _grdersRelationalDatabaseEvents (\ s a -> s{_grdersRelationalDatabaseEvents = a}) . _Default . _Coerce

-- | -- | The response status code.
grdersResponseStatus :: Lens' GetRelationalDatabaseEventsResponse Int
grdersResponseStatus = lens _grdersResponseStatus (\ s a -> s{_grdersResponseStatus = a})

instance NFData GetRelationalDatabaseEventsResponse
         where
