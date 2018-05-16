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
-- Module      : Network.AWS.AlexaBusiness.ListDeviceEvents
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Device Event history for up to 30 days. If EventType isn't specified in the request, this returns a list of all device events in reverse chronological order. If EventType is specified, this returns a list of device events for that EventType in reverse chronological order.
--
--
module Network.AWS.AlexaBusiness.ListDeviceEvents
    (
    -- * Creating a Request
      listDeviceEvents
    , ListDeviceEvents
    -- * Request Lenses
    , ldeNextToken
    , ldeEventType
    , ldeMaxResults
    , ldeDeviceARN

    -- * Destructuring the Response
    , listDeviceEventsResponse
    , ListDeviceEventsResponse
    -- * Response Lenses
    , ldersNextToken
    , ldersDeviceEvents
    , ldersResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listDeviceEvents' smart constructor.
data ListDeviceEvents = ListDeviceEvents'
  { _ldeNextToken  :: !(Maybe Text)
  , _ldeEventType  :: !(Maybe DeviceEventType)
  , _ldeMaxResults :: !(Maybe Nat)
  , _ldeDeviceARN  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDeviceEvents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldeNextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response only includes results beyond the token, up to the value specified by MaxResults.
--
-- * 'ldeEventType' - The event type to filter device events.
--
-- * 'ldeMaxResults' - The maximum number of results to include in the response. If more results exist than the specified MaxResults value, a token is included in the response so that the remaining results can be retrieved. Required.
--
-- * 'ldeDeviceARN' - The ARN of a device.
listDeviceEvents
    :: Text -- ^ 'ldeDeviceARN'
    -> ListDeviceEvents
listDeviceEvents pDeviceARN_ =
  ListDeviceEvents'
    { _ldeNextToken = Nothing
    , _ldeEventType = Nothing
    , _ldeMaxResults = Nothing
    , _ldeDeviceARN = pDeviceARN_
    }


-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response only includes results beyond the token, up to the value specified by MaxResults.
ldeNextToken :: Lens' ListDeviceEvents (Maybe Text)
ldeNextToken = lens _ldeNextToken (\ s a -> s{_ldeNextToken = a})

-- | The event type to filter device events.
ldeEventType :: Lens' ListDeviceEvents (Maybe DeviceEventType)
ldeEventType = lens _ldeEventType (\ s a -> s{_ldeEventType = a})

-- | The maximum number of results to include in the response. If more results exist than the specified MaxResults value, a token is included in the response so that the remaining results can be retrieved. Required.
ldeMaxResults :: Lens' ListDeviceEvents (Maybe Natural)
ldeMaxResults = lens _ldeMaxResults (\ s a -> s{_ldeMaxResults = a}) . mapping _Nat

-- | The ARN of a device.
ldeDeviceARN :: Lens' ListDeviceEvents Text
ldeDeviceARN = lens _ldeDeviceARN (\ s a -> s{_ldeDeviceARN = a})

instance AWSRequest ListDeviceEvents where
        type Rs ListDeviceEvents = ListDeviceEventsResponse
        request = postJSON alexaBusiness
        response
          = receiveJSON
              (\ s h x ->
                 ListDeviceEventsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "DeviceEvents" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListDeviceEvents where

instance NFData ListDeviceEvents where

instance ToHeaders ListDeviceEvents where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.ListDeviceEvents" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListDeviceEvents where
        toJSON ListDeviceEvents'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _ldeNextToken,
                  ("EventType" .=) <$> _ldeEventType,
                  ("MaxResults" .=) <$> _ldeMaxResults,
                  Just ("DeviceArn" .= _ldeDeviceARN)])

instance ToPath ListDeviceEvents where
        toPath = const "/"

instance ToQuery ListDeviceEvents where
        toQuery = const mempty

-- | /See:/ 'listDeviceEventsResponse' smart constructor.
data ListDeviceEventsResponse = ListDeviceEventsResponse'
  { _ldersNextToken      :: !(Maybe Text)
  , _ldersDeviceEvents   :: !(Maybe [DeviceEvent])
  , _ldersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDeviceEventsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldersNextToken' -
--
-- * 'ldersDeviceEvents' -
--
-- * 'ldersResponseStatus' - -- | The response status code.
listDeviceEventsResponse
    :: Int -- ^ 'ldersResponseStatus'
    -> ListDeviceEventsResponse
listDeviceEventsResponse pResponseStatus_ =
  ListDeviceEventsResponse'
    { _ldersNextToken = Nothing
    , _ldersDeviceEvents = Nothing
    , _ldersResponseStatus = pResponseStatus_
    }


-- |
ldersNextToken :: Lens' ListDeviceEventsResponse (Maybe Text)
ldersNextToken = lens _ldersNextToken (\ s a -> s{_ldersNextToken = a})

-- |
ldersDeviceEvents :: Lens' ListDeviceEventsResponse [DeviceEvent]
ldersDeviceEvents = lens _ldersDeviceEvents (\ s a -> s{_ldersDeviceEvents = a}) . _Default . _Coerce

-- | -- | The response status code.
ldersResponseStatus :: Lens' ListDeviceEventsResponse Int
ldersResponseStatus = lens _ldersResponseStatus (\ s a -> s{_ldersResponseStatus = a})

instance NFData ListDeviceEventsResponse where
