{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ItemResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ItemResponse where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.EndpointItemResponse
import Network.AWS.Pinpoint.Types.EventItemResponse
import Network.AWS.Prelude

-- | Provides information about the results of a request to create or update an endpoint that's associated with an event.
--
--
--
-- /See:/ 'itemResponse' smart constructor.
data ItemResponse = ItemResponse'
  { _iEndpointItemResponse ::
      !(Maybe EndpointItemResponse),
    _iEventsItemResponse :: !(Maybe (Map Text (EventItemResponse)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ItemResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iEndpointItemResponse' - The response that was received after the endpoint data was accepted.
--
-- * 'iEventsItemResponse' - A multipart response object that contains a key and a value for each event in the request. In each object, the event ID is the key and an EventItemResponse object is the value.
itemResponse ::
  ItemResponse
itemResponse =
  ItemResponse'
    { _iEndpointItemResponse = Nothing,
      _iEventsItemResponse = Nothing
    }

-- | The response that was received after the endpoint data was accepted.
iEndpointItemResponse :: Lens' ItemResponse (Maybe EndpointItemResponse)
iEndpointItemResponse = lens _iEndpointItemResponse (\s a -> s {_iEndpointItemResponse = a})

-- | A multipart response object that contains a key and a value for each event in the request. In each object, the event ID is the key and an EventItemResponse object is the value.
iEventsItemResponse :: Lens' ItemResponse (HashMap Text (EventItemResponse))
iEventsItemResponse = lens _iEventsItemResponse (\s a -> s {_iEventsItemResponse = a}) . _Default . _Map

instance FromJSON ItemResponse where
  parseJSON =
    withObject
      "ItemResponse"
      ( \x ->
          ItemResponse'
            <$> (x .:? "EndpointItemResponse")
            <*> (x .:? "EventsItemResponse" .!= mempty)
      )

instance Hashable ItemResponse

instance NFData ItemResponse
