{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListRealtimeLogConfigs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of real-time log configurations.
--
--
-- You can optionally specify the maximum number of items to receive in the response. If the total number of items in the list exceeds the maximum that you specify, or the default maximum, the response is paginated. To get the next page of items, send a subsequent request that specifies the @NextMarker@ value from the current response as the @Marker@ value in the subsequent request.
module Network.AWS.CloudFront.ListRealtimeLogConfigs
  ( -- * Creating a Request
    listRealtimeLogConfigs,
    ListRealtimeLogConfigs,

    -- * Request Lenses
    lrlcMarker,
    lrlcMaxItems,

    -- * Destructuring the Response
    listRealtimeLogConfigsResponse,
    ListRealtimeLogConfigsResponse,

    -- * Response Lenses
    lrlcrsRealtimeLogConfigs,
    lrlcrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listRealtimeLogConfigs' smart constructor.
data ListRealtimeLogConfigs = ListRealtimeLogConfigs'
  { _lrlcMarker ::
      !(Maybe Text),
    _lrlcMaxItems :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListRealtimeLogConfigs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrlcMarker' - Use this field when paginating results to indicate where to begin in your list of real-time log configurations. The response includes real-time log configurations in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
--
-- * 'lrlcMaxItems' - The maximum number of real-time log configurations that you want in the response.
listRealtimeLogConfigs ::
  ListRealtimeLogConfigs
listRealtimeLogConfigs =
  ListRealtimeLogConfigs'
    { _lrlcMarker = Nothing,
      _lrlcMaxItems = Nothing
    }

-- | Use this field when paginating results to indicate where to begin in your list of real-time log configurations. The response includes real-time log configurations in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
lrlcMarker :: Lens' ListRealtimeLogConfigs (Maybe Text)
lrlcMarker = lens _lrlcMarker (\s a -> s {_lrlcMarker = a})

-- | The maximum number of real-time log configurations that you want in the response.
lrlcMaxItems :: Lens' ListRealtimeLogConfigs (Maybe Text)
lrlcMaxItems = lens _lrlcMaxItems (\s a -> s {_lrlcMaxItems = a})

instance AWSRequest ListRealtimeLogConfigs where
  type Rs ListRealtimeLogConfigs = ListRealtimeLogConfigsResponse
  request = get cloudFront
  response =
    receiveXML
      ( \s h x ->
          ListRealtimeLogConfigsResponse'
            <$> (parseXML x) <*> (pure (fromEnum s))
      )

instance Hashable ListRealtimeLogConfigs

instance NFData ListRealtimeLogConfigs

instance ToHeaders ListRealtimeLogConfigs where
  toHeaders = const mempty

instance ToPath ListRealtimeLogConfigs where
  toPath = const "/2020-05-31/realtime-log-config"

instance ToQuery ListRealtimeLogConfigs where
  toQuery ListRealtimeLogConfigs' {..} =
    mconcat ["Marker" =: _lrlcMarker, "MaxItems" =: _lrlcMaxItems]

-- | /See:/ 'listRealtimeLogConfigsResponse' smart constructor.
data ListRealtimeLogConfigsResponse = ListRealtimeLogConfigsResponse'
  { _lrlcrsRealtimeLogConfigs ::
      !(Maybe RealtimeLogConfigs),
    _lrlcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListRealtimeLogConfigsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrlcrsRealtimeLogConfigs' - A list of real-time log configurations.
--
-- * 'lrlcrsResponseStatus' - -- | The response status code.
listRealtimeLogConfigsResponse ::
  -- | 'lrlcrsResponseStatus'
  Int ->
  ListRealtimeLogConfigsResponse
listRealtimeLogConfigsResponse pResponseStatus_ =
  ListRealtimeLogConfigsResponse'
    { _lrlcrsRealtimeLogConfigs =
        Nothing,
      _lrlcrsResponseStatus = pResponseStatus_
    }

-- | A list of real-time log configurations.
lrlcrsRealtimeLogConfigs :: Lens' ListRealtimeLogConfigsResponse (Maybe RealtimeLogConfigs)
lrlcrsRealtimeLogConfigs = lens _lrlcrsRealtimeLogConfigs (\s a -> s {_lrlcrsRealtimeLogConfigs = a})

-- | -- | The response status code.
lrlcrsResponseStatus :: Lens' ListRealtimeLogConfigsResponse Int
lrlcrsResponseStatus = lens _lrlcrsResponseStatus (\s a -> s {_lrlcrsResponseStatus = a})

instance NFData ListRealtimeLogConfigsResponse
