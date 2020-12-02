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
-- Module      : Network.AWS.CloudFront.ListDistributionsByRealtimeLogConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of distributions that have a cache behavior that’s associated with the specified real-time log configuration.
--
--
-- You can specify the real-time log configuration by its name or its Amazon Resource Name (ARN). You must provide at least one. If you provide both, CloudFront uses the name to identify the real-time log configuration to list distributions for.
--
-- You can optionally specify the maximum number of items to receive in the response. If the total number of items in the list exceeds the maximum that you specify, or the default maximum, the response is paginated. To get the next page of items, send a subsequent request that specifies the @NextMarker@ value from the current response as the @Marker@ value in the subsequent request.
module Network.AWS.CloudFront.ListDistributionsByRealtimeLogConfig
  ( -- * Creating a Request
    listDistributionsByRealtimeLogConfig,
    ListDistributionsByRealtimeLogConfig,

    -- * Request Lenses
    ldbrlcRealtimeLogConfigName,
    ldbrlcRealtimeLogConfigARN,
    ldbrlcMarker,
    ldbrlcMaxItems,

    -- * Destructuring the Response
    listDistributionsByRealtimeLogConfigResponse,
    ListDistributionsByRealtimeLogConfigResponse,

    -- * Response Lenses
    ldbrlcrsDistributionList,
    ldbrlcrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listDistributionsByRealtimeLogConfig' smart constructor.
data ListDistributionsByRealtimeLogConfig = ListDistributionsByRealtimeLogConfig'
  { _ldbrlcRealtimeLogConfigName ::
      !(Maybe Text),
    _ldbrlcRealtimeLogConfigARN ::
      !(Maybe Text),
    _ldbrlcMarker ::
      !(Maybe Text),
    _ldbrlcMaxItems ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListDistributionsByRealtimeLogConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldbrlcRealtimeLogConfigName' - The name of the real-time log configuration whose associated distributions you want to list.
--
-- * 'ldbrlcRealtimeLogConfigARN' - The Amazon Resource Name (ARN) of the real-time log configuration whose associated distributions you want to list.
--
-- * 'ldbrlcMarker' - Use this field when paginating results to indicate where to begin in your list of distributions. The response includes distributions in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
--
-- * 'ldbrlcMaxItems' - The maximum number of distributions that you want in the response.
listDistributionsByRealtimeLogConfig ::
  ListDistributionsByRealtimeLogConfig
listDistributionsByRealtimeLogConfig =
  ListDistributionsByRealtimeLogConfig'
    { _ldbrlcRealtimeLogConfigName =
        Nothing,
      _ldbrlcRealtimeLogConfigARN = Nothing,
      _ldbrlcMarker = Nothing,
      _ldbrlcMaxItems = Nothing
    }

-- | The name of the real-time log configuration whose associated distributions you want to list.
ldbrlcRealtimeLogConfigName :: Lens' ListDistributionsByRealtimeLogConfig (Maybe Text)
ldbrlcRealtimeLogConfigName = lens _ldbrlcRealtimeLogConfigName (\s a -> s {_ldbrlcRealtimeLogConfigName = a})

-- | The Amazon Resource Name (ARN) of the real-time log configuration whose associated distributions you want to list.
ldbrlcRealtimeLogConfigARN :: Lens' ListDistributionsByRealtimeLogConfig (Maybe Text)
ldbrlcRealtimeLogConfigARN = lens _ldbrlcRealtimeLogConfigARN (\s a -> s {_ldbrlcRealtimeLogConfigARN = a})

-- | Use this field when paginating results to indicate where to begin in your list of distributions. The response includes distributions in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
ldbrlcMarker :: Lens' ListDistributionsByRealtimeLogConfig (Maybe Text)
ldbrlcMarker = lens _ldbrlcMarker (\s a -> s {_ldbrlcMarker = a})

-- | The maximum number of distributions that you want in the response.
ldbrlcMaxItems :: Lens' ListDistributionsByRealtimeLogConfig (Maybe Text)
ldbrlcMaxItems = lens _ldbrlcMaxItems (\s a -> s {_ldbrlcMaxItems = a})

instance AWSRequest ListDistributionsByRealtimeLogConfig where
  type
    Rs ListDistributionsByRealtimeLogConfig =
      ListDistributionsByRealtimeLogConfigResponse
  request = postXML cloudFront
  response =
    receiveXML
      ( \s h x ->
          ListDistributionsByRealtimeLogConfigResponse'
            <$> (parseXML x) <*> (pure (fromEnum s))
      )

instance Hashable ListDistributionsByRealtimeLogConfig

instance NFData ListDistributionsByRealtimeLogConfig

instance ToElement ListDistributionsByRealtimeLogConfig where
  toElement =
    mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}ListDistributionsByRealtimeLogConfigRequest"

instance ToHeaders ListDistributionsByRealtimeLogConfig where
  toHeaders = const mempty

instance ToPath ListDistributionsByRealtimeLogConfig where
  toPath = const "/2020-05-31/distributionsByRealtimeLogConfig/"

instance ToQuery ListDistributionsByRealtimeLogConfig where
  toQuery = const mempty

instance ToXML ListDistributionsByRealtimeLogConfig where
  toXML ListDistributionsByRealtimeLogConfig' {..} =
    mconcat
      [ "RealtimeLogConfigName" @= _ldbrlcRealtimeLogConfigName,
        "RealtimeLogConfigArn" @= _ldbrlcRealtimeLogConfigARN,
        "Marker" @= _ldbrlcMarker,
        "MaxItems" @= _ldbrlcMaxItems
      ]

-- | /See:/ 'listDistributionsByRealtimeLogConfigResponse' smart constructor.
data ListDistributionsByRealtimeLogConfigResponse = ListDistributionsByRealtimeLogConfigResponse'
  { _ldbrlcrsDistributionList ::
      !( Maybe
           DistributionList
       ),
    _ldbrlcrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'ListDistributionsByRealtimeLogConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldbrlcrsDistributionList' - Undocumented member.
--
-- * 'ldbrlcrsResponseStatus' - -- | The response status code.
listDistributionsByRealtimeLogConfigResponse ::
  -- | 'ldbrlcrsResponseStatus'
  Int ->
  ListDistributionsByRealtimeLogConfigResponse
listDistributionsByRealtimeLogConfigResponse pResponseStatus_ =
  ListDistributionsByRealtimeLogConfigResponse'
    { _ldbrlcrsDistributionList =
        Nothing,
      _ldbrlcrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
ldbrlcrsDistributionList :: Lens' ListDistributionsByRealtimeLogConfigResponse (Maybe DistributionList)
ldbrlcrsDistributionList = lens _ldbrlcrsDistributionList (\s a -> s {_ldbrlcrsDistributionList = a})

-- | -- | The response status code.
ldbrlcrsResponseStatus :: Lens' ListDistributionsByRealtimeLogConfigResponse Int
ldbrlcrsResponseStatus = lens _ldbrlcrsResponseStatus (\s a -> s {_ldbrlcrsResponseStatus = a})

instance NFData ListDistributionsByRealtimeLogConfigResponse
