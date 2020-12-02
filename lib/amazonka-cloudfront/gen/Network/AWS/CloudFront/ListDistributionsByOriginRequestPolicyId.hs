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
-- Module      : Network.AWS.CloudFront.ListDistributionsByOriginRequestPolicyId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of distribution IDs for distributions that have a cache behavior that’s associated with the specified origin request policy.
--
--
-- You can optionally specify the maximum number of items to receive in the response. If the total number of items in the list exceeds the maximum that you specify, or the default maximum, the response is paginated. To get the next page of items, send a subsequent request that specifies the @NextMarker@ value from the current response as the @Marker@ value in the subsequent request.
module Network.AWS.CloudFront.ListDistributionsByOriginRequestPolicyId
  ( -- * Creating a Request
    listDistributionsByOriginRequestPolicyId,
    ListDistributionsByOriginRequestPolicyId,

    -- * Request Lenses
    ldborpiMarker,
    ldborpiMaxItems,
    ldborpiOriginRequestPolicyId,

    -- * Destructuring the Response
    listDistributionsByOriginRequestPolicyIdResponse,
    ListDistributionsByOriginRequestPolicyIdResponse,

    -- * Response Lenses
    ldborpirsDistributionIdList,
    ldborpirsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listDistributionsByOriginRequestPolicyId' smart constructor.
data ListDistributionsByOriginRequestPolicyId = ListDistributionsByOriginRequestPolicyId'
  { _ldborpiMarker ::
      !( Maybe
           Text
       ),
    _ldborpiMaxItems ::
      !( Maybe
           Text
       ),
    _ldborpiOriginRequestPolicyId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListDistributionsByOriginRequestPolicyId' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldborpiMarker' - Use this field when paginating results to indicate where to begin in your list of distribution IDs. The response includes distribution IDs in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
--
-- * 'ldborpiMaxItems' - The maximum number of distribution IDs that you want in the response.
--
-- * 'ldborpiOriginRequestPolicyId' - The ID of the origin request policy whose associated distribution IDs you want to list.
listDistributionsByOriginRequestPolicyId ::
  -- | 'ldborpiOriginRequestPolicyId'
  Text ->
  ListDistributionsByOriginRequestPolicyId
listDistributionsByOriginRequestPolicyId pOriginRequestPolicyId_ =
  ListDistributionsByOriginRequestPolicyId'
    { _ldborpiMarker =
        Nothing,
      _ldborpiMaxItems = Nothing,
      _ldborpiOriginRequestPolicyId =
        pOriginRequestPolicyId_
    }

-- | Use this field when paginating results to indicate where to begin in your list of distribution IDs. The response includes distribution IDs in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
ldborpiMarker :: Lens' ListDistributionsByOriginRequestPolicyId (Maybe Text)
ldborpiMarker = lens _ldborpiMarker (\s a -> s {_ldborpiMarker = a})

-- | The maximum number of distribution IDs that you want in the response.
ldborpiMaxItems :: Lens' ListDistributionsByOriginRequestPolicyId (Maybe Text)
ldborpiMaxItems = lens _ldborpiMaxItems (\s a -> s {_ldborpiMaxItems = a})

-- | The ID of the origin request policy whose associated distribution IDs you want to list.
ldborpiOriginRequestPolicyId :: Lens' ListDistributionsByOriginRequestPolicyId Text
ldborpiOriginRequestPolicyId = lens _ldborpiOriginRequestPolicyId (\s a -> s {_ldborpiOriginRequestPolicyId = a})

instance AWSRequest ListDistributionsByOriginRequestPolicyId where
  type
    Rs ListDistributionsByOriginRequestPolicyId =
      ListDistributionsByOriginRequestPolicyIdResponse
  request = get cloudFront
  response =
    receiveXML
      ( \s h x ->
          ListDistributionsByOriginRequestPolicyIdResponse'
            <$> (parseXML x) <*> (pure (fromEnum s))
      )

instance Hashable ListDistributionsByOriginRequestPolicyId

instance NFData ListDistributionsByOriginRequestPolicyId

instance ToHeaders ListDistributionsByOriginRequestPolicyId where
  toHeaders = const mempty

instance ToPath ListDistributionsByOriginRequestPolicyId where
  toPath ListDistributionsByOriginRequestPolicyId' {..} =
    mconcat
      [ "/2020-05-31/distributionsByOriginRequestPolicyId/",
        toBS _ldborpiOriginRequestPolicyId
      ]

instance ToQuery ListDistributionsByOriginRequestPolicyId where
  toQuery ListDistributionsByOriginRequestPolicyId' {..} =
    mconcat
      ["Marker" =: _ldborpiMarker, "MaxItems" =: _ldborpiMaxItems]

-- | /See:/ 'listDistributionsByOriginRequestPolicyIdResponse' smart constructor.
data ListDistributionsByOriginRequestPolicyIdResponse = ListDistributionsByOriginRequestPolicyIdResponse'
  { _ldborpirsDistributionIdList ::
      !( Maybe
           DistributionIdList
       ),
    _ldborpirsResponseStatus ::
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

-- | Creates a value of 'ListDistributionsByOriginRequestPolicyIdResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldborpirsDistributionIdList' - A list of distribution IDs.
--
-- * 'ldborpirsResponseStatus' - -- | The response status code.
listDistributionsByOriginRequestPolicyIdResponse ::
  -- | 'ldborpirsResponseStatus'
  Int ->
  ListDistributionsByOriginRequestPolicyIdResponse
listDistributionsByOriginRequestPolicyIdResponse pResponseStatus_ =
  ListDistributionsByOriginRequestPolicyIdResponse'
    { _ldborpirsDistributionIdList =
        Nothing,
      _ldborpirsResponseStatus = pResponseStatus_
    }

-- | A list of distribution IDs.
ldborpirsDistributionIdList :: Lens' ListDistributionsByOriginRequestPolicyIdResponse (Maybe DistributionIdList)
ldborpirsDistributionIdList = lens _ldborpirsDistributionIdList (\s a -> s {_ldborpirsDistributionIdList = a})

-- | -- | The response status code.
ldborpirsResponseStatus :: Lens' ListDistributionsByOriginRequestPolicyIdResponse Int
ldborpirsResponseStatus = lens _ldborpirsResponseStatus (\s a -> s {_ldborpirsResponseStatus = a})

instance NFData ListDistributionsByOriginRequestPolicyIdResponse
