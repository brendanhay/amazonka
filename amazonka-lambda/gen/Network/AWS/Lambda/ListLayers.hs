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
-- Module      : Network.AWS.Lambda.ListLayers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layers> and shows information about the latest version of each. Specify a <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime identifier> to list only layers that indicate that they're compatible with that runtime.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListLayers
    (
    -- * Creating a Request
      listLayers
    , ListLayers
    -- * Request Lenses
    , llCompatibleRuntime
    , llMarker
    , llMaxItems

    -- * Destructuring the Response
    , listLayersResponse
    , ListLayersResponse
    -- * Response Lenses
    , llrsNextMarker
    , llrsLayers
    , llrsResponseStatus
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Lambda.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listLayers' smart constructor.
data ListLayers = ListLayers'
  { _llCompatibleRuntime :: !(Maybe Runtime)
  , _llMarker            :: !(Maybe Text)
  , _llMaxItems          :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListLayers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'llCompatibleRuntime' - A runtime identifier. For example, @go1.x@ .
--
-- * 'llMarker' - A pagination token returned by a previous call.
--
-- * 'llMaxItems' - The maximum number of layers to return.
listLayers
    :: ListLayers
listLayers =
  ListLayers'
    {_llCompatibleRuntime = Nothing, _llMarker = Nothing, _llMaxItems = Nothing}


-- | A runtime identifier. For example, @go1.x@ .
llCompatibleRuntime :: Lens' ListLayers (Maybe Runtime)
llCompatibleRuntime = lens _llCompatibleRuntime (\ s a -> s{_llCompatibleRuntime = a})

-- | A pagination token returned by a previous call.
llMarker :: Lens' ListLayers (Maybe Text)
llMarker = lens _llMarker (\ s a -> s{_llMarker = a})

-- | The maximum number of layers to return.
llMaxItems :: Lens' ListLayers (Maybe Natural)
llMaxItems = lens _llMaxItems (\ s a -> s{_llMaxItems = a}) . mapping _Nat

instance AWSPager ListLayers where
        page rq rs
          | stop (rs ^. llrsNextMarker) = Nothing
          | stop (rs ^. llrsLayers) = Nothing
          | otherwise =
            Just $ rq & llMarker .~ rs ^. llrsNextMarker

instance AWSRequest ListLayers where
        type Rs ListLayers = ListLayersResponse
        request = get lambda
        response
          = receiveJSON
              (\ s h x ->
                 ListLayersResponse' <$>
                   (x .?> "NextMarker") <*> (x .?> "Layers" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListLayers where

instance NFData ListLayers where

instance ToHeaders ListLayers where
        toHeaders = const mempty

instance ToPath ListLayers where
        toPath = const "/2018-10-31/layers"

instance ToQuery ListLayers where
        toQuery ListLayers'{..}
          = mconcat
              ["CompatibleRuntime" =: _llCompatibleRuntime,
               "Marker" =: _llMarker, "MaxItems" =: _llMaxItems]

-- | /See:/ 'listLayersResponse' smart constructor.
data ListLayersResponse = ListLayersResponse'
  { _llrsNextMarker     :: !(Maybe Text)
  , _llrsLayers         :: !(Maybe [LayersListItem])
  , _llrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListLayersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'llrsNextMarker' - A pagination token returned when the response doesn't contain all layers.
--
-- * 'llrsLayers' - A list of function layers.
--
-- * 'llrsResponseStatus' - -- | The response status code.
listLayersResponse
    :: Int -- ^ 'llrsResponseStatus'
    -> ListLayersResponse
listLayersResponse pResponseStatus_ =
  ListLayersResponse'
    { _llrsNextMarker = Nothing
    , _llrsLayers = Nothing
    , _llrsResponseStatus = pResponseStatus_
    }


-- | A pagination token returned when the response doesn't contain all layers.
llrsNextMarker :: Lens' ListLayersResponse (Maybe Text)
llrsNextMarker = lens _llrsNextMarker (\ s a -> s{_llrsNextMarker = a})

-- | A list of function layers.
llrsLayers :: Lens' ListLayersResponse [LayersListItem]
llrsLayers = lens _llrsLayers (\ s a -> s{_llrsLayers = a}) . _Default . _Coerce

-- | -- | The response status code.
llrsResponseStatus :: Lens' ListLayersResponse Int
llrsResponseStatus = lens _llrsResponseStatus (\ s a -> s{_llrsResponseStatus = a})

instance NFData ListLayersResponse where
