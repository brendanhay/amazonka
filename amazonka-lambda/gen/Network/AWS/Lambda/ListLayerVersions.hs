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
-- Module      : Network.AWS.Lambda.ListLayerVersions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> . Versions that have been deleted aren't listed. Specify a <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime identifier> to list only versions that indicate that they're compatible with that runtime.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListLayerVersions
    (
    -- * Creating a Request
      listLayerVersions
    , ListLayerVersions
    -- * Request Lenses
    , llvCompatibleRuntime
    , llvMarker
    , llvMaxItems
    , llvLayerName

    -- * Destructuring the Response
    , listLayerVersionsResponse
    , ListLayerVersionsResponse
    -- * Response Lenses
    , llvrsLayerVersions
    , llvrsNextMarker
    , llvrsResponseStatus
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Lambda.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listLayerVersions' smart constructor.
data ListLayerVersions = ListLayerVersions'
  { _llvCompatibleRuntime :: !(Maybe Runtime)
  , _llvMarker            :: !(Maybe Text)
  , _llvMaxItems          :: !(Maybe Nat)
  , _llvLayerName         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListLayerVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'llvCompatibleRuntime' - A runtime identifier. For example, @go1.x@ .
--
-- * 'llvMarker' - A pagination token returned by a previous call.
--
-- * 'llvMaxItems' - The maximum number of versions to return.
--
-- * 'llvLayerName' - The name or Amazon Resource Name (ARN) of the layer.
listLayerVersions
    :: Text -- ^ 'llvLayerName'
    -> ListLayerVersions
listLayerVersions pLayerName_ =
  ListLayerVersions'
    { _llvCompatibleRuntime = Nothing
    , _llvMarker = Nothing
    , _llvMaxItems = Nothing
    , _llvLayerName = pLayerName_
    }


-- | A runtime identifier. For example, @go1.x@ .
llvCompatibleRuntime :: Lens' ListLayerVersions (Maybe Runtime)
llvCompatibleRuntime = lens _llvCompatibleRuntime (\ s a -> s{_llvCompatibleRuntime = a})

-- | A pagination token returned by a previous call.
llvMarker :: Lens' ListLayerVersions (Maybe Text)
llvMarker = lens _llvMarker (\ s a -> s{_llvMarker = a})

-- | The maximum number of versions to return.
llvMaxItems :: Lens' ListLayerVersions (Maybe Natural)
llvMaxItems = lens _llvMaxItems (\ s a -> s{_llvMaxItems = a}) . mapping _Nat

-- | The name or Amazon Resource Name (ARN) of the layer.
llvLayerName :: Lens' ListLayerVersions Text
llvLayerName = lens _llvLayerName (\ s a -> s{_llvLayerName = a})

instance AWSPager ListLayerVersions where
        page rq rs
          | stop (rs ^. llvrsNextMarker) = Nothing
          | stop (rs ^. llvrsLayerVersions) = Nothing
          | otherwise =
            Just $ rq & llvMarker .~ rs ^. llvrsNextMarker

instance AWSRequest ListLayerVersions where
        type Rs ListLayerVersions = ListLayerVersionsResponse
        request = get lambda
        response
          = receiveJSON
              (\ s h x ->
                 ListLayerVersionsResponse' <$>
                   (x .?> "LayerVersions" .!@ mempty) <*>
                     (x .?> "NextMarker")
                     <*> (pure (fromEnum s)))

instance Hashable ListLayerVersions where

instance NFData ListLayerVersions where

instance ToHeaders ListLayerVersions where
        toHeaders = const mempty

instance ToPath ListLayerVersions where
        toPath ListLayerVersions'{..}
          = mconcat
              ["/2018-10-31/layers/", toBS _llvLayerName,
               "/versions"]

instance ToQuery ListLayerVersions where
        toQuery ListLayerVersions'{..}
          = mconcat
              ["CompatibleRuntime" =: _llvCompatibleRuntime,
               "Marker" =: _llvMarker, "MaxItems" =: _llvMaxItems]

-- | /See:/ 'listLayerVersionsResponse' smart constructor.
data ListLayerVersionsResponse = ListLayerVersionsResponse'
  { _llvrsLayerVersions  :: !(Maybe [LayerVersionsListItem])
  , _llvrsNextMarker     :: !(Maybe Text)
  , _llvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListLayerVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'llvrsLayerVersions' - A list of versions.
--
-- * 'llvrsNextMarker' - A pagination token returned when the response doesn't contain all versions.
--
-- * 'llvrsResponseStatus' - -- | The response status code.
listLayerVersionsResponse
    :: Int -- ^ 'llvrsResponseStatus'
    -> ListLayerVersionsResponse
listLayerVersionsResponse pResponseStatus_ =
  ListLayerVersionsResponse'
    { _llvrsLayerVersions = Nothing
    , _llvrsNextMarker = Nothing
    , _llvrsResponseStatus = pResponseStatus_
    }


-- | A list of versions.
llvrsLayerVersions :: Lens' ListLayerVersionsResponse [LayerVersionsListItem]
llvrsLayerVersions = lens _llvrsLayerVersions (\ s a -> s{_llvrsLayerVersions = a}) . _Default . _Coerce

-- | A pagination token returned when the response doesn't contain all versions.
llvrsNextMarker :: Lens' ListLayerVersionsResponse (Maybe Text)
llvrsNextMarker = lens _llvrsNextMarker (\ s a -> s{_llvrsNextMarker = a})

-- | -- | The response status code.
llvrsResponseStatus :: Lens' ListLayerVersionsResponse Int
llvrsResponseStatus = lens _llvrsResponseStatus (\ s a -> s{_llvrsResponseStatus = a})

instance NFData ListLayerVersionsResponse where
