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
-- Module      : Network.AWS.StorageGateway.ListTagsForResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags that have been added to the specified resource. This operation is only supported in the cached volume, stored volume and tape gateway type.
--
--
module Network.AWS.StorageGateway.ListTagsForResource
    (
    -- * Creating a Request
      listTagsForResource
    , ListTagsForResource
    -- * Request Lenses
    , ltfrMarker
    , ltfrLimit
    , ltfrResourceARN

    -- * Destructuring the Response
    , listTagsForResourceResponse
    , ListTagsForResourceResponse
    -- * Response Lenses
    , ltfrrsResourceARN
    , ltfrrsMarker
    , ltfrrsTags
    , ltfrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | ListTagsForResourceInput
--
--
--
-- /See:/ 'listTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { _ltfrMarker      :: !(Maybe Text)
  , _ltfrLimit       :: !(Maybe Nat)
  , _ltfrResourceARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagsForResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfrMarker' - An opaque string that indicates the position at which to begin returning the list of tags.
--
-- * 'ltfrLimit' - Specifies that the list of tags returned be limited to the specified number of items.
--
-- * 'ltfrResourceARN' - The Amazon Resource Name (ARN) of the resource for which you want to list tags.
listTagsForResource
    :: Text -- ^ 'ltfrResourceARN'
    -> ListTagsForResource
listTagsForResource pResourceARN_ =
  ListTagsForResource'
    { _ltfrMarker = Nothing
    , _ltfrLimit = Nothing
    , _ltfrResourceARN = pResourceARN_
    }


-- | An opaque string that indicates the position at which to begin returning the list of tags.
ltfrMarker :: Lens' ListTagsForResource (Maybe Text)
ltfrMarker = lens _ltfrMarker (\ s a -> s{_ltfrMarker = a})

-- | Specifies that the list of tags returned be limited to the specified number of items.
ltfrLimit :: Lens' ListTagsForResource (Maybe Natural)
ltfrLimit = lens _ltfrLimit (\ s a -> s{_ltfrLimit = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the resource for which you want to list tags.
ltfrResourceARN :: Lens' ListTagsForResource Text
ltfrResourceARN = lens _ltfrResourceARN (\ s a -> s{_ltfrResourceARN = a})

instance AWSRequest ListTagsForResource where
        type Rs ListTagsForResource =
             ListTagsForResourceResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 ListTagsForResourceResponse' <$>
                   (x .?> "ResourceARN") <*> (x .?> "Marker") <*>
                     (x .?> "Tags" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListTagsForResource where

instance NFData ListTagsForResource where

instance ToHeaders ListTagsForResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.ListTagsForResource" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListTagsForResource where
        toJSON ListTagsForResource'{..}
          = object
              (catMaybes
                 [("Marker" .=) <$> _ltfrMarker,
                  ("Limit" .=) <$> _ltfrLimit,
                  Just ("ResourceARN" .= _ltfrResourceARN)])

instance ToPath ListTagsForResource where
        toPath = const "/"

instance ToQuery ListTagsForResource where
        toQuery = const mempty

-- | ListTagsForResourceOutput
--
--
--
-- /See:/ 'listTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { _ltfrrsResourceARN    :: !(Maybe Text)
  , _ltfrrsMarker         :: !(Maybe Text)
  , _ltfrrsTags           :: !(Maybe [Tag])
  , _ltfrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagsForResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfrrsResourceARN' - he Amazon Resource Name (ARN) of the resource for which you want to list tags.
--
-- * 'ltfrrsMarker' - An opaque string that indicates the position at which to stop returning the list of tags.
--
-- * 'ltfrrsTags' - An array that contains the tags for the specified resource.
--
-- * 'ltfrrsResponseStatus' - -- | The response status code.
listTagsForResourceResponse
    :: Int -- ^ 'ltfrrsResponseStatus'
    -> ListTagsForResourceResponse
listTagsForResourceResponse pResponseStatus_ =
  ListTagsForResourceResponse'
    { _ltfrrsResourceARN = Nothing
    , _ltfrrsMarker = Nothing
    , _ltfrrsTags = Nothing
    , _ltfrrsResponseStatus = pResponseStatus_
    }


-- | he Amazon Resource Name (ARN) of the resource for which you want to list tags.
ltfrrsResourceARN :: Lens' ListTagsForResourceResponse (Maybe Text)
ltfrrsResourceARN = lens _ltfrrsResourceARN (\ s a -> s{_ltfrrsResourceARN = a})

-- | An opaque string that indicates the position at which to stop returning the list of tags.
ltfrrsMarker :: Lens' ListTagsForResourceResponse (Maybe Text)
ltfrrsMarker = lens _ltfrrsMarker (\ s a -> s{_ltfrrsMarker = a})

-- | An array that contains the tags for the specified resource.
ltfrrsTags :: Lens' ListTagsForResourceResponse [Tag]
ltfrrsTags = lens _ltfrrsTags (\ s a -> s{_ltfrrsTags = a}) . _Default . _Coerce

-- | -- | The response status code.
ltfrrsResponseStatus :: Lens' ListTagsForResourceResponse Int
ltfrrsResponseStatus = lens _ltfrrsResponseStatus (\ s a -> s{_ltfrrsResponseStatus = a})

instance NFData ListTagsForResourceResponse where
