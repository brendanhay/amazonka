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
-- Module      : Network.AWS.CloudFront.ListTagsForResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List tags for a CloudFront resource.
--
--
module Network.AWS.CloudFront.ListTagsForResource
    (
    -- * Creating a Request
      listTagsForResource
    , ListTagsForResource
    -- * Request Lenses
    , ltfrResource

    -- * Destructuring the Response
    , listTagsForResourceResponse
    , ListTagsForResourceResponse
    -- * Response Lenses
    , ltfrrsResponseStatus
    , ltfrrsTags
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The request to list tags for a CloudFront resource.
--
--
--
-- /See:/ 'listTagsForResource' smart constructor.
newtype ListTagsForResource = ListTagsForResource'
  { _ltfrResource :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagsForResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfrResource' - An ARN of a CloudFront resource.
listTagsForResource
    :: Text -- ^ 'ltfrResource'
    -> ListTagsForResource
listTagsForResource pResource_ =
  ListTagsForResource' {_ltfrResource = pResource_}


-- | An ARN of a CloudFront resource.
ltfrResource :: Lens' ListTagsForResource Text
ltfrResource = lens _ltfrResource (\ s a -> s{_ltfrResource = a})

instance AWSRequest ListTagsForResource where
        type Rs ListTagsForResource =
             ListTagsForResourceResponse
        request = get cloudFront
        response
          = receiveXML
              (\ s h x ->
                 ListTagsForResourceResponse' <$>
                   (pure (fromEnum s)) <*> (parseXML x))

instance Hashable ListTagsForResource where

instance NFData ListTagsForResource where

instance ToHeaders ListTagsForResource where
        toHeaders = const mempty

instance ToPath ListTagsForResource where
        toPath = const "/2017-10-30/tagging"

instance ToQuery ListTagsForResource where
        toQuery ListTagsForResource'{..}
          = mconcat ["Resource" =: _ltfrResource]

-- | The returned result of the corresponding request.
--
--
--
-- /See:/ 'listTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { _ltfrrsResponseStatus :: !Int
  , _ltfrrsTags           :: !Tags
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagsForResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfrrsResponseStatus' - -- | The response status code.
--
-- * 'ltfrrsTags' - A complex type that contains zero or more @Tag@ elements.
listTagsForResourceResponse
    :: Int -- ^ 'ltfrrsResponseStatus'
    -> Tags -- ^ 'ltfrrsTags'
    -> ListTagsForResourceResponse
listTagsForResourceResponse pResponseStatus_ pTags_ =
  ListTagsForResourceResponse'
    {_ltfrrsResponseStatus = pResponseStatus_, _ltfrrsTags = pTags_}


-- | -- | The response status code.
ltfrrsResponseStatus :: Lens' ListTagsForResourceResponse Int
ltfrrsResponseStatus = lens _ltfrrsResponseStatus (\ s a -> s{_ltfrrsResponseStatus = a})

-- | A complex type that contains zero or more @Tag@ elements.
ltfrrsTags :: Lens' ListTagsForResourceResponse Tags
ltfrrsTags = lens _ltfrrsTags (\ s a -> s{_ltfrrsTags = a})

instance NFData ListTagsForResourceResponse where
