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
-- Module      : Network.AWS.CloudWatchEvents.ListTagsForResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays the tags associated with an EventBridge resource. In EventBridge, rules can be tagged.
--
--
module Network.AWS.CloudWatchEvents.ListTagsForResource
    (
    -- * Creating a Request
      listTagsForResource
    , ListTagsForResource
    -- * Request Lenses
    , ltfrResourceARN

    -- * Destructuring the Response
    , listTagsForResourceResponse
    , ListTagsForResourceResponse
    -- * Response Lenses
    , ltfrrsTags
    , ltfrrsResponseStatus
    ) where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.CloudWatchEvents.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTagsForResource' smart constructor.
newtype ListTagsForResource = ListTagsForResource'
  { _ltfrResourceARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagsForResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfrResourceARN' - The ARN of the rule for which you want to view tags.
listTagsForResource
    :: Text -- ^ 'ltfrResourceARN'
    -> ListTagsForResource
listTagsForResource pResourceARN_ =
  ListTagsForResource' {_ltfrResourceARN = pResourceARN_}


-- | The ARN of the rule for which you want to view tags.
ltfrResourceARN :: Lens' ListTagsForResource Text
ltfrResourceARN = lens _ltfrResourceARN (\ s a -> s{_ltfrResourceARN = a})

instance AWSRequest ListTagsForResource where
        type Rs ListTagsForResource =
             ListTagsForResourceResponse
        request = postJSON cloudWatchEvents
        response
          = receiveJSON
              (\ s h x ->
                 ListTagsForResourceResponse' <$>
                   (x .?> "Tags" .!@ mempty) <*> (pure (fromEnum s)))

instance Hashable ListTagsForResource where

instance NFData ListTagsForResource where

instance ToHeaders ListTagsForResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSEvents.ListTagsForResource" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListTagsForResource where
        toJSON ListTagsForResource'{..}
          = object
              (catMaybes
                 [Just ("ResourceARN" .= _ltfrResourceARN)])

instance ToPath ListTagsForResource where
        toPath = const "/"

instance ToQuery ListTagsForResource where
        toQuery = const mempty

-- | /See:/ 'listTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { _ltfrrsTags           :: !(Maybe [Tag])
  , _ltfrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagsForResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfrrsTags' - The list of tag keys and values associated with the rule that you specified.
--
-- * 'ltfrrsResponseStatus' - -- | The response status code.
listTagsForResourceResponse
    :: Int -- ^ 'ltfrrsResponseStatus'
    -> ListTagsForResourceResponse
listTagsForResourceResponse pResponseStatus_ =
  ListTagsForResourceResponse'
    {_ltfrrsTags = Nothing, _ltfrrsResponseStatus = pResponseStatus_}


-- | The list of tag keys and values associated with the rule that you specified.
ltfrrsTags :: Lens' ListTagsForResourceResponse [Tag]
ltfrrsTags = lens _ltfrrsTags (\ s a -> s{_ltfrrsTags = a}) . _Default . _Coerce

-- | -- | The response status code.
ltfrrsResponseStatus :: Lens' ListTagsForResourceResponse Int
ltfrrsResponseStatus = lens _ltfrrsResponseStatus (\ s a -> s{_ltfrrsResponseStatus = a})

instance NFData ListTagsForResourceResponse where
