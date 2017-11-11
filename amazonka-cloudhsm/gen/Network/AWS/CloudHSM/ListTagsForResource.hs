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
-- Module      : Network.AWS.CloudHSM.ListTagsForResource
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all tags for the specified AWS CloudHSM resource.
--
--
module Network.AWS.CloudHSM.ListTagsForResource
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
    , ltfrrsResponseStatus
    , ltfrrsTagList
    ) where

import Network.AWS.CloudHSM.Types
import Network.AWS.CloudHSM.Types.Product
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
-- * 'ltfrResourceARN' - The Amazon Resource Name (ARN) of the AWS CloudHSM resource.
listTagsForResource
    :: Text -- ^ 'ltfrResourceARN'
    -> ListTagsForResource
listTagsForResource pResourceARN_ =
  ListTagsForResource' {_ltfrResourceARN = pResourceARN_}


-- | The Amazon Resource Name (ARN) of the AWS CloudHSM resource.
ltfrResourceARN :: Lens' ListTagsForResource Text
ltfrResourceARN = lens _ltfrResourceARN (\ s a -> s{_ltfrResourceARN = a});

instance AWSRequest ListTagsForResource where
        type Rs ListTagsForResource =
             ListTagsForResourceResponse
        request = postJSON cloudHSM
        response
          = receiveJSON
              (\ s h x ->
                 ListTagsForResourceResponse' <$>
                   (pure (fromEnum s)) <*> (x .?> "TagList" .!@ mempty))

instance Hashable ListTagsForResource where

instance NFData ListTagsForResource where

instance ToHeaders ListTagsForResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.ListTagsForResource" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListTagsForResource where
        toJSON ListTagsForResource'{..}
          = object
              (catMaybes
                 [Just ("ResourceArn" .= _ltfrResourceARN)])

instance ToPath ListTagsForResource where
        toPath = const "/"

instance ToQuery ListTagsForResource where
        toQuery = const mempty

-- | /See:/ 'listTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { _ltfrrsResponseStatus :: {-# NOUNPACK #-}!Int
  , _ltfrrsTagList        :: {-# NOUNPACK #-}![Tag]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagsForResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfrrsResponseStatus' - -- | The response status code.
--
-- * 'ltfrrsTagList' - One or more tags.
listTagsForResourceResponse
    :: Int -- ^ 'ltfrrsResponseStatus'
    -> ListTagsForResourceResponse
listTagsForResourceResponse pResponseStatus_ =
  ListTagsForResourceResponse'
  {_ltfrrsResponseStatus = pResponseStatus_, _ltfrrsTagList = mempty}


-- | -- | The response status code.
ltfrrsResponseStatus :: Lens' ListTagsForResourceResponse Int
ltfrrsResponseStatus = lens _ltfrrsResponseStatus (\ s a -> s{_ltfrrsResponseStatus = a});

-- | One or more tags.
ltfrrsTagList :: Lens' ListTagsForResourceResponse [Tag]
ltfrrsTagList = lens _ltfrrsTagList (\ s a -> s{_ltfrrsTagList = a}) . _Coerce;

instance NFData ListTagsForResourceResponse where
