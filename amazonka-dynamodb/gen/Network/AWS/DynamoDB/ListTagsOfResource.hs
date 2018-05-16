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
-- Module      : Network.AWS.DynamoDB.ListTagsOfResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all tags on an Amazon DynamoDB resource. You can call ListTagsOfResource up to 10 times per second, per account.
--
--
-- For an overview on tagging DynamoDB resources, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Tagging.html Tagging for DynamoDB> in the /Amazon DynamoDB Developer Guide/ .
--
module Network.AWS.DynamoDB.ListTagsOfResource
    (
    -- * Creating a Request
      listTagsOfResource
    , ListTagsOfResource
    -- * Request Lenses
    , ltorNextToken
    , ltorResourceARN

    -- * Destructuring the Response
    , listTagsOfResourceResponse
    , ListTagsOfResourceResponse
    -- * Response Lenses
    , ltorrsNextToken
    , ltorrsTags
    , ltorrsResponseStatus
    ) where

import Network.AWS.DynamoDB.Types
import Network.AWS.DynamoDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTagsOfResource' smart constructor.
data ListTagsOfResource = ListTagsOfResource'
  { _ltorNextToken   :: !(Maybe Text)
  , _ltorResourceARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagsOfResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltorNextToken' - An optional string that, if supplied, must be copied from the output of a previous call to ListTagOfResource. When provided in this manner, this API fetches the next page of results.
--
-- * 'ltorResourceARN' - The Amazon DynamoDB resource with tags to be listed. This value is an Amazon Resource Name (ARN).
listTagsOfResource
    :: Text -- ^ 'ltorResourceARN'
    -> ListTagsOfResource
listTagsOfResource pResourceARN_ =
  ListTagsOfResource'
    {_ltorNextToken = Nothing, _ltorResourceARN = pResourceARN_}


-- | An optional string that, if supplied, must be copied from the output of a previous call to ListTagOfResource. When provided in this manner, this API fetches the next page of results.
ltorNextToken :: Lens' ListTagsOfResource (Maybe Text)
ltorNextToken = lens _ltorNextToken (\ s a -> s{_ltorNextToken = a})

-- | The Amazon DynamoDB resource with tags to be listed. This value is an Amazon Resource Name (ARN).
ltorResourceARN :: Lens' ListTagsOfResource Text
ltorResourceARN = lens _ltorResourceARN (\ s a -> s{_ltorResourceARN = a})

instance AWSRequest ListTagsOfResource where
        type Rs ListTagsOfResource =
             ListTagsOfResourceResponse
        request = postJSON dynamoDB
        response
          = receiveJSON
              (\ s h x ->
                 ListTagsOfResourceResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Tags" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable ListTagsOfResource where

instance NFData ListTagsOfResource where

instance ToHeaders ListTagsOfResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DynamoDB_20120810.ListTagsOfResource" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON ListTagsOfResource where
        toJSON ListTagsOfResource'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _ltorNextToken,
                  Just ("ResourceArn" .= _ltorResourceARN)])

instance ToPath ListTagsOfResource where
        toPath = const "/"

instance ToQuery ListTagsOfResource where
        toQuery = const mempty

-- | /See:/ 'listTagsOfResourceResponse' smart constructor.
data ListTagsOfResourceResponse = ListTagsOfResourceResponse'
  { _ltorrsNextToken      :: !(Maybe Text)
  , _ltorrsTags           :: !(Maybe [Tag])
  , _ltorrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagsOfResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltorrsNextToken' - If this value is returned, there are additional results to be displayed. To retrieve them, call ListTagsOfResource again, with NextToken set to this value.
--
-- * 'ltorrsTags' - The tags currently associated with the Amazon DynamoDB resource.
--
-- * 'ltorrsResponseStatus' - -- | The response status code.
listTagsOfResourceResponse
    :: Int -- ^ 'ltorrsResponseStatus'
    -> ListTagsOfResourceResponse
listTagsOfResourceResponse pResponseStatus_ =
  ListTagsOfResourceResponse'
    { _ltorrsNextToken = Nothing
    , _ltorrsTags = Nothing
    , _ltorrsResponseStatus = pResponseStatus_
    }


-- | If this value is returned, there are additional results to be displayed. To retrieve them, call ListTagsOfResource again, with NextToken set to this value.
ltorrsNextToken :: Lens' ListTagsOfResourceResponse (Maybe Text)
ltorrsNextToken = lens _ltorrsNextToken (\ s a -> s{_ltorrsNextToken = a})

-- | The tags currently associated with the Amazon DynamoDB resource.
ltorrsTags :: Lens' ListTagsOfResourceResponse [Tag]
ltorrsTags = lens _ltorrsTags (\ s a -> s{_ltorrsTags = a}) . _Default . _Coerce

-- | -- | The response status code.
ltorrsResponseStatus :: Lens' ListTagsOfResourceResponse Int
ltorrsResponseStatus = lens _ltorrsResponseStatus (\ s a -> s{_ltorrsResponseStatus = a})

instance NFData ListTagsOfResourceResponse where
