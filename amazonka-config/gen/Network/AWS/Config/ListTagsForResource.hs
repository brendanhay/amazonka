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
-- Module      : Network.AWS.Config.ListTagsForResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the tags for AWS Config resource.
--
--
module Network.AWS.Config.ListTagsForResource
    (
    -- * Creating a Request
      listTagsForResource
    , ListTagsForResource
    -- * Request Lenses
    , ltfrNextToken
    , ltfrLimit
    , ltfrResourceARN

    -- * Destructuring the Response
    , listTagsForResourceResponse
    , ListTagsForResourceResponse
    -- * Response Lenses
    , ltfrrsNextToken
    , ltfrrsTags
    , ltfrrsResponseStatus
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { _ltfrNextToken   :: !(Maybe Text)
  , _ltfrLimit       :: !(Maybe Nat)
  , _ltfrResourceARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagsForResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfrNextToken' - The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'ltfrLimit' - The maximum number of tags returned on each page. The limit maximum is 50. You cannot specify a number greater than 50. If you specify 0, AWS Config uses the default.
--
-- * 'ltfrResourceARN' - The Amazon Resource Name (ARN) that identifies the resource for which to list the tags. Currently, the supported resources are @ConfigRule@ , @ConfigurationAggregator@ and @AggregatorAuthorization@ .
listTagsForResource
    :: Text -- ^ 'ltfrResourceARN'
    -> ListTagsForResource
listTagsForResource pResourceARN_ =
  ListTagsForResource'
    { _ltfrNextToken = Nothing
    , _ltfrLimit = Nothing
    , _ltfrResourceARN = pResourceARN_
    }


-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
ltfrNextToken :: Lens' ListTagsForResource (Maybe Text)
ltfrNextToken = lens _ltfrNextToken (\ s a -> s{_ltfrNextToken = a})

-- | The maximum number of tags returned on each page. The limit maximum is 50. You cannot specify a number greater than 50. If you specify 0, AWS Config uses the default.
ltfrLimit :: Lens' ListTagsForResource (Maybe Natural)
ltfrLimit = lens _ltfrLimit (\ s a -> s{_ltfrLimit = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) that identifies the resource for which to list the tags. Currently, the supported resources are @ConfigRule@ , @ConfigurationAggregator@ and @AggregatorAuthorization@ .
ltfrResourceARN :: Lens' ListTagsForResource Text
ltfrResourceARN = lens _ltfrResourceARN (\ s a -> s{_ltfrResourceARN = a})

instance AWSRequest ListTagsForResource where
        type Rs ListTagsForResource =
             ListTagsForResourceResponse
        request = postJSON config
        response
          = receiveJSON
              (\ s h x ->
                 ListTagsForResourceResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Tags") <*>
                     (pure (fromEnum s)))

instance Hashable ListTagsForResource where

instance NFData ListTagsForResource where

instance ToHeaders ListTagsForResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.ListTagsForResource" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListTagsForResource where
        toJSON ListTagsForResource'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _ltfrNextToken,
                  ("Limit" .=) <$> _ltfrLimit,
                  Just ("ResourceArn" .= _ltfrResourceARN)])

instance ToPath ListTagsForResource where
        toPath = const "/"

instance ToQuery ListTagsForResource where
        toQuery = const mempty

-- | /See:/ 'listTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { _ltfrrsNextToken      :: !(Maybe Text)
  , _ltfrrsTags           :: !(Maybe (List1 Tag))
  , _ltfrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagsForResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfrrsNextToken' - The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'ltfrrsTags' - The tags for the resource.
--
-- * 'ltfrrsResponseStatus' - -- | The response status code.
listTagsForResourceResponse
    :: Int -- ^ 'ltfrrsResponseStatus'
    -> ListTagsForResourceResponse
listTagsForResourceResponse pResponseStatus_ =
  ListTagsForResourceResponse'
    { _ltfrrsNextToken = Nothing
    , _ltfrrsTags = Nothing
    , _ltfrrsResponseStatus = pResponseStatus_
    }


-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
ltfrrsNextToken :: Lens' ListTagsForResourceResponse (Maybe Text)
ltfrrsNextToken = lens _ltfrrsNextToken (\ s a -> s{_ltfrrsNextToken = a})

-- | The tags for the resource.
ltfrrsTags :: Lens' ListTagsForResourceResponse (Maybe (NonEmpty Tag))
ltfrrsTags = lens _ltfrrsTags (\ s a -> s{_ltfrrsTags = a}) . mapping _List1

-- | -- | The response status code.
ltfrrsResponseStatus :: Lens' ListTagsForResourceResponse Int
ltfrrsResponseStatus = lens _ltfrrsResponseStatus (\ s a -> s{_ltfrrsResponseStatus = a})

instance NFData ListTagsForResourceResponse where
