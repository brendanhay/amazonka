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
-- Module      : Network.AWS.CostExplorer.GetTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Queries for available tag keys and tag values for a specified period. You can search the tag values for an arbitrary string.
--
--
module Network.AWS.CostExplorer.GetTags
    (
    -- * Creating a Request
      getTags
    , GetTags
    -- * Request Lenses
    , gtNextPageToken
    , gtSearchString
    , gtTagKey
    , gtTimePeriod

    -- * Destructuring the Response
    , getTagsResponse
    , GetTagsResponse
    -- * Response Lenses
    , gtrsNextPageToken
    , gtrsResponseStatus
    , gtrsTags
    , gtrsReturnSize
    , gtrsTotalSize
    ) where

import Network.AWS.CostExplorer.Types
import Network.AWS.CostExplorer.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getTags' smart constructor.
data GetTags = GetTags'
  { _gtNextPageToken :: !(Maybe Text)
  , _gtSearchString  :: !(Maybe Text)
  , _gtTagKey        :: !(Maybe Text)
  , _gtTimePeriod    :: !DateInterval
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtNextPageToken' - The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- * 'gtSearchString' - The value that you want to search for.
--
-- * 'gtTagKey' - The key of the tag that you want to return values for.
--
-- * 'gtTimePeriod' - The start and end dates for retrieving the dimension values. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
getTags
    :: DateInterval -- ^ 'gtTimePeriod'
    -> GetTags
getTags pTimePeriod_ =
  GetTags'
    { _gtNextPageToken = Nothing
    , _gtSearchString = Nothing
    , _gtTagKey = Nothing
    , _gtTimePeriod = pTimePeriod_
    }


-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
gtNextPageToken :: Lens' GetTags (Maybe Text)
gtNextPageToken = lens _gtNextPageToken (\ s a -> s{_gtNextPageToken = a})

-- | The value that you want to search for.
gtSearchString :: Lens' GetTags (Maybe Text)
gtSearchString = lens _gtSearchString (\ s a -> s{_gtSearchString = a})

-- | The key of the tag that you want to return values for.
gtTagKey :: Lens' GetTags (Maybe Text)
gtTagKey = lens _gtTagKey (\ s a -> s{_gtTagKey = a})

-- | The start and end dates for retrieving the dimension values. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
gtTimePeriod :: Lens' GetTags DateInterval
gtTimePeriod = lens _gtTimePeriod (\ s a -> s{_gtTimePeriod = a})

instance AWSRequest GetTags where
        type Rs GetTags = GetTagsResponse
        request = postJSON costExplorer
        response
          = receiveJSON
              (\ s h x ->
                 GetTagsResponse' <$>
                   (x .?> "NextPageToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "Tags" .!@ mempty)
                     <*> (x .:> "ReturnSize")
                     <*> (x .:> "TotalSize"))

instance Hashable GetTags where

instance NFData GetTags where

instance ToHeaders GetTags where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSInsightsIndexService.GetTags" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetTags where
        toJSON GetTags'{..}
          = object
              (catMaybes
                 [("NextPageToken" .=) <$> _gtNextPageToken,
                  ("SearchString" .=) <$> _gtSearchString,
                  ("TagKey" .=) <$> _gtTagKey,
                  Just ("TimePeriod" .= _gtTimePeriod)])

instance ToPath GetTags where
        toPath = const "/"

instance ToQuery GetTags where
        toQuery = const mempty

-- | /See:/ 'getTagsResponse' smart constructor.
data GetTagsResponse = GetTagsResponse'
  { _gtrsNextPageToken  :: !(Maybe Text)
  , _gtrsResponseStatus :: !Int
  , _gtrsTags           :: ![Text]
  , _gtrsReturnSize     :: !Int
  , _gtrsTotalSize      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtrsNextPageToken' - The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- * 'gtrsResponseStatus' - -- | The response status code.
--
-- * 'gtrsTags' - The tags that match your request.
--
-- * 'gtrsReturnSize' - The number of query results that AWS returns at a time.
--
-- * 'gtrsTotalSize' - The total number of query results.
getTagsResponse
    :: Int -- ^ 'gtrsResponseStatus'
    -> Int -- ^ 'gtrsReturnSize'
    -> Int -- ^ 'gtrsTotalSize'
    -> GetTagsResponse
getTagsResponse pResponseStatus_ pReturnSize_ pTotalSize_ =
  GetTagsResponse'
    { _gtrsNextPageToken = Nothing
    , _gtrsResponseStatus = pResponseStatus_
    , _gtrsTags = mempty
    , _gtrsReturnSize = pReturnSize_
    , _gtrsTotalSize = pTotalSize_
    }


-- | The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
gtrsNextPageToken :: Lens' GetTagsResponse (Maybe Text)
gtrsNextPageToken = lens _gtrsNextPageToken (\ s a -> s{_gtrsNextPageToken = a})

-- | -- | The response status code.
gtrsResponseStatus :: Lens' GetTagsResponse Int
gtrsResponseStatus = lens _gtrsResponseStatus (\ s a -> s{_gtrsResponseStatus = a})

-- | The tags that match your request.
gtrsTags :: Lens' GetTagsResponse [Text]
gtrsTags = lens _gtrsTags (\ s a -> s{_gtrsTags = a}) . _Coerce

-- | The number of query results that AWS returns at a time.
gtrsReturnSize :: Lens' GetTagsResponse Int
gtrsReturnSize = lens _gtrsReturnSize (\ s a -> s{_gtrsReturnSize = a})

-- | The total number of query results.
gtrsTotalSize :: Lens' GetTagsResponse Int
gtrsTotalSize = lens _gtrsTotalSize (\ s a -> s{_gtrsTotalSize = a})

instance NFData GetTagsResponse where
