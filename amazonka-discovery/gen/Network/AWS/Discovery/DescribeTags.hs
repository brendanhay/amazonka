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
-- Module      : Network.AWS.Discovery.DescribeTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of configuration items that are tagged with a specific tag. Or retrieves a list of all tags assigned to a specific configuration item.
--
--
module Network.AWS.Discovery.DescribeTags
    (
    -- * Creating a Request
      describeTags
    , DescribeTags
    -- * Request Lenses
    , dtFilters
    , dtNextToken
    , dtMaxResults

    -- * Destructuring the Response
    , describeTagsResponse
    , DescribeTagsResponse
    -- * Response Lenses
    , dtrsNextToken
    , dtrsTags
    , dtrsResponseStatus
    ) where

import Network.AWS.Discovery.Types
import Network.AWS.Discovery.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeTags' smart constructor.
data DescribeTags = DescribeTags'
  { _dtFilters    :: !(Maybe [TagFilter])
  , _dtNextToken  :: !(Maybe Text)
  , _dtMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtFilters' - You can filter the list using a /key/ -/value/ format. You can separate these items by using logical operators. Allowed filters include @tagKey@ , @tagValue@ , and @configurationId@ .
--
-- * 'dtNextToken' - A token to start the list. Use this token to get the next set of results.
--
-- * 'dtMaxResults' - The total number of items to return in a single page of output. The maximum value is 100.
describeTags
    :: DescribeTags
describeTags =
  DescribeTags'
    {_dtFilters = Nothing, _dtNextToken = Nothing, _dtMaxResults = Nothing}


-- | You can filter the list using a /key/ -/value/ format. You can separate these items by using logical operators. Allowed filters include @tagKey@ , @tagValue@ , and @configurationId@ .
dtFilters :: Lens' DescribeTags [TagFilter]
dtFilters = lens _dtFilters (\ s a -> s{_dtFilters = a}) . _Default . _Coerce

-- | A token to start the list. Use this token to get the next set of results.
dtNextToken :: Lens' DescribeTags (Maybe Text)
dtNextToken = lens _dtNextToken (\ s a -> s{_dtNextToken = a})

-- | The total number of items to return in a single page of output. The maximum value is 100.
dtMaxResults :: Lens' DescribeTags (Maybe Int)
dtMaxResults = lens _dtMaxResults (\ s a -> s{_dtMaxResults = a})

instance AWSRequest DescribeTags where
        type Rs DescribeTags = DescribeTagsResponse
        request = postJSON discovery
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTagsResponse' <$>
                   (x .?> "nextToken") <*> (x .?> "tags" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeTags where

instance NFData DescribeTags where

instance ToHeaders DescribeTags where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSPoseidonService_V2015_11_01.DescribeTags" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeTags where
        toJSON DescribeTags'{..}
          = object
              (catMaybes
                 [("filters" .=) <$> _dtFilters,
                  ("nextToken" .=) <$> _dtNextToken,
                  ("maxResults" .=) <$> _dtMaxResults])

instance ToPath DescribeTags where
        toPath = const "/"

instance ToQuery DescribeTags where
        toQuery = const mempty

-- | /See:/ 'describeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
  { _dtrsNextToken      :: !(Maybe Text)
  , _dtrsTags           :: !(Maybe [ConfigurationTag])
  , _dtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtrsNextToken' - The call returns a token. Use this token to get the next set of results.
--
-- * 'dtrsTags' - Depending on the input, this is a list of configuration items tagged with a specific tag, or a list of tags for a specific configuration item.
--
-- * 'dtrsResponseStatus' - -- | The response status code.
describeTagsResponse
    :: Int -- ^ 'dtrsResponseStatus'
    -> DescribeTagsResponse
describeTagsResponse pResponseStatus_ =
  DescribeTagsResponse'
    { _dtrsNextToken = Nothing
    , _dtrsTags = Nothing
    , _dtrsResponseStatus = pResponseStatus_
    }


-- | The call returns a token. Use this token to get the next set of results.
dtrsNextToken :: Lens' DescribeTagsResponse (Maybe Text)
dtrsNextToken = lens _dtrsNextToken (\ s a -> s{_dtrsNextToken = a})

-- | Depending on the input, this is a list of configuration items tagged with a specific tag, or a list of tags for a specific configuration item.
dtrsTags :: Lens' DescribeTagsResponse [ConfigurationTag]
dtrsTags = lens _dtrsTags (\ s a -> s{_dtrsTags = a}) . _Default . _Coerce

-- | -- | The response status code.
dtrsResponseStatus :: Lens' DescribeTagsResponse Int
dtrsResponseStatus = lens _dtrsResponseStatus (\ s a -> s{_dtrsResponseStatus = a})

instance NFData DescribeTagsResponse where
