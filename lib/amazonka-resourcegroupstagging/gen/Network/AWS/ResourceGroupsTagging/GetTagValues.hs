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
-- Module      : Network.AWS.ResourceGroupsTagging.GetTagValues
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all tag values for the specified key in the specified region for the AWS account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ResourceGroupsTagging.GetTagValues
    (
    -- * Creating a Request
      getTagValues
    , GetTagValues
    -- * Request Lenses
    , gtvPaginationToken
    , gtvKey

    -- * Destructuring the Response
    , getTagValuesResponse
    , GetTagValuesResponse
    -- * Response Lenses
    , gtvrsPaginationToken
    , gtvrsTagValues
    , gtvrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ResourceGroupsTagging.Types
import Network.AWS.ResourceGroupsTagging.Types.Product
import Network.AWS.Response

-- | /See:/ 'getTagValues' smart constructor.
data GetTagValues = GetTagValues'
  { _gtvPaginationToken :: !(Maybe Text)
  , _gtvKey             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTagValues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtvPaginationToken' - A string that indicates that additional data is available. Leave this value empty for your initial request. If the response includes a PaginationToken, use that string for this value to request an additional page of data.
--
-- * 'gtvKey' - The key for which you want to list all existing values in the specified region for the AWS account.
getTagValues
    :: Text -- ^ 'gtvKey'
    -> GetTagValues
getTagValues pKey_ =
  GetTagValues' {_gtvPaginationToken = Nothing, _gtvKey = pKey_}


-- | A string that indicates that additional data is available. Leave this value empty for your initial request. If the response includes a PaginationToken, use that string for this value to request an additional page of data.
gtvPaginationToken :: Lens' GetTagValues (Maybe Text)
gtvPaginationToken = lens _gtvPaginationToken (\ s a -> s{_gtvPaginationToken = a})

-- | The key for which you want to list all existing values in the specified region for the AWS account.
gtvKey :: Lens' GetTagValues Text
gtvKey = lens _gtvKey (\ s a -> s{_gtvKey = a})

instance AWSPager GetTagValues where
        page rq rs
          | stop (rs ^. gtvrsPaginationToken) = Nothing
          | stop (rs ^. gtvrsTagValues) = Nothing
          | otherwise =
            Just $ rq &
              gtvPaginationToken .~ rs ^. gtvrsPaginationToken

instance AWSRequest GetTagValues where
        type Rs GetTagValues = GetTagValuesResponse
        request = postJSON resourceGroupsTagging
        response
          = receiveJSON
              (\ s h x ->
                 GetTagValuesResponse' <$>
                   (x .?> "PaginationToken") <*>
                     (x .?> "TagValues" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetTagValues where

instance NFData GetTagValues where

instance ToHeaders GetTagValues where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ResourceGroupsTaggingAPI_20170126.GetTagValues" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetTagValues where
        toJSON GetTagValues'{..}
          = object
              (catMaybes
                 [("PaginationToken" .=) <$> _gtvPaginationToken,
                  Just ("Key" .= _gtvKey)])

instance ToPath GetTagValues where
        toPath = const "/"

instance ToQuery GetTagValues where
        toQuery = const mempty

-- | /See:/ 'getTagValuesResponse' smart constructor.
data GetTagValuesResponse = GetTagValuesResponse'
  { _gtvrsPaginationToken :: !(Maybe Text)
  , _gtvrsTagValues       :: !(Maybe [Text])
  , _gtvrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTagValuesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtvrsPaginationToken' - A string that indicates that the response contains more data than can be returned in a single response. To receive additional data, specify this string for the @PaginationToken@ value in a subsequent request.
--
-- * 'gtvrsTagValues' - A list of all tag values for the specified key in the AWS account.
--
-- * 'gtvrsResponseStatus' - -- | The response status code.
getTagValuesResponse
    :: Int -- ^ 'gtvrsResponseStatus'
    -> GetTagValuesResponse
getTagValuesResponse pResponseStatus_ =
  GetTagValuesResponse'
    { _gtvrsPaginationToken = Nothing
    , _gtvrsTagValues = Nothing
    , _gtvrsResponseStatus = pResponseStatus_
    }


-- | A string that indicates that the response contains more data than can be returned in a single response. To receive additional data, specify this string for the @PaginationToken@ value in a subsequent request.
gtvrsPaginationToken :: Lens' GetTagValuesResponse (Maybe Text)
gtvrsPaginationToken = lens _gtvrsPaginationToken (\ s a -> s{_gtvrsPaginationToken = a})

-- | A list of all tag values for the specified key in the AWS account.
gtvrsTagValues :: Lens' GetTagValuesResponse [Text]
gtvrsTagValues = lens _gtvrsTagValues (\ s a -> s{_gtvrsTagValues = a}) . _Default . _Coerce

-- | -- | The response status code.
gtvrsResponseStatus :: Lens' GetTagValuesResponse Int
gtvrsResponseStatus = lens _gtvrsResponseStatus (\ s a -> s{_gtvrsResponseStatus = a})

instance NFData GetTagValuesResponse where
