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
-- Module      : Network.AWS.ResourceGroupsTagging.GetTagKeys
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all tag keys in the specified region for the AWS account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ResourceGroupsTagging.GetTagKeys
    (
    -- * Creating a Request
      getTagKeys
    , GetTagKeys
    -- * Request Lenses
    , gtkPaginationToken

    -- * Destructuring the Response
    , getTagKeysResponse
    , GetTagKeysResponse
    -- * Response Lenses
    , gtkrsPaginationToken
    , gtkrsTagKeys
    , gtkrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ResourceGroupsTagging.Types
import Network.AWS.ResourceGroupsTagging.Types.Product
import Network.AWS.Response

-- | /See:/ 'getTagKeys' smart constructor.
newtype GetTagKeys = GetTagKeys'
  { _gtkPaginationToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTagKeys' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtkPaginationToken' - A string that indicates that additional data is available. Leave this value empty for your initial request. If the response includes a PaginationToken, use that string for this value to request an additional page of data.
getTagKeys
    :: GetTagKeys
getTagKeys = GetTagKeys' {_gtkPaginationToken = Nothing}


-- | A string that indicates that additional data is available. Leave this value empty for your initial request. If the response includes a PaginationToken, use that string for this value to request an additional page of data.
gtkPaginationToken :: Lens' GetTagKeys (Maybe Text)
gtkPaginationToken = lens _gtkPaginationToken (\ s a -> s{_gtkPaginationToken = a})

instance AWSPager GetTagKeys where
        page rq rs
          | stop (rs ^. gtkrsPaginationToken) = Nothing
          | stop (rs ^. gtkrsTagKeys) = Nothing
          | otherwise =
            Just $ rq &
              gtkPaginationToken .~ rs ^. gtkrsPaginationToken

instance AWSRequest GetTagKeys where
        type Rs GetTagKeys = GetTagKeysResponse
        request = postJSON resourceGroupsTagging
        response
          = receiveJSON
              (\ s h x ->
                 GetTagKeysResponse' <$>
                   (x .?> "PaginationToken") <*>
                     (x .?> "TagKeys" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetTagKeys where

instance NFData GetTagKeys where

instance ToHeaders GetTagKeys where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ResourceGroupsTaggingAPI_20170126.GetTagKeys" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetTagKeys where
        toJSON GetTagKeys'{..}
          = object
              (catMaybes
                 [("PaginationToken" .=) <$> _gtkPaginationToken])

instance ToPath GetTagKeys where
        toPath = const "/"

instance ToQuery GetTagKeys where
        toQuery = const mempty

-- | /See:/ 'getTagKeysResponse' smart constructor.
data GetTagKeysResponse = GetTagKeysResponse'
  { _gtkrsPaginationToken :: !(Maybe Text)
  , _gtkrsTagKeys         :: !(Maybe [Text])
  , _gtkrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTagKeysResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtkrsPaginationToken' - A string that indicates that the response contains more data than can be returned in a single response. To receive additional data, specify this string for the @PaginationToken@ value in a subsequent request.
--
-- * 'gtkrsTagKeys' - A list of all tag keys in the AWS account.
--
-- * 'gtkrsResponseStatus' - -- | The response status code.
getTagKeysResponse
    :: Int -- ^ 'gtkrsResponseStatus'
    -> GetTagKeysResponse
getTagKeysResponse pResponseStatus_ =
  GetTagKeysResponse'
    { _gtkrsPaginationToken = Nothing
    , _gtkrsTagKeys = Nothing
    , _gtkrsResponseStatus = pResponseStatus_
    }


-- | A string that indicates that the response contains more data than can be returned in a single response. To receive additional data, specify this string for the @PaginationToken@ value in a subsequent request.
gtkrsPaginationToken :: Lens' GetTagKeysResponse (Maybe Text)
gtkrsPaginationToken = lens _gtkrsPaginationToken (\ s a -> s{_gtkrsPaginationToken = a})

-- | A list of all tag keys in the AWS account.
gtkrsTagKeys :: Lens' GetTagKeysResponse [Text]
gtkrsTagKeys = lens _gtkrsTagKeys (\ s a -> s{_gtkrsTagKeys = a}) . _Default . _Coerce

-- | -- | The response status code.
gtkrsResponseStatus :: Lens' GetTagKeysResponse Int
gtkrsResponseStatus = lens _gtkrsResponseStatus (\ s a -> s{_gtkrsResponseStatus = a})

instance NFData GetTagKeysResponse where
