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
-- Module      : Network.AWS.AppStream.DescribeImageBuilders
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified image builders or all image builders in the account.
--
--
module Network.AWS.AppStream.DescribeImageBuilders
    (
    -- * Creating a Request
      describeImageBuilders
    , DescribeImageBuilders
    -- * Request Lenses
    , dibNextToken
    , dibNames
    , dibMaxResults

    -- * Destructuring the Response
    , describeImageBuildersResponse
    , DescribeImageBuildersResponse
    -- * Response Lenses
    , dibsrsImageBuilders
    , dibsrsNextToken
    , dibsrsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeImageBuilders' smart constructor.
data DescribeImageBuilders = DescribeImageBuilders'
  { _dibNextToken  :: !(Maybe Text)
  , _dibNames      :: !(Maybe [Text])
  , _dibMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeImageBuilders' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dibNextToken' - The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- * 'dibNames' - The names of the image builders to describe.
--
-- * 'dibMaxResults' - The maximum size of each page of results.
describeImageBuilders
    :: DescribeImageBuilders
describeImageBuilders =
  DescribeImageBuilders'
    {_dibNextToken = Nothing, _dibNames = Nothing, _dibMaxResults = Nothing}


-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
dibNextToken :: Lens' DescribeImageBuilders (Maybe Text)
dibNextToken = lens _dibNextToken (\ s a -> s{_dibNextToken = a})

-- | The names of the image builders to describe.
dibNames :: Lens' DescribeImageBuilders [Text]
dibNames = lens _dibNames (\ s a -> s{_dibNames = a}) . _Default . _Coerce

-- | The maximum size of each page of results.
dibMaxResults :: Lens' DescribeImageBuilders (Maybe Int)
dibMaxResults = lens _dibMaxResults (\ s a -> s{_dibMaxResults = a})

instance AWSRequest DescribeImageBuilders where
        type Rs DescribeImageBuilders =
             DescribeImageBuildersResponse
        request = postJSON appStream
        response
          = receiveJSON
              (\ s h x ->
                 DescribeImageBuildersResponse' <$>
                   (x .?> "ImageBuilders" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeImageBuilders where

instance NFData DescribeImageBuilders where

instance ToHeaders DescribeImageBuilders where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.DescribeImageBuilders" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeImageBuilders where
        toJSON DescribeImageBuilders'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _dibNextToken,
                  ("Names" .=) <$> _dibNames,
                  ("MaxResults" .=) <$> _dibMaxResults])

instance ToPath DescribeImageBuilders where
        toPath = const "/"

instance ToQuery DescribeImageBuilders where
        toQuery = const mempty

-- | /See:/ 'describeImageBuildersResponse' smart constructor.
data DescribeImageBuildersResponse = DescribeImageBuildersResponse'
  { _dibsrsImageBuilders  :: !(Maybe [ImageBuilder])
  , _dibsrsNextToken      :: !(Maybe Text)
  , _dibsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeImageBuildersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dibsrsImageBuilders' - Information about the image builders.
--
-- * 'dibsrsNextToken' - The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- * 'dibsrsResponseStatus' - -- | The response status code.
describeImageBuildersResponse
    :: Int -- ^ 'dibsrsResponseStatus'
    -> DescribeImageBuildersResponse
describeImageBuildersResponse pResponseStatus_ =
  DescribeImageBuildersResponse'
    { _dibsrsImageBuilders = Nothing
    , _dibsrsNextToken = Nothing
    , _dibsrsResponseStatus = pResponseStatus_
    }


-- | Information about the image builders.
dibsrsImageBuilders :: Lens' DescribeImageBuildersResponse [ImageBuilder]
dibsrsImageBuilders = lens _dibsrsImageBuilders (\ s a -> s{_dibsrsImageBuilders = a}) . _Default . _Coerce

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
dibsrsNextToken :: Lens' DescribeImageBuildersResponse (Maybe Text)
dibsrsNextToken = lens _dibsrsNextToken (\ s a -> s{_dibsrsNextToken = a})

-- | -- | The response status code.
dibsrsResponseStatus :: Lens' DescribeImageBuildersResponse Int
dibsrsResponseStatus = lens _dibsrsResponseStatus (\ s a -> s{_dibsrsResponseStatus = a})

instance NFData DescribeImageBuildersResponse where
