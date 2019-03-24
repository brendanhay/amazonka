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
-- Module      : Network.AWS.Discovery.DescribeContinuousExports
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists exports as specified by ID. All continuous exports associated with your user account can be listed if you call @DescribeContinuousExports@ as is without passing any parameters.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Discovery.DescribeContinuousExports
    (
    -- * Creating a Request
      describeContinuousExports
    , DescribeContinuousExports
    -- * Request Lenses
    , dceNextToken
    , dceExportIds
    , dceMaxResults

    -- * Destructuring the Response
    , describeContinuousExportsResponse
    , DescribeContinuousExportsResponse
    -- * Response Lenses
    , dcersNextToken
    , dcersDescriptions
    , dcersResponseStatus
    ) where

import Network.AWS.Discovery.Types
import Network.AWS.Discovery.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeContinuousExports' smart constructor.
data DescribeContinuousExports = DescribeContinuousExports'
  { _dceNextToken  :: !(Maybe Text)
  , _dceExportIds  :: !(Maybe [Text])
  , _dceMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeContinuousExports' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dceNextToken' - The token from the previous call to @DescribeExportTasks@ .
--
-- * 'dceExportIds' - The unique IDs assigned to the exports.
--
-- * 'dceMaxResults' - A number between 1 and 100 specifying the maximum number of continuous export descriptions returned.
describeContinuousExports
    :: DescribeContinuousExports
describeContinuousExports =
  DescribeContinuousExports'
    {_dceNextToken = Nothing, _dceExportIds = Nothing, _dceMaxResults = Nothing}


-- | The token from the previous call to @DescribeExportTasks@ .
dceNextToken :: Lens' DescribeContinuousExports (Maybe Text)
dceNextToken = lens _dceNextToken (\ s a -> s{_dceNextToken = a})

-- | The unique IDs assigned to the exports.
dceExportIds :: Lens' DescribeContinuousExports [Text]
dceExportIds = lens _dceExportIds (\ s a -> s{_dceExportIds = a}) . _Default . _Coerce

-- | A number between 1 and 100 specifying the maximum number of continuous export descriptions returned.
dceMaxResults :: Lens' DescribeContinuousExports (Maybe Natural)
dceMaxResults = lens _dceMaxResults (\ s a -> s{_dceMaxResults = a}) . mapping _Nat

instance AWSPager DescribeContinuousExports where
        page rq rs
          | stop (rs ^. dcersNextToken) = Nothing
          | stop (rs ^. dcersDescriptions) = Nothing
          | otherwise =
            Just $ rq & dceNextToken .~ rs ^. dcersNextToken

instance AWSRequest DescribeContinuousExports where
        type Rs DescribeContinuousExports =
             DescribeContinuousExportsResponse
        request = postJSON discovery
        response
          = receiveJSON
              (\ s h x ->
                 DescribeContinuousExportsResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "descriptions" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeContinuousExports where

instance NFData DescribeContinuousExports where

instance ToHeaders DescribeContinuousExports where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSPoseidonService_V2015_11_01.DescribeContinuousExports"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeContinuousExports where
        toJSON DescribeContinuousExports'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _dceNextToken,
                  ("exportIds" .=) <$> _dceExportIds,
                  ("maxResults" .=) <$> _dceMaxResults])

instance ToPath DescribeContinuousExports where
        toPath = const "/"

instance ToQuery DescribeContinuousExports where
        toQuery = const mempty

-- | /See:/ 'describeContinuousExportsResponse' smart constructor.
data DescribeContinuousExportsResponse = DescribeContinuousExportsResponse'
  { _dcersNextToken      :: !(Maybe Text)
  , _dcersDescriptions   :: !(Maybe [ContinuousExportDescription])
  , _dcersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeContinuousExportsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcersNextToken' - The token from the previous call to @DescribeExportTasks@ .
--
-- * 'dcersDescriptions' - A list of continuous export descriptions.
--
-- * 'dcersResponseStatus' - -- | The response status code.
describeContinuousExportsResponse
    :: Int -- ^ 'dcersResponseStatus'
    -> DescribeContinuousExportsResponse
describeContinuousExportsResponse pResponseStatus_ =
  DescribeContinuousExportsResponse'
    { _dcersNextToken = Nothing
    , _dcersDescriptions = Nothing
    , _dcersResponseStatus = pResponseStatus_
    }


-- | The token from the previous call to @DescribeExportTasks@ .
dcersNextToken :: Lens' DescribeContinuousExportsResponse (Maybe Text)
dcersNextToken = lens _dcersNextToken (\ s a -> s{_dcersNextToken = a})

-- | A list of continuous export descriptions.
dcersDescriptions :: Lens' DescribeContinuousExportsResponse [ContinuousExportDescription]
dcersDescriptions = lens _dcersDescriptions (\ s a -> s{_dcersDescriptions = a}) . _Default . _Coerce

-- | -- | The response status code.
dcersResponseStatus :: Lens' DescribeContinuousExportsResponse Int
dcersResponseStatus = lens _dcersResponseStatus (\ s a -> s{_dcersResponseStatus = a})

instance NFData DescribeContinuousExportsResponse
         where
