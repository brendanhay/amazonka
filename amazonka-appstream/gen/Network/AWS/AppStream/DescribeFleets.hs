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
-- Module      : Network.AWS.AppStream.DescribeFleets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified fleets or all fleets in the account.
--
--
module Network.AWS.AppStream.DescribeFleets
    (
    -- * Creating a Request
      describeFleets
    , DescribeFleets
    -- * Request Lenses
    , dfNextToken
    , dfNames

    -- * Destructuring the Response
    , describeFleetsResponse
    , DescribeFleetsResponse
    -- * Response Lenses
    , dfsrsNextToken
    , dfsrsFleets
    , dfsrsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeFleets' smart constructor.
data DescribeFleets = DescribeFleets'
  { _dfNextToken :: !(Maybe Text)
  , _dfNames     :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeFleets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfNextToken' - The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- * 'dfNames' - The names of the fleets to describe.
describeFleets
    :: DescribeFleets
describeFleets = DescribeFleets' {_dfNextToken = Nothing, _dfNames = Nothing}


-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
dfNextToken :: Lens' DescribeFleets (Maybe Text)
dfNextToken = lens _dfNextToken (\ s a -> s{_dfNextToken = a})

-- | The names of the fleets to describe.
dfNames :: Lens' DescribeFleets [Text]
dfNames = lens _dfNames (\ s a -> s{_dfNames = a}) . _Default . _Coerce

instance AWSRequest DescribeFleets where
        type Rs DescribeFleets = DescribeFleetsResponse
        request = postJSON appStream
        response
          = receiveJSON
              (\ s h x ->
                 DescribeFleetsResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Fleets" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeFleets where

instance NFData DescribeFleets where

instance ToHeaders DescribeFleets where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.DescribeFleets" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeFleets where
        toJSON DescribeFleets'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _dfNextToken,
                  ("Names" .=) <$> _dfNames])

instance ToPath DescribeFleets where
        toPath = const "/"

instance ToQuery DescribeFleets where
        toQuery = const mempty

-- | /See:/ 'describeFleetsResponse' smart constructor.
data DescribeFleetsResponse = DescribeFleetsResponse'
  { _dfsrsNextToken      :: !(Maybe Text)
  , _dfsrsFleets         :: !(Maybe [Fleet])
  , _dfsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeFleetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfsrsNextToken' - The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- * 'dfsrsFleets' - Information about the fleets.
--
-- * 'dfsrsResponseStatus' - -- | The response status code.
describeFleetsResponse
    :: Int -- ^ 'dfsrsResponseStatus'
    -> DescribeFleetsResponse
describeFleetsResponse pResponseStatus_ =
  DescribeFleetsResponse'
    { _dfsrsNextToken = Nothing
    , _dfsrsFleets = Nothing
    , _dfsrsResponseStatus = pResponseStatus_
    }


-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
dfsrsNextToken :: Lens' DescribeFleetsResponse (Maybe Text)
dfsrsNextToken = lens _dfsrsNextToken (\ s a -> s{_dfsrsNextToken = a})

-- | Information about the fleets.
dfsrsFleets :: Lens' DescribeFleetsResponse [Fleet]
dfsrsFleets = lens _dfsrsFleets (\ s a -> s{_dfsrsFleets = a}) . _Default . _Coerce

-- | -- | The response status code.
dfsrsResponseStatus :: Lens' DescribeFleetsResponse Int
dfsrsResponseStatus = lens _dfsrsResponseStatus (\ s a -> s{_dfsrsResponseStatus = a})

instance NFData DescribeFleetsResponse where
