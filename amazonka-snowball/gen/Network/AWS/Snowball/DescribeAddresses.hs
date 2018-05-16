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
-- Module      : Network.AWS.Snowball.DescribeAddresses
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a specified number of @ADDRESS@ objects. Calling this API in one of the US regions will return addresses from the list of all addresses associated with this account in all US regions.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Snowball.DescribeAddresses
    (
    -- * Creating a Request
      describeAddresses
    , DescribeAddresses
    -- * Request Lenses
    , daNextToken
    , daMaxResults

    -- * Destructuring the Response
    , describeAddressesResponse
    , DescribeAddressesResponse
    -- * Response Lenses
    , drsAddresses
    , drsNextToken
    , drsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Snowball.Types
import Network.AWS.Snowball.Types.Product

-- | /See:/ 'describeAddresses' smart constructor.
data DescribeAddresses = DescribeAddresses'
  { _daNextToken  :: !(Maybe Text)
  , _daMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAddresses' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daNextToken' - HTTP requests are stateless. To identify what object comes "next" in the list of @ADDRESS@ objects, you have the option of specifying a value for @NextToken@ as the starting point for your list of returned addresses.
--
-- * 'daMaxResults' - The number of @ADDRESS@ objects to return.
describeAddresses
    :: DescribeAddresses
describeAddresses =
  DescribeAddresses' {_daNextToken = Nothing, _daMaxResults = Nothing}


-- | HTTP requests are stateless. To identify what object comes "next" in the list of @ADDRESS@ objects, you have the option of specifying a value for @NextToken@ as the starting point for your list of returned addresses.
daNextToken :: Lens' DescribeAddresses (Maybe Text)
daNextToken = lens _daNextToken (\ s a -> s{_daNextToken = a})

-- | The number of @ADDRESS@ objects to return.
daMaxResults :: Lens' DescribeAddresses (Maybe Natural)
daMaxResults = lens _daMaxResults (\ s a -> s{_daMaxResults = a}) . mapping _Nat

instance AWSPager DescribeAddresses where
        page rq rs
          | stop (rs ^. drsNextToken) = Nothing
          | stop (rs ^. drsAddresses) = Nothing
          | otherwise =
            Just $ rq & daNextToken .~ rs ^. drsNextToken

instance AWSRequest DescribeAddresses where
        type Rs DescribeAddresses = DescribeAddressesResponse
        request = postJSON snowball
        response
          = receiveJSON
              (\ s h x ->
                 DescribeAddressesResponse' <$>
                   (x .?> "Addresses" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeAddresses where

instance NFData DescribeAddresses where

instance ToHeaders DescribeAddresses where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSIESnowballJobManagementService.DescribeAddresses"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeAddresses where
        toJSON DescribeAddresses'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _daNextToken,
                  ("MaxResults" .=) <$> _daMaxResults])

instance ToPath DescribeAddresses where
        toPath = const "/"

instance ToQuery DescribeAddresses where
        toQuery = const mempty

-- | /See:/ 'describeAddressesResponse' smart constructor.
data DescribeAddressesResponse = DescribeAddressesResponse'
  { _drsAddresses      :: !(Maybe [Address])
  , _drsNextToken      :: !(Maybe Text)
  , _drsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAddressesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsAddresses' - The Snowball shipping addresses that were created for this account.
--
-- * 'drsNextToken' - HTTP requests are stateless. If you use the automatically generated @NextToken@ value in your next @DescribeAddresses@ call, your list of returned addresses will start from this point in the array.
--
-- * 'drsResponseStatus' - -- | The response status code.
describeAddressesResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeAddressesResponse
describeAddressesResponse pResponseStatus_ =
  DescribeAddressesResponse'
    { _drsAddresses = Nothing
    , _drsNextToken = Nothing
    , _drsResponseStatus = pResponseStatus_
    }


-- | The Snowball shipping addresses that were created for this account.
drsAddresses :: Lens' DescribeAddressesResponse [Address]
drsAddresses = lens _drsAddresses (\ s a -> s{_drsAddresses = a}) . _Default . _Coerce

-- | HTTP requests are stateless. If you use the automatically generated @NextToken@ value in your next @DescribeAddresses@ call, your list of returned addresses will start from this point in the array.
drsNextToken :: Lens' DescribeAddressesResponse (Maybe Text)
drsNextToken = lens _drsNextToken (\ s a -> s{_drsNextToken = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeAddressesResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DescribeAddressesResponse where
