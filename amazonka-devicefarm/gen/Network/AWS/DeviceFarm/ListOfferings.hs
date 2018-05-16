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
-- Module      : Network.AWS.DeviceFarm.ListOfferings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of products or offerings that the user can manage through the API. Each offering record indicates the recurring price per unit and the frequency for that offering. The API returns a @NotEligible@ error if the user is not permitted to invoke the operation. Please contact <mailto:aws-devicefarm-support@amazon.com aws-devicefarm-support@amazon.com> if you believe that you should be able to invoke this operation.
--
--
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListOfferings
    (
    -- * Creating a Request
      listOfferings
    , ListOfferings
    -- * Request Lenses
    , loNextToken

    -- * Destructuring the Response
    , listOfferingsResponse
    , ListOfferingsResponse
    -- * Response Lenses
    , lorsNextToken
    , lorsOfferings
    , lorsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to list all offerings.
--
--
--
-- /See:/ 'listOfferings' smart constructor.
newtype ListOfferings = ListOfferings'
  { _loNextToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListOfferings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'loNextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
listOfferings
    :: ListOfferings
listOfferings = ListOfferings' {_loNextToken = Nothing}


-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
loNextToken :: Lens' ListOfferings (Maybe Text)
loNextToken = lens _loNextToken (\ s a -> s{_loNextToken = a})

instance AWSPager ListOfferings where
        page rq rs
          | stop (rs ^. lorsNextToken) = Nothing
          | stop (rs ^. lorsOfferings) = Nothing
          | otherwise =
            Just $ rq & loNextToken .~ rs ^. lorsNextToken

instance AWSRequest ListOfferings where
        type Rs ListOfferings = ListOfferingsResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 ListOfferingsResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "offerings" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListOfferings where

instance NFData ListOfferings where

instance ToHeaders ListOfferings where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.ListOfferings" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListOfferings where
        toJSON ListOfferings'{..}
          = object
              (catMaybes [("nextToken" .=) <$> _loNextToken])

instance ToPath ListOfferings where
        toPath = const "/"

instance ToQuery ListOfferings where
        toQuery = const mempty

-- | Represents the return values of the list of offerings.
--
--
--
-- /See:/ 'listOfferingsResponse' smart constructor.
data ListOfferingsResponse = ListOfferingsResponse'
  { _lorsNextToken      :: !(Maybe Text)
  , _lorsOfferings      :: !(Maybe [Offering])
  , _lorsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListOfferingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lorsNextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- * 'lorsOfferings' - A value representing the list offering results.
--
-- * 'lorsResponseStatus' - -- | The response status code.
listOfferingsResponse
    :: Int -- ^ 'lorsResponseStatus'
    -> ListOfferingsResponse
listOfferingsResponse pResponseStatus_ =
  ListOfferingsResponse'
    { _lorsNextToken = Nothing
    , _lorsOfferings = Nothing
    , _lorsResponseStatus = pResponseStatus_
    }


-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
lorsNextToken :: Lens' ListOfferingsResponse (Maybe Text)
lorsNextToken = lens _lorsNextToken (\ s a -> s{_lorsNextToken = a})

-- | A value representing the list offering results.
lorsOfferings :: Lens' ListOfferingsResponse [Offering]
lorsOfferings = lens _lorsOfferings (\ s a -> s{_lorsOfferings = a}) . _Default . _Coerce

-- | -- | The response status code.
lorsResponseStatus :: Lens' ListOfferingsResponse Int
lorsResponseStatus = lens _lorsResponseStatus (\ s a -> s{_lorsResponseStatus = a})

instance NFData ListOfferingsResponse where
