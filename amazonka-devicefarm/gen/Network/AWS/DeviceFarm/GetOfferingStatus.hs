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
-- Module      : Network.AWS.DeviceFarm.GetOfferingStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the current status and future status of all offerings purchased by an AWS account. The response indicates how many offerings are currently available and the offerings that will be available in the next period. The API returns a @NotEligible@ error if the user is not permitted to invoke the operation. Please contact <mailto:aws-devicefarm-support@amazon.com aws-devicefarm-support@amazon.com> if you believe that you should be able to invoke this operation.
--
--
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.GetOfferingStatus
    (
    -- * Creating a Request
      getOfferingStatus
    , GetOfferingStatus
    -- * Request Lenses
    , gosNextToken

    -- * Destructuring the Response
    , getOfferingStatusResponse
    , GetOfferingStatusResponse
    -- * Response Lenses
    , gosrsNextPeriod
    , gosrsCurrent
    , gosrsNextToken
    , gosrsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to retrieve the offering status for the specified customer or account.
--
--
--
-- /See:/ 'getOfferingStatus' smart constructor.
newtype GetOfferingStatus = GetOfferingStatus'
  { _gosNextToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetOfferingStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gosNextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
getOfferingStatus
    :: GetOfferingStatus
getOfferingStatus = GetOfferingStatus' {_gosNextToken = Nothing}


-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
gosNextToken :: Lens' GetOfferingStatus (Maybe Text)
gosNextToken = lens _gosNextToken (\ s a -> s{_gosNextToken = a})

instance AWSPager GetOfferingStatus where
        page rq rs
          | stop (rs ^. gosrsNextToken) = Nothing
          | stop (rs ^. gosrsCurrent) = Nothing
          | stop (rs ^. gosrsNextPeriod) = Nothing
          | otherwise =
            Just $ rq & gosNextToken .~ rs ^. gosrsNextToken

instance AWSRequest GetOfferingStatus where
        type Rs GetOfferingStatus = GetOfferingStatusResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 GetOfferingStatusResponse' <$>
                   (x .?> "nextPeriod" .!@ mempty) <*>
                     (x .?> "current" .!@ mempty)
                     <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetOfferingStatus where

instance NFData GetOfferingStatus where

instance ToHeaders GetOfferingStatus where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.GetOfferingStatus" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetOfferingStatus where
        toJSON GetOfferingStatus'{..}
          = object
              (catMaybes [("nextToken" .=) <$> _gosNextToken])

instance ToPath GetOfferingStatus where
        toPath = const "/"

instance ToQuery GetOfferingStatus where
        toQuery = const mempty

-- | Returns the status result for a device offering.
--
--
--
-- /See:/ 'getOfferingStatusResponse' smart constructor.
data GetOfferingStatusResponse = GetOfferingStatusResponse'
  { _gosrsNextPeriod     :: !(Maybe (Map Text OfferingStatus))
  , _gosrsCurrent        :: !(Maybe (Map Text OfferingStatus))
  , _gosrsNextToken      :: !(Maybe Text)
  , _gosrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetOfferingStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gosrsNextPeriod' - When specified, gets the offering status for the next period.
--
-- * 'gosrsCurrent' - When specified, gets the offering status for the current period.
--
-- * 'gosrsNextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- * 'gosrsResponseStatus' - -- | The response status code.
getOfferingStatusResponse
    :: Int -- ^ 'gosrsResponseStatus'
    -> GetOfferingStatusResponse
getOfferingStatusResponse pResponseStatus_ =
  GetOfferingStatusResponse'
    { _gosrsNextPeriod = Nothing
    , _gosrsCurrent = Nothing
    , _gosrsNextToken = Nothing
    , _gosrsResponseStatus = pResponseStatus_
    }


-- | When specified, gets the offering status for the next period.
gosrsNextPeriod :: Lens' GetOfferingStatusResponse (HashMap Text OfferingStatus)
gosrsNextPeriod = lens _gosrsNextPeriod (\ s a -> s{_gosrsNextPeriod = a}) . _Default . _Map

-- | When specified, gets the offering status for the current period.
gosrsCurrent :: Lens' GetOfferingStatusResponse (HashMap Text OfferingStatus)
gosrsCurrent = lens _gosrsCurrent (\ s a -> s{_gosrsCurrent = a}) . _Default . _Map

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
gosrsNextToken :: Lens' GetOfferingStatusResponse (Maybe Text)
gosrsNextToken = lens _gosrsNextToken (\ s a -> s{_gosrsNextToken = a})

-- | -- | The response status code.
gosrsResponseStatus :: Lens' GetOfferingStatusResponse Int
gosrsResponseStatus = lens _gosrsResponseStatus (\ s a -> s{_gosrsResponseStatus = a})

instance NFData GetOfferingStatusResponse where
