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
-- Module      : Network.AWS.DeviceFarm.ListOfferingPromotions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of offering promotions. Each offering promotion record contains the ID and description of the promotion. The API returns a @NotEligible@ error if the caller is not permitted to invoke the operation. Contact <mailto:aws-devicefarm-support@amazon.com aws-devicefarm-support@amazon.com> if you believe that you should be able to invoke this operation.
--
--
module Network.AWS.DeviceFarm.ListOfferingPromotions
    (
    -- * Creating a Request
      listOfferingPromotions
    , ListOfferingPromotions
    -- * Request Lenses
    , lopNextToken

    -- * Destructuring the Response
    , listOfferingPromotionsResponse
    , ListOfferingPromotionsResponse
    -- * Response Lenses
    , loprsNextToken
    , loprsOfferingPromotions
    , loprsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listOfferingPromotions' smart constructor.
newtype ListOfferingPromotions = ListOfferingPromotions'
  { _lopNextToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListOfferingPromotions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lopNextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
listOfferingPromotions
    :: ListOfferingPromotions
listOfferingPromotions = ListOfferingPromotions' {_lopNextToken = Nothing}


-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
lopNextToken :: Lens' ListOfferingPromotions (Maybe Text)
lopNextToken = lens _lopNextToken (\ s a -> s{_lopNextToken = a})

instance AWSRequest ListOfferingPromotions where
        type Rs ListOfferingPromotions =
             ListOfferingPromotionsResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 ListOfferingPromotionsResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "offeringPromotions" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListOfferingPromotions where

instance NFData ListOfferingPromotions where

instance ToHeaders ListOfferingPromotions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.ListOfferingPromotions" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListOfferingPromotions where
        toJSON ListOfferingPromotions'{..}
          = object
              (catMaybes [("nextToken" .=) <$> _lopNextToken])

instance ToPath ListOfferingPromotions where
        toPath = const "/"

instance ToQuery ListOfferingPromotions where
        toQuery = const mempty

-- | /See:/ 'listOfferingPromotionsResponse' smart constructor.
data ListOfferingPromotionsResponse = ListOfferingPromotionsResponse'
  { _loprsNextToken          :: !(Maybe Text)
  , _loprsOfferingPromotions :: !(Maybe [OfferingPromotion])
  , _loprsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListOfferingPromotionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'loprsNextToken' - An identifier to be used in the next call to this operation, to return the next set of items in the list.
--
-- * 'loprsOfferingPromotions' - Information about the offering promotions.
--
-- * 'loprsResponseStatus' - -- | The response status code.
listOfferingPromotionsResponse
    :: Int -- ^ 'loprsResponseStatus'
    -> ListOfferingPromotionsResponse
listOfferingPromotionsResponse pResponseStatus_ =
  ListOfferingPromotionsResponse'
    { _loprsNextToken = Nothing
    , _loprsOfferingPromotions = Nothing
    , _loprsResponseStatus = pResponseStatus_
    }


-- | An identifier to be used in the next call to this operation, to return the next set of items in the list.
loprsNextToken :: Lens' ListOfferingPromotionsResponse (Maybe Text)
loprsNextToken = lens _loprsNextToken (\ s a -> s{_loprsNextToken = a})

-- | Information about the offering promotions.
loprsOfferingPromotions :: Lens' ListOfferingPromotionsResponse [OfferingPromotion]
loprsOfferingPromotions = lens _loprsOfferingPromotions (\ s a -> s{_loprsOfferingPromotions = a}) . _Default . _Coerce

-- | -- | The response status code.
loprsResponseStatus :: Lens' ListOfferingPromotionsResponse Int
loprsResponseStatus = lens _loprsResponseStatus (\ s a -> s{_loprsResponseStatus = a})

instance NFData ListOfferingPromotionsResponse where
