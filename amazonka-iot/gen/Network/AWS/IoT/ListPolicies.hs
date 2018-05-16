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
-- Module      : Network.AWS.IoT.ListPolicies
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your policies.
--
--
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListPolicies
    (
    -- * Creating a Request
      listPolicies
    , ListPolicies
    -- * Request Lenses
    , lpMarker
    , lpAscendingOrder
    , lpPageSize

    -- * Destructuring the Response
    , listPoliciesResponse
    , ListPoliciesResponse
    -- * Response Lenses
    , lprsNextMarker
    , lprsPolicies
    , lprsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the ListPolicies operation.
--
--
--
-- /See:/ 'listPolicies' smart constructor.
data ListPolicies = ListPolicies'
  { _lpMarker         :: !(Maybe Text)
  , _lpAscendingOrder :: !(Maybe Bool)
  , _lpPageSize       :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpMarker' - The marker for the next set of results.
--
-- * 'lpAscendingOrder' - Specifies the order for results. If true, the results are returned in ascending creation order.
--
-- * 'lpPageSize' - The result page size.
listPolicies
    :: ListPolicies
listPolicies =
  ListPolicies'
    {_lpMarker = Nothing, _lpAscendingOrder = Nothing, _lpPageSize = Nothing}


-- | The marker for the next set of results.
lpMarker :: Lens' ListPolicies (Maybe Text)
lpMarker = lens _lpMarker (\ s a -> s{_lpMarker = a})

-- | Specifies the order for results. If true, the results are returned in ascending creation order.
lpAscendingOrder :: Lens' ListPolicies (Maybe Bool)
lpAscendingOrder = lens _lpAscendingOrder (\ s a -> s{_lpAscendingOrder = a})

-- | The result page size.
lpPageSize :: Lens' ListPolicies (Maybe Natural)
lpPageSize = lens _lpPageSize (\ s a -> s{_lpPageSize = a}) . mapping _Nat

instance AWSPager ListPolicies where
        page rq rs
          | stop (rs ^. lprsNextMarker) = Nothing
          | stop (rs ^. lprsPolicies) = Nothing
          | otherwise =
            Just $ rq & lpMarker .~ rs ^. lprsNextMarker

instance AWSRequest ListPolicies where
        type Rs ListPolicies = ListPoliciesResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 ListPoliciesResponse' <$>
                   (x .?> "nextMarker") <*>
                     (x .?> "policies" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListPolicies where

instance NFData ListPolicies where

instance ToHeaders ListPolicies where
        toHeaders = const mempty

instance ToPath ListPolicies where
        toPath = const "/policies"

instance ToQuery ListPolicies where
        toQuery ListPolicies'{..}
          = mconcat
              ["marker" =: _lpMarker,
               "isAscendingOrder" =: _lpAscendingOrder,
               "pageSize" =: _lpPageSize]

-- | The output from the ListPolicies operation.
--
--
--
-- /See:/ 'listPoliciesResponse' smart constructor.
data ListPoliciesResponse = ListPoliciesResponse'
  { _lprsNextMarker     :: !(Maybe Text)
  , _lprsPolicies       :: !(Maybe [Policy])
  , _lprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lprsNextMarker' - The marker for the next set of results, or null if there are no additional results.
--
-- * 'lprsPolicies' - The descriptions of the policies.
--
-- * 'lprsResponseStatus' - -- | The response status code.
listPoliciesResponse
    :: Int -- ^ 'lprsResponseStatus'
    -> ListPoliciesResponse
listPoliciesResponse pResponseStatus_ =
  ListPoliciesResponse'
    { _lprsNextMarker = Nothing
    , _lprsPolicies = Nothing
    , _lprsResponseStatus = pResponseStatus_
    }


-- | The marker for the next set of results, or null if there are no additional results.
lprsNextMarker :: Lens' ListPoliciesResponse (Maybe Text)
lprsNextMarker = lens _lprsNextMarker (\ s a -> s{_lprsNextMarker = a})

-- | The descriptions of the policies.
lprsPolicies :: Lens' ListPoliciesResponse [Policy]
lprsPolicies = lens _lprsPolicies (\ s a -> s{_lprsPolicies = a}) . _Default . _Coerce

-- | -- | The response status code.
lprsResponseStatus :: Lens' ListPoliciesResponse Int
lprsResponseStatus = lens _lprsResponseStatus (\ s a -> s{_lprsResponseStatus = a})

instance NFData ListPoliciesResponse where
