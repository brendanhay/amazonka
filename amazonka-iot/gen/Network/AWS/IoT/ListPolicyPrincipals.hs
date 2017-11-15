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
-- Module      : Network.AWS.IoT.ListPolicyPrincipals
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the principals associated with the specified policy.
--
--
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListPolicyPrincipals
    (
    -- * Creating a Request
      listPolicyPrincipals
    , ListPolicyPrincipals
    -- * Request Lenses
    , lMarker
    , lAscendingOrder
    , lPageSize
    , lPolicyName

    -- * Destructuring the Response
    , listPolicyPrincipalsResponse
    , ListPolicyPrincipalsResponse
    -- * Response Lenses
    , lrsPrincipals
    , lrsNextMarker
    , lrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the ListPolicyPrincipals operation.
--
--
--
-- /See:/ 'listPolicyPrincipals' smart constructor.
data ListPolicyPrincipals = ListPolicyPrincipals'
  { _lMarker         :: !(Maybe Text)
  , _lAscendingOrder :: !(Maybe Bool)
  , _lPageSize       :: !(Maybe Nat)
  , _lPolicyName     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPolicyPrincipals' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lMarker' - The marker for the next set of results.
--
-- * 'lAscendingOrder' - Specifies the order for results. If true, the results are returned in ascending creation order.
--
-- * 'lPageSize' - The result page size.
--
-- * 'lPolicyName' - The policy name.
listPolicyPrincipals
    :: Text -- ^ 'lPolicyName'
    -> ListPolicyPrincipals
listPolicyPrincipals pPolicyName_ =
  ListPolicyPrincipals'
  { _lMarker = Nothing
  , _lAscendingOrder = Nothing
  , _lPageSize = Nothing
  , _lPolicyName = pPolicyName_
  }


-- | The marker for the next set of results.
lMarker :: Lens' ListPolicyPrincipals (Maybe Text)
lMarker = lens _lMarker (\ s a -> s{_lMarker = a});

-- | Specifies the order for results. If true, the results are returned in ascending creation order.
lAscendingOrder :: Lens' ListPolicyPrincipals (Maybe Bool)
lAscendingOrder = lens _lAscendingOrder (\ s a -> s{_lAscendingOrder = a});

-- | The result page size.
lPageSize :: Lens' ListPolicyPrincipals (Maybe Natural)
lPageSize = lens _lPageSize (\ s a -> s{_lPageSize = a}) . mapping _Nat;

-- | The policy name.
lPolicyName :: Lens' ListPolicyPrincipals Text
lPolicyName = lens _lPolicyName (\ s a -> s{_lPolicyName = a});

instance AWSPager ListPolicyPrincipals where
        page rq rs
          | stop (rs ^. lrsNextMarker) = Nothing
          | stop (rs ^. lrsPrincipals) = Nothing
          | otherwise =
            Just $ rq & lMarker .~ rs ^. lrsNextMarker

instance AWSRequest ListPolicyPrincipals where
        type Rs ListPolicyPrincipals =
             ListPolicyPrincipalsResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 ListPolicyPrincipalsResponse' <$>
                   (x .?> "principals" .!@ mempty) <*>
                     (x .?> "nextMarker")
                     <*> (pure (fromEnum s)))

instance Hashable ListPolicyPrincipals where

instance NFData ListPolicyPrincipals where

instance ToHeaders ListPolicyPrincipals where
        toHeaders ListPolicyPrincipals'{..}
          = mconcat ["x-amzn-iot-policy" =# _lPolicyName]

instance ToPath ListPolicyPrincipals where
        toPath = const "/policy-principals"

instance ToQuery ListPolicyPrincipals where
        toQuery ListPolicyPrincipals'{..}
          = mconcat
              ["marker" =: _lMarker,
               "isAscendingOrder" =: _lAscendingOrder,
               "pageSize" =: _lPageSize]

-- | The output from the ListPolicyPrincipals operation.
--
--
--
-- /See:/ 'listPolicyPrincipalsResponse' smart constructor.
data ListPolicyPrincipalsResponse = ListPolicyPrincipalsResponse'
  { _lrsPrincipals     :: !(Maybe [Text])
  , _lrsNextMarker     :: !(Maybe Text)
  , _lrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPolicyPrincipalsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrsPrincipals' - The descriptions of the principals.
--
-- * 'lrsNextMarker' - The marker for the next set of results, or null if there are no additional results.
--
-- * 'lrsResponseStatus' - -- | The response status code.
listPolicyPrincipalsResponse
    :: Int -- ^ 'lrsResponseStatus'
    -> ListPolicyPrincipalsResponse
listPolicyPrincipalsResponse pResponseStatus_ =
  ListPolicyPrincipalsResponse'
  { _lrsPrincipals = Nothing
  , _lrsNextMarker = Nothing
  , _lrsResponseStatus = pResponseStatus_
  }


-- | The descriptions of the principals.
lrsPrincipals :: Lens' ListPolicyPrincipalsResponse [Text]
lrsPrincipals = lens _lrsPrincipals (\ s a -> s{_lrsPrincipals = a}) . _Default . _Coerce;

-- | The marker for the next set of results, or null if there are no additional results.
lrsNextMarker :: Lens' ListPolicyPrincipalsResponse (Maybe Text)
lrsNextMarker = lens _lrsNextMarker (\ s a -> s{_lrsNextMarker = a});

-- | -- | The response status code.
lrsResponseStatus :: Lens' ListPolicyPrincipalsResponse Int
lrsResponseStatus = lens _lrsResponseStatus (\ s a -> s{_lrsResponseStatus = a});

instance NFData ListPolicyPrincipalsResponse where
