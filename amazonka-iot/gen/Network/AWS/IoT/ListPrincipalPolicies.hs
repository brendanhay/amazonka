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
-- Module      : Network.AWS.IoT.ListPrincipalPolicies
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the policies attached to the specified principal. If you use an Cognito identity, the ID must be in <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_GetCredentialsForIdentity.html#API_GetCredentialsForIdentity_RequestSyntax AmazonCognito Identity format> .
--
--
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListPrincipalPolicies
    (
    -- * Creating a Request
      listPrincipalPolicies
    , ListPrincipalPolicies
    -- * Request Lenses
    , lppMarker
    , lppAscendingOrder
    , lppPageSize
    , lppPrincipal

    -- * Destructuring the Response
    , listPrincipalPoliciesResponse
    , ListPrincipalPoliciesResponse
    -- * Response Lenses
    , lpprsNextMarker
    , lpprsPolicies
    , lpprsResponseStatus
    ) where

import           Network.AWS.IoT.Types
import           Network.AWS.IoT.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the ListPrincipalPolicies operation.
--
--
--
-- /See:/ 'listPrincipalPolicies' smart constructor.
data ListPrincipalPolicies = ListPrincipalPolicies'
    { _lppMarker         :: !(Maybe Text)
    , _lppAscendingOrder :: !(Maybe Bool)
    , _lppPageSize       :: !(Maybe Nat)
    , _lppPrincipal      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListPrincipalPolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lppMarker' - The marker for the next set of results.
--
-- * 'lppAscendingOrder' - Specifies the order for results. If true, results are returned in ascending creation order.
--
-- * 'lppPageSize' - The result page size.
--
-- * 'lppPrincipal' - The principal.
listPrincipalPolicies
    :: Text -- ^ 'lppPrincipal'
    -> ListPrincipalPolicies
listPrincipalPolicies pPrincipal_ =
    ListPrincipalPolicies'
    { _lppMarker = Nothing
    , _lppAscendingOrder = Nothing
    , _lppPageSize = Nothing
    , _lppPrincipal = pPrincipal_
    }

-- | The marker for the next set of results.
lppMarker :: Lens' ListPrincipalPolicies (Maybe Text)
lppMarker = lens _lppMarker (\ s a -> s{_lppMarker = a});

-- | Specifies the order for results. If true, results are returned in ascending creation order.
lppAscendingOrder :: Lens' ListPrincipalPolicies (Maybe Bool)
lppAscendingOrder = lens _lppAscendingOrder (\ s a -> s{_lppAscendingOrder = a});

-- | The result page size.
lppPageSize :: Lens' ListPrincipalPolicies (Maybe Natural)
lppPageSize = lens _lppPageSize (\ s a -> s{_lppPageSize = a}) . mapping _Nat;

-- | The principal.
lppPrincipal :: Lens' ListPrincipalPolicies Text
lppPrincipal = lens _lppPrincipal (\ s a -> s{_lppPrincipal = a});

instance AWSPager ListPrincipalPolicies where
        page rq rs
          | stop (rs ^. lpprsNextMarker) = Nothing
          | stop (rs ^. lpprsPolicies) = Nothing
          | otherwise =
            Just $ rq & lppMarker .~ rs ^. lpprsNextMarker

instance AWSRequest ListPrincipalPolicies where
        type Rs ListPrincipalPolicies =
             ListPrincipalPoliciesResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 ListPrincipalPoliciesResponse' <$>
                   (x .?> "nextMarker") <*>
                     (x .?> "policies" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListPrincipalPolicies

instance NFData ListPrincipalPolicies

instance ToHeaders ListPrincipalPolicies where
        toHeaders ListPrincipalPolicies'{..}
          = mconcat ["x-amzn-iot-principal" =# _lppPrincipal]

instance ToPath ListPrincipalPolicies where
        toPath = const "/principal-policies"

instance ToQuery ListPrincipalPolicies where
        toQuery ListPrincipalPolicies'{..}
          = mconcat
              ["marker" =: _lppMarker,
               "isAscendingOrder" =: _lppAscendingOrder,
               "pageSize" =: _lppPageSize]

-- | The output from the ListPrincipalPolicies operation.
--
--
--
-- /See:/ 'listPrincipalPoliciesResponse' smart constructor.
data ListPrincipalPoliciesResponse = ListPrincipalPoliciesResponse'
    { _lpprsNextMarker     :: !(Maybe Text)
    , _lpprsPolicies       :: !(Maybe [Policy])
    , _lpprsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListPrincipalPoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpprsNextMarker' - The marker for the next set of results, or null if there are no additional results.
--
-- * 'lpprsPolicies' - The policies.
--
-- * 'lpprsResponseStatus' - -- | The response status code.
listPrincipalPoliciesResponse
    :: Int -- ^ 'lpprsResponseStatus'
    -> ListPrincipalPoliciesResponse
listPrincipalPoliciesResponse pResponseStatus_ =
    ListPrincipalPoliciesResponse'
    { _lpprsNextMarker = Nothing
    , _lpprsPolicies = Nothing
    , _lpprsResponseStatus = pResponseStatus_
    }

-- | The marker for the next set of results, or null if there are no additional results.
lpprsNextMarker :: Lens' ListPrincipalPoliciesResponse (Maybe Text)
lpprsNextMarker = lens _lpprsNextMarker (\ s a -> s{_lpprsNextMarker = a});

-- | The policies.
lpprsPolicies :: Lens' ListPrincipalPoliciesResponse [Policy]
lpprsPolicies = lens _lpprsPolicies (\ s a -> s{_lpprsPolicies = a}) . _Default . _Coerce;

-- | -- | The response status code.
lpprsResponseStatus :: Lens' ListPrincipalPoliciesResponse Int
lpprsResponseStatus = lens _lpprsResponseStatus (\ s a -> s{_lpprsResponseStatus = a});

instance NFData ListPrincipalPoliciesResponse
