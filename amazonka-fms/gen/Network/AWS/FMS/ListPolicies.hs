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
-- Module      : Network.AWS.FMS.ListPolicies
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @PolicySummary@ objects in the response.
--
--
module Network.AWS.FMS.ListPolicies
    (
    -- * Creating a Request
      listPolicies
    , ListPolicies
    -- * Request Lenses
    , lpNextToken
    , lpMaxResults

    -- * Destructuring the Response
    , listPoliciesResponse
    , ListPoliciesResponse
    -- * Response Lenses
    , lprsNextToken
    , lprsPolicyList
    , lprsResponseStatus
    ) where

import Network.AWS.FMS.Types
import Network.AWS.FMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listPolicies' smart constructor.
data ListPolicies = ListPolicies'
  { _lpNextToken  :: !(Maybe Text)
  , _lpMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpNextToken' - If you specify a value for @MaxResults@ and you have more @PolicySummary@ objects than the number that you specify for @MaxResults@ , AWS Firewall Manager returns a @NextToken@ value in the response that allows you to list another group of @PolicySummary@ objects. For the second and subsequent @ListPolicies@ requests, specify the value of @NextToken@ from the previous response to get information about another batch of @PolicySummary@ objects.
--
-- * 'lpMaxResults' - Specifies the number of @PolicySummary@ objects that you want AWS Firewall Manager to return for this request. If you have more @PolicySummary@ objects than the number that you specify for @MaxResults@ , the response includes a @NextToken@ value that you can use to get another batch of @PolicySummary@ objects.
listPolicies
    :: ListPolicies
listPolicies = ListPolicies' {_lpNextToken = Nothing, _lpMaxResults = Nothing}


-- | If you specify a value for @MaxResults@ and you have more @PolicySummary@ objects than the number that you specify for @MaxResults@ , AWS Firewall Manager returns a @NextToken@ value in the response that allows you to list another group of @PolicySummary@ objects. For the second and subsequent @ListPolicies@ requests, specify the value of @NextToken@ from the previous response to get information about another batch of @PolicySummary@ objects.
lpNextToken :: Lens' ListPolicies (Maybe Text)
lpNextToken = lens _lpNextToken (\ s a -> s{_lpNextToken = a})

-- | Specifies the number of @PolicySummary@ objects that you want AWS Firewall Manager to return for this request. If you have more @PolicySummary@ objects than the number that you specify for @MaxResults@ , the response includes a @NextToken@ value that you can use to get another batch of @PolicySummary@ objects.
lpMaxResults :: Lens' ListPolicies (Maybe Natural)
lpMaxResults = lens _lpMaxResults (\ s a -> s{_lpMaxResults = a}) . mapping _Nat

instance AWSRequest ListPolicies where
        type Rs ListPolicies = ListPoliciesResponse
        request = postJSON fms
        response
          = receiveJSON
              (\ s h x ->
                 ListPoliciesResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "PolicyList" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListPolicies where

instance NFData ListPolicies where

instance ToHeaders ListPolicies where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSFMS_20180101.ListPolicies" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListPolicies where
        toJSON ListPolicies'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lpNextToken,
                  ("MaxResults" .=) <$> _lpMaxResults])

instance ToPath ListPolicies where
        toPath = const "/"

instance ToQuery ListPolicies where
        toQuery = const mempty

-- | /See:/ 'listPoliciesResponse' smart constructor.
data ListPoliciesResponse = ListPoliciesResponse'
  { _lprsNextToken      :: !(Maybe Text)
  , _lprsPolicyList     :: !(Maybe [PolicySummary])
  , _lprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lprsNextToken' - If you have more @PolicySummary@ objects than the number that you specified for @MaxResults@ in the request, the response includes a @NextToken@ value. To list more @PolicySummary@ objects, submit another @ListPolicies@ request, and specify the @NextToken@ value from the response in the @NextToken@ value in the next request.
--
-- * 'lprsPolicyList' - An array of @PolicySummary@ objects.
--
-- * 'lprsResponseStatus' - -- | The response status code.
listPoliciesResponse
    :: Int -- ^ 'lprsResponseStatus'
    -> ListPoliciesResponse
listPoliciesResponse pResponseStatus_ =
  ListPoliciesResponse'
    { _lprsNextToken = Nothing
    , _lprsPolicyList = Nothing
    , _lprsResponseStatus = pResponseStatus_
    }


-- | If you have more @PolicySummary@ objects than the number that you specified for @MaxResults@ in the request, the response includes a @NextToken@ value. To list more @PolicySummary@ objects, submit another @ListPolicies@ request, and specify the @NextToken@ value from the response in the @NextToken@ value in the next request.
lprsNextToken :: Lens' ListPoliciesResponse (Maybe Text)
lprsNextToken = lens _lprsNextToken (\ s a -> s{_lprsNextToken = a})

-- | An array of @PolicySummary@ objects.
lprsPolicyList :: Lens' ListPoliciesResponse [PolicySummary]
lprsPolicyList = lens _lprsPolicyList (\ s a -> s{_lprsPolicyList = a}) . _Default . _Coerce

-- | -- | The response status code.
lprsResponseStatus :: Lens' ListPoliciesResponse Int
lprsResponseStatus = lens _lprsResponseStatus (\ s a -> s{_lprsResponseStatus = a})

instance NFData ListPoliciesResponse where
