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
-- Module      : Network.AWS.FMS.ListComplianceStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @PolicyComplianceStatus@ objects in the response. Use @PolicyComplianceStatus@ to get a summary of which member accounts are protected by the specified policy.
--
--
module Network.AWS.FMS.ListComplianceStatus
    (
    -- * Creating a Request
      listComplianceStatus
    , ListComplianceStatus
    -- * Request Lenses
    , lcsNextToken
    , lcsMaxResults
    , lcsPolicyId

    -- * Destructuring the Response
    , listComplianceStatusResponse
    , ListComplianceStatusResponse
    -- * Response Lenses
    , lcsrsNextToken
    , lcsrsPolicyComplianceStatusList
    , lcsrsResponseStatus
    ) where

import Network.AWS.FMS.Types
import Network.AWS.FMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listComplianceStatus' smart constructor.
data ListComplianceStatus = ListComplianceStatus'
  { _lcsNextToken  :: !(Maybe Text)
  , _lcsMaxResults :: !(Maybe Nat)
  , _lcsPolicyId   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListComplianceStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcsNextToken' - If you specify a value for @MaxResults@ and you have more @PolicyComplianceStatus@ objects than the number that you specify for @MaxResults@ , AWS Firewall Manager returns a @NextToken@ value in the response that allows you to list another group of @PolicyComplianceStatus@ objects. For the second and subsequent @ListComplianceStatus@ requests, specify the value of @NextToken@ from the previous response to get information about another batch of @PolicyComplianceStatus@ objects.
--
-- * 'lcsMaxResults' - Specifies the number of @PolicyComplianceStatus@ objects that you want AWS Firewall Manager to return for this request. If you have more @PolicyComplianceStatus@ objects than the number that you specify for @MaxResults@ , the response includes a @NextToken@ value that you can use to get another batch of @PolicyComplianceStatus@ objects.
--
-- * 'lcsPolicyId' - The ID of the AWS Firewall Manager policy that you want the details for.
listComplianceStatus
    :: Text -- ^ 'lcsPolicyId'
    -> ListComplianceStatus
listComplianceStatus pPolicyId_ =
  ListComplianceStatus'
    { _lcsNextToken = Nothing
    , _lcsMaxResults = Nothing
    , _lcsPolicyId = pPolicyId_
    }


-- | If you specify a value for @MaxResults@ and you have more @PolicyComplianceStatus@ objects than the number that you specify for @MaxResults@ , AWS Firewall Manager returns a @NextToken@ value in the response that allows you to list another group of @PolicyComplianceStatus@ objects. For the second and subsequent @ListComplianceStatus@ requests, specify the value of @NextToken@ from the previous response to get information about another batch of @PolicyComplianceStatus@ objects.
lcsNextToken :: Lens' ListComplianceStatus (Maybe Text)
lcsNextToken = lens _lcsNextToken (\ s a -> s{_lcsNextToken = a})

-- | Specifies the number of @PolicyComplianceStatus@ objects that you want AWS Firewall Manager to return for this request. If you have more @PolicyComplianceStatus@ objects than the number that you specify for @MaxResults@ , the response includes a @NextToken@ value that you can use to get another batch of @PolicyComplianceStatus@ objects.
lcsMaxResults :: Lens' ListComplianceStatus (Maybe Natural)
lcsMaxResults = lens _lcsMaxResults (\ s a -> s{_lcsMaxResults = a}) . mapping _Nat

-- | The ID of the AWS Firewall Manager policy that you want the details for.
lcsPolicyId :: Lens' ListComplianceStatus Text
lcsPolicyId = lens _lcsPolicyId (\ s a -> s{_lcsPolicyId = a})

instance AWSRequest ListComplianceStatus where
        type Rs ListComplianceStatus =
             ListComplianceStatusResponse
        request = postJSON fms
        response
          = receiveJSON
              (\ s h x ->
                 ListComplianceStatusResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "PolicyComplianceStatusList" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListComplianceStatus where

instance NFData ListComplianceStatus where

instance ToHeaders ListComplianceStatus where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSFMS_20180101.ListComplianceStatus" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListComplianceStatus where
        toJSON ListComplianceStatus'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lcsNextToken,
                  ("MaxResults" .=) <$> _lcsMaxResults,
                  Just ("PolicyId" .= _lcsPolicyId)])

instance ToPath ListComplianceStatus where
        toPath = const "/"

instance ToQuery ListComplianceStatus where
        toQuery = const mempty

-- | /See:/ 'listComplianceStatusResponse' smart constructor.
data ListComplianceStatusResponse = ListComplianceStatusResponse'
  { _lcsrsNextToken                  :: !(Maybe Text)
  , _lcsrsPolicyComplianceStatusList :: !(Maybe [PolicyComplianceStatus])
  , _lcsrsResponseStatus             :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListComplianceStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcsrsNextToken' - If you have more @PolicyComplianceStatus@ objects than the number that you specified for @MaxResults@ in the request, the response includes a @NextToken@ value. To list more @PolicyComplianceStatus@ objects, submit another @ListComplianceStatus@ request, and specify the @NextToken@ value from the response in the @NextToken@ value in the next request.
--
-- * 'lcsrsPolicyComplianceStatusList' - An array of @PolicyComplianceStatus@ objects.
--
-- * 'lcsrsResponseStatus' - -- | The response status code.
listComplianceStatusResponse
    :: Int -- ^ 'lcsrsResponseStatus'
    -> ListComplianceStatusResponse
listComplianceStatusResponse pResponseStatus_ =
  ListComplianceStatusResponse'
    { _lcsrsNextToken = Nothing
    , _lcsrsPolicyComplianceStatusList = Nothing
    , _lcsrsResponseStatus = pResponseStatus_
    }


-- | If you have more @PolicyComplianceStatus@ objects than the number that you specified for @MaxResults@ in the request, the response includes a @NextToken@ value. To list more @PolicyComplianceStatus@ objects, submit another @ListComplianceStatus@ request, and specify the @NextToken@ value from the response in the @NextToken@ value in the next request.
lcsrsNextToken :: Lens' ListComplianceStatusResponse (Maybe Text)
lcsrsNextToken = lens _lcsrsNextToken (\ s a -> s{_lcsrsNextToken = a})

-- | An array of @PolicyComplianceStatus@ objects.
lcsrsPolicyComplianceStatusList :: Lens' ListComplianceStatusResponse [PolicyComplianceStatus]
lcsrsPolicyComplianceStatusList = lens _lcsrsPolicyComplianceStatusList (\ s a -> s{_lcsrsPolicyComplianceStatusList = a}) . _Default . _Coerce

-- | -- | The response status code.
lcsrsResponseStatus :: Lens' ListComplianceStatusResponse Int
lcsrsResponseStatus = lens _lcsrsResponseStatus (\ s a -> s{_lcsrsResponseStatus = a})

instance NFData ListComplianceStatusResponse where
