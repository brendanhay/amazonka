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
-- Module      : Network.AWS.Route53AutoNaming.ListServices
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists summary information for all the services that are associated with one or more specified namespaces.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Route53AutoNaming.ListServices
    (
    -- * Creating a Request
      listServices
    , ListServices
    -- * Request Lenses
    , lsFilters
    , lsNextToken
    , lsMaxResults

    -- * Destructuring the Response
    , listServicesResponse
    , ListServicesResponse
    -- * Response Lenses
    , lsrsNextToken
    , lsrsServices
    , lsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53AutoNaming.Types
import Network.AWS.Route53AutoNaming.Types.Product

-- | /See:/ 'listServices' smart constructor.
data ListServices = ListServices'
  { _lsFilters    :: !(Maybe [ServiceFilter])
  , _lsNextToken  :: !(Maybe Text)
  , _lsMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListServices' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsFilters' - A complex type that contains specifications for the namespaces that you want to list services for.  If you specify more than one filter, an operation must match all filters to be returned by @ListServices@ .
--
-- * 'lsNextToken' - For the first @ListServices@ request, omit this value. If the response contains @NextToken@ , submit another @ListServices@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
--
-- * 'lsMaxResults' - The maximum number of services that you want Amazon Route 53 to return in the response to a @ListServices@ request. If you don't specify a value for @MaxResults@ , Route 53 returns up to 100 services.
listServices
    :: ListServices
listServices =
  ListServices'
    {_lsFilters = Nothing, _lsNextToken = Nothing, _lsMaxResults = Nothing}


-- | A complex type that contains specifications for the namespaces that you want to list services for.  If you specify more than one filter, an operation must match all filters to be returned by @ListServices@ .
lsFilters :: Lens' ListServices [ServiceFilter]
lsFilters = lens _lsFilters (\ s a -> s{_lsFilters = a}) . _Default . _Coerce

-- | For the first @ListServices@ request, omit this value. If the response contains @NextToken@ , submit another @ListServices@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
lsNextToken :: Lens' ListServices (Maybe Text)
lsNextToken = lens _lsNextToken (\ s a -> s{_lsNextToken = a})

-- | The maximum number of services that you want Amazon Route 53 to return in the response to a @ListServices@ request. If you don't specify a value for @MaxResults@ , Route 53 returns up to 100 services.
lsMaxResults :: Lens' ListServices (Maybe Natural)
lsMaxResults = lens _lsMaxResults (\ s a -> s{_lsMaxResults = a}) . mapping _Nat

instance AWSPager ListServices where
        page rq rs
          | stop (rs ^. lsrsNextToken) = Nothing
          | stop (rs ^. lsrsServices) = Nothing
          | otherwise =
            Just $ rq & lsNextToken .~ rs ^. lsrsNextToken

instance AWSRequest ListServices where
        type Rs ListServices = ListServicesResponse
        request = postJSON route53AutoNaming
        response
          = receiveJSON
              (\ s h x ->
                 ListServicesResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Services" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListServices where

instance NFData ListServices where

instance ToHeaders ListServices where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53AutoNaming_v20170314.ListServices" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListServices where
        toJSON ListServices'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _lsFilters,
                  ("NextToken" .=) <$> _lsNextToken,
                  ("MaxResults" .=) <$> _lsMaxResults])

instance ToPath ListServices where
        toPath = const "/"

instance ToQuery ListServices where
        toQuery = const mempty

-- | /See:/ 'listServicesResponse' smart constructor.
data ListServicesResponse = ListServicesResponse'
  { _lsrsNextToken      :: !(Maybe Text)
  , _lsrsServices       :: !(Maybe [ServiceSummary])
  , _lsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListServicesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsrsNextToken' - If the response contains @NextToken@ , submit another @ListServices@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
--
-- * 'lsrsServices' - An array that contains one @ServiceSummary@ object for each service that matches the specified filter criteria.
--
-- * 'lsrsResponseStatus' - -- | The response status code.
listServicesResponse
    :: Int -- ^ 'lsrsResponseStatus'
    -> ListServicesResponse
listServicesResponse pResponseStatus_ =
  ListServicesResponse'
    { _lsrsNextToken = Nothing
    , _lsrsServices = Nothing
    , _lsrsResponseStatus = pResponseStatus_
    }


-- | If the response contains @NextToken@ , submit another @ListServices@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
lsrsNextToken :: Lens' ListServicesResponse (Maybe Text)
lsrsNextToken = lens _lsrsNextToken (\ s a -> s{_lsrsNextToken = a})

-- | An array that contains one @ServiceSummary@ object for each service that matches the specified filter criteria.
lsrsServices :: Lens' ListServicesResponse [ServiceSummary]
lsrsServices = lens _lsrsServices (\ s a -> s{_lsrsServices = a}) . _Default . _Coerce

-- | -- | The response status code.
lsrsResponseStatus :: Lens' ListServicesResponse Int
lsrsResponseStatus = lens _lsrsResponseStatus (\ s a -> s{_lsrsResponseStatus = a})

instance NFData ListServicesResponse where
