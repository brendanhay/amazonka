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
-- Module      : Network.AWS.Route53AutoNaming.ListNamespaces
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists summary information about the namespaces that were created by the current AWS account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Route53AutoNaming.ListNamespaces
    (
    -- * Creating a Request
      listNamespaces
    , ListNamespaces
    -- * Request Lenses
    , lnFilters
    , lnNextToken
    , lnMaxResults

    -- * Destructuring the Response
    , listNamespacesResponse
    , ListNamespacesResponse
    -- * Response Lenses
    , lnrsNamespaces
    , lnrsNextToken
    , lnrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53AutoNaming.Types
import Network.AWS.Route53AutoNaming.Types.Product

-- | /See:/ 'listNamespaces' smart constructor.
data ListNamespaces = ListNamespaces'
  { _lnFilters    :: !(Maybe [NamespaceFilter])
  , _lnNextToken  :: !(Maybe Text)
  , _lnMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListNamespaces' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lnFilters' - A complex type that contains specifications for the namespaces that you want to list. If you specify more than one filter, a namespace must match all filters to be returned by @ListNamespaces@ .
--
-- * 'lnNextToken' - For the first @ListNamespaces@ request, omit this value. If the response contains @NextToken@ , submit another @ListNamespaces@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
--
-- * 'lnMaxResults' - The maximum number of namespaces that you want Amazon Route 53 to return in the response to a @ListNamespaces@ request. If you don't specify a value for @MaxResults@ , Route 53 returns up to 100 namespaces.
listNamespaces
    :: ListNamespaces
listNamespaces =
  ListNamespaces'
    {_lnFilters = Nothing, _lnNextToken = Nothing, _lnMaxResults = Nothing}


-- | A complex type that contains specifications for the namespaces that you want to list. If you specify more than one filter, a namespace must match all filters to be returned by @ListNamespaces@ .
lnFilters :: Lens' ListNamespaces [NamespaceFilter]
lnFilters = lens _lnFilters (\ s a -> s{_lnFilters = a}) . _Default . _Coerce

-- | For the first @ListNamespaces@ request, omit this value. If the response contains @NextToken@ , submit another @ListNamespaces@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
lnNextToken :: Lens' ListNamespaces (Maybe Text)
lnNextToken = lens _lnNextToken (\ s a -> s{_lnNextToken = a})

-- | The maximum number of namespaces that you want Amazon Route 53 to return in the response to a @ListNamespaces@ request. If you don't specify a value for @MaxResults@ , Route 53 returns up to 100 namespaces.
lnMaxResults :: Lens' ListNamespaces (Maybe Natural)
lnMaxResults = lens _lnMaxResults (\ s a -> s{_lnMaxResults = a}) . mapping _Nat

instance AWSPager ListNamespaces where
        page rq rs
          | stop (rs ^. lnrsNextToken) = Nothing
          | stop (rs ^. lnrsNamespaces) = Nothing
          | otherwise =
            Just $ rq & lnNextToken .~ rs ^. lnrsNextToken

instance AWSRequest ListNamespaces where
        type Rs ListNamespaces = ListNamespacesResponse
        request = postJSON route53AutoNaming
        response
          = receiveJSON
              (\ s h x ->
                 ListNamespacesResponse' <$>
                   (x .?> "Namespaces" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListNamespaces where

instance NFData ListNamespaces where

instance ToHeaders ListNamespaces where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53AutoNaming_v20170314.ListNamespaces" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListNamespaces where
        toJSON ListNamespaces'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _lnFilters,
                  ("NextToken" .=) <$> _lnNextToken,
                  ("MaxResults" .=) <$> _lnMaxResults])

instance ToPath ListNamespaces where
        toPath = const "/"

instance ToQuery ListNamespaces where
        toQuery = const mempty

-- | /See:/ 'listNamespacesResponse' smart constructor.
data ListNamespacesResponse = ListNamespacesResponse'
  { _lnrsNamespaces     :: !(Maybe [NamespaceSummary])
  , _lnrsNextToken      :: !(Maybe Text)
  , _lnrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListNamespacesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lnrsNamespaces' - An array that contains one @NamespaceSummary@ object for each namespace that matches the specified filter criteria.
--
-- * 'lnrsNextToken' - If the response contains @NextToken@ , submit another @ListNamespaces@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
--
-- * 'lnrsResponseStatus' - -- | The response status code.
listNamespacesResponse
    :: Int -- ^ 'lnrsResponseStatus'
    -> ListNamespacesResponse
listNamespacesResponse pResponseStatus_ =
  ListNamespacesResponse'
    { _lnrsNamespaces = Nothing
    , _lnrsNextToken = Nothing
    , _lnrsResponseStatus = pResponseStatus_
    }


-- | An array that contains one @NamespaceSummary@ object for each namespace that matches the specified filter criteria.
lnrsNamespaces :: Lens' ListNamespacesResponse [NamespaceSummary]
lnrsNamespaces = lens _lnrsNamespaces (\ s a -> s{_lnrsNamespaces = a}) . _Default . _Coerce

-- | If the response contains @NextToken@ , submit another @ListNamespaces@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
lnrsNextToken :: Lens' ListNamespacesResponse (Maybe Text)
lnrsNextToken = lens _lnrsNextToken (\ s a -> s{_lnrsNextToken = a})

-- | -- | The response status code.
lnrsResponseStatus :: Lens' ListNamespacesResponse Int
lnrsResponseStatus = lens _lnrsResponseStatus (\ s a -> s{_lnrsResponseStatus = a})

instance NFData ListNamespacesResponse where
