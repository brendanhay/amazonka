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
-- Module      : Network.AWS.Route53Domains.ListDomains
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns all the domain names registered with Amazon Route 53 for the current AWS account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Route53Domains.ListDomains
    (
    -- * Creating a Request
      listDomains
    , ListDomains
    -- * Request Lenses
    , ldMarker
    , ldMaxItems

    -- * Destructuring the Response
    , listDomainsResponse
    , ListDomainsResponse
    -- * Response Lenses
    , ldrsNextPageMarker
    , ldrsResponseStatus
    , ldrsDomains
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53Domains.Types
import Network.AWS.Route53Domains.Types.Product

-- | The ListDomains request includes the following elements.
--
--
--
-- /See:/ 'listDomains' smart constructor.
data ListDomains = ListDomains'
  { _ldMarker   :: !(Maybe Text)
  , _ldMaxItems :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDomains' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldMarker' - For an initial request for a list of domains, omit this element. If the number of domains that are associated with the current AWS account is greater than the value that you specified for @MaxItems@ , you can use @Marker@ to return additional domains. Get the value of @NextPageMarker@ from the previous response, and submit another request that includes the value of @NextPageMarker@ in the @Marker@ element. Constraints: The marker must match the value specified in the previous request.
--
-- * 'ldMaxItems' - Number of domains to be returned. Default: 20
listDomains
    :: ListDomains
listDomains = ListDomains' {_ldMarker = Nothing, _ldMaxItems = Nothing}


-- | For an initial request for a list of domains, omit this element. If the number of domains that are associated with the current AWS account is greater than the value that you specified for @MaxItems@ , you can use @Marker@ to return additional domains. Get the value of @NextPageMarker@ from the previous response, and submit another request that includes the value of @NextPageMarker@ in the @Marker@ element. Constraints: The marker must match the value specified in the previous request.
ldMarker :: Lens' ListDomains (Maybe Text)
ldMarker = lens _ldMarker (\ s a -> s{_ldMarker = a})

-- | Number of domains to be returned. Default: 20
ldMaxItems :: Lens' ListDomains (Maybe Int)
ldMaxItems = lens _ldMaxItems (\ s a -> s{_ldMaxItems = a})

instance AWSPager ListDomains where
        page rq rs
          | stop (rs ^. ldrsNextPageMarker) = Nothing
          | stop (rs ^. ldrsDomains) = Nothing
          | otherwise =
            Just $ rq & ldMarker .~ rs ^. ldrsNextPageMarker

instance AWSRequest ListDomains where
        type Rs ListDomains = ListDomainsResponse
        request = postJSON route53Domains
        response
          = receiveJSON
              (\ s h x ->
                 ListDomainsResponse' <$>
                   (x .?> "NextPageMarker") <*> (pure (fromEnum s)) <*>
                     (x .?> "Domains" .!@ mempty))

instance Hashable ListDomains where

instance NFData ListDomains where

instance ToHeaders ListDomains where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53Domains_v20140515.ListDomains" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListDomains where
        toJSON ListDomains'{..}
          = object
              (catMaybes
                 [("Marker" .=) <$> _ldMarker,
                  ("MaxItems" .=) <$> _ldMaxItems])

instance ToPath ListDomains where
        toPath = const "/"

instance ToQuery ListDomains where
        toQuery = const mempty

-- | The ListDomains response includes the following elements.
--
--
--
-- /See:/ 'listDomainsResponse' smart constructor.
data ListDomainsResponse = ListDomainsResponse'
  { _ldrsNextPageMarker :: !(Maybe Text)
  , _ldrsResponseStatus :: !Int
  , _ldrsDomains        :: ![DomainSummary]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDomainsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldrsNextPageMarker' - If there are more domains than you specified for @MaxItems@ in the request, submit another request and include the value of @NextPageMarker@ in the value of @Marker@ .
--
-- * 'ldrsResponseStatus' - -- | The response status code.
--
-- * 'ldrsDomains' - A summary of domains.
listDomainsResponse
    :: Int -- ^ 'ldrsResponseStatus'
    -> ListDomainsResponse
listDomainsResponse pResponseStatus_ =
  ListDomainsResponse'
    { _ldrsNextPageMarker = Nothing
    , _ldrsResponseStatus = pResponseStatus_
    , _ldrsDomains = mempty
    }


-- | If there are more domains than you specified for @MaxItems@ in the request, submit another request and include the value of @NextPageMarker@ in the value of @Marker@ .
ldrsNextPageMarker :: Lens' ListDomainsResponse (Maybe Text)
ldrsNextPageMarker = lens _ldrsNextPageMarker (\ s a -> s{_ldrsNextPageMarker = a})

-- | -- | The response status code.
ldrsResponseStatus :: Lens' ListDomainsResponse Int
ldrsResponseStatus = lens _ldrsResponseStatus (\ s a -> s{_ldrsResponseStatus = a})

-- | A summary of domains.
ldrsDomains :: Lens' ListDomainsResponse [DomainSummary]
ldrsDomains = lens _ldrsDomains (\ s a -> s{_ldrsDomains = a}) . _Coerce

instance NFData ListDomainsResponse where
