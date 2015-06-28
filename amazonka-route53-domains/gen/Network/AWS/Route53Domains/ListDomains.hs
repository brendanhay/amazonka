{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Route53Domains.ListDomains
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation returns all the domain names registered with Amazon Route
-- 53 for the current AWS account.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/api-ListDomains.html>
module Network.AWS.Route53Domains.ListDomains
    (
    -- * Request
      ListDomains
    -- ** Request constructor
    , listDomains
    -- ** Request lenses
    , ldMaxItems
    , ldMarker

    -- * Response
    , ListDomainsResponse
    -- ** Response constructor
    , listDomainsResponse
    -- ** Response lenses
    , ldrNextPageMarker
    , ldrDomains
    , ldrStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53Domains.Types

-- | The ListDomains request includes the following elements.
--
-- /See:/ 'listDomains' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldMaxItems'
--
-- * 'ldMarker'
data ListDomains = ListDomains'
    { _ldMaxItems :: !(Maybe Int)
    , _ldMarker   :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'ListDomains' smart constructor.
listDomains :: ListDomains
listDomains =
    ListDomains'
    { _ldMaxItems = Nothing
    , _ldMarker = Nothing
    }

-- | Number of domains to be returned.
--
-- Type: Integer
--
-- Default: 20
--
-- Constraints: A numeral between 1 and 100.
--
-- Required: No
ldMaxItems :: Lens' ListDomains (Maybe Int)
ldMaxItems = lens _ldMaxItems (\ s a -> s{_ldMaxItems = a});

-- | For an initial request for a list of domains, omit this element. If the
-- number of domains that are associated with the current AWS account is
-- greater than the value that you specified for @MaxItems@, you can use
-- @Marker@ to return additional domains. Get the value of @NextPageMarker@
-- from the previous response, and submit another request that includes the
-- value of @NextPageMarker@ in the @Marker@ element.
--
-- Type: String
--
-- Default: None
--
-- Constraints: The marker must match the value specified in the previous
-- request.
--
-- Required: No
ldMarker :: Lens' ListDomains (Maybe Text)
ldMarker = lens _ldMarker (\ s a -> s{_ldMarker = a});

instance AWSPager ListDomains where
        page rq rs
          | stop (rs ^. ldrNextPageMarker) = Nothing
          | stop (rs ^. ldrDomains) = Nothing
          | otherwise =
            Just $ rq & ldMarker .~ rs ^. ldrNextPageMarker

instance AWSRequest ListDomains where
        type Sv ListDomains = Route53Domains
        type Rs ListDomains = ListDomainsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListDomainsResponse' <$>
                   (x .?> "NextPageMarker") <*>
                     (x .?> "Domains" .!@ mempty)
                     <*> (pure s))

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
              ["MaxItems" .= _ldMaxItems, "Marker" .= _ldMarker]

instance ToPath ListDomains where
        toPath = const "/"

instance ToQuery ListDomains where
        toQuery = const mempty

-- | The ListDomains response includes the following elements.
--
-- /See:/ 'listDomainsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldrNextPageMarker'
--
-- * 'ldrDomains'
--
-- * 'ldrStatus'
data ListDomainsResponse = ListDomainsResponse'
    { _ldrNextPageMarker :: !(Maybe Text)
    , _ldrDomains        :: ![DomainSummary]
    , _ldrStatus         :: !Status
    } deriving (Eq,Show)

-- | 'ListDomainsResponse' smart constructor.
listDomainsResponse :: Status -> ListDomainsResponse
listDomainsResponse pStatus =
    ListDomainsResponse'
    { _ldrNextPageMarker = Nothing
    , _ldrDomains = mempty
    , _ldrStatus = pStatus
    }

-- | If there are more domains than you specified for @MaxItems@ in the
-- request, submit another request and include the value of
-- @NextPageMarker@ in the value of @Marker@.
--
-- Type: String
--
-- Parent: @Operations@
ldrNextPageMarker :: Lens' ListDomainsResponse (Maybe Text)
ldrNextPageMarker = lens _ldrNextPageMarker (\ s a -> s{_ldrNextPageMarker = a});

-- | A summary of domains.
--
-- Type: Complex type containing a list of domain summaries.
--
-- Children: @AutoRenew@, @DomainName@, @Expiry@, @TransferLock@
ldrDomains :: Lens' ListDomainsResponse [DomainSummary]
ldrDomains = lens _ldrDomains (\ s a -> s{_ldrDomains = a});

-- | FIXME: Undocumented member.
ldrStatus :: Lens' ListDomainsResponse Status
ldrStatus = lens _ldrStatus (\ s a -> s{_ldrStatus = a});
