{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.ListDomains
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation returns all the domain names registered with Amazon Route
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
    , ldrsNextPageMarker
    , ldrsStatus
    , ldrsDomains
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
          | stop (rs ^. ldrsNextPageMarker) = Nothing
          | stop (rs ^. ldrsDomains) = Nothing
          | otherwise =
            Just $ rq & ldMarker .~ rs ^. ldrsNextPageMarker

instance AWSRequest ListDomains where
        type Sv ListDomains = Route53Domains
        type Rs ListDomains = ListDomainsResponse
        request = postJSON "ListDomains"
        response
          = receiveJSON
              (\ s h x ->
                 ListDomainsResponse' <$>
                   (x .?> "NextPageMarker") <*> (pure (fromEnum s)) <*>
                     (x .?> "Domains" .!@ mempty))

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
-- * 'ldrsNextPageMarker'
--
-- * 'ldrsStatus'
--
-- * 'ldrsDomains'
data ListDomainsResponse = ListDomainsResponse'
    { _ldrsNextPageMarker :: !(Maybe Text)
    , _ldrsStatus         :: !Int
    , _ldrsDomains        :: ![DomainSummary]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListDomainsResponse' smart constructor.
listDomainsResponse :: Int -> ListDomainsResponse
listDomainsResponse pStatus_ =
    ListDomainsResponse'
    { _ldrsNextPageMarker = Nothing
    , _ldrsStatus = pStatus_
    , _ldrsDomains = mempty
    }

-- | If there are more domains than you specified for @MaxItems@ in the
-- request, submit another request and include the value of
-- @NextPageMarker@ in the value of @Marker@.
--
-- Type: String
--
-- Parent: @Operations@
ldrsNextPageMarker :: Lens' ListDomainsResponse (Maybe Text)
ldrsNextPageMarker = lens _ldrsNextPageMarker (\ s a -> s{_ldrsNextPageMarker = a});

-- | FIXME: Undocumented member.
ldrsStatus :: Lens' ListDomainsResponse Int
ldrsStatus = lens _ldrsStatus (\ s a -> s{_ldrsStatus = a});

-- | A summary of domains.
--
-- Type: Complex type containing a list of domain summaries.
--
-- Children: @AutoRenew@, @DomainName@, @Expiry@, @TransferLock@
ldrsDomains :: Lens' ListDomainsResponse [DomainSummary]
ldrsDomains = lens _ldrsDomains (\ s a -> s{_ldrsDomains = a});
