{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DescribeDomains
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the search domains owned by this account. Can be
-- limited to specific domains. Shows all domains by default. To get the
-- number of searchable documents in a domain, use the console or submit a
-- @matchall@ request to your domain\'s search endpoint:
-- @q=matchall&amp;q.parser=structured&amp;size=0@. For more information,
-- see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-domain-info.html Getting Information about a Search Domain>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DescribeDomains.html>
module Network.AWS.CloudSearch.DescribeDomains
    (
    -- * Request
      DescribeDomains
    -- ** Request constructor
    , describeDomains
    -- ** Request lenses
    , ddDomainNames

    -- * Response
    , DescribeDomainsResponse
    -- ** Response constructor
    , describeDomainsResponse
    -- ** Response lenses
    , ddsrsStatus
    , ddsrsDomainStatusList
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the @DescribeDomains@ operation. By
-- default shows the status of all domains. To restrict the response to
-- particular domains, specify the names of the domains you want to
-- describe.
--
-- /See:/ 'describeDomains' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddDomainNames'
newtype DescribeDomains = DescribeDomains'
    { _ddDomainNames :: Maybe [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDomains' smart constructor.
describeDomains :: DescribeDomains
describeDomains =
    DescribeDomains'
    { _ddDomainNames = Nothing
    }

-- | The names of the domains you want to include in the response.
ddDomainNames :: Lens' DescribeDomains [Text]
ddDomainNames = lens _ddDomainNames (\ s a -> s{_ddDomainNames = a}) . _Default . _Coerce;

instance AWSRequest DescribeDomains where
        type Sv DescribeDomains = CloudSearch
        type Rs DescribeDomains = DescribeDomainsResponse
        request = postQuery
        response
          = receiveXMLWrapper "DescribeDomainsResult"
              (\ s h x ->
                 DescribeDomainsResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .@? "DomainStatusList" .!@ mempty >>=
                        parseXMLList "member"))

instance ToHeaders DescribeDomains where
        toHeaders = const mempty

instance ToPath DescribeDomains where
        toPath = const mempty

instance ToQuery DescribeDomains where
        toQuery DescribeDomains'{..}
          = mconcat
              ["Action" =: ("DescribeDomains" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "DomainNames" =:
                 toQuery (toQueryList "member" <$> _ddDomainNames)]

-- | The result of a @DescribeDomains@ request. Contains the status of the
-- domains specified in the request or all domains owned by the account.
--
-- /See:/ 'describeDomainsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddsrsStatus'
--
-- * 'ddsrsDomainStatusList'
data DescribeDomainsResponse = DescribeDomainsResponse'
    { _ddsrsStatus           :: !Int
    , _ddsrsDomainStatusList :: ![DomainStatus]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDomainsResponse' smart constructor.
describeDomainsResponse :: Int -> DescribeDomainsResponse
describeDomainsResponse pStatus_ =
    DescribeDomainsResponse'
    { _ddsrsStatus = pStatus_
    , _ddsrsDomainStatusList = mempty
    }

-- | FIXME: Undocumented member.
ddsrsStatus :: Lens' DescribeDomainsResponse Int
ddsrsStatus = lens _ddsrsStatus (\ s a -> s{_ddsrsStatus = a});

-- | FIXME: Undocumented member.
ddsrsDomainStatusList :: Lens' DescribeDomainsResponse [DomainStatus]
ddsrsDomainStatusList = lens _ddsrsDomainStatusList (\ s a -> s{_ddsrsDomainStatusList = a}) . _Coerce;
