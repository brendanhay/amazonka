{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudSearch.DescribeDomains
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

-- | Gets information about the search domains owned by this account. Can be
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
    , ddrDomainStatusList
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudSearch.Types

-- | /See:/ 'describeDomains' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddDomainNames'
newtype DescribeDomains = DescribeDomains'{_ddDomainNames :: Maybe [Text]} deriving (Eq, Read, Show)

-- | 'DescribeDomains' smart constructor.
describeDomains :: DescribeDomains
describeDomains = DescribeDomains'{_ddDomainNames = Nothing};

-- | The names of the domains you want to include in the response.
ddDomainNames :: Lens' DescribeDomains [Text]
ddDomainNames = lens _ddDomainNames (\ s a -> s{_ddDomainNames = a}) . _Default;

instance AWSRequest DescribeDomains where
        type Sv DescribeDomains = CloudSearch
        type Rs DescribeDomains = DescribeDomainsResponse
        request = post
        response
          = receiveXMLWrapper "DescribeDomainsResult"
              (\ s h x ->
                 DescribeDomainsResponse' <$>
                   (x .@? "DomainStatusList" .!@ mempty >>=
                      parseXMLList "member"))

instance ToHeaders DescribeDomains where
        toHeaders = const mempty

instance ToPath DescribeDomains where
        toPath = const "/"

instance ToQuery DescribeDomains where
        toQuery DescribeDomains'{..}
          = mconcat
              ["Action" =: ("DescribeDomains" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "DomainNames" =:
                 toQuery (toQueryList "member" <$> _ddDomainNames)]

-- | /See:/ 'describeDomainsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddrDomainStatusList'
newtype DescribeDomainsResponse = DescribeDomainsResponse'{_ddrDomainStatusList :: [DomainStatus]} deriving (Eq, Read, Show)

-- | 'DescribeDomainsResponse' smart constructor.
describeDomainsResponse :: DescribeDomainsResponse
describeDomainsResponse = DescribeDomainsResponse'{_ddrDomainStatusList = mempty};

-- | FIXME: Undocumented member.
ddrDomainStatusList :: Lens' DescribeDomainsResponse [DomainStatus]
ddrDomainStatusList = lens _ddrDomainStatusList (\ s a -> s{_ddrDomainStatusList = a});
