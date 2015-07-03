{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SDB.ListDomains
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | The @ListDomains@ operation lists all domains associated with the Access
-- Key ID. It returns domain names up to the limit set by
-- <#MaxNumberOfDomains MaxNumberOfDomains>. A <#NextToken NextToken> is
-- returned if there are more than @MaxNumberOfDomains@ domains. Calling
-- @ListDomains@ successive times with the @NextToken@ provided by the
-- operation returns up to @MaxNumberOfDomains@ more domain names with each
-- successive operation call.
--
-- <http://docs.aws.amazon.com/AmazonSimpleDB/latest/DeveloperGuide/SDB_API_ListDomains.html>
module Network.AWS.SDB.ListDomains
    (
    -- * Request
      ListDomains
    -- ** Request constructor
    , listDomains
    -- ** Request lenses
    , ldMaxNumberOfDomains
    , ldNextToken

    -- * Response
    , ListDomainsResponse
    -- ** Response constructor
    , listDomainsResponse
    -- ** Response lenses
    , ldrDomainNames
    , ldrNextToken
    , ldrStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SDB.Types

-- | /See:/ 'listDomains' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldMaxNumberOfDomains'
--
-- * 'ldNextToken'
data ListDomains = ListDomains'
    { _ldMaxNumberOfDomains :: !(Maybe Int)
    , _ldNextToken          :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'ListDomains' smart constructor.
listDomains :: ListDomains
listDomains =
    ListDomains'
    { _ldMaxNumberOfDomains = Nothing
    , _ldNextToken = Nothing
    }

-- | The maximum number of domain names you want returned. The range is 1 to
-- 100. The default setting is 100.
ldMaxNumberOfDomains :: Lens' ListDomains (Maybe Int)
ldMaxNumberOfDomains = lens _ldMaxNumberOfDomains (\ s a -> s{_ldMaxNumberOfDomains = a});

-- | A string informing Amazon SimpleDB where to start the next list of
-- domain names.
ldNextToken :: Lens' ListDomains (Maybe Text)
ldNextToken = lens _ldNextToken (\ s a -> s{_ldNextToken = a});

instance AWSPager ListDomains where
        page rq rs
          | stop (rs ^. ldrNextToken) = Nothing
          | stop (rs ^. ldrDomainNames) = Nothing
          | otherwise =
            Just $ rq & ldNextToken .~ rs ^. ldrNextToken

instance AWSRequest ListDomains where
        type Sv ListDomains = SDB
        type Rs ListDomains = ListDomainsResponse
        request = post
        response
          = receiveXMLWrapper "ListDomainsResult"
              (\ s h x ->
                 ListDomainsResponse' <$>
                   (may (parseXMLList "DomainName") x) <*>
                     (x .@? "NextToken")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListDomains where
        toHeaders = const mempty

instance ToPath ListDomains where
        toPath = const "/"

instance ToQuery ListDomains where
        toQuery ListDomains'{..}
          = mconcat
              ["Action" =: ("ListDomains" :: ByteString),
               "Version" =: ("2009-04-15" :: ByteString),
               "MaxNumberOfDomains" =: _ldMaxNumberOfDomains,
               "NextToken" =: _ldNextToken]

-- | /See:/ 'listDomainsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldrDomainNames'
--
-- * 'ldrNextToken'
--
-- * 'ldrStatus'
data ListDomainsResponse = ListDomainsResponse'
    { _ldrDomainNames :: !(Maybe [Text])
    , _ldrNextToken   :: !(Maybe Text)
    , _ldrStatus      :: !Int
    } deriving (Eq,Read,Show)

-- | 'ListDomainsResponse' smart constructor.
listDomainsResponse :: Int -> ListDomainsResponse
listDomainsResponse pStatus =
    ListDomainsResponse'
    { _ldrDomainNames = Nothing
    , _ldrNextToken = Nothing
    , _ldrStatus = pStatus
    }

-- | A list of domain names that match the expression.
ldrDomainNames :: Lens' ListDomainsResponse [Text]
ldrDomainNames = lens _ldrDomainNames (\ s a -> s{_ldrDomainNames = a}) . _Default;

-- | An opaque token indicating that there are more domains than the
-- specified @MaxNumberOfDomains@ still available.
ldrNextToken :: Lens' ListDomainsResponse (Maybe Text)
ldrNextToken = lens _ldrNextToken (\ s a -> s{_ldrNextToken = a});

-- | FIXME: Undocumented member.
ldrStatus :: Lens' ListDomainsResponse Int
ldrStatus = lens _ldrStatus (\ s a -> s{_ldrStatus = a});
