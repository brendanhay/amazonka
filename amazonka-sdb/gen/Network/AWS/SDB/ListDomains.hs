{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.ListDomains
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The @ListDomains@ operation lists all domains associated with the Access
-- Key ID. It returns domain names up to the limit set by
-- <#MaxNumberOfDomains MaxNumberOfDomains>. A <#NextToken NextToken> is
-- returned if there are more than @MaxNumberOfDomains@ domains. Calling
-- @ListDomains@ successive times with the @NextToken@ provided by the
-- operation returns up to @MaxNumberOfDomains@ more domain names with each
-- successive operation call.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonSimpleDB/latest/DeveloperGuide/SDB_API_ListDomains.html AWS API Reference> for ListDomains.
module Network.AWS.SDB.ListDomains
    (
    -- * Creating a Request
      ListDomains
    , listDomains
    -- * Request Lenses
    , ldMaxNumberOfDomains
    , ldNextToken

    -- * Destructuring the Response
    , ListDomainsResponse
    , listDomainsResponse
    -- * Response Lenses
    , ldrsDomainNames
    , ldrsNextToken
    , ldrsStatus
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
          | stop (rs ^. ldrsNextToken) = Nothing
          | stop (rs ^. ldrsDomainNames) = Nothing
          | otherwise =
            Just $ rq & ldNextToken .~ rs ^. ldrsNextToken

instance AWSRequest ListDomains where
        type Sv ListDomains = SDB
        type Rs ListDomains = ListDomainsResponse
        request = postQuery
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
-- * 'ldrsDomainNames'
--
-- * 'ldrsNextToken'
--
-- * 'ldrsStatus'
data ListDomainsResponse = ListDomainsResponse'
    { _ldrsDomainNames :: !(Maybe [Text])
    , _ldrsNextToken   :: !(Maybe Text)
    , _ldrsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListDomainsResponse' smart constructor.
listDomainsResponse :: Int -> ListDomainsResponse
listDomainsResponse pStatus_ =
    ListDomainsResponse'
    { _ldrsDomainNames = Nothing
    , _ldrsNextToken = Nothing
    , _ldrsStatus = pStatus_
    }

-- | A list of domain names that match the expression.
ldrsDomainNames :: Lens' ListDomainsResponse [Text]
ldrsDomainNames = lens _ldrsDomainNames (\ s a -> s{_ldrsDomainNames = a}) . _Default . _Coerce;

-- | An opaque token indicating that there are more domains than the
-- specified @MaxNumberOfDomains@ still available.
ldrsNextToken :: Lens' ListDomainsResponse (Maybe Text)
ldrsNextToken = lens _ldrsNextToken (\ s a -> s{_ldrsNextToken = a});

-- | Undocumented member.
ldrsStatus :: Lens' ListDomainsResponse Int
ldrsStatus = lens _ldrsStatus (\ s a -> s{_ldrsStatus = a});
