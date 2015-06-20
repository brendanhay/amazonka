{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudSearch.ListDomainNames
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

-- | Lists all search domains owned by an account.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_ListDomainNames.html>
module Network.AWS.CloudSearch.ListDomainNames
    (
    -- * Request
      ListDomainNames
    -- ** Request constructor
    , listDomainNames

    -- * Response
    , ListDomainNamesResponse
    -- ** Response constructor
    , listDomainNamesResponse
    -- ** Response lenses
    , ldnrDomainNames
    ) where

import Network.AWS.CloudSearch.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listDomainNames' smart constructor.
data ListDomainNames = ListDomainNames' deriving (Eq, Read, Show)

-- | 'ListDomainNames' smart constructor.
listDomainNames :: ListDomainNames
listDomainNames = ListDomainNames';

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest ListDomainNames where
        type Sv ListDomainNames = CloudSearch
        type Rs ListDomainNames = ListDomainNamesResponse
        request = post
        response
          = receiveXMLWrapper "ListDomainNamesResult"
              (\ s h x ->
                 ListDomainNamesResponse' <$>
                   (x .@? "DomainNames" .!@ mempty >>=
                      may (parseXMLMap "entry" "key" "value")))

instance ToHeaders ListDomainNames where
        toHeaders = const mempty

instance ToPath ListDomainNames where
        toPath = const "/"

instance ToQuery ListDomainNames where
        toQuery
          = const
              (mconcat
                 ["Action" =: ("ListDomainNames" :: ByteString),
                  "Version" =: ("2013-01-01" :: ByteString)])

-- | /See:/ 'listDomainNamesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldnrDomainNames'
newtype ListDomainNamesResponse = ListDomainNamesResponse'{_ldnrDomainNames :: Maybe (Map Text Text)} deriving (Eq, Read, Show)

-- | 'ListDomainNamesResponse' smart constructor.
listDomainNamesResponse :: ListDomainNamesResponse
listDomainNamesResponse = ListDomainNamesResponse'{_ldnrDomainNames = Nothing};

-- | The names of the search domains owned by an account.
ldnrDomainNames :: Lens' ListDomainNamesResponse (HashMap Text Text)
ldnrDomainNames = lens _ldnrDomainNames (\ s a -> s{_ldnrDomainNames = a}) . _Default . _Map;
