{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

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
    , ldnrStatus
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listDomainNames' smart constructor.
data ListDomainNames =
    ListDomainNames'
    deriving (Eq,Read,Show)

-- | 'ListDomainNames' smart constructor.
listDomainNames :: ListDomainNames
listDomainNames = ListDomainNames'

instance AWSRequest ListDomainNames where
        type Sv ListDomainNames = CloudSearch
        type Rs ListDomainNames = ListDomainNamesResponse
        request = post
        response
          = receiveXMLWrapper "ListDomainNamesResult"
              (\ s h x ->
                 ListDomainNamesResponse' <$>
                   (x .@? "DomainNames" .!@ mempty >>=
                      may (parseXMLMap "entry" "key" "value"))
                     <*> (pure s))

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

-- | The result of a @ListDomainNames@ request. Contains a list of the
-- domains owned by an account.
--
-- /See:/ 'listDomainNamesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldnrDomainNames'
--
-- * 'ldnrStatus'
data ListDomainNamesResponse = ListDomainNamesResponse'
    { _ldnrDomainNames :: !(Maybe (Map Text Text))
    , _ldnrStatus      :: !Status
    } deriving (Eq,Read,Show)

-- | 'ListDomainNamesResponse' smart constructor.
listDomainNamesResponse :: Status -> ListDomainNamesResponse
listDomainNamesResponse pStatus =
    ListDomainNamesResponse'
    { _ldnrDomainNames = Nothing
    , _ldnrStatus = pStatus
    }

-- | The names of the search domains owned by an account.
ldnrDomainNames :: Lens' ListDomainNamesResponse (HashMap Text Text)
ldnrDomainNames = lens _ldnrDomainNames (\ s a -> s{_ldnrDomainNames = a}) . _Default . _Map;

-- | FIXME: Undocumented member.
ldnrStatus :: Lens' ListDomainNamesResponse Status
ldnrStatus = lens _ldnrStatus (\ s a -> s{_ldnrStatus = a});
