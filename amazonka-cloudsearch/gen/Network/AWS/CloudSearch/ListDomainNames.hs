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
-- Module      : Network.AWS.CloudSearch.ListDomainNames
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all search domains owned by an account.
--
-- /See:/ <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_ListDomainNames.html AWS API Reference> for ListDomainNames.
module Network.AWS.CloudSearch.ListDomainNames
    (
    -- * Creating a Request
      listDomainNames
    , ListDomainNames

    -- * Destructuring the Response
    , listDomainNamesResponse
    , ListDomainNamesResponse
    -- * Response Lenses
    , ldnrsDomainNames
    , ldnrsStatus
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.CloudSearch.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listDomainNames' smart constructor.
data ListDomainNames =
    ListDomainNames'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListDomainNames' with the minimum fields required to make a request.
--
listDomainNames
    :: ListDomainNames
listDomainNames = ListDomainNames'

instance AWSRequest ListDomainNames where
        type Rs ListDomainNames = ListDomainNamesResponse
        request = postQuery cloudSearch
        response
          = receiveXMLWrapper "ListDomainNamesResult"
              (\ s h x ->
                 ListDomainNamesResponse' <$>
                   (x .@? "DomainNames" .!@ mempty >>=
                      may (parseXMLMap "entry" "key" "value"))
                     <*> (pure (fromEnum s)))

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

-- | The result of a 'ListDomainNames' request. Contains a list of the
-- domains owned by an account.
--
-- /See:/ 'listDomainNamesResponse' smart constructor.
data ListDomainNamesResponse = ListDomainNamesResponse'
    { _ldnrsDomainNames :: !(Maybe (Map Text Text))
    , _ldnrsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListDomainNamesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldnrsDomainNames'
--
-- * 'ldnrsStatus'
listDomainNamesResponse
    :: Int -- ^ 'ldnrsStatus'
    -> ListDomainNamesResponse
listDomainNamesResponse pStatus_ =
    ListDomainNamesResponse'
    { _ldnrsDomainNames = Nothing
    , _ldnrsStatus = pStatus_
    }

-- | The names of the search domains owned by an account.
ldnrsDomainNames :: Lens' ListDomainNamesResponse (HashMap Text Text)
ldnrsDomainNames = lens _ldnrsDomainNames (\ s a -> s{_ldnrsDomainNames = a}) . _Default . _Map;

-- | The response status code.
ldnrsStatus :: Lens' ListDomainNamesResponse Int
ldnrsStatus = lens _ldnrsStatus (\ s a -> s{_ldnrsStatus = a});
