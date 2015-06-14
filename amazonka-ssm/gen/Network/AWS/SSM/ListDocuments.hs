{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SSM.ListDocuments
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

-- | Describes one or more of your configuration documents.
--
-- <http://docs.aws.amazon.com/ssm/latest/APIReference/API_ListDocuments.html>
module Network.AWS.SSM.ListDocuments
    (
    -- * Request
      ListDocuments
    -- ** Request constructor
    , listDocuments
    -- ** Request lenses
    , ldNextToken
    , ldDocumentFilterList
    , ldMaxResults

    -- * Response
    , ListDocumentsResponse
    -- ** Response constructor
    , listDocumentsResponse
    -- ** Response lenses
    , ldrDocumentIdentifiers
    , ldrNextToken
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.SSM.Types

-- | /See:/ 'listDocuments' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldNextToken'
--
-- * 'ldDocumentFilterList'
--
-- * 'ldMaxResults'
data ListDocuments = ListDocuments'{_ldNextToken :: Maybe Text, _ldDocumentFilterList :: List1 DocumentFilter, _ldMaxResults :: Nat} deriving (Eq, Read, Show)

-- | 'ListDocuments' smart constructor.
listDocuments :: NonEmpty DocumentFilter -> Natural -> ListDocuments
listDocuments pDocumentFilterList pMaxResults = ListDocuments'{_ldNextToken = Nothing, _ldDocumentFilterList = _List1 # pDocumentFilterList, _ldMaxResults = _Nat # pMaxResults};

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
ldNextToken :: Lens' ListDocuments (Maybe Text)
ldNextToken = lens _ldNextToken (\ s a -> s{_ldNextToken = a});

-- | One or more filters. Use a filter to return a more specific list of
-- results.
ldDocumentFilterList :: Lens' ListDocuments (NonEmpty DocumentFilter)
ldDocumentFilterList = lens _ldDocumentFilterList (\ s a -> s{_ldDocumentFilterList = a}) . _List1;

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
ldMaxResults :: Lens' ListDocuments Natural
ldMaxResults = lens _ldMaxResults (\ s a -> s{_ldMaxResults = a}) . _Nat;

instance AWSRequest ListDocuments where
        type Sv ListDocuments = SSM
        type Rs ListDocuments = ListDocumentsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListDocumentsResponse' <$>
                   x .?> "DocumentIdentifiers" .!@ mempty <*>
                     x .?> "NextToken")

instance ToHeaders ListDocuments where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.ListDocuments" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListDocuments where
        toJSON ListDocuments'{..}
          = object
              ["NextToken" .= _ldNextToken,
               "DocumentFilterList" .= _ldDocumentFilterList,
               "MaxResults" .= _ldMaxResults]

instance ToPath ListDocuments where
        toPath = const "/"

instance ToQuery ListDocuments where
        toQuery = const mempty

-- | /See:/ 'listDocumentsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldrDocumentIdentifiers'
--
-- * 'ldrNextToken'
data ListDocumentsResponse = ListDocumentsResponse'{_ldrDocumentIdentifiers :: [DocumentIdentifier], _ldrNextToken :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ListDocumentsResponse' smart constructor.
listDocumentsResponse :: ListDocumentsResponse
listDocumentsResponse = ListDocumentsResponse'{_ldrDocumentIdentifiers = mempty, _ldrNextToken = Nothing};

-- | The names of the configuration documents.
ldrDocumentIdentifiers :: Lens' ListDocumentsResponse [DocumentIdentifier]
ldrDocumentIdentifiers = lens _ldrDocumentIdentifiers (\ s a -> s{_ldrDocumentIdentifiers = a});

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
ldrNextToken :: Lens' ListDocumentsResponse (Maybe Text)
ldrNextToken = lens _ldrNextToken (\ s a -> s{_ldrNextToken = a});
