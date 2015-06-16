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
    , ldDocumentFilterList
    , ldNextToken
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
-- * 'ldDocumentFilterList'
--
-- * 'ldNextToken'
--
-- * 'ldMaxResults'
data ListDocuments = ListDocuments'{_ldDocumentFilterList :: Maybe (List1 DocumentFilter), _ldNextToken :: Maybe Text, _ldMaxResults :: Maybe Nat} deriving (Eq, Read, Show)

-- | 'ListDocuments' smart constructor.
listDocuments :: ListDocuments
listDocuments = ListDocuments'{_ldDocumentFilterList = Nothing, _ldNextToken = Nothing, _ldMaxResults = Nothing};

-- | One or more filters. Use a filter to return a more specific list of
-- results.
ldDocumentFilterList :: Lens' ListDocuments (Maybe (NonEmpty DocumentFilter))
ldDocumentFilterList = lens _ldDocumentFilterList (\ s a -> s{_ldDocumentFilterList = a}) . mapping _List1;

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
ldNextToken :: Lens' ListDocuments (Maybe Text)
ldNextToken = lens _ldNextToken (\ s a -> s{_ldNextToken = a});

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
ldMaxResults :: Lens' ListDocuments (Maybe Natural)
ldMaxResults = lens _ldMaxResults (\ s a -> s{_ldMaxResults = a}) . mapping _Nat;

instance AWSRequest ListDocuments where
        type Sv ListDocuments = SSM
        type Rs ListDocuments = ListDocumentsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListDocumentsResponse' <$>
                   (x .?> "DocumentIdentifiers" .!@ mempty) <*>
                     (x .?> "NextToken"))

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
              ["DocumentFilterList" .= _ldDocumentFilterList,
               "NextToken" .= _ldNextToken,
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
data ListDocumentsResponse = ListDocumentsResponse'{_ldrDocumentIdentifiers :: Maybe [DocumentIdentifier], _ldrNextToken :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ListDocumentsResponse' smart constructor.
listDocumentsResponse :: ListDocumentsResponse
listDocumentsResponse = ListDocumentsResponse'{_ldrDocumentIdentifiers = Nothing, _ldrNextToken = Nothing};

-- | The names of the configuration documents.
ldrDocumentIdentifiers :: Lens' ListDocumentsResponse [DocumentIdentifier]
ldrDocumentIdentifiers = lens _ldrDocumentIdentifiers (\ s a -> s{_ldrDocumentIdentifiers = a}) . _Default;

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
ldrNextToken :: Lens' ListDocumentsResponse (Maybe Text)
ldrNextToken = lens _ldrNextToken (\ s a -> s{_ldrNextToken = a});
