{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.ListDocuments
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your configuration documents.
--
-- <http://docs.aws.amazon.com/ssm/latest/APIReference/API_ListDocuments.html>
module Network.AWS.SSM.ListDocuments
    (
    -- * Request
      ListDocuments
    -- ** Request constructor
    , listDocuments
    -- ** Request lenses
    , ldrqDocumentFilterList
    , ldrqNextToken
    , ldrqMaxResults

    -- * Response
    , ListDocumentsResponse
    -- ** Response constructor
    , listDocumentsResponse
    -- ** Response lenses
    , ldrsDocumentIdentifiers
    , ldrsNextToken
    , ldrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SSM.Types

-- | /See:/ 'listDocuments' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldrqDocumentFilterList'
--
-- * 'ldrqNextToken'
--
-- * 'ldrqMaxResults'
data ListDocuments = ListDocuments'
    { _ldrqDocumentFilterList :: !(Maybe (List1 DocumentFilter))
    , _ldrqNextToken          :: !(Maybe Text)
    , _ldrqMaxResults         :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListDocuments' smart constructor.
listDocuments :: ListDocuments
listDocuments =
    ListDocuments'
    { _ldrqDocumentFilterList = Nothing
    , _ldrqNextToken = Nothing
    , _ldrqMaxResults = Nothing
    }

-- | One or more filters. Use a filter to return a more specific list of
-- results.
ldrqDocumentFilterList :: Lens' ListDocuments (Maybe (NonEmpty DocumentFilter))
ldrqDocumentFilterList = lens _ldrqDocumentFilterList (\ s a -> s{_ldrqDocumentFilterList = a}) . mapping _List1;

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
ldrqNextToken :: Lens' ListDocuments (Maybe Text)
ldrqNextToken = lens _ldrqNextToken (\ s a -> s{_ldrqNextToken = a});

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
ldrqMaxResults :: Lens' ListDocuments (Maybe Natural)
ldrqMaxResults = lens _ldrqMaxResults (\ s a -> s{_ldrqMaxResults = a}) . mapping _Nat;

instance AWSRequest ListDocuments where
        type Sv ListDocuments = SSM
        type Rs ListDocuments = ListDocumentsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListDocumentsResponse' <$>
                   (x .?> "DocumentIdentifiers" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

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
              ["DocumentFilterList" .= _ldrqDocumentFilterList,
               "NextToken" .= _ldrqNextToken,
               "MaxResults" .= _ldrqMaxResults]

instance ToPath ListDocuments where
        toPath = const "/"

instance ToQuery ListDocuments where
        toQuery = const mempty

-- | /See:/ 'listDocumentsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldrsDocumentIdentifiers'
--
-- * 'ldrsNextToken'
--
-- * 'ldrsStatus'
data ListDocumentsResponse = ListDocumentsResponse'
    { _ldrsDocumentIdentifiers :: !(Maybe [DocumentIdentifier])
    , _ldrsNextToken           :: !(Maybe Text)
    , _ldrsStatus              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListDocumentsResponse' smart constructor.
listDocumentsResponse :: Int -> ListDocumentsResponse
listDocumentsResponse pStatus_ =
    ListDocumentsResponse'
    { _ldrsDocumentIdentifiers = Nothing
    , _ldrsNextToken = Nothing
    , _ldrsStatus = pStatus_
    }

-- | The names of the configuration documents.
ldrsDocumentIdentifiers :: Lens' ListDocumentsResponse [DocumentIdentifier]
ldrsDocumentIdentifiers = lens _ldrsDocumentIdentifiers (\ s a -> s{_ldrsDocumentIdentifiers = a}) . _Default;

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
ldrsNextToken :: Lens' ListDocumentsResponse (Maybe Text)
ldrsNextToken = lens _ldrsNextToken (\ s a -> s{_ldrsNextToken = a});

-- | FIXME: Undocumented member.
ldrsStatus :: Lens' ListDocumentsResponse Int
ldrsStatus = lens _ldrsStatus (\ s a -> s{_ldrsStatus = a});
