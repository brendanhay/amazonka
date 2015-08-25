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
-- Module      : Network.AWS.SSM.ListDocuments
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your configuration documents.
--
-- /See:/ <http://docs.aws.amazon.com/ssm/latest/APIReference/API_ListDocuments.html AWS API Reference> for ListDocuments.
module Network.AWS.SSM.ListDocuments
    (
    -- * Creating a Request
      listDocuments
    , ListDocuments
    -- * Request Lenses
    , ldDocumentFilterList
    , ldNextToken
    , ldMaxResults

    -- * Destructuring the Response
    , listDocumentsResponse
    , ListDocumentsResponse
    -- * Response Lenses
    , ldrsDocumentIdentifiers
    , ldrsNextToken
    , ldrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SSM.Types
import           Network.AWS.SSM.Types.Product

-- | /See:/ 'listDocuments' smart constructor.
data ListDocuments = ListDocuments'
    { _ldDocumentFilterList :: !(Maybe (List1 DocumentFilter))
    , _ldNextToken          :: !(Maybe Text)
    , _ldMaxResults         :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListDocuments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldDocumentFilterList'
--
-- * 'ldNextToken'
--
-- * 'ldMaxResults'
listDocuments
    :: ListDocuments
listDocuments =
    ListDocuments'
    { _ldDocumentFilterList = Nothing
    , _ldNextToken = Nothing
    , _ldMaxResults = Nothing
    }

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
        type Rs ListDocuments = ListDocumentsResponse
        request = postJSON sSM
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
              (catMaybes
                 [("DocumentFilterList" .=) <$> _ldDocumentFilterList,
                  ("NextToken" .=) <$> _ldNextToken,
                  ("MaxResults" .=) <$> _ldMaxResults])

instance ToPath ListDocuments where
        toPath = const "/"

instance ToQuery ListDocuments where
        toQuery = const mempty

-- | /See:/ 'listDocumentsResponse' smart constructor.
data ListDocumentsResponse = ListDocumentsResponse'
    { _ldrsDocumentIdentifiers :: !(Maybe [DocumentIdentifier])
    , _ldrsNextToken           :: !(Maybe Text)
    , _ldrsStatus              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListDocumentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldrsDocumentIdentifiers'
--
-- * 'ldrsNextToken'
--
-- * 'ldrsStatus'
listDocumentsResponse
    :: Int -- ^ 'ldrsStatus'
    -> ListDocumentsResponse
listDocumentsResponse pStatus_ =
    ListDocumentsResponse'
    { _ldrsDocumentIdentifiers = Nothing
    , _ldrsNextToken = Nothing
    , _ldrsStatus = pStatus_
    }

-- | The names of the configuration documents.
ldrsDocumentIdentifiers :: Lens' ListDocumentsResponse [DocumentIdentifier]
ldrsDocumentIdentifiers = lens _ldrsDocumentIdentifiers (\ s a -> s{_ldrsDocumentIdentifiers = a}) . _Default . _Coerce;

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
ldrsNextToken :: Lens' ListDocumentsResponse (Maybe Text)
ldrsNextToken = lens _ldrsNextToken (\ s a -> s{_ldrsNextToken = a});

-- | The response status code.
ldrsStatus :: Lens' ListDocumentsResponse Int
ldrsStatus = lens _ldrsStatus (\ s a -> s{_ldrsStatus = a});
