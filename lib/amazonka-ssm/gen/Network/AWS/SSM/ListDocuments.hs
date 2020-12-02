{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.ListDocuments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all Systems Manager (SSM) documents in the current AWS account and Region. You can limit the results of this request by using a filter.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListDocuments
  ( -- * Creating a Request
    listDocuments,
    ListDocuments,

    -- * Request Lenses
    ldDocumentFilterList,
    ldFilters,
    ldNextToken,
    ldMaxResults,

    -- * Destructuring the Response
    listDocumentsResponse,
    ListDocumentsResponse,

    -- * Response Lenses
    ldrsDocumentIdentifiers,
    ldrsNextToken,
    ldrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types

-- | /See:/ 'listDocuments' smart constructor.
data ListDocuments = ListDocuments'
  { _ldDocumentFilterList ::
      !(Maybe (List1 DocumentFilter)),
    _ldFilters :: !(Maybe [DocumentKeyValuesFilter]),
    _ldNextToken :: !(Maybe Text),
    _ldMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListDocuments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldDocumentFilterList' - This data type is deprecated. Instead, use @Filters@ .
--
-- * 'ldFilters' - One or more DocumentKeyValuesFilter objects. Use a filter to return a more specific list of results. For keys, you can specify one or more key-value pair tags that have been applied to a document. Other valid keys include @Owner@ , @Name@ , @PlatformTypes@ , @DocumentType@ , and @TargetType@ . For example, to return documents you own use @Key=Owner,Values=Self@ . To specify a custom key-value pair, use the format @Key=tag:tagName,Values=valueName@ .
--
-- * 'ldNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'ldMaxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
listDocuments ::
  ListDocuments
listDocuments =
  ListDocuments'
    { _ldDocumentFilterList = Nothing,
      _ldFilters = Nothing,
      _ldNextToken = Nothing,
      _ldMaxResults = Nothing
    }

-- | This data type is deprecated. Instead, use @Filters@ .
ldDocumentFilterList :: Lens' ListDocuments (Maybe (NonEmpty DocumentFilter))
ldDocumentFilterList = lens _ldDocumentFilterList (\s a -> s {_ldDocumentFilterList = a}) . mapping _List1

-- | One or more DocumentKeyValuesFilter objects. Use a filter to return a more specific list of results. For keys, you can specify one or more key-value pair tags that have been applied to a document. Other valid keys include @Owner@ , @Name@ , @PlatformTypes@ , @DocumentType@ , and @TargetType@ . For example, to return documents you own use @Key=Owner,Values=Self@ . To specify a custom key-value pair, use the format @Key=tag:tagName,Values=valueName@ .
ldFilters :: Lens' ListDocuments [DocumentKeyValuesFilter]
ldFilters = lens _ldFilters (\s a -> s {_ldFilters = a}) . _Default . _Coerce

-- | The token for the next set of items to return. (You received this token from a previous call.)
ldNextToken :: Lens' ListDocuments (Maybe Text)
ldNextToken = lens _ldNextToken (\s a -> s {_ldNextToken = a})

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
ldMaxResults :: Lens' ListDocuments (Maybe Natural)
ldMaxResults = lens _ldMaxResults (\s a -> s {_ldMaxResults = a}) . mapping _Nat

instance AWSPager ListDocuments where
  page rq rs
    | stop (rs ^. ldrsNextToken) = Nothing
    | stop (rs ^. ldrsDocumentIdentifiers) = Nothing
    | otherwise = Just $ rq & ldNextToken .~ rs ^. ldrsNextToken

instance AWSRequest ListDocuments where
  type Rs ListDocuments = ListDocumentsResponse
  request = postJSON ssm
  response =
    receiveJSON
      ( \s h x ->
          ListDocumentsResponse'
            <$> (x .?> "DocumentIdentifiers" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListDocuments

instance NFData ListDocuments

instance ToHeaders ListDocuments where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AmazonSSM.ListDocuments" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListDocuments where
  toJSON ListDocuments' {..} =
    object
      ( catMaybes
          [ ("DocumentFilterList" .=) <$> _ldDocumentFilterList,
            ("Filters" .=) <$> _ldFilters,
            ("NextToken" .=) <$> _ldNextToken,
            ("MaxResults" .=) <$> _ldMaxResults
          ]
      )

instance ToPath ListDocuments where
  toPath = const "/"

instance ToQuery ListDocuments where
  toQuery = const mempty

-- | /See:/ 'listDocumentsResponse' smart constructor.
data ListDocumentsResponse = ListDocumentsResponse'
  { _ldrsDocumentIdentifiers ::
      !(Maybe [DocumentIdentifier]),
    _ldrsNextToken :: !(Maybe Text),
    _ldrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListDocumentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldrsDocumentIdentifiers' - The names of the Systems Manager documents.
--
-- * 'ldrsNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'ldrsResponseStatus' - -- | The response status code.
listDocumentsResponse ::
  -- | 'ldrsResponseStatus'
  Int ->
  ListDocumentsResponse
listDocumentsResponse pResponseStatus_ =
  ListDocumentsResponse'
    { _ldrsDocumentIdentifiers = Nothing,
      _ldrsNextToken = Nothing,
      _ldrsResponseStatus = pResponseStatus_
    }

-- | The names of the Systems Manager documents.
ldrsDocumentIdentifiers :: Lens' ListDocumentsResponse [DocumentIdentifier]
ldrsDocumentIdentifiers = lens _ldrsDocumentIdentifiers (\s a -> s {_ldrsDocumentIdentifiers = a}) . _Default . _Coerce

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
ldrsNextToken :: Lens' ListDocumentsResponse (Maybe Text)
ldrsNextToken = lens _ldrsNextToken (\s a -> s {_ldrsNextToken = a})

-- | -- | The response status code.
ldrsResponseStatus :: Lens' ListDocumentsResponse Int
ldrsResponseStatus = lens _ldrsResponseStatus (\s a -> s {_ldrsResponseStatus = a})

instance NFData ListDocumentsResponse
