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
-- Module      : Network.AWS.Glue.ListSchemaVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of schema versions that you have created, with minimal information. Schema versions in Deleted status will not be included in the results. Empty results will be returned if there are no schema versions available.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Glue.ListSchemaVersions
  ( -- * Creating a Request
    listSchemaVersions,
    ListSchemaVersions,

    -- * Request Lenses
    lsvNextToken,
    lsvMaxResults,
    lsvSchemaId,

    -- * Destructuring the Response
    listSchemaVersionsResponse,
    ListSchemaVersionsResponse,

    -- * Response Lenses
    lsvrsSchemas,
    lsvrsNextToken,
    lsvrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listSchemaVersions' smart constructor.
data ListSchemaVersions = ListSchemaVersions'
  { _lsvNextToken ::
      !(Maybe Text),
    _lsvMaxResults :: !(Maybe Nat),
    _lsvSchemaId :: !SchemaId
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListSchemaVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsvNextToken' - A continuation token, if this is a continuation call.
--
-- * 'lsvMaxResults' - Maximum number of results required per page. If the value is not supplied, this will be defaulted to 25 per page.
--
-- * 'lsvSchemaId' - This is a wrapper structure to contain schema identity fields. The structure contains:     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.     * SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
listSchemaVersions ::
  -- | 'lsvSchemaId'
  SchemaId ->
  ListSchemaVersions
listSchemaVersions pSchemaId_ =
  ListSchemaVersions'
    { _lsvNextToken = Nothing,
      _lsvMaxResults = Nothing,
      _lsvSchemaId = pSchemaId_
    }

-- | A continuation token, if this is a continuation call.
lsvNextToken :: Lens' ListSchemaVersions (Maybe Text)
lsvNextToken = lens _lsvNextToken (\s a -> s {_lsvNextToken = a})

-- | Maximum number of results required per page. If the value is not supplied, this will be defaulted to 25 per page.
lsvMaxResults :: Lens' ListSchemaVersions (Maybe Natural)
lsvMaxResults = lens _lsvMaxResults (\s a -> s {_lsvMaxResults = a}) . mapping _Nat

-- | This is a wrapper structure to contain schema identity fields. The structure contains:     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.     * SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
lsvSchemaId :: Lens' ListSchemaVersions SchemaId
lsvSchemaId = lens _lsvSchemaId (\s a -> s {_lsvSchemaId = a})

instance AWSPager ListSchemaVersions where
  page rq rs
    | stop (rs ^. lsvrsNextToken) = Nothing
    | stop (rs ^. lsvrsSchemas) = Nothing
    | otherwise = Just $ rq & lsvNextToken .~ rs ^. lsvrsNextToken

instance AWSRequest ListSchemaVersions where
  type Rs ListSchemaVersions = ListSchemaVersionsResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          ListSchemaVersionsResponse'
            <$> (x .?> "Schemas" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListSchemaVersions

instance NFData ListSchemaVersions

instance ToHeaders ListSchemaVersions where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.ListSchemaVersions" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListSchemaVersions where
  toJSON ListSchemaVersions' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _lsvNextToken,
            ("MaxResults" .=) <$> _lsvMaxResults,
            Just ("SchemaId" .= _lsvSchemaId)
          ]
      )

instance ToPath ListSchemaVersions where
  toPath = const "/"

instance ToQuery ListSchemaVersions where
  toQuery = const mempty

-- | /See:/ 'listSchemaVersionsResponse' smart constructor.
data ListSchemaVersionsResponse = ListSchemaVersionsResponse'
  { _lsvrsSchemas ::
      !(Maybe [SchemaVersionListItem]),
    _lsvrsNextToken :: !(Maybe Text),
    _lsvrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListSchemaVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsvrsSchemas' - An array of @SchemaVersionList@ objects containing details of each schema version.
--
-- * 'lsvrsNextToken' - A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
--
-- * 'lsvrsResponseStatus' - -- | The response status code.
listSchemaVersionsResponse ::
  -- | 'lsvrsResponseStatus'
  Int ->
  ListSchemaVersionsResponse
listSchemaVersionsResponse pResponseStatus_ =
  ListSchemaVersionsResponse'
    { _lsvrsSchemas = Nothing,
      _lsvrsNextToken = Nothing,
      _lsvrsResponseStatus = pResponseStatus_
    }

-- | An array of @SchemaVersionList@ objects containing details of each schema version.
lsvrsSchemas :: Lens' ListSchemaVersionsResponse [SchemaVersionListItem]
lsvrsSchemas = lens _lsvrsSchemas (\s a -> s {_lsvrsSchemas = a}) . _Default . _Coerce

-- | A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
lsvrsNextToken :: Lens' ListSchemaVersionsResponse (Maybe Text)
lsvrsNextToken = lens _lsvrsNextToken (\s a -> s {_lsvrsNextToken = a})

-- | -- | The response status code.
lsvrsResponseStatus :: Lens' ListSchemaVersionsResponse Int
lsvrsResponseStatus = lens _lsvrsResponseStatus (\s a -> s {_lsvrsResponseStatus = a})

instance NFData ListSchemaVersionsResponse
