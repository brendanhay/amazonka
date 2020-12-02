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
-- Module      : Network.AWS.Glue.ListSchemas
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of schemas with minimal details. Schemas in Deleting status will not be included in the results. Empty results will be returned if there are no schemas available.
--
--
-- When the @RegistryId@ is not provided, all the schemas across registries will be part of the API response.
--
--
-- This operation returns paginated results.
module Network.AWS.Glue.ListSchemas
  ( -- * Creating a Request
    listSchemas,
    ListSchemas,

    -- * Request Lenses
    lsRegistryId,
    lsNextToken,
    lsMaxResults,

    -- * Destructuring the Response
    listSchemasResponse,
    ListSchemasResponse,

    -- * Response Lenses
    lsrsSchemas,
    lsrsNextToken,
    lsrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listSchemas' smart constructor.
data ListSchemas = ListSchemas'
  { _lsRegistryId ::
      !(Maybe RegistryId),
    _lsNextToken :: !(Maybe Text),
    _lsMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListSchemas' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsRegistryId' - A wrapper structure that may contain the registry name and Amazon Resource Name (ARN).
--
-- * 'lsNextToken' - A continuation token, if this is a continuation call.
--
-- * 'lsMaxResults' - Maximum number of results required per page. If the value is not supplied, this will be defaulted to 25 per page.
listSchemas ::
  ListSchemas
listSchemas =
  ListSchemas'
    { _lsRegistryId = Nothing,
      _lsNextToken = Nothing,
      _lsMaxResults = Nothing
    }

-- | A wrapper structure that may contain the registry name and Amazon Resource Name (ARN).
lsRegistryId :: Lens' ListSchemas (Maybe RegistryId)
lsRegistryId = lens _lsRegistryId (\s a -> s {_lsRegistryId = a})

-- | A continuation token, if this is a continuation call.
lsNextToken :: Lens' ListSchemas (Maybe Text)
lsNextToken = lens _lsNextToken (\s a -> s {_lsNextToken = a})

-- | Maximum number of results required per page. If the value is not supplied, this will be defaulted to 25 per page.
lsMaxResults :: Lens' ListSchemas (Maybe Natural)
lsMaxResults = lens _lsMaxResults (\s a -> s {_lsMaxResults = a}) . mapping _Nat

instance AWSPager ListSchemas where
  page rq rs
    | stop (rs ^. lsrsNextToken) = Nothing
    | stop (rs ^. lsrsSchemas) = Nothing
    | otherwise = Just $ rq & lsNextToken .~ rs ^. lsrsNextToken

instance AWSRequest ListSchemas where
  type Rs ListSchemas = ListSchemasResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          ListSchemasResponse'
            <$> (x .?> "Schemas" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListSchemas

instance NFData ListSchemas

instance ToHeaders ListSchemas where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.ListSchemas" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListSchemas where
  toJSON ListSchemas' {..} =
    object
      ( catMaybes
          [ ("RegistryId" .=) <$> _lsRegistryId,
            ("NextToken" .=) <$> _lsNextToken,
            ("MaxResults" .=) <$> _lsMaxResults
          ]
      )

instance ToPath ListSchemas where
  toPath = const "/"

instance ToQuery ListSchemas where
  toQuery = const mempty

-- | /See:/ 'listSchemasResponse' smart constructor.
data ListSchemasResponse = ListSchemasResponse'
  { _lsrsSchemas ::
      !(Maybe [SchemaListItem]),
    _lsrsNextToken :: !(Maybe Text),
    _lsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListSchemasResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsrsSchemas' - An array of @SchemaListItem@ objects containing details of each schema.
--
-- * 'lsrsNextToken' - A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
--
-- * 'lsrsResponseStatus' - -- | The response status code.
listSchemasResponse ::
  -- | 'lsrsResponseStatus'
  Int ->
  ListSchemasResponse
listSchemasResponse pResponseStatus_ =
  ListSchemasResponse'
    { _lsrsSchemas = Nothing,
      _lsrsNextToken = Nothing,
      _lsrsResponseStatus = pResponseStatus_
    }

-- | An array of @SchemaListItem@ objects containing details of each schema.
lsrsSchemas :: Lens' ListSchemasResponse [SchemaListItem]
lsrsSchemas = lens _lsrsSchemas (\s a -> s {_lsrsSchemas = a}) . _Default . _Coerce

-- | A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
lsrsNextToken :: Lens' ListSchemasResponse (Maybe Text)
lsrsNextToken = lens _lsrsNextToken (\s a -> s {_lsrsNextToken = a})

-- | -- | The response status code.
lsrsResponseStatus :: Lens' ListSchemasResponse Int
lsrsResponseStatus = lens _lsrsResponseStatus (\s a -> s {_lsrsResponseStatus = a})

instance NFData ListSchemasResponse
