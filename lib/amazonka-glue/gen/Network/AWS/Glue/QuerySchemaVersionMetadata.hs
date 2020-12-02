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
-- Module      : Network.AWS.Glue.QuerySchemaVersionMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Queries for the schema version metadata information.
module Network.AWS.Glue.QuerySchemaVersionMetadata
  ( -- * Creating a Request
    querySchemaVersionMetadata,
    QuerySchemaVersionMetadata,

    -- * Request Lenses
    qsvmSchemaVersionId,
    qsvmSchemaId,
    qsvmNextToken,
    qsvmMetadataList,
    qsvmSchemaVersionNumber,
    qsvmMaxResults,

    -- * Destructuring the Response
    querySchemaVersionMetadataResponse,
    QuerySchemaVersionMetadataResponse,

    -- * Response Lenses
    qsvmrsSchemaVersionId,
    qsvmrsNextToken,
    qsvmrsMetadataInfoMap,
    qsvmrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'querySchemaVersionMetadata' smart constructor.
data QuerySchemaVersionMetadata = QuerySchemaVersionMetadata'
  { _qsvmSchemaVersionId ::
      !(Maybe Text),
    _qsvmSchemaId :: !(Maybe SchemaId),
    _qsvmNextToken :: !(Maybe Text),
    _qsvmMetadataList ::
      !(Maybe [MetadataKeyValuePair]),
    _qsvmSchemaVersionNumber ::
      !(Maybe SchemaVersionNumber),
    _qsvmMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'QuerySchemaVersionMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qsvmSchemaVersionId' - The unique version ID of the schema version.
--
-- * 'qsvmSchemaId' - A wrapper structure that may contain the schema name and Amazon Resource Name (ARN).
--
-- * 'qsvmNextToken' - A continuation token, if this is a continuation call.
--
-- * 'qsvmMetadataList' - Search key-value pairs for metadata, if they are not provided all the metadata information will be fetched.
--
-- * 'qsvmSchemaVersionNumber' - The version number of the schema.
--
-- * 'qsvmMaxResults' - Maximum number of results required per page. If the value is not supplied, this will be defaulted to 25 per page.
querySchemaVersionMetadata ::
  QuerySchemaVersionMetadata
querySchemaVersionMetadata =
  QuerySchemaVersionMetadata'
    { _qsvmSchemaVersionId = Nothing,
      _qsvmSchemaId = Nothing,
      _qsvmNextToken = Nothing,
      _qsvmMetadataList = Nothing,
      _qsvmSchemaVersionNumber = Nothing,
      _qsvmMaxResults = Nothing
    }

-- | The unique version ID of the schema version.
qsvmSchemaVersionId :: Lens' QuerySchemaVersionMetadata (Maybe Text)
qsvmSchemaVersionId = lens _qsvmSchemaVersionId (\s a -> s {_qsvmSchemaVersionId = a})

-- | A wrapper structure that may contain the schema name and Amazon Resource Name (ARN).
qsvmSchemaId :: Lens' QuerySchemaVersionMetadata (Maybe SchemaId)
qsvmSchemaId = lens _qsvmSchemaId (\s a -> s {_qsvmSchemaId = a})

-- | A continuation token, if this is a continuation call.
qsvmNextToken :: Lens' QuerySchemaVersionMetadata (Maybe Text)
qsvmNextToken = lens _qsvmNextToken (\s a -> s {_qsvmNextToken = a})

-- | Search key-value pairs for metadata, if they are not provided all the metadata information will be fetched.
qsvmMetadataList :: Lens' QuerySchemaVersionMetadata [MetadataKeyValuePair]
qsvmMetadataList = lens _qsvmMetadataList (\s a -> s {_qsvmMetadataList = a}) . _Default . _Coerce

-- | The version number of the schema.
qsvmSchemaVersionNumber :: Lens' QuerySchemaVersionMetadata (Maybe SchemaVersionNumber)
qsvmSchemaVersionNumber = lens _qsvmSchemaVersionNumber (\s a -> s {_qsvmSchemaVersionNumber = a})

-- | Maximum number of results required per page. If the value is not supplied, this will be defaulted to 25 per page.
qsvmMaxResults :: Lens' QuerySchemaVersionMetadata (Maybe Natural)
qsvmMaxResults = lens _qsvmMaxResults (\s a -> s {_qsvmMaxResults = a}) . mapping _Nat

instance AWSRequest QuerySchemaVersionMetadata where
  type
    Rs QuerySchemaVersionMetadata =
      QuerySchemaVersionMetadataResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          QuerySchemaVersionMetadataResponse'
            <$> (x .?> "SchemaVersionId")
            <*> (x .?> "NextToken")
            <*> (x .?> "MetadataInfoMap" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable QuerySchemaVersionMetadata

instance NFData QuerySchemaVersionMetadata

instance ToHeaders QuerySchemaVersionMetadata where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSGlue.QuerySchemaVersionMetadata" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON QuerySchemaVersionMetadata where
  toJSON QuerySchemaVersionMetadata' {..} =
    object
      ( catMaybes
          [ ("SchemaVersionId" .=) <$> _qsvmSchemaVersionId,
            ("SchemaId" .=) <$> _qsvmSchemaId,
            ("NextToken" .=) <$> _qsvmNextToken,
            ("MetadataList" .=) <$> _qsvmMetadataList,
            ("SchemaVersionNumber" .=) <$> _qsvmSchemaVersionNumber,
            ("MaxResults" .=) <$> _qsvmMaxResults
          ]
      )

instance ToPath QuerySchemaVersionMetadata where
  toPath = const "/"

instance ToQuery QuerySchemaVersionMetadata where
  toQuery = const mempty

-- | /See:/ 'querySchemaVersionMetadataResponse' smart constructor.
data QuerySchemaVersionMetadataResponse = QuerySchemaVersionMetadataResponse'
  { _qsvmrsSchemaVersionId ::
      !(Maybe Text),
    _qsvmrsNextToken ::
      !(Maybe Text),
    _qsvmrsMetadataInfoMap ::
      !( Maybe
           ( Map
               Text
               (MetadataInfo)
           )
       ),
    _qsvmrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'QuerySchemaVersionMetadataResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qsvmrsSchemaVersionId' - The unique version ID of the schema version.
--
-- * 'qsvmrsNextToken' - A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
--
-- * 'qsvmrsMetadataInfoMap' - A map of a metadata key and associated values.
--
-- * 'qsvmrsResponseStatus' - -- | The response status code.
querySchemaVersionMetadataResponse ::
  -- | 'qsvmrsResponseStatus'
  Int ->
  QuerySchemaVersionMetadataResponse
querySchemaVersionMetadataResponse pResponseStatus_ =
  QuerySchemaVersionMetadataResponse'
    { _qsvmrsSchemaVersionId =
        Nothing,
      _qsvmrsNextToken = Nothing,
      _qsvmrsMetadataInfoMap = Nothing,
      _qsvmrsResponseStatus = pResponseStatus_
    }

-- | The unique version ID of the schema version.
qsvmrsSchemaVersionId :: Lens' QuerySchemaVersionMetadataResponse (Maybe Text)
qsvmrsSchemaVersionId = lens _qsvmrsSchemaVersionId (\s a -> s {_qsvmrsSchemaVersionId = a})

-- | A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
qsvmrsNextToken :: Lens' QuerySchemaVersionMetadataResponse (Maybe Text)
qsvmrsNextToken = lens _qsvmrsNextToken (\s a -> s {_qsvmrsNextToken = a})

-- | A map of a metadata key and associated values.
qsvmrsMetadataInfoMap :: Lens' QuerySchemaVersionMetadataResponse (HashMap Text (MetadataInfo))
qsvmrsMetadataInfoMap = lens _qsvmrsMetadataInfoMap (\s a -> s {_qsvmrsMetadataInfoMap = a}) . _Default . _Map

-- | -- | The response status code.
qsvmrsResponseStatus :: Lens' QuerySchemaVersionMetadataResponse Int
qsvmrsResponseStatus = lens _qsvmrsResponseStatus (\s a -> s {_qsvmrsResponseStatus = a})

instance NFData QuerySchemaVersionMetadataResponse
