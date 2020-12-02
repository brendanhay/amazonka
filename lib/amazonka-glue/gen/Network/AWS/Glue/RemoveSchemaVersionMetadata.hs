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
-- Module      : Network.AWS.Glue.RemoveSchemaVersionMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a key value pair from the schema version metadata for the specified schema version ID.
module Network.AWS.Glue.RemoveSchemaVersionMetadata
  ( -- * Creating a Request
    removeSchemaVersionMetadata,
    RemoveSchemaVersionMetadata,

    -- * Request Lenses
    rsvmSchemaVersionId,
    rsvmSchemaId,
    rsvmSchemaVersionNumber,
    rsvmMetadataKeyValue,

    -- * Destructuring the Response
    removeSchemaVersionMetadataResponse,
    RemoveSchemaVersionMetadataResponse,

    -- * Response Lenses
    rsvmrsRegistryName,
    rsvmrsSchemaName,
    rsvmrsSchemaVersionId,
    rsvmrsVersionNumber,
    rsvmrsSchemaARN,
    rsvmrsMetadataKey,
    rsvmrsMetadataValue,
    rsvmrsLatestVersion,
    rsvmrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'removeSchemaVersionMetadata' smart constructor.
data RemoveSchemaVersionMetadata = RemoveSchemaVersionMetadata'
  { _rsvmSchemaVersionId ::
      !(Maybe Text),
    _rsvmSchemaId :: !(Maybe SchemaId),
    _rsvmSchemaVersionNumber ::
      !(Maybe SchemaVersionNumber),
    _rsvmMetadataKeyValue ::
      !MetadataKeyValuePair
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RemoveSchemaVersionMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsvmSchemaVersionId' - The unique version ID of the schema version.
--
-- * 'rsvmSchemaId' - A wrapper structure that may contain the schema name and Amazon Resource Name (ARN).
--
-- * 'rsvmSchemaVersionNumber' - The version number of the schema.
--
-- * 'rsvmMetadataKeyValue' - The value of the metadata key.
removeSchemaVersionMetadata ::
  -- | 'rsvmMetadataKeyValue'
  MetadataKeyValuePair ->
  RemoveSchemaVersionMetadata
removeSchemaVersionMetadata pMetadataKeyValue_ =
  RemoveSchemaVersionMetadata'
    { _rsvmSchemaVersionId = Nothing,
      _rsvmSchemaId = Nothing,
      _rsvmSchemaVersionNumber = Nothing,
      _rsvmMetadataKeyValue = pMetadataKeyValue_
    }

-- | The unique version ID of the schema version.
rsvmSchemaVersionId :: Lens' RemoveSchemaVersionMetadata (Maybe Text)
rsvmSchemaVersionId = lens _rsvmSchemaVersionId (\s a -> s {_rsvmSchemaVersionId = a})

-- | A wrapper structure that may contain the schema name and Amazon Resource Name (ARN).
rsvmSchemaId :: Lens' RemoveSchemaVersionMetadata (Maybe SchemaId)
rsvmSchemaId = lens _rsvmSchemaId (\s a -> s {_rsvmSchemaId = a})

-- | The version number of the schema.
rsvmSchemaVersionNumber :: Lens' RemoveSchemaVersionMetadata (Maybe SchemaVersionNumber)
rsvmSchemaVersionNumber = lens _rsvmSchemaVersionNumber (\s a -> s {_rsvmSchemaVersionNumber = a})

-- | The value of the metadata key.
rsvmMetadataKeyValue :: Lens' RemoveSchemaVersionMetadata MetadataKeyValuePair
rsvmMetadataKeyValue = lens _rsvmMetadataKeyValue (\s a -> s {_rsvmMetadataKeyValue = a})

instance AWSRequest RemoveSchemaVersionMetadata where
  type
    Rs RemoveSchemaVersionMetadata =
      RemoveSchemaVersionMetadataResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          RemoveSchemaVersionMetadataResponse'
            <$> (x .?> "RegistryName")
            <*> (x .?> "SchemaName")
            <*> (x .?> "SchemaVersionId")
            <*> (x .?> "VersionNumber")
            <*> (x .?> "SchemaArn")
            <*> (x .?> "MetadataKey")
            <*> (x .?> "MetadataValue")
            <*> (x .?> "LatestVersion")
            <*> (pure (fromEnum s))
      )

instance Hashable RemoveSchemaVersionMetadata

instance NFData RemoveSchemaVersionMetadata

instance ToHeaders RemoveSchemaVersionMetadata where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSGlue.RemoveSchemaVersionMetadata" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON RemoveSchemaVersionMetadata where
  toJSON RemoveSchemaVersionMetadata' {..} =
    object
      ( catMaybes
          [ ("SchemaVersionId" .=) <$> _rsvmSchemaVersionId,
            ("SchemaId" .=) <$> _rsvmSchemaId,
            ("SchemaVersionNumber" .=) <$> _rsvmSchemaVersionNumber,
            Just ("MetadataKeyValue" .= _rsvmMetadataKeyValue)
          ]
      )

instance ToPath RemoveSchemaVersionMetadata where
  toPath = const "/"

instance ToQuery RemoveSchemaVersionMetadata where
  toQuery = const mempty

-- | /See:/ 'removeSchemaVersionMetadataResponse' smart constructor.
data RemoveSchemaVersionMetadataResponse = RemoveSchemaVersionMetadataResponse'
  { _rsvmrsRegistryName ::
      !(Maybe Text),
    _rsvmrsSchemaName ::
      !(Maybe Text),
    _rsvmrsSchemaVersionId ::
      !(Maybe Text),
    _rsvmrsVersionNumber ::
      !(Maybe Nat),
    _rsvmrsSchemaARN ::
      !(Maybe Text),
    _rsvmrsMetadataKey ::
      !(Maybe Text),
    _rsvmrsMetadataValue ::
      !(Maybe Text),
    _rsvmrsLatestVersion ::
      !(Maybe Bool),
    _rsvmrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RemoveSchemaVersionMetadataResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsvmrsRegistryName' - The name of the registry.
--
-- * 'rsvmrsSchemaName' - The name of the schema.
--
-- * 'rsvmrsSchemaVersionId' - The version ID for the schema version.
--
-- * 'rsvmrsVersionNumber' - The version number of the schema.
--
-- * 'rsvmrsSchemaARN' - The Amazon Resource Name (ARN) of the schema.
--
-- * 'rsvmrsMetadataKey' - The metadata key.
--
-- * 'rsvmrsMetadataValue' - The value of the metadata key.
--
-- * 'rsvmrsLatestVersion' - The latest version of the schema.
--
-- * 'rsvmrsResponseStatus' - -- | The response status code.
removeSchemaVersionMetadataResponse ::
  -- | 'rsvmrsResponseStatus'
  Int ->
  RemoveSchemaVersionMetadataResponse
removeSchemaVersionMetadataResponse pResponseStatus_ =
  RemoveSchemaVersionMetadataResponse'
    { _rsvmrsRegistryName =
        Nothing,
      _rsvmrsSchemaName = Nothing,
      _rsvmrsSchemaVersionId = Nothing,
      _rsvmrsVersionNumber = Nothing,
      _rsvmrsSchemaARN = Nothing,
      _rsvmrsMetadataKey = Nothing,
      _rsvmrsMetadataValue = Nothing,
      _rsvmrsLatestVersion = Nothing,
      _rsvmrsResponseStatus = pResponseStatus_
    }

-- | The name of the registry.
rsvmrsRegistryName :: Lens' RemoveSchemaVersionMetadataResponse (Maybe Text)
rsvmrsRegistryName = lens _rsvmrsRegistryName (\s a -> s {_rsvmrsRegistryName = a})

-- | The name of the schema.
rsvmrsSchemaName :: Lens' RemoveSchemaVersionMetadataResponse (Maybe Text)
rsvmrsSchemaName = lens _rsvmrsSchemaName (\s a -> s {_rsvmrsSchemaName = a})

-- | The version ID for the schema version.
rsvmrsSchemaVersionId :: Lens' RemoveSchemaVersionMetadataResponse (Maybe Text)
rsvmrsSchemaVersionId = lens _rsvmrsSchemaVersionId (\s a -> s {_rsvmrsSchemaVersionId = a})

-- | The version number of the schema.
rsvmrsVersionNumber :: Lens' RemoveSchemaVersionMetadataResponse (Maybe Natural)
rsvmrsVersionNumber = lens _rsvmrsVersionNumber (\s a -> s {_rsvmrsVersionNumber = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the schema.
rsvmrsSchemaARN :: Lens' RemoveSchemaVersionMetadataResponse (Maybe Text)
rsvmrsSchemaARN = lens _rsvmrsSchemaARN (\s a -> s {_rsvmrsSchemaARN = a})

-- | The metadata key.
rsvmrsMetadataKey :: Lens' RemoveSchemaVersionMetadataResponse (Maybe Text)
rsvmrsMetadataKey = lens _rsvmrsMetadataKey (\s a -> s {_rsvmrsMetadataKey = a})

-- | The value of the metadata key.
rsvmrsMetadataValue :: Lens' RemoveSchemaVersionMetadataResponse (Maybe Text)
rsvmrsMetadataValue = lens _rsvmrsMetadataValue (\s a -> s {_rsvmrsMetadataValue = a})

-- | The latest version of the schema.
rsvmrsLatestVersion :: Lens' RemoveSchemaVersionMetadataResponse (Maybe Bool)
rsvmrsLatestVersion = lens _rsvmrsLatestVersion (\s a -> s {_rsvmrsLatestVersion = a})

-- | -- | The response status code.
rsvmrsResponseStatus :: Lens' RemoveSchemaVersionMetadataResponse Int
rsvmrsResponseStatus = lens _rsvmrsResponseStatus (\s a -> s {_rsvmrsResponseStatus = a})

instance NFData RemoveSchemaVersionMetadataResponse
