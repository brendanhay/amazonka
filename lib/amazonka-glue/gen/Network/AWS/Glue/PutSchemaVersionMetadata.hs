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
-- Module      : Network.AWS.Glue.PutSchemaVersionMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Puts the metadata key value pair for a specified schema version ID. A maximum of 10 key value pairs will be allowed per schema version. They can be added over one or more calls.
module Network.AWS.Glue.PutSchemaVersionMetadata
  ( -- * Creating a Request
    putSchemaVersionMetadata,
    PutSchemaVersionMetadata,

    -- * Request Lenses
    psvmSchemaVersionId,
    psvmSchemaId,
    psvmSchemaVersionNumber,
    psvmMetadataKeyValue,

    -- * Destructuring the Response
    putSchemaVersionMetadataResponse,
    PutSchemaVersionMetadataResponse,

    -- * Response Lenses
    psvmrsRegistryName,
    psvmrsSchemaName,
    psvmrsSchemaVersionId,
    psvmrsVersionNumber,
    psvmrsSchemaARN,
    psvmrsMetadataKey,
    psvmrsMetadataValue,
    psvmrsLatestVersion,
    psvmrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putSchemaVersionMetadata' smart constructor.
data PutSchemaVersionMetadata = PutSchemaVersionMetadata'
  { _psvmSchemaVersionId ::
      !(Maybe Text),
    _psvmSchemaId :: !(Maybe SchemaId),
    _psvmSchemaVersionNumber ::
      !(Maybe SchemaVersionNumber),
    _psvmMetadataKeyValue ::
      !MetadataKeyValuePair
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutSchemaVersionMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psvmSchemaVersionId' - The unique version ID of the schema version.
--
-- * 'psvmSchemaId' - The unique ID for the schema.
--
-- * 'psvmSchemaVersionNumber' - The version number of the schema.
--
-- * 'psvmMetadataKeyValue' - The metadata key's corresponding value.
putSchemaVersionMetadata ::
  -- | 'psvmMetadataKeyValue'
  MetadataKeyValuePair ->
  PutSchemaVersionMetadata
putSchemaVersionMetadata pMetadataKeyValue_ =
  PutSchemaVersionMetadata'
    { _psvmSchemaVersionId = Nothing,
      _psvmSchemaId = Nothing,
      _psvmSchemaVersionNumber = Nothing,
      _psvmMetadataKeyValue = pMetadataKeyValue_
    }

-- | The unique version ID of the schema version.
psvmSchemaVersionId :: Lens' PutSchemaVersionMetadata (Maybe Text)
psvmSchemaVersionId = lens _psvmSchemaVersionId (\s a -> s {_psvmSchemaVersionId = a})

-- | The unique ID for the schema.
psvmSchemaId :: Lens' PutSchemaVersionMetadata (Maybe SchemaId)
psvmSchemaId = lens _psvmSchemaId (\s a -> s {_psvmSchemaId = a})

-- | The version number of the schema.
psvmSchemaVersionNumber :: Lens' PutSchemaVersionMetadata (Maybe SchemaVersionNumber)
psvmSchemaVersionNumber = lens _psvmSchemaVersionNumber (\s a -> s {_psvmSchemaVersionNumber = a})

-- | The metadata key's corresponding value.
psvmMetadataKeyValue :: Lens' PutSchemaVersionMetadata MetadataKeyValuePair
psvmMetadataKeyValue = lens _psvmMetadataKeyValue (\s a -> s {_psvmMetadataKeyValue = a})

instance AWSRequest PutSchemaVersionMetadata where
  type Rs PutSchemaVersionMetadata = PutSchemaVersionMetadataResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          PutSchemaVersionMetadataResponse'
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

instance Hashable PutSchemaVersionMetadata

instance NFData PutSchemaVersionMetadata

instance ToHeaders PutSchemaVersionMetadata where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSGlue.PutSchemaVersionMetadata" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON PutSchemaVersionMetadata where
  toJSON PutSchemaVersionMetadata' {..} =
    object
      ( catMaybes
          [ ("SchemaVersionId" .=) <$> _psvmSchemaVersionId,
            ("SchemaId" .=) <$> _psvmSchemaId,
            ("SchemaVersionNumber" .=) <$> _psvmSchemaVersionNumber,
            Just ("MetadataKeyValue" .= _psvmMetadataKeyValue)
          ]
      )

instance ToPath PutSchemaVersionMetadata where
  toPath = const "/"

instance ToQuery PutSchemaVersionMetadata where
  toQuery = const mempty

-- | /See:/ 'putSchemaVersionMetadataResponse' smart constructor.
data PutSchemaVersionMetadataResponse = PutSchemaVersionMetadataResponse'
  { _psvmrsRegistryName ::
      !(Maybe Text),
    _psvmrsSchemaName ::
      !(Maybe Text),
    _psvmrsSchemaVersionId ::
      !(Maybe Text),
    _psvmrsVersionNumber ::
      !(Maybe Nat),
    _psvmrsSchemaARN ::
      !(Maybe Text),
    _psvmrsMetadataKey ::
      !(Maybe Text),
    _psvmrsMetadataValue ::
      !(Maybe Text),
    _psvmrsLatestVersion ::
      !(Maybe Bool),
    _psvmrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutSchemaVersionMetadataResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psvmrsRegistryName' - The name for the registry.
--
-- * 'psvmrsSchemaName' - The name for the schema.
--
-- * 'psvmrsSchemaVersionId' - The unique version ID of the schema version.
--
-- * 'psvmrsVersionNumber' - The version number of the schema.
--
-- * 'psvmrsSchemaARN' - The Amazon Resource Name (ARN) for the schema.
--
-- * 'psvmrsMetadataKey' - The metadata key.
--
-- * 'psvmrsMetadataValue' - The value of the metadata key.
--
-- * 'psvmrsLatestVersion' - The latest version of the schema.
--
-- * 'psvmrsResponseStatus' - -- | The response status code.
putSchemaVersionMetadataResponse ::
  -- | 'psvmrsResponseStatus'
  Int ->
  PutSchemaVersionMetadataResponse
putSchemaVersionMetadataResponse pResponseStatus_ =
  PutSchemaVersionMetadataResponse'
    { _psvmrsRegistryName = Nothing,
      _psvmrsSchemaName = Nothing,
      _psvmrsSchemaVersionId = Nothing,
      _psvmrsVersionNumber = Nothing,
      _psvmrsSchemaARN = Nothing,
      _psvmrsMetadataKey = Nothing,
      _psvmrsMetadataValue = Nothing,
      _psvmrsLatestVersion = Nothing,
      _psvmrsResponseStatus = pResponseStatus_
    }

-- | The name for the registry.
psvmrsRegistryName :: Lens' PutSchemaVersionMetadataResponse (Maybe Text)
psvmrsRegistryName = lens _psvmrsRegistryName (\s a -> s {_psvmrsRegistryName = a})

-- | The name for the schema.
psvmrsSchemaName :: Lens' PutSchemaVersionMetadataResponse (Maybe Text)
psvmrsSchemaName = lens _psvmrsSchemaName (\s a -> s {_psvmrsSchemaName = a})

-- | The unique version ID of the schema version.
psvmrsSchemaVersionId :: Lens' PutSchemaVersionMetadataResponse (Maybe Text)
psvmrsSchemaVersionId = lens _psvmrsSchemaVersionId (\s a -> s {_psvmrsSchemaVersionId = a})

-- | The version number of the schema.
psvmrsVersionNumber :: Lens' PutSchemaVersionMetadataResponse (Maybe Natural)
psvmrsVersionNumber = lens _psvmrsVersionNumber (\s a -> s {_psvmrsVersionNumber = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) for the schema.
psvmrsSchemaARN :: Lens' PutSchemaVersionMetadataResponse (Maybe Text)
psvmrsSchemaARN = lens _psvmrsSchemaARN (\s a -> s {_psvmrsSchemaARN = a})

-- | The metadata key.
psvmrsMetadataKey :: Lens' PutSchemaVersionMetadataResponse (Maybe Text)
psvmrsMetadataKey = lens _psvmrsMetadataKey (\s a -> s {_psvmrsMetadataKey = a})

-- | The value of the metadata key.
psvmrsMetadataValue :: Lens' PutSchemaVersionMetadataResponse (Maybe Text)
psvmrsMetadataValue = lens _psvmrsMetadataValue (\s a -> s {_psvmrsMetadataValue = a})

-- | The latest version of the schema.
psvmrsLatestVersion :: Lens' PutSchemaVersionMetadataResponse (Maybe Bool)
psvmrsLatestVersion = lens _psvmrsLatestVersion (\s a -> s {_psvmrsLatestVersion = a})

-- | -- | The response status code.
psvmrsResponseStatus :: Lens' PutSchemaVersionMetadataResponse Int
psvmrsResponseStatus = lens _psvmrsResponseStatus (\s a -> s {_psvmrsResponseStatus = a})

instance NFData PutSchemaVersionMetadataResponse
