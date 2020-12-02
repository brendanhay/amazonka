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
-- Module      : Network.AWS.Glue.GetSchemaVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the specified schema by its unique ID assigned when a version of the schema is created or registered. Schema versions in Deleted status will not be included in the results.
module Network.AWS.Glue.GetSchemaVersion
  ( -- * Creating a Request
    getSchemaVersion,
    GetSchemaVersion,

    -- * Request Lenses
    gsvSchemaVersionId,
    gsvSchemaId,
    gsvSchemaVersionNumber,

    -- * Destructuring the Response
    getSchemaVersionResponse,
    GetSchemaVersionResponse,

    -- * Response Lenses
    gsvrsStatus,
    gsvrsSchemaDefinition,
    gsvrsCreatedTime,
    gsvrsDataFormat,
    gsvrsSchemaVersionId,
    gsvrsVersionNumber,
    gsvrsSchemaARN,
    gsvrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getSchemaVersion' smart constructor.
data GetSchemaVersion = GetSchemaVersion'
  { _gsvSchemaVersionId ::
      !(Maybe Text),
    _gsvSchemaId :: !(Maybe SchemaId),
    _gsvSchemaVersionNumber :: !(Maybe SchemaVersionNumber)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSchemaVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsvSchemaVersionId' - The @SchemaVersionId@ of the schema version. This field is required for fetching by schema ID. Either this or the @SchemaId@ wrapper has to be provided.
--
-- * 'gsvSchemaId' - This is a wrapper structure to contain schema identity fields. The structure contains:     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.     * SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
--
-- * 'gsvSchemaVersionNumber' - The version number of the schema.
getSchemaVersion ::
  GetSchemaVersion
getSchemaVersion =
  GetSchemaVersion'
    { _gsvSchemaVersionId = Nothing,
      _gsvSchemaId = Nothing,
      _gsvSchemaVersionNumber = Nothing
    }

-- | The @SchemaVersionId@ of the schema version. This field is required for fetching by schema ID. Either this or the @SchemaId@ wrapper has to be provided.
gsvSchemaVersionId :: Lens' GetSchemaVersion (Maybe Text)
gsvSchemaVersionId = lens _gsvSchemaVersionId (\s a -> s {_gsvSchemaVersionId = a})

-- | This is a wrapper structure to contain schema identity fields. The structure contains:     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.     * SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
gsvSchemaId :: Lens' GetSchemaVersion (Maybe SchemaId)
gsvSchemaId = lens _gsvSchemaId (\s a -> s {_gsvSchemaId = a})

-- | The version number of the schema.
gsvSchemaVersionNumber :: Lens' GetSchemaVersion (Maybe SchemaVersionNumber)
gsvSchemaVersionNumber = lens _gsvSchemaVersionNumber (\s a -> s {_gsvSchemaVersionNumber = a})

instance AWSRequest GetSchemaVersion where
  type Rs GetSchemaVersion = GetSchemaVersionResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          GetSchemaVersionResponse'
            <$> (x .?> "Status")
            <*> (x .?> "SchemaDefinition")
            <*> (x .?> "CreatedTime")
            <*> (x .?> "DataFormat")
            <*> (x .?> "SchemaVersionId")
            <*> (x .?> "VersionNumber")
            <*> (x .?> "SchemaArn")
            <*> (pure (fromEnum s))
      )

instance Hashable GetSchemaVersion

instance NFData GetSchemaVersion

instance ToHeaders GetSchemaVersion where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.GetSchemaVersion" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetSchemaVersion where
  toJSON GetSchemaVersion' {..} =
    object
      ( catMaybes
          [ ("SchemaVersionId" .=) <$> _gsvSchemaVersionId,
            ("SchemaId" .=) <$> _gsvSchemaId,
            ("SchemaVersionNumber" .=) <$> _gsvSchemaVersionNumber
          ]
      )

instance ToPath GetSchemaVersion where
  toPath = const "/"

instance ToQuery GetSchemaVersion where
  toQuery = const mempty

-- | /See:/ 'getSchemaVersionResponse' smart constructor.
data GetSchemaVersionResponse = GetSchemaVersionResponse'
  { _gsvrsStatus ::
      !(Maybe SchemaVersionStatus),
    _gsvrsSchemaDefinition :: !(Maybe Text),
    _gsvrsCreatedTime :: !(Maybe Text),
    _gsvrsDataFormat :: !(Maybe DataFormat),
    _gsvrsSchemaVersionId :: !(Maybe Text),
    _gsvrsVersionNumber :: !(Maybe Nat),
    _gsvrsSchemaARN :: !(Maybe Text),
    _gsvrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSchemaVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsvrsStatus' - The status of the schema version.
--
-- * 'gsvrsSchemaDefinition' - The schema definition for the schema ID.
--
-- * 'gsvrsCreatedTime' - The date and time the schema version was created.
--
-- * 'gsvrsDataFormat' - The data format of the schema definition. Currently only @AVRO@ is supported.
--
-- * 'gsvrsSchemaVersionId' - The @SchemaVersionId@ of the schema version.
--
-- * 'gsvrsVersionNumber' - The version number of the schema.
--
-- * 'gsvrsSchemaARN' - The Amazon Resource Name (ARN) of the schema.
--
-- * 'gsvrsResponseStatus' - -- | The response status code.
getSchemaVersionResponse ::
  -- | 'gsvrsResponseStatus'
  Int ->
  GetSchemaVersionResponse
getSchemaVersionResponse pResponseStatus_ =
  GetSchemaVersionResponse'
    { _gsvrsStatus = Nothing,
      _gsvrsSchemaDefinition = Nothing,
      _gsvrsCreatedTime = Nothing,
      _gsvrsDataFormat = Nothing,
      _gsvrsSchemaVersionId = Nothing,
      _gsvrsVersionNumber = Nothing,
      _gsvrsSchemaARN = Nothing,
      _gsvrsResponseStatus = pResponseStatus_
    }

-- | The status of the schema version.
gsvrsStatus :: Lens' GetSchemaVersionResponse (Maybe SchemaVersionStatus)
gsvrsStatus = lens _gsvrsStatus (\s a -> s {_gsvrsStatus = a})

-- | The schema definition for the schema ID.
gsvrsSchemaDefinition :: Lens' GetSchemaVersionResponse (Maybe Text)
gsvrsSchemaDefinition = lens _gsvrsSchemaDefinition (\s a -> s {_gsvrsSchemaDefinition = a})

-- | The date and time the schema version was created.
gsvrsCreatedTime :: Lens' GetSchemaVersionResponse (Maybe Text)
gsvrsCreatedTime = lens _gsvrsCreatedTime (\s a -> s {_gsvrsCreatedTime = a})

-- | The data format of the schema definition. Currently only @AVRO@ is supported.
gsvrsDataFormat :: Lens' GetSchemaVersionResponse (Maybe DataFormat)
gsvrsDataFormat = lens _gsvrsDataFormat (\s a -> s {_gsvrsDataFormat = a})

-- | The @SchemaVersionId@ of the schema version.
gsvrsSchemaVersionId :: Lens' GetSchemaVersionResponse (Maybe Text)
gsvrsSchemaVersionId = lens _gsvrsSchemaVersionId (\s a -> s {_gsvrsSchemaVersionId = a})

-- | The version number of the schema.
gsvrsVersionNumber :: Lens' GetSchemaVersionResponse (Maybe Natural)
gsvrsVersionNumber = lens _gsvrsVersionNumber (\s a -> s {_gsvrsVersionNumber = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the schema.
gsvrsSchemaARN :: Lens' GetSchemaVersionResponse (Maybe Text)
gsvrsSchemaARN = lens _gsvrsSchemaARN (\s a -> s {_gsvrsSchemaARN = a})

-- | -- | The response status code.
gsvrsResponseStatus :: Lens' GetSchemaVersionResponse Int
gsvrsResponseStatus = lens _gsvrsResponseStatus (\s a -> s {_gsvrsResponseStatus = a})

instance NFData GetSchemaVersionResponse
