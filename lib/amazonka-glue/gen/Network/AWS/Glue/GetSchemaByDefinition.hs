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
-- Module      : Network.AWS.Glue.GetSchemaByDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a schema by the @SchemaDefinition@ . The schema definition is sent to the Schema Registry, canonicalized, and hashed. If the hash is matched within the scope of the @SchemaName@ or ARN (or the default registry, if none is supplied), that schema’s metadata is returned. Otherwise, a 404 or NotFound error is returned. Schema versions in @Deleted@ statuses will not be included in the results.
module Network.AWS.Glue.GetSchemaByDefinition
  ( -- * Creating a Request
    getSchemaByDefinition,
    GetSchemaByDefinition,

    -- * Request Lenses
    gsbdSchemaId,
    gsbdSchemaDefinition,

    -- * Destructuring the Response
    getSchemaByDefinitionResponse,
    GetSchemaByDefinitionResponse,

    -- * Response Lenses
    gsbdrsStatus,
    gsbdrsCreatedTime,
    gsbdrsDataFormat,
    gsbdrsSchemaVersionId,
    gsbdrsSchemaARN,
    gsbdrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getSchemaByDefinition' smart constructor.
data GetSchemaByDefinition = GetSchemaByDefinition'
  { _gsbdSchemaId ::
      !SchemaId,
    _gsbdSchemaDefinition :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSchemaByDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsbdSchemaId' - This is a wrapper structure to contain schema identity fields. The structure contains:     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.     * SchemaId$SchemaName: The name of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.
--
-- * 'gsbdSchemaDefinition' - The definition of the schema for which schema details are required.
getSchemaByDefinition ::
  -- | 'gsbdSchemaId'
  SchemaId ->
  -- | 'gsbdSchemaDefinition'
  Text ->
  GetSchemaByDefinition
getSchemaByDefinition pSchemaId_ pSchemaDefinition_ =
  GetSchemaByDefinition'
    { _gsbdSchemaId = pSchemaId_,
      _gsbdSchemaDefinition = pSchemaDefinition_
    }

-- | This is a wrapper structure to contain schema identity fields. The structure contains:     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.     * SchemaId$SchemaName: The name of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.
gsbdSchemaId :: Lens' GetSchemaByDefinition SchemaId
gsbdSchemaId = lens _gsbdSchemaId (\s a -> s {_gsbdSchemaId = a})

-- | The definition of the schema for which schema details are required.
gsbdSchemaDefinition :: Lens' GetSchemaByDefinition Text
gsbdSchemaDefinition = lens _gsbdSchemaDefinition (\s a -> s {_gsbdSchemaDefinition = a})

instance AWSRequest GetSchemaByDefinition where
  type Rs GetSchemaByDefinition = GetSchemaByDefinitionResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          GetSchemaByDefinitionResponse'
            <$> (x .?> "Status")
            <*> (x .?> "CreatedTime")
            <*> (x .?> "DataFormat")
            <*> (x .?> "SchemaVersionId")
            <*> (x .?> "SchemaArn")
            <*> (pure (fromEnum s))
      )

instance Hashable GetSchemaByDefinition

instance NFData GetSchemaByDefinition

instance ToHeaders GetSchemaByDefinition where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.GetSchemaByDefinition" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetSchemaByDefinition where
  toJSON GetSchemaByDefinition' {..} =
    object
      ( catMaybes
          [ Just ("SchemaId" .= _gsbdSchemaId),
            Just ("SchemaDefinition" .= _gsbdSchemaDefinition)
          ]
      )

instance ToPath GetSchemaByDefinition where
  toPath = const "/"

instance ToQuery GetSchemaByDefinition where
  toQuery = const mempty

-- | /See:/ 'getSchemaByDefinitionResponse' smart constructor.
data GetSchemaByDefinitionResponse = GetSchemaByDefinitionResponse'
  { _gsbdrsStatus ::
      !(Maybe SchemaVersionStatus),
    _gsbdrsCreatedTime ::
      !(Maybe Text),
    _gsbdrsDataFormat ::
      !(Maybe DataFormat),
    _gsbdrsSchemaVersionId ::
      !(Maybe Text),
    _gsbdrsSchemaARN ::
      !(Maybe Text),
    _gsbdrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSchemaByDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsbdrsStatus' - The status of the schema version.
--
-- * 'gsbdrsCreatedTime' - The date and time the schema was created.
--
-- * 'gsbdrsDataFormat' - The data format of the schema definition. Currently only @AVRO@ is supported.
--
-- * 'gsbdrsSchemaVersionId' - The schema ID of the schema version.
--
-- * 'gsbdrsSchemaARN' - The Amazon Resource Name (ARN) of the schema.
--
-- * 'gsbdrsResponseStatus' - -- | The response status code.
getSchemaByDefinitionResponse ::
  -- | 'gsbdrsResponseStatus'
  Int ->
  GetSchemaByDefinitionResponse
getSchemaByDefinitionResponse pResponseStatus_ =
  GetSchemaByDefinitionResponse'
    { _gsbdrsStatus = Nothing,
      _gsbdrsCreatedTime = Nothing,
      _gsbdrsDataFormat = Nothing,
      _gsbdrsSchemaVersionId = Nothing,
      _gsbdrsSchemaARN = Nothing,
      _gsbdrsResponseStatus = pResponseStatus_
    }

-- | The status of the schema version.
gsbdrsStatus :: Lens' GetSchemaByDefinitionResponse (Maybe SchemaVersionStatus)
gsbdrsStatus = lens _gsbdrsStatus (\s a -> s {_gsbdrsStatus = a})

-- | The date and time the schema was created.
gsbdrsCreatedTime :: Lens' GetSchemaByDefinitionResponse (Maybe Text)
gsbdrsCreatedTime = lens _gsbdrsCreatedTime (\s a -> s {_gsbdrsCreatedTime = a})

-- | The data format of the schema definition. Currently only @AVRO@ is supported.
gsbdrsDataFormat :: Lens' GetSchemaByDefinitionResponse (Maybe DataFormat)
gsbdrsDataFormat = lens _gsbdrsDataFormat (\s a -> s {_gsbdrsDataFormat = a})

-- | The schema ID of the schema version.
gsbdrsSchemaVersionId :: Lens' GetSchemaByDefinitionResponse (Maybe Text)
gsbdrsSchemaVersionId = lens _gsbdrsSchemaVersionId (\s a -> s {_gsbdrsSchemaVersionId = a})

-- | The Amazon Resource Name (ARN) of the schema.
gsbdrsSchemaARN :: Lens' GetSchemaByDefinitionResponse (Maybe Text)
gsbdrsSchemaARN = lens _gsbdrsSchemaARN (\s a -> s {_gsbdrsSchemaARN = a})

-- | -- | The response status code.
gsbdrsResponseStatus :: Lens' GetSchemaByDefinitionResponse Int
gsbdrsResponseStatus = lens _gsbdrsResponseStatus (\s a -> s {_gsbdrsResponseStatus = a})

instance NFData GetSchemaByDefinitionResponse
