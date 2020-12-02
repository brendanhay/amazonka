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
-- Module      : Network.AWS.Glue.GetSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified schema in detail.
module Network.AWS.Glue.GetSchema
  ( -- * Creating a Request
    getSchema,
    GetSchema,

    -- * Request Lenses
    gsSchemaId,

    -- * Destructuring the Response
    getSchemaResponse,
    GetSchemaResponse,

    -- * Response Lenses
    gsrsRegistryName,
    gsrsCreatedTime,
    gsrsSchemaStatus,
    gsrsRegistryARN,
    gsrsLatestSchemaVersion,
    gsrsDataFormat,
    gsrsSchemaCheckpoint,
    gsrsSchemaName,
    gsrsSchemaARN,
    gsrsNextSchemaVersion,
    gsrsUpdatedTime,
    gsrsDescription,
    gsrsCompatibility,
    gsrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getSchema' smart constructor.
newtype GetSchema = GetSchema' {_gsSchemaId :: SchemaId}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSchema' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsSchemaId' - This is a wrapper structure to contain schema identity fields. The structure contains:     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.     * SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
getSchema ::
  -- | 'gsSchemaId'
  SchemaId ->
  GetSchema
getSchema pSchemaId_ = GetSchema' {_gsSchemaId = pSchemaId_}

-- | This is a wrapper structure to contain schema identity fields. The structure contains:     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.     * SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
gsSchemaId :: Lens' GetSchema SchemaId
gsSchemaId = lens _gsSchemaId (\s a -> s {_gsSchemaId = a})

instance AWSRequest GetSchema where
  type Rs GetSchema = GetSchemaResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          GetSchemaResponse'
            <$> (x .?> "RegistryName")
            <*> (x .?> "CreatedTime")
            <*> (x .?> "SchemaStatus")
            <*> (x .?> "RegistryArn")
            <*> (x .?> "LatestSchemaVersion")
            <*> (x .?> "DataFormat")
            <*> (x .?> "SchemaCheckpoint")
            <*> (x .?> "SchemaName")
            <*> (x .?> "SchemaArn")
            <*> (x .?> "NextSchemaVersion")
            <*> (x .?> "UpdatedTime")
            <*> (x .?> "Description")
            <*> (x .?> "Compatibility")
            <*> (pure (fromEnum s))
      )

instance Hashable GetSchema

instance NFData GetSchema

instance ToHeaders GetSchema where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.GetSchema" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetSchema where
  toJSON GetSchema' {..} =
    object (catMaybes [Just ("SchemaId" .= _gsSchemaId)])

instance ToPath GetSchema where
  toPath = const "/"

instance ToQuery GetSchema where
  toQuery = const mempty

-- | /See:/ 'getSchemaResponse' smart constructor.
data GetSchemaResponse = GetSchemaResponse'
  { _gsrsRegistryName ::
      !(Maybe Text),
    _gsrsCreatedTime :: !(Maybe Text),
    _gsrsSchemaStatus :: !(Maybe SchemaStatus),
    _gsrsRegistryARN :: !(Maybe Text),
    _gsrsLatestSchemaVersion :: !(Maybe Nat),
    _gsrsDataFormat :: !(Maybe DataFormat),
    _gsrsSchemaCheckpoint :: !(Maybe Nat),
    _gsrsSchemaName :: !(Maybe Text),
    _gsrsSchemaARN :: !(Maybe Text),
    _gsrsNextSchemaVersion :: !(Maybe Nat),
    _gsrsUpdatedTime :: !(Maybe Text),
    _gsrsDescription :: !(Maybe Text),
    _gsrsCompatibility :: !(Maybe Compatibility),
    _gsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSchemaResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsrsRegistryName' - The name of the registry.
--
-- * 'gsrsCreatedTime' - The date and time the schema was created.
--
-- * 'gsrsSchemaStatus' - The status of the schema.
--
-- * 'gsrsRegistryARN' - The Amazon Resource Name (ARN) of the registry.
--
-- * 'gsrsLatestSchemaVersion' - The latest version of the schema associated with the returned schema definition.
--
-- * 'gsrsDataFormat' - The data format of the schema definition. Currently only @AVRO@ is supported.
--
-- * 'gsrsSchemaCheckpoint' - The version number of the checkpoint (the last time the compatibility mode was changed).
--
-- * 'gsrsSchemaName' - The name of the schema.
--
-- * 'gsrsSchemaARN' - The Amazon Resource Name (ARN) of the schema.
--
-- * 'gsrsNextSchemaVersion' - The next version of the schema associated with the returned schema definition.
--
-- * 'gsrsUpdatedTime' - The date and time the schema was updated.
--
-- * 'gsrsDescription' - A description of schema if specified when created
--
-- * 'gsrsCompatibility' - The compatibility mode of the schema.
--
-- * 'gsrsResponseStatus' - -- | The response status code.
getSchemaResponse ::
  -- | 'gsrsResponseStatus'
  Int ->
  GetSchemaResponse
getSchemaResponse pResponseStatus_ =
  GetSchemaResponse'
    { _gsrsRegistryName = Nothing,
      _gsrsCreatedTime = Nothing,
      _gsrsSchemaStatus = Nothing,
      _gsrsRegistryARN = Nothing,
      _gsrsLatestSchemaVersion = Nothing,
      _gsrsDataFormat = Nothing,
      _gsrsSchemaCheckpoint = Nothing,
      _gsrsSchemaName = Nothing,
      _gsrsSchemaARN = Nothing,
      _gsrsNextSchemaVersion = Nothing,
      _gsrsUpdatedTime = Nothing,
      _gsrsDescription = Nothing,
      _gsrsCompatibility = Nothing,
      _gsrsResponseStatus = pResponseStatus_
    }

-- | The name of the registry.
gsrsRegistryName :: Lens' GetSchemaResponse (Maybe Text)
gsrsRegistryName = lens _gsrsRegistryName (\s a -> s {_gsrsRegistryName = a})

-- | The date and time the schema was created.
gsrsCreatedTime :: Lens' GetSchemaResponse (Maybe Text)
gsrsCreatedTime = lens _gsrsCreatedTime (\s a -> s {_gsrsCreatedTime = a})

-- | The status of the schema.
gsrsSchemaStatus :: Lens' GetSchemaResponse (Maybe SchemaStatus)
gsrsSchemaStatus = lens _gsrsSchemaStatus (\s a -> s {_gsrsSchemaStatus = a})

-- | The Amazon Resource Name (ARN) of the registry.
gsrsRegistryARN :: Lens' GetSchemaResponse (Maybe Text)
gsrsRegistryARN = lens _gsrsRegistryARN (\s a -> s {_gsrsRegistryARN = a})

-- | The latest version of the schema associated with the returned schema definition.
gsrsLatestSchemaVersion :: Lens' GetSchemaResponse (Maybe Natural)
gsrsLatestSchemaVersion = lens _gsrsLatestSchemaVersion (\s a -> s {_gsrsLatestSchemaVersion = a}) . mapping _Nat

-- | The data format of the schema definition. Currently only @AVRO@ is supported.
gsrsDataFormat :: Lens' GetSchemaResponse (Maybe DataFormat)
gsrsDataFormat = lens _gsrsDataFormat (\s a -> s {_gsrsDataFormat = a})

-- | The version number of the checkpoint (the last time the compatibility mode was changed).
gsrsSchemaCheckpoint :: Lens' GetSchemaResponse (Maybe Natural)
gsrsSchemaCheckpoint = lens _gsrsSchemaCheckpoint (\s a -> s {_gsrsSchemaCheckpoint = a}) . mapping _Nat

-- | The name of the schema.
gsrsSchemaName :: Lens' GetSchemaResponse (Maybe Text)
gsrsSchemaName = lens _gsrsSchemaName (\s a -> s {_gsrsSchemaName = a})

-- | The Amazon Resource Name (ARN) of the schema.
gsrsSchemaARN :: Lens' GetSchemaResponse (Maybe Text)
gsrsSchemaARN = lens _gsrsSchemaARN (\s a -> s {_gsrsSchemaARN = a})

-- | The next version of the schema associated with the returned schema definition.
gsrsNextSchemaVersion :: Lens' GetSchemaResponse (Maybe Natural)
gsrsNextSchemaVersion = lens _gsrsNextSchemaVersion (\s a -> s {_gsrsNextSchemaVersion = a}) . mapping _Nat

-- | The date and time the schema was updated.
gsrsUpdatedTime :: Lens' GetSchemaResponse (Maybe Text)
gsrsUpdatedTime = lens _gsrsUpdatedTime (\s a -> s {_gsrsUpdatedTime = a})

-- | A description of schema if specified when created
gsrsDescription :: Lens' GetSchemaResponse (Maybe Text)
gsrsDescription = lens _gsrsDescription (\s a -> s {_gsrsDescription = a})

-- | The compatibility mode of the schema.
gsrsCompatibility :: Lens' GetSchemaResponse (Maybe Compatibility)
gsrsCompatibility = lens _gsrsCompatibility (\s a -> s {_gsrsCompatibility = a})

-- | -- | The response status code.
gsrsResponseStatus :: Lens' GetSchemaResponse Int
gsrsResponseStatus = lens _gsrsResponseStatus (\s a -> s {_gsrsResponseStatus = a})

instance NFData GetSchemaResponse
