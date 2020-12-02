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
-- Module      : Network.AWS.Glue.CreateSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new schema set and registers the schema definition. Returns an error if the schema set already exists without actually registering the version.
--
--
-- When the schema set is created, a version checkpoint will be set to the first version. Compatibility mode "DISABLED" restricts any additional schema versions from being added after the first schema version. For all other compatibility modes, validation of compatibility settings will be applied only from the second version onwards when the @RegisterSchemaVersion@ API is used.
--
-- When this API is called without a @RegistryId@ , this will create an entry for a "default-registry" in the registry database tables, if it is not already present.
module Network.AWS.Glue.CreateSchema
  ( -- * Creating a Request
    createSchema,
    CreateSchema,

    -- * Request Lenses
    csSchemaDefinition,
    csRegistryId,
    csDescription,
    csCompatibility,
    csTags,
    csSchemaName,
    csDataFormat,

    -- * Destructuring the Response
    createSchemaResponse,
    CreateSchemaResponse,

    -- * Response Lenses
    csrsSchemaVersionStatus,
    csrsRegistryName,
    csrsSchemaStatus,
    csrsRegistryARN,
    csrsLatestSchemaVersion,
    csrsDataFormat,
    csrsSchemaCheckpoint,
    csrsSchemaName,
    csrsSchemaVersionId,
    csrsSchemaARN,
    csrsNextSchemaVersion,
    csrsDescription,
    csrsCompatibility,
    csrsTags,
    csrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createSchema' smart constructor.
data CreateSchema = CreateSchema'
  { _csSchemaDefinition ::
      !(Maybe Text),
    _csRegistryId :: !(Maybe RegistryId),
    _csDescription :: !(Maybe Text),
    _csCompatibility :: !(Maybe Compatibility),
    _csTags :: !(Maybe (Map Text (Text))),
    _csSchemaName :: !Text,
    _csDataFormat :: !DataFormat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateSchema' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csSchemaDefinition' - The schema definition using the @DataFormat@ setting for @SchemaName@ .
--
-- * 'csRegistryId' - This is a wrapper shape to contain the registry identity fields. If this is not provided, the default registry will be used. The ARN format for the same will be: @arn:aws:glue:us-east-2:<customer id>:registry/default-registry:random-5-letter-id@ .
--
-- * 'csDescription' - An optional description of the schema. If description is not provided, there will not be any automatic default value for this.
--
-- * 'csCompatibility' - The compatibility mode of the schema. The possible values are:     * /NONE/ : No compatibility mode applies. You can use this choice in development scenarios or if you do not know the compatibility mode that you want to apply to schemas. Any new version added will be accepted without undergoing a compatibility check.     * /DISABLED/ : This compatibility choice prevents versioning for a particular schema. You can use this choice to prevent future versioning of a schema.     * /BACKWARD/ : This compatibility choice is recommended as it allows data receivers to read both the current and one previous schema version. This means that for instance, a new schema version cannot drop data fields or change the type of these fields, so they can't be read by readers using the previous version.     * /BACKWARD_ALL/ : This compatibility choice allows data receivers to read both the current and all previous schema versions. You can use this choice when you need to delete fields or add optional fields, and check compatibility against all previous schema versions.      * /FORWARD/ : This compatibility choice allows data receivers to read both the current and one next schema version, but not necessarily later versions. You can use this choice when you need to add fields or delete optional fields, but only check compatibility against the last schema version.     * /FORWARD_ALL/ : This compatibility choice allows data receivers to read written by producers of any new registered schema. You can use this choice when you need to add fields or delete optional fields, and check compatibility against all previous schema versions.     * /FULL/ : This compatibility choice allows data receivers to read data written by producers using the previous or next version of the schema, but not necessarily earlier or later versions. You can use this choice when you need to add or remove optional fields, but only check compatibility against the last schema version.     * /FULL_ALL/ : This compatibility choice allows data receivers to read data written by producers using all previous schema versions. You can use this choice when you need to add or remove optional fields, and check compatibility against all previous schema versions.
--
-- * 'csTags' - AWS tags that contain a key value pair and may be searched by console, command line, or API. If specified, follows the AWS tags-on-create pattern.
--
-- * 'csSchemaName' - Name of the schema to be created of max length of 255, and may only contain letters, numbers, hyphen, underscore, dollar sign, or hash mark. No whitespace.
--
-- * 'csDataFormat' - The data format of the schema definition. Currently only @AVRO@ is supported.
createSchema ::
  -- | 'csSchemaName'
  Text ->
  -- | 'csDataFormat'
  DataFormat ->
  CreateSchema
createSchema pSchemaName_ pDataFormat_ =
  CreateSchema'
    { _csSchemaDefinition = Nothing,
      _csRegistryId = Nothing,
      _csDescription = Nothing,
      _csCompatibility = Nothing,
      _csTags = Nothing,
      _csSchemaName = pSchemaName_,
      _csDataFormat = pDataFormat_
    }

-- | The schema definition using the @DataFormat@ setting for @SchemaName@ .
csSchemaDefinition :: Lens' CreateSchema (Maybe Text)
csSchemaDefinition = lens _csSchemaDefinition (\s a -> s {_csSchemaDefinition = a})

-- | This is a wrapper shape to contain the registry identity fields. If this is not provided, the default registry will be used. The ARN format for the same will be: @arn:aws:glue:us-east-2:<customer id>:registry/default-registry:random-5-letter-id@ .
csRegistryId :: Lens' CreateSchema (Maybe RegistryId)
csRegistryId = lens _csRegistryId (\s a -> s {_csRegistryId = a})

-- | An optional description of the schema. If description is not provided, there will not be any automatic default value for this.
csDescription :: Lens' CreateSchema (Maybe Text)
csDescription = lens _csDescription (\s a -> s {_csDescription = a})

-- | The compatibility mode of the schema. The possible values are:     * /NONE/ : No compatibility mode applies. You can use this choice in development scenarios or if you do not know the compatibility mode that you want to apply to schemas. Any new version added will be accepted without undergoing a compatibility check.     * /DISABLED/ : This compatibility choice prevents versioning for a particular schema. You can use this choice to prevent future versioning of a schema.     * /BACKWARD/ : This compatibility choice is recommended as it allows data receivers to read both the current and one previous schema version. This means that for instance, a new schema version cannot drop data fields or change the type of these fields, so they can't be read by readers using the previous version.     * /BACKWARD_ALL/ : This compatibility choice allows data receivers to read both the current and all previous schema versions. You can use this choice when you need to delete fields or add optional fields, and check compatibility against all previous schema versions.      * /FORWARD/ : This compatibility choice allows data receivers to read both the current and one next schema version, but not necessarily later versions. You can use this choice when you need to add fields or delete optional fields, but only check compatibility against the last schema version.     * /FORWARD_ALL/ : This compatibility choice allows data receivers to read written by producers of any new registered schema. You can use this choice when you need to add fields or delete optional fields, and check compatibility against all previous schema versions.     * /FULL/ : This compatibility choice allows data receivers to read data written by producers using the previous or next version of the schema, but not necessarily earlier or later versions. You can use this choice when you need to add or remove optional fields, but only check compatibility against the last schema version.     * /FULL_ALL/ : This compatibility choice allows data receivers to read data written by producers using all previous schema versions. You can use this choice when you need to add or remove optional fields, and check compatibility against all previous schema versions.
csCompatibility :: Lens' CreateSchema (Maybe Compatibility)
csCompatibility = lens _csCompatibility (\s a -> s {_csCompatibility = a})

-- | AWS tags that contain a key value pair and may be searched by console, command line, or API. If specified, follows the AWS tags-on-create pattern.
csTags :: Lens' CreateSchema (HashMap Text (Text))
csTags = lens _csTags (\s a -> s {_csTags = a}) . _Default . _Map

-- | Name of the schema to be created of max length of 255, and may only contain letters, numbers, hyphen, underscore, dollar sign, or hash mark. No whitespace.
csSchemaName :: Lens' CreateSchema Text
csSchemaName = lens _csSchemaName (\s a -> s {_csSchemaName = a})

-- | The data format of the schema definition. Currently only @AVRO@ is supported.
csDataFormat :: Lens' CreateSchema DataFormat
csDataFormat = lens _csDataFormat (\s a -> s {_csDataFormat = a})

instance AWSRequest CreateSchema where
  type Rs CreateSchema = CreateSchemaResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          CreateSchemaResponse'
            <$> (x .?> "SchemaVersionStatus")
            <*> (x .?> "RegistryName")
            <*> (x .?> "SchemaStatus")
            <*> (x .?> "RegistryArn")
            <*> (x .?> "LatestSchemaVersion")
            <*> (x .?> "DataFormat")
            <*> (x .?> "SchemaCheckpoint")
            <*> (x .?> "SchemaName")
            <*> (x .?> "SchemaVersionId")
            <*> (x .?> "SchemaArn")
            <*> (x .?> "NextSchemaVersion")
            <*> (x .?> "Description")
            <*> (x .?> "Compatibility")
            <*> (x .?> "Tags" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable CreateSchema

instance NFData CreateSchema

instance ToHeaders CreateSchema where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.CreateSchema" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateSchema where
  toJSON CreateSchema' {..} =
    object
      ( catMaybes
          [ ("SchemaDefinition" .=) <$> _csSchemaDefinition,
            ("RegistryId" .=) <$> _csRegistryId,
            ("Description" .=) <$> _csDescription,
            ("Compatibility" .=) <$> _csCompatibility,
            ("Tags" .=) <$> _csTags,
            Just ("SchemaName" .= _csSchemaName),
            Just ("DataFormat" .= _csDataFormat)
          ]
      )

instance ToPath CreateSchema where
  toPath = const "/"

instance ToQuery CreateSchema where
  toQuery = const mempty

-- | /See:/ 'createSchemaResponse' smart constructor.
data CreateSchemaResponse = CreateSchemaResponse'
  { _csrsSchemaVersionStatus ::
      !(Maybe SchemaVersionStatus),
    _csrsRegistryName :: !(Maybe Text),
    _csrsSchemaStatus :: !(Maybe SchemaStatus),
    _csrsRegistryARN :: !(Maybe Text),
    _csrsLatestSchemaVersion :: !(Maybe Nat),
    _csrsDataFormat :: !(Maybe DataFormat),
    _csrsSchemaCheckpoint :: !(Maybe Nat),
    _csrsSchemaName :: !(Maybe Text),
    _csrsSchemaVersionId :: !(Maybe Text),
    _csrsSchemaARN :: !(Maybe Text),
    _csrsNextSchemaVersion :: !(Maybe Nat),
    _csrsDescription :: !(Maybe Text),
    _csrsCompatibility :: !(Maybe Compatibility),
    _csrsTags :: !(Maybe (Map Text (Text))),
    _csrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateSchemaResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csrsSchemaVersionStatus' - The status of the first schema version created.
--
-- * 'csrsRegistryName' - The name of the registry.
--
-- * 'csrsSchemaStatus' - The status of the schema.
--
-- * 'csrsRegistryARN' - The Amazon Resource Name (ARN) of the registry.
--
-- * 'csrsLatestSchemaVersion' - The latest version of the schema associated with the returned schema definition.
--
-- * 'csrsDataFormat' - The data format of the schema definition. Currently only @AVRO@ is supported.
--
-- * 'csrsSchemaCheckpoint' - The version number of the checkpoint (the last time the compatibility mode was changed).
--
-- * 'csrsSchemaName' - The name of the schema.
--
-- * 'csrsSchemaVersionId' - The unique identifier of the first schema version.
--
-- * 'csrsSchemaARN' - The Amazon Resource Name (ARN) of the schema.
--
-- * 'csrsNextSchemaVersion' - The next version of the schema associated with the returned schema definition.
--
-- * 'csrsDescription' - A description of the schema if specified when created.
--
-- * 'csrsCompatibility' - The schema compatibility mode.
--
-- * 'csrsTags' - The tags for the schema.
--
-- * 'csrsResponseStatus' - -- | The response status code.
createSchemaResponse ::
  -- | 'csrsResponseStatus'
  Int ->
  CreateSchemaResponse
createSchemaResponse pResponseStatus_ =
  CreateSchemaResponse'
    { _csrsSchemaVersionStatus = Nothing,
      _csrsRegistryName = Nothing,
      _csrsSchemaStatus = Nothing,
      _csrsRegistryARN = Nothing,
      _csrsLatestSchemaVersion = Nothing,
      _csrsDataFormat = Nothing,
      _csrsSchemaCheckpoint = Nothing,
      _csrsSchemaName = Nothing,
      _csrsSchemaVersionId = Nothing,
      _csrsSchemaARN = Nothing,
      _csrsNextSchemaVersion = Nothing,
      _csrsDescription = Nothing,
      _csrsCompatibility = Nothing,
      _csrsTags = Nothing,
      _csrsResponseStatus = pResponseStatus_
    }

-- | The status of the first schema version created.
csrsSchemaVersionStatus :: Lens' CreateSchemaResponse (Maybe SchemaVersionStatus)
csrsSchemaVersionStatus = lens _csrsSchemaVersionStatus (\s a -> s {_csrsSchemaVersionStatus = a})

-- | The name of the registry.
csrsRegistryName :: Lens' CreateSchemaResponse (Maybe Text)
csrsRegistryName = lens _csrsRegistryName (\s a -> s {_csrsRegistryName = a})

-- | The status of the schema.
csrsSchemaStatus :: Lens' CreateSchemaResponse (Maybe SchemaStatus)
csrsSchemaStatus = lens _csrsSchemaStatus (\s a -> s {_csrsSchemaStatus = a})

-- | The Amazon Resource Name (ARN) of the registry.
csrsRegistryARN :: Lens' CreateSchemaResponse (Maybe Text)
csrsRegistryARN = lens _csrsRegistryARN (\s a -> s {_csrsRegistryARN = a})

-- | The latest version of the schema associated with the returned schema definition.
csrsLatestSchemaVersion :: Lens' CreateSchemaResponse (Maybe Natural)
csrsLatestSchemaVersion = lens _csrsLatestSchemaVersion (\s a -> s {_csrsLatestSchemaVersion = a}) . mapping _Nat

-- | The data format of the schema definition. Currently only @AVRO@ is supported.
csrsDataFormat :: Lens' CreateSchemaResponse (Maybe DataFormat)
csrsDataFormat = lens _csrsDataFormat (\s a -> s {_csrsDataFormat = a})

-- | The version number of the checkpoint (the last time the compatibility mode was changed).
csrsSchemaCheckpoint :: Lens' CreateSchemaResponse (Maybe Natural)
csrsSchemaCheckpoint = lens _csrsSchemaCheckpoint (\s a -> s {_csrsSchemaCheckpoint = a}) . mapping _Nat

-- | The name of the schema.
csrsSchemaName :: Lens' CreateSchemaResponse (Maybe Text)
csrsSchemaName = lens _csrsSchemaName (\s a -> s {_csrsSchemaName = a})

-- | The unique identifier of the first schema version.
csrsSchemaVersionId :: Lens' CreateSchemaResponse (Maybe Text)
csrsSchemaVersionId = lens _csrsSchemaVersionId (\s a -> s {_csrsSchemaVersionId = a})

-- | The Amazon Resource Name (ARN) of the schema.
csrsSchemaARN :: Lens' CreateSchemaResponse (Maybe Text)
csrsSchemaARN = lens _csrsSchemaARN (\s a -> s {_csrsSchemaARN = a})

-- | The next version of the schema associated with the returned schema definition.
csrsNextSchemaVersion :: Lens' CreateSchemaResponse (Maybe Natural)
csrsNextSchemaVersion = lens _csrsNextSchemaVersion (\s a -> s {_csrsNextSchemaVersion = a}) . mapping _Nat

-- | A description of the schema if specified when created.
csrsDescription :: Lens' CreateSchemaResponse (Maybe Text)
csrsDescription = lens _csrsDescription (\s a -> s {_csrsDescription = a})

-- | The schema compatibility mode.
csrsCompatibility :: Lens' CreateSchemaResponse (Maybe Compatibility)
csrsCompatibility = lens _csrsCompatibility (\s a -> s {_csrsCompatibility = a})

-- | The tags for the schema.
csrsTags :: Lens' CreateSchemaResponse (HashMap Text (Text))
csrsTags = lens _csrsTags (\s a -> s {_csrsTags = a}) . _Default . _Map

-- | -- | The response status code.
csrsResponseStatus :: Lens' CreateSchemaResponse Int
csrsResponseStatus = lens _csrsResponseStatus (\s a -> s {_csrsResponseStatus = a})

instance NFData CreateSchemaResponse
