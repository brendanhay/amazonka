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
-- Module      : Network.AWS.Glue.UpdateSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the description, compatibility setting, or version checkpoint for a schema set.
--
--
-- For updating the compatibility setting, the call will not validate compatibility for the entire set of schema versions with the new compatibility setting. If the value for @Compatibility@ is provided, the @VersionNumber@ (a checkpoint) is also required. The API will validate the checkpoint version number for consistency.
--
-- If the value for the @VersionNumber@ (checkpoint) is provided, @Compatibility@ is optional and this can be used to set/reset a checkpoint for the schema.
--
-- This update will happen only if the schema is in the AVAILABLE state.
module Network.AWS.Glue.UpdateSchema
  ( -- * Creating a Request
    updateSchema,
    UpdateSchema,

    -- * Request Lenses
    usSchemaVersionNumber,
    usDescription,
    usCompatibility,
    usSchemaId,

    -- * Destructuring the Response
    updateSchemaResponse,
    UpdateSchemaResponse,

    -- * Response Lenses
    usrsRegistryName,
    usrsSchemaName,
    usrsSchemaARN,
    usrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateSchema' smart constructor.
data UpdateSchema = UpdateSchema'
  { _usSchemaVersionNumber ::
      !(Maybe SchemaVersionNumber),
    _usDescription :: !(Maybe Text),
    _usCompatibility :: !(Maybe Compatibility),
    _usSchemaId :: !SchemaId
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateSchema' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usSchemaVersionNumber' - Version number required for check pointing. One of @VersionNumber@ or @Compatibility@ has to be provided.
--
-- * 'usDescription' - The new description for the schema.
--
-- * 'usCompatibility' - The new compatibility setting for the schema.
--
-- * 'usSchemaId' - This is a wrapper structure to contain schema identity fields. The structure contains:     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.     * SchemaId$SchemaName: The name of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.
updateSchema ::
  -- | 'usSchemaId'
  SchemaId ->
  UpdateSchema
updateSchema pSchemaId_ =
  UpdateSchema'
    { _usSchemaVersionNumber = Nothing,
      _usDescription = Nothing,
      _usCompatibility = Nothing,
      _usSchemaId = pSchemaId_
    }

-- | Version number required for check pointing. One of @VersionNumber@ or @Compatibility@ has to be provided.
usSchemaVersionNumber :: Lens' UpdateSchema (Maybe SchemaVersionNumber)
usSchemaVersionNumber = lens _usSchemaVersionNumber (\s a -> s {_usSchemaVersionNumber = a})

-- | The new description for the schema.
usDescription :: Lens' UpdateSchema (Maybe Text)
usDescription = lens _usDescription (\s a -> s {_usDescription = a})

-- | The new compatibility setting for the schema.
usCompatibility :: Lens' UpdateSchema (Maybe Compatibility)
usCompatibility = lens _usCompatibility (\s a -> s {_usCompatibility = a})

-- | This is a wrapper structure to contain schema identity fields. The structure contains:     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.     * SchemaId$SchemaName: The name of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.
usSchemaId :: Lens' UpdateSchema SchemaId
usSchemaId = lens _usSchemaId (\s a -> s {_usSchemaId = a})

instance AWSRequest UpdateSchema where
  type Rs UpdateSchema = UpdateSchemaResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          UpdateSchemaResponse'
            <$> (x .?> "RegistryName")
            <*> (x .?> "SchemaName")
            <*> (x .?> "SchemaArn")
            <*> (pure (fromEnum s))
      )

instance Hashable UpdateSchema

instance NFData UpdateSchema

instance ToHeaders UpdateSchema where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.UpdateSchema" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateSchema where
  toJSON UpdateSchema' {..} =
    object
      ( catMaybes
          [ ("SchemaVersionNumber" .=) <$> _usSchemaVersionNumber,
            ("Description" .=) <$> _usDescription,
            ("Compatibility" .=) <$> _usCompatibility,
            Just ("SchemaId" .= _usSchemaId)
          ]
      )

instance ToPath UpdateSchema where
  toPath = const "/"

instance ToQuery UpdateSchema where
  toQuery = const mempty

-- | /See:/ 'updateSchemaResponse' smart constructor.
data UpdateSchemaResponse = UpdateSchemaResponse'
  { _usrsRegistryName ::
      !(Maybe Text),
    _usrsSchemaName :: !(Maybe Text),
    _usrsSchemaARN :: !(Maybe Text),
    _usrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateSchemaResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usrsRegistryName' - The name of the registry that contains the schema.
--
-- * 'usrsSchemaName' - The name of the schema.
--
-- * 'usrsSchemaARN' - The Amazon Resource Name (ARN) of the schema.
--
-- * 'usrsResponseStatus' - -- | The response status code.
updateSchemaResponse ::
  -- | 'usrsResponseStatus'
  Int ->
  UpdateSchemaResponse
updateSchemaResponse pResponseStatus_ =
  UpdateSchemaResponse'
    { _usrsRegistryName = Nothing,
      _usrsSchemaName = Nothing,
      _usrsSchemaARN = Nothing,
      _usrsResponseStatus = pResponseStatus_
    }

-- | The name of the registry that contains the schema.
usrsRegistryName :: Lens' UpdateSchemaResponse (Maybe Text)
usrsRegistryName = lens _usrsRegistryName (\s a -> s {_usrsRegistryName = a})

-- | The name of the schema.
usrsSchemaName :: Lens' UpdateSchemaResponse (Maybe Text)
usrsSchemaName = lens _usrsSchemaName (\s a -> s {_usrsSchemaName = a})

-- | The Amazon Resource Name (ARN) of the schema.
usrsSchemaARN :: Lens' UpdateSchemaResponse (Maybe Text)
usrsSchemaARN = lens _usrsSchemaARN (\s a -> s {_usrsSchemaARN = a})

-- | -- | The response status code.
usrsResponseStatus :: Lens' UpdateSchemaResponse Int
usrsResponseStatus = lens _usrsResponseStatus (\s a -> s {_usrsResponseStatus = a})

instance NFData UpdateSchemaResponse
