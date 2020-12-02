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
-- Module      : Network.AWS.Glue.RegisterSchemaVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new version to the existing schema. Returns an error if new version of schema does not meet the compatibility requirements of the schema set. This API will not create a new schema set and will return a 404 error if the schema set is not already present in the Schema Registry.
--
--
-- If this is the first schema definition to be registered in the Schema Registry, this API will store the schema version and return immediately. Otherwise, this call has the potential to run longer than other operations due to compatibility modes. You can call the @GetSchemaVersion@ API with the @SchemaVersionId@ to check compatibility modes.
--
-- If the same schema definition is already stored in Schema Registry as a version, the schema ID of the existing schema is returned to the caller.
module Network.AWS.Glue.RegisterSchemaVersion
  ( -- * Creating a Request
    registerSchemaVersion,
    RegisterSchemaVersion,

    -- * Request Lenses
    rsvSchemaId,
    rsvSchemaDefinition,

    -- * Destructuring the Response
    registerSchemaVersionResponse,
    RegisterSchemaVersionResponse,

    -- * Response Lenses
    rsvrsStatus,
    rsvrsSchemaVersionId,
    rsvrsVersionNumber,
    rsvrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'registerSchemaVersion' smart constructor.
data RegisterSchemaVersion = RegisterSchemaVersion'
  { _rsvSchemaId ::
      !SchemaId,
    _rsvSchemaDefinition :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegisterSchemaVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsvSchemaId' - This is a wrapper structure to contain schema identity fields. The structure contains:     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.     * SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
--
-- * 'rsvSchemaDefinition' - The schema definition using the @DataFormat@ setting for the @SchemaName@ .
registerSchemaVersion ::
  -- | 'rsvSchemaId'
  SchemaId ->
  -- | 'rsvSchemaDefinition'
  Text ->
  RegisterSchemaVersion
registerSchemaVersion pSchemaId_ pSchemaDefinition_ =
  RegisterSchemaVersion'
    { _rsvSchemaId = pSchemaId_,
      _rsvSchemaDefinition = pSchemaDefinition_
    }

-- | This is a wrapper structure to contain schema identity fields. The structure contains:     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.     * SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
rsvSchemaId :: Lens' RegisterSchemaVersion SchemaId
rsvSchemaId = lens _rsvSchemaId (\s a -> s {_rsvSchemaId = a})

-- | The schema definition using the @DataFormat@ setting for the @SchemaName@ .
rsvSchemaDefinition :: Lens' RegisterSchemaVersion Text
rsvSchemaDefinition = lens _rsvSchemaDefinition (\s a -> s {_rsvSchemaDefinition = a})

instance AWSRequest RegisterSchemaVersion where
  type Rs RegisterSchemaVersion = RegisterSchemaVersionResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          RegisterSchemaVersionResponse'
            <$> (x .?> "Status")
            <*> (x .?> "SchemaVersionId")
            <*> (x .?> "VersionNumber")
            <*> (pure (fromEnum s))
      )

instance Hashable RegisterSchemaVersion

instance NFData RegisterSchemaVersion

instance ToHeaders RegisterSchemaVersion where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.RegisterSchemaVersion" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON RegisterSchemaVersion where
  toJSON RegisterSchemaVersion' {..} =
    object
      ( catMaybes
          [ Just ("SchemaId" .= _rsvSchemaId),
            Just ("SchemaDefinition" .= _rsvSchemaDefinition)
          ]
      )

instance ToPath RegisterSchemaVersion where
  toPath = const "/"

instance ToQuery RegisterSchemaVersion where
  toQuery = const mempty

-- | /See:/ 'registerSchemaVersionResponse' smart constructor.
data RegisterSchemaVersionResponse = RegisterSchemaVersionResponse'
  { _rsvrsStatus ::
      !(Maybe SchemaVersionStatus),
    _rsvrsSchemaVersionId ::
      !(Maybe Text),
    _rsvrsVersionNumber ::
      !(Maybe Nat),
    _rsvrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegisterSchemaVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsvrsStatus' - The status of the schema version.
--
-- * 'rsvrsSchemaVersionId' - The unique ID that represents the version of this schema.
--
-- * 'rsvrsVersionNumber' - The version of this schema (for sync flow only, in case this is the first version).
--
-- * 'rsvrsResponseStatus' - -- | The response status code.
registerSchemaVersionResponse ::
  -- | 'rsvrsResponseStatus'
  Int ->
  RegisterSchemaVersionResponse
registerSchemaVersionResponse pResponseStatus_ =
  RegisterSchemaVersionResponse'
    { _rsvrsStatus = Nothing,
      _rsvrsSchemaVersionId = Nothing,
      _rsvrsVersionNumber = Nothing,
      _rsvrsResponseStatus = pResponseStatus_
    }

-- | The status of the schema version.
rsvrsStatus :: Lens' RegisterSchemaVersionResponse (Maybe SchemaVersionStatus)
rsvrsStatus = lens _rsvrsStatus (\s a -> s {_rsvrsStatus = a})

-- | The unique ID that represents the version of this schema.
rsvrsSchemaVersionId :: Lens' RegisterSchemaVersionResponse (Maybe Text)
rsvrsSchemaVersionId = lens _rsvrsSchemaVersionId (\s a -> s {_rsvrsSchemaVersionId = a})

-- | The version of this schema (for sync flow only, in case this is the first version).
rsvrsVersionNumber :: Lens' RegisterSchemaVersionResponse (Maybe Natural)
rsvrsVersionNumber = lens _rsvrsVersionNumber (\s a -> s {_rsvrsVersionNumber = a}) . mapping _Nat

-- | -- | The response status code.
rsvrsResponseStatus :: Lens' RegisterSchemaVersionResponse Int
rsvrsResponseStatus = lens _rsvrsResponseStatus (\s a -> s {_rsvrsResponseStatus = a})

instance NFData RegisterSchemaVersionResponse
