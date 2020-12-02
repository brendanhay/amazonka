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
-- Module      : Network.AWS.Connect.UpdateInstanceStorageConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing configuration for a resource type. This API is idempotent.
module Network.AWS.Connect.UpdateInstanceStorageConfig
  ( -- * Creating a Request
    updateInstanceStorageConfig,
    UpdateInstanceStorageConfig,

    -- * Request Lenses
    uiscInstanceId,
    uiscAssociationId,
    uiscResourceType,
    uiscStorageConfig,

    -- * Destructuring the Response
    updateInstanceStorageConfigResponse,
    UpdateInstanceStorageConfigResponse,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateInstanceStorageConfig' smart constructor.
data UpdateInstanceStorageConfig = UpdateInstanceStorageConfig'
  { _uiscInstanceId ::
      !Text,
    _uiscAssociationId :: !Text,
    _uiscResourceType ::
      !InstanceStorageResourceType,
    _uiscStorageConfig ::
      !InstanceStorageConfig
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateInstanceStorageConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uiscInstanceId' - The identifier of the Amazon Connect instance.
--
-- * 'uiscAssociationId' - The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
--
-- * 'uiscResourceType' - A valid resource type.
--
-- * 'uiscStorageConfig' - Undocumented member.
updateInstanceStorageConfig ::
  -- | 'uiscInstanceId'
  Text ->
  -- | 'uiscAssociationId'
  Text ->
  -- | 'uiscResourceType'
  InstanceStorageResourceType ->
  -- | 'uiscStorageConfig'
  InstanceStorageConfig ->
  UpdateInstanceStorageConfig
updateInstanceStorageConfig
  pInstanceId_
  pAssociationId_
  pResourceType_
  pStorageConfig_ =
    UpdateInstanceStorageConfig'
      { _uiscInstanceId = pInstanceId_,
        _uiscAssociationId = pAssociationId_,
        _uiscResourceType = pResourceType_,
        _uiscStorageConfig = pStorageConfig_
      }

-- | The identifier of the Amazon Connect instance.
uiscInstanceId :: Lens' UpdateInstanceStorageConfig Text
uiscInstanceId = lens _uiscInstanceId (\s a -> s {_uiscInstanceId = a})

-- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
uiscAssociationId :: Lens' UpdateInstanceStorageConfig Text
uiscAssociationId = lens _uiscAssociationId (\s a -> s {_uiscAssociationId = a})

-- | A valid resource type.
uiscResourceType :: Lens' UpdateInstanceStorageConfig InstanceStorageResourceType
uiscResourceType = lens _uiscResourceType (\s a -> s {_uiscResourceType = a})

-- | Undocumented member.
uiscStorageConfig :: Lens' UpdateInstanceStorageConfig InstanceStorageConfig
uiscStorageConfig = lens _uiscStorageConfig (\s a -> s {_uiscStorageConfig = a})

instance AWSRequest UpdateInstanceStorageConfig where
  type
    Rs UpdateInstanceStorageConfig =
      UpdateInstanceStorageConfigResponse
  request = postJSON connect
  response = receiveNull UpdateInstanceStorageConfigResponse'

instance Hashable UpdateInstanceStorageConfig

instance NFData UpdateInstanceStorageConfig

instance ToHeaders UpdateInstanceStorageConfig where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateInstanceStorageConfig where
  toJSON UpdateInstanceStorageConfig' {..} =
    object (catMaybes [Just ("StorageConfig" .= _uiscStorageConfig)])

instance ToPath UpdateInstanceStorageConfig where
  toPath UpdateInstanceStorageConfig' {..} =
    mconcat
      [ "/instance/",
        toBS _uiscInstanceId,
        "/storage-config/",
        toBS _uiscAssociationId
      ]

instance ToQuery UpdateInstanceStorageConfig where
  toQuery UpdateInstanceStorageConfig' {..} =
    mconcat ["resourceType" =: _uiscResourceType]

-- | /See:/ 'updateInstanceStorageConfigResponse' smart constructor.
data UpdateInstanceStorageConfigResponse = UpdateInstanceStorageConfigResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateInstanceStorageConfigResponse' with the minimum fields required to make a request.
updateInstanceStorageConfigResponse ::
  UpdateInstanceStorageConfigResponse
updateInstanceStorageConfigResponse =
  UpdateInstanceStorageConfigResponse'

instance NFData UpdateInstanceStorageConfigResponse
