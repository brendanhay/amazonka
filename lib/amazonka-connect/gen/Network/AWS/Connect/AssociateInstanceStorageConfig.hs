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
-- Module      : Network.AWS.Connect.AssociateInstanceStorageConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a storage resource type for the first time. You can only associate one type of storage configuration in a single call. This means, for example, that you can't define an instance with multiple S3 buckets for storing chat transcripts.
--
--
-- This API does not create a resource that doesn't exist. It only associates it to the instance. Ensure that the resource being specified in the storage configuration, like an Amazon S3 bucket, exists when being used for association.
module Network.AWS.Connect.AssociateInstanceStorageConfig
  ( -- * Creating a Request
    associateInstanceStorageConfig,
    AssociateInstanceStorageConfig,

    -- * Request Lenses
    aiscInstanceId,
    aiscResourceType,
    aiscStorageConfig,

    -- * Destructuring the Response
    associateInstanceStorageConfigResponse,
    AssociateInstanceStorageConfigResponse,

    -- * Response Lenses
    aiscrsAssociationId,
    aiscrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateInstanceStorageConfig' smart constructor.
data AssociateInstanceStorageConfig = AssociateInstanceStorageConfig'
  { _aiscInstanceId ::
      !Text,
    _aiscResourceType ::
      !InstanceStorageResourceType,
    _aiscStorageConfig ::
      !InstanceStorageConfig
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociateInstanceStorageConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiscInstanceId' - The identifier of the Amazon Connect instance.
--
-- * 'aiscResourceType' - A valid resource type.
--
-- * 'aiscStorageConfig' - A valid storage type.
associateInstanceStorageConfig ::
  -- | 'aiscInstanceId'
  Text ->
  -- | 'aiscResourceType'
  InstanceStorageResourceType ->
  -- | 'aiscStorageConfig'
  InstanceStorageConfig ->
  AssociateInstanceStorageConfig
associateInstanceStorageConfig
  pInstanceId_
  pResourceType_
  pStorageConfig_ =
    AssociateInstanceStorageConfig'
      { _aiscInstanceId = pInstanceId_,
        _aiscResourceType = pResourceType_,
        _aiscStorageConfig = pStorageConfig_
      }

-- | The identifier of the Amazon Connect instance.
aiscInstanceId :: Lens' AssociateInstanceStorageConfig Text
aiscInstanceId = lens _aiscInstanceId (\s a -> s {_aiscInstanceId = a})

-- | A valid resource type.
aiscResourceType :: Lens' AssociateInstanceStorageConfig InstanceStorageResourceType
aiscResourceType = lens _aiscResourceType (\s a -> s {_aiscResourceType = a})

-- | A valid storage type.
aiscStorageConfig :: Lens' AssociateInstanceStorageConfig InstanceStorageConfig
aiscStorageConfig = lens _aiscStorageConfig (\s a -> s {_aiscStorageConfig = a})

instance AWSRequest AssociateInstanceStorageConfig where
  type
    Rs AssociateInstanceStorageConfig =
      AssociateInstanceStorageConfigResponse
  request = putJSON connect
  response =
    receiveJSON
      ( \s h x ->
          AssociateInstanceStorageConfigResponse'
            <$> (x .?> "AssociationId") <*> (pure (fromEnum s))
      )

instance Hashable AssociateInstanceStorageConfig

instance NFData AssociateInstanceStorageConfig

instance ToHeaders AssociateInstanceStorageConfig where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON AssociateInstanceStorageConfig where
  toJSON AssociateInstanceStorageConfig' {..} =
    object
      ( catMaybes
          [ Just ("ResourceType" .= _aiscResourceType),
            Just ("StorageConfig" .= _aiscStorageConfig)
          ]
      )

instance ToPath AssociateInstanceStorageConfig where
  toPath AssociateInstanceStorageConfig' {..} =
    mconcat ["/instance/", toBS _aiscInstanceId, "/storage-config"]

instance ToQuery AssociateInstanceStorageConfig where
  toQuery = const mempty

-- | /See:/ 'associateInstanceStorageConfigResponse' smart constructor.
data AssociateInstanceStorageConfigResponse = AssociateInstanceStorageConfigResponse'
  { _aiscrsAssociationId ::
      !(Maybe Text),
    _aiscrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociateInstanceStorageConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiscrsAssociationId' - The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
--
-- * 'aiscrsResponseStatus' - -- | The response status code.
associateInstanceStorageConfigResponse ::
  -- | 'aiscrsResponseStatus'
  Int ->
  AssociateInstanceStorageConfigResponse
associateInstanceStorageConfigResponse pResponseStatus_ =
  AssociateInstanceStorageConfigResponse'
    { _aiscrsAssociationId =
        Nothing,
      _aiscrsResponseStatus = pResponseStatus_
    }

-- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
aiscrsAssociationId :: Lens' AssociateInstanceStorageConfigResponse (Maybe Text)
aiscrsAssociationId = lens _aiscrsAssociationId (\s a -> s {_aiscrsAssociationId = a})

-- | -- | The response status code.
aiscrsResponseStatus :: Lens' AssociateInstanceStorageConfigResponse Int
aiscrsResponseStatus = lens _aiscrsResponseStatus (\s a -> s {_aiscrsResponseStatus = a})

instance NFData AssociateInstanceStorageConfigResponse
