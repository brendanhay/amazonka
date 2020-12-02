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
-- Module      : Network.AWS.Connect.DisassociateInstanceStorageConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the storage type configurations for the specified resource type and association ID.
module Network.AWS.Connect.DisassociateInstanceStorageConfig
  ( -- * Creating a Request
    disassociateInstanceStorageConfig,
    DisassociateInstanceStorageConfig,

    -- * Request Lenses
    disInstanceId,
    disAssociationId,
    disResourceType,

    -- * Destructuring the Response
    disassociateInstanceStorageConfigResponse,
    DisassociateInstanceStorageConfigResponse,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateInstanceStorageConfig' smart constructor.
data DisassociateInstanceStorageConfig = DisassociateInstanceStorageConfig'
  { _disInstanceId ::
      !Text,
    _disAssociationId ::
      !Text,
    _disResourceType ::
      !InstanceStorageResourceType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisassociateInstanceStorageConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'disInstanceId' - The identifier of the Amazon Connect instance.
--
-- * 'disAssociationId' - The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
--
-- * 'disResourceType' - A valid resource type.
disassociateInstanceStorageConfig ::
  -- | 'disInstanceId'
  Text ->
  -- | 'disAssociationId'
  Text ->
  -- | 'disResourceType'
  InstanceStorageResourceType ->
  DisassociateInstanceStorageConfig
disassociateInstanceStorageConfig
  pInstanceId_
  pAssociationId_
  pResourceType_ =
    DisassociateInstanceStorageConfig'
      { _disInstanceId = pInstanceId_,
        _disAssociationId = pAssociationId_,
        _disResourceType = pResourceType_
      }

-- | The identifier of the Amazon Connect instance.
disInstanceId :: Lens' DisassociateInstanceStorageConfig Text
disInstanceId = lens _disInstanceId (\s a -> s {_disInstanceId = a})

-- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
disAssociationId :: Lens' DisassociateInstanceStorageConfig Text
disAssociationId = lens _disAssociationId (\s a -> s {_disAssociationId = a})

-- | A valid resource type.
disResourceType :: Lens' DisassociateInstanceStorageConfig InstanceStorageResourceType
disResourceType = lens _disResourceType (\s a -> s {_disResourceType = a})

instance AWSRequest DisassociateInstanceStorageConfig where
  type
    Rs DisassociateInstanceStorageConfig =
      DisassociateInstanceStorageConfigResponse
  request = delete connect
  response = receiveNull DisassociateInstanceStorageConfigResponse'

instance Hashable DisassociateInstanceStorageConfig

instance NFData DisassociateInstanceStorageConfig

instance ToHeaders DisassociateInstanceStorageConfig where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DisassociateInstanceStorageConfig where
  toPath DisassociateInstanceStorageConfig' {..} =
    mconcat
      [ "/instance/",
        toBS _disInstanceId,
        "/storage-config/",
        toBS _disAssociationId
      ]

instance ToQuery DisassociateInstanceStorageConfig where
  toQuery DisassociateInstanceStorageConfig' {..} =
    mconcat ["resourceType" =: _disResourceType]

-- | /See:/ 'disassociateInstanceStorageConfigResponse' smart constructor.
data DisassociateInstanceStorageConfigResponse = DisassociateInstanceStorageConfigResponse'
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DisassociateInstanceStorageConfigResponse' with the minimum fields required to make a request.
disassociateInstanceStorageConfigResponse ::
  DisassociateInstanceStorageConfigResponse
disassociateInstanceStorageConfigResponse =
  DisassociateInstanceStorageConfigResponse'

instance NFData DisassociateInstanceStorageConfigResponse
