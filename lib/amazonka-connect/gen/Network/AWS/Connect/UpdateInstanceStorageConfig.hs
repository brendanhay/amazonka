{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
  ( -- * Creating a request
    UpdateInstanceStorageConfig (..),
    mkUpdateInstanceStorageConfig,

    -- ** Request lenses
    uiscAssociationId,
    uiscInstanceId,
    uiscResourceType,
    uiscStorageConfig,

    -- * Destructuring the response
    UpdateInstanceStorageConfigResponse (..),
    mkUpdateInstanceStorageConfigResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateInstanceStorageConfig' smart constructor.
data UpdateInstanceStorageConfig = UpdateInstanceStorageConfig'
  { -- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
    associationId :: Lude.Text,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Lude.Text,
    -- | A valid resource type.
    resourceType :: InstanceStorageResourceType,
    storageConfig :: InstanceStorageConfig
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateInstanceStorageConfig' with the minimum fields required to make a request.
--
-- * 'associationId' - The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'resourceType' - A valid resource type.
-- * 'storageConfig' -
mkUpdateInstanceStorageConfig ::
  -- | 'associationId'
  Lude.Text ->
  -- | 'instanceId'
  Lude.Text ->
  -- | 'resourceType'
  InstanceStorageResourceType ->
  -- | 'storageConfig'
  InstanceStorageConfig ->
  UpdateInstanceStorageConfig
mkUpdateInstanceStorageConfig
  pAssociationId_
  pInstanceId_
  pResourceType_
  pStorageConfig_ =
    UpdateInstanceStorageConfig'
      { associationId = pAssociationId_,
        instanceId = pInstanceId_,
        resourceType = pResourceType_,
        storageConfig = pStorageConfig_
      }

-- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiscAssociationId :: Lens.Lens' UpdateInstanceStorageConfig Lude.Text
uiscAssociationId = Lens.lens (associationId :: UpdateInstanceStorageConfig -> Lude.Text) (\s a -> s {associationId = a} :: UpdateInstanceStorageConfig)
{-# DEPRECATED uiscAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiscInstanceId :: Lens.Lens' UpdateInstanceStorageConfig Lude.Text
uiscInstanceId = Lens.lens (instanceId :: UpdateInstanceStorageConfig -> Lude.Text) (\s a -> s {instanceId = a} :: UpdateInstanceStorageConfig)
{-# DEPRECATED uiscInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | A valid resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiscResourceType :: Lens.Lens' UpdateInstanceStorageConfig InstanceStorageResourceType
uiscResourceType = Lens.lens (resourceType :: UpdateInstanceStorageConfig -> InstanceStorageResourceType) (\s a -> s {resourceType = a} :: UpdateInstanceStorageConfig)
{-# DEPRECATED uiscResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'storageConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiscStorageConfig :: Lens.Lens' UpdateInstanceStorageConfig InstanceStorageConfig
uiscStorageConfig = Lens.lens (storageConfig :: UpdateInstanceStorageConfig -> InstanceStorageConfig) (\s a -> s {storageConfig = a} :: UpdateInstanceStorageConfig)
{-# DEPRECATED uiscStorageConfig "Use generic-lens or generic-optics with 'storageConfig' instead." #-}

instance Lude.AWSRequest UpdateInstanceStorageConfig where
  type
    Rs UpdateInstanceStorageConfig =
      UpdateInstanceStorageConfigResponse
  request = Req.postJSON connectService
  response = Res.receiveNull UpdateInstanceStorageConfigResponse'

instance Lude.ToHeaders UpdateInstanceStorageConfig where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateInstanceStorageConfig where
  toJSON UpdateInstanceStorageConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("StorageConfig" Lude..= storageConfig)]
      )

instance Lude.ToPath UpdateInstanceStorageConfig where
  toPath UpdateInstanceStorageConfig' {..} =
    Lude.mconcat
      [ "/instance/",
        Lude.toBS instanceId,
        "/storage-config/",
        Lude.toBS associationId
      ]

instance Lude.ToQuery UpdateInstanceStorageConfig where
  toQuery UpdateInstanceStorageConfig' {..} =
    Lude.mconcat ["resourceType" Lude.=: resourceType]

-- | /See:/ 'mkUpdateInstanceStorageConfigResponse' smart constructor.
data UpdateInstanceStorageConfigResponse = UpdateInstanceStorageConfigResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateInstanceStorageConfigResponse' with the minimum fields required to make a request.
mkUpdateInstanceStorageConfigResponse ::
  UpdateInstanceStorageConfigResponse
mkUpdateInstanceStorageConfigResponse =
  UpdateInstanceStorageConfigResponse'
