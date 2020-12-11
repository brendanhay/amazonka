{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
-- This API does not create a resource that doesn't exist. It only associates it to the instance. Ensure that the resource being specified in the storage configuration, like an Amazon S3 bucket, exists when being used for association.
module Network.AWS.Connect.AssociateInstanceStorageConfig
  ( -- * Creating a request
    AssociateInstanceStorageConfig (..),
    mkAssociateInstanceStorageConfig,

    -- ** Request lenses
    aiscInstanceId,
    aiscResourceType,
    aiscStorageConfig,

    -- * Destructuring the response
    AssociateInstanceStorageConfigResponse (..),
    mkAssociateInstanceStorageConfigResponse,

    -- ** Response lenses
    aiscrsAssociationId,
    aiscrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateInstanceStorageConfig' smart constructor.
data AssociateInstanceStorageConfig = AssociateInstanceStorageConfig'
  { instanceId ::
      Lude.Text,
    resourceType ::
      InstanceStorageResourceType,
    storageConfig ::
      InstanceStorageConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateInstanceStorageConfig' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'resourceType' - A valid resource type.
-- * 'storageConfig' - A valid storage type.
mkAssociateInstanceStorageConfig ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'resourceType'
  InstanceStorageResourceType ->
  -- | 'storageConfig'
  InstanceStorageConfig ->
  AssociateInstanceStorageConfig
mkAssociateInstanceStorageConfig
  pInstanceId_
  pResourceType_
  pStorageConfig_ =
    AssociateInstanceStorageConfig'
      { instanceId = pInstanceId_,
        resourceType = pResourceType_,
        storageConfig = pStorageConfig_
      }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiscInstanceId :: Lens.Lens' AssociateInstanceStorageConfig Lude.Text
aiscInstanceId = Lens.lens (instanceId :: AssociateInstanceStorageConfig -> Lude.Text) (\s a -> s {instanceId = a} :: AssociateInstanceStorageConfig)
{-# DEPRECATED aiscInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | A valid resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiscResourceType :: Lens.Lens' AssociateInstanceStorageConfig InstanceStorageResourceType
aiscResourceType = Lens.lens (resourceType :: AssociateInstanceStorageConfig -> InstanceStorageResourceType) (\s a -> s {resourceType = a} :: AssociateInstanceStorageConfig)
{-# DEPRECATED aiscResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | A valid storage type.
--
-- /Note:/ Consider using 'storageConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiscStorageConfig :: Lens.Lens' AssociateInstanceStorageConfig InstanceStorageConfig
aiscStorageConfig = Lens.lens (storageConfig :: AssociateInstanceStorageConfig -> InstanceStorageConfig) (\s a -> s {storageConfig = a} :: AssociateInstanceStorageConfig)
{-# DEPRECATED aiscStorageConfig "Use generic-lens or generic-optics with 'storageConfig' instead." #-}

instance Lude.AWSRequest AssociateInstanceStorageConfig where
  type
    Rs AssociateInstanceStorageConfig =
      AssociateInstanceStorageConfigResponse
  request = Req.putJSON connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          AssociateInstanceStorageConfigResponse'
            Lude.<$> (x Lude..?> "AssociationId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateInstanceStorageConfig where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateInstanceStorageConfig where
  toJSON AssociateInstanceStorageConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceType" Lude..= resourceType),
            Lude.Just ("StorageConfig" Lude..= storageConfig)
          ]
      )

instance Lude.ToPath AssociateInstanceStorageConfig where
  toPath AssociateInstanceStorageConfig' {..} =
    Lude.mconcat
      ["/instance/", Lude.toBS instanceId, "/storage-config"]

instance Lude.ToQuery AssociateInstanceStorageConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateInstanceStorageConfigResponse' smart constructor.
data AssociateInstanceStorageConfigResponse = AssociateInstanceStorageConfigResponse'
  { associationId ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateInstanceStorageConfigResponse' with the minimum fields required to make a request.
--
-- * 'associationId' - The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
-- * 'responseStatus' - The response status code.
mkAssociateInstanceStorageConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateInstanceStorageConfigResponse
mkAssociateInstanceStorageConfigResponse pResponseStatus_ =
  AssociateInstanceStorageConfigResponse'
    { associationId =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiscrsAssociationId :: Lens.Lens' AssociateInstanceStorageConfigResponse (Lude.Maybe Lude.Text)
aiscrsAssociationId = Lens.lens (associationId :: AssociateInstanceStorageConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {associationId = a} :: AssociateInstanceStorageConfigResponse)
{-# DEPRECATED aiscrsAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiscrsResponseStatus :: Lens.Lens' AssociateInstanceStorageConfigResponse Lude.Int
aiscrsResponseStatus = Lens.lens (responseStatus :: AssociateInstanceStorageConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateInstanceStorageConfigResponse)
{-# DEPRECATED aiscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
