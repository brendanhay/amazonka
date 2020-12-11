{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DescribeInstanceStorageConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current storage configurations for the specified resource type, association ID, and instance ID.
module Network.AWS.Connect.DescribeInstanceStorageConfig
  ( -- * Creating a request
    DescribeInstanceStorageConfig (..),
    mkDescribeInstanceStorageConfig,

    -- ** Request lenses
    discInstanceId,
    discAssociationId,
    discResourceType,

    -- * Destructuring the response
    DescribeInstanceStorageConfigResponse (..),
    mkDescribeInstanceStorageConfigResponse,

    -- ** Response lenses
    discrsStorageConfig,
    discrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeInstanceStorageConfig' smart constructor.
data DescribeInstanceStorageConfig = DescribeInstanceStorageConfig'
  { instanceId ::
      Lude.Text,
    associationId :: Lude.Text,
    resourceType ::
      InstanceStorageResourceType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstanceStorageConfig' with the minimum fields required to make a request.
--
-- * 'associationId' - The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'resourceType' - A valid resource type.
mkDescribeInstanceStorageConfig ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'associationId'
  Lude.Text ->
  -- | 'resourceType'
  InstanceStorageResourceType ->
  DescribeInstanceStorageConfig
mkDescribeInstanceStorageConfig
  pInstanceId_
  pAssociationId_
  pResourceType_ =
    DescribeInstanceStorageConfig'
      { instanceId = pInstanceId_,
        associationId = pAssociationId_,
        resourceType = pResourceType_
      }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
discInstanceId :: Lens.Lens' DescribeInstanceStorageConfig Lude.Text
discInstanceId = Lens.lens (instanceId :: DescribeInstanceStorageConfig -> Lude.Text) (\s a -> s {instanceId = a} :: DescribeInstanceStorageConfig)
{-# DEPRECATED discInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
discAssociationId :: Lens.Lens' DescribeInstanceStorageConfig Lude.Text
discAssociationId = Lens.lens (associationId :: DescribeInstanceStorageConfig -> Lude.Text) (\s a -> s {associationId = a} :: DescribeInstanceStorageConfig)
{-# DEPRECATED discAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | A valid resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
discResourceType :: Lens.Lens' DescribeInstanceStorageConfig InstanceStorageResourceType
discResourceType = Lens.lens (resourceType :: DescribeInstanceStorageConfig -> InstanceStorageResourceType) (\s a -> s {resourceType = a} :: DescribeInstanceStorageConfig)
{-# DEPRECATED discResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

instance Lude.AWSRequest DescribeInstanceStorageConfig where
  type
    Rs DescribeInstanceStorageConfig =
      DescribeInstanceStorageConfigResponse
  request = Req.get connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeInstanceStorageConfigResponse'
            Lude.<$> (x Lude..?> "StorageConfig")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeInstanceStorageConfig where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeInstanceStorageConfig where
  toPath DescribeInstanceStorageConfig' {..} =
    Lude.mconcat
      [ "/instance/",
        Lude.toBS instanceId,
        "/storage-config/",
        Lude.toBS associationId
      ]

instance Lude.ToQuery DescribeInstanceStorageConfig where
  toQuery DescribeInstanceStorageConfig' {..} =
    Lude.mconcat ["resourceType" Lude.=: resourceType]

-- | /See:/ 'mkDescribeInstanceStorageConfigResponse' smart constructor.
data DescribeInstanceStorageConfigResponse = DescribeInstanceStorageConfigResponse'
  { storageConfig ::
      Lude.Maybe
        InstanceStorageConfig,
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

-- | Creates a value of 'DescribeInstanceStorageConfigResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'storageConfig' - A valid storage type.
mkDescribeInstanceStorageConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeInstanceStorageConfigResponse
mkDescribeInstanceStorageConfigResponse pResponseStatus_ =
  DescribeInstanceStorageConfigResponse'
    { storageConfig =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A valid storage type.
--
-- /Note:/ Consider using 'storageConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
discrsStorageConfig :: Lens.Lens' DescribeInstanceStorageConfigResponse (Lude.Maybe InstanceStorageConfig)
discrsStorageConfig = Lens.lens (storageConfig :: DescribeInstanceStorageConfigResponse -> Lude.Maybe InstanceStorageConfig) (\s a -> s {storageConfig = a} :: DescribeInstanceStorageConfigResponse)
{-# DEPRECATED discrsStorageConfig "Use generic-lens or generic-optics with 'storageConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
discrsResponseStatus :: Lens.Lens' DescribeInstanceStorageConfigResponse Lude.Int
discrsResponseStatus = Lens.lens (responseStatus :: DescribeInstanceStorageConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeInstanceStorageConfigResponse)
{-# DEPRECATED discrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
