{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
  ( -- * Creating a request
    DisassociateInstanceStorageConfig (..),
    mkDisassociateInstanceStorageConfig,

    -- ** Request lenses
    discfAssociationId,
    discfInstanceId,
    discfResourceType,

    -- * Destructuring the response
    DisassociateInstanceStorageConfigResponse (..),
    mkDisassociateInstanceStorageConfigResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateInstanceStorageConfig' smart constructor.
data DisassociateInstanceStorageConfig = DisassociateInstanceStorageConfig'
  { -- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
    associationId :: Lude.Text,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Lude.Text,
    -- | A valid resource type.
    resourceType :: InstanceStorageResourceType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateInstanceStorageConfig' with the minimum fields required to make a request.
--
-- * 'associationId' - The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'resourceType' - A valid resource type.
mkDisassociateInstanceStorageConfig ::
  -- | 'associationId'
  Lude.Text ->
  -- | 'instanceId'
  Lude.Text ->
  -- | 'resourceType'
  InstanceStorageResourceType ->
  DisassociateInstanceStorageConfig
mkDisassociateInstanceStorageConfig
  pAssociationId_
  pInstanceId_
  pResourceType_ =
    DisassociateInstanceStorageConfig'
      { associationId =
          pAssociationId_,
        instanceId = pInstanceId_,
        resourceType = pResourceType_
      }

-- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
discfAssociationId :: Lens.Lens' DisassociateInstanceStorageConfig Lude.Text
discfAssociationId = Lens.lens (associationId :: DisassociateInstanceStorageConfig -> Lude.Text) (\s a -> s {associationId = a} :: DisassociateInstanceStorageConfig)
{-# DEPRECATED discfAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
discfInstanceId :: Lens.Lens' DisassociateInstanceStorageConfig Lude.Text
discfInstanceId = Lens.lens (instanceId :: DisassociateInstanceStorageConfig -> Lude.Text) (\s a -> s {instanceId = a} :: DisassociateInstanceStorageConfig)
{-# DEPRECATED discfInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | A valid resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
discfResourceType :: Lens.Lens' DisassociateInstanceStorageConfig InstanceStorageResourceType
discfResourceType = Lens.lens (resourceType :: DisassociateInstanceStorageConfig -> InstanceStorageResourceType) (\s a -> s {resourceType = a} :: DisassociateInstanceStorageConfig)
{-# DEPRECATED discfResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

instance Lude.AWSRequest DisassociateInstanceStorageConfig where
  type
    Rs DisassociateInstanceStorageConfig =
      DisassociateInstanceStorageConfigResponse
  request = Req.delete connectService
  response =
    Res.receiveNull DisassociateInstanceStorageConfigResponse'

instance Lude.ToHeaders DisassociateInstanceStorageConfig where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DisassociateInstanceStorageConfig where
  toPath DisassociateInstanceStorageConfig' {..} =
    Lude.mconcat
      [ "/instance/",
        Lude.toBS instanceId,
        "/storage-config/",
        Lude.toBS associationId
      ]

instance Lude.ToQuery DisassociateInstanceStorageConfig where
  toQuery DisassociateInstanceStorageConfig' {..} =
    Lude.mconcat ["resourceType" Lude.=: resourceType]

-- | /See:/ 'mkDisassociateInstanceStorageConfigResponse' smart constructor.
data DisassociateInstanceStorageConfigResponse = DisassociateInstanceStorageConfigResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateInstanceStorageConfigResponse' with the minimum fields required to make a request.
mkDisassociateInstanceStorageConfigResponse ::
  DisassociateInstanceStorageConfigResponse
mkDisassociateInstanceStorageConfigResponse =
  DisassociateInstanceStorageConfigResponse'
