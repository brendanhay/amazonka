{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeleteResourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Records the configuration state for a custom resource that has been deleted. This API records a new ConfigurationItem with a ResourceDeleted status. You can retrieve the ConfigurationItems recorded for this resource in your AWS Config History.
module Network.AWS.Config.DeleteResourceConfig
  ( -- * Creating a request
    DeleteResourceConfig (..),
    mkDeleteResourceConfig,

    -- ** Request lenses
    drcResourceType,
    drcResourceId,

    -- * Destructuring the response
    DeleteResourceConfigResponse (..),
    mkDeleteResourceConfigResponse,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteResourceConfig' smart constructor.
data DeleteResourceConfig = DeleteResourceConfig'
  { resourceType ::
      Lude.Text,
    resourceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteResourceConfig' with the minimum fields required to make a request.
--
-- * 'resourceId' - Unique identifier of the resource.
-- * 'resourceType' - The type of the resource.
mkDeleteResourceConfig ::
  -- | 'resourceType'
  Lude.Text ->
  -- | 'resourceId'
  Lude.Text ->
  DeleteResourceConfig
mkDeleteResourceConfig pResourceType_ pResourceId_ =
  DeleteResourceConfig'
    { resourceType = pResourceType_,
      resourceId = pResourceId_
    }

-- | The type of the resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcResourceType :: Lens.Lens' DeleteResourceConfig Lude.Text
drcResourceType = Lens.lens (resourceType :: DeleteResourceConfig -> Lude.Text) (\s a -> s {resourceType = a} :: DeleteResourceConfig)
{-# DEPRECATED drcResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | Unique identifier of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcResourceId :: Lens.Lens' DeleteResourceConfig Lude.Text
drcResourceId = Lens.lens (resourceId :: DeleteResourceConfig -> Lude.Text) (\s a -> s {resourceId = a} :: DeleteResourceConfig)
{-# DEPRECATED drcResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

instance Lude.AWSRequest DeleteResourceConfig where
  type Rs DeleteResourceConfig = DeleteResourceConfigResponse
  request = Req.postJSON configService
  response = Res.receiveNull DeleteResourceConfigResponse'

instance Lude.ToHeaders DeleteResourceConfig where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StarlingDoveService.DeleteResourceConfig" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteResourceConfig where
  toJSON DeleteResourceConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceType" Lude..= resourceType),
            Lude.Just ("ResourceId" Lude..= resourceId)
          ]
      )

instance Lude.ToPath DeleteResourceConfig where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteResourceConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteResourceConfigResponse' smart constructor.
data DeleteResourceConfigResponse = DeleteResourceConfigResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteResourceConfigResponse' with the minimum fields required to make a request.
mkDeleteResourceConfigResponse ::
  DeleteResourceConfigResponse
mkDeleteResourceConfigResponse = DeleteResourceConfigResponse'
