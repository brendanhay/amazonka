{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.DeleteDeviceDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a device definition.
module Network.AWS.Greengrass.DeleteDeviceDefinition
  ( -- * Creating a request
    DeleteDeviceDefinition (..),
    mkDeleteDeviceDefinition,

    -- ** Request lenses
    dddDeviceDefinitionId,

    -- * Destructuring the response
    DeleteDeviceDefinitionResponse (..),
    mkDeleteDeviceDefinitionResponse,

    -- ** Response lenses
    dddrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteDeviceDefinition' smart constructor.
newtype DeleteDeviceDefinition = DeleteDeviceDefinition'
  { deviceDefinitionId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDeviceDefinition' with the minimum fields required to make a request.
--
-- * 'deviceDefinitionId' - The ID of the device definition.
mkDeleteDeviceDefinition ::
  -- | 'deviceDefinitionId'
  Lude.Text ->
  DeleteDeviceDefinition
mkDeleteDeviceDefinition pDeviceDefinitionId_ =
  DeleteDeviceDefinition'
    { deviceDefinitionId =
        pDeviceDefinitionId_
    }

-- | The ID of the device definition.
--
-- /Note:/ Consider using 'deviceDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dddDeviceDefinitionId :: Lens.Lens' DeleteDeviceDefinition Lude.Text
dddDeviceDefinitionId = Lens.lens (deviceDefinitionId :: DeleteDeviceDefinition -> Lude.Text) (\s a -> s {deviceDefinitionId = a} :: DeleteDeviceDefinition)
{-# DEPRECATED dddDeviceDefinitionId "Use generic-lens or generic-optics with 'deviceDefinitionId' instead." #-}

instance Lude.AWSRequest DeleteDeviceDefinition where
  type Rs DeleteDeviceDefinition = DeleteDeviceDefinitionResponse
  request = Req.delete greengrassService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteDeviceDefinitionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDeviceDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteDeviceDefinition where
  toPath DeleteDeviceDefinition' {..} =
    Lude.mconcat
      ["/greengrass/definition/devices/", Lude.toBS deviceDefinitionId]

instance Lude.ToQuery DeleteDeviceDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDeviceDefinitionResponse' smart constructor.
newtype DeleteDeviceDefinitionResponse = DeleteDeviceDefinitionResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDeviceDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteDeviceDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDeviceDefinitionResponse
mkDeleteDeviceDefinitionResponse pResponseStatus_ =
  DeleteDeviceDefinitionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dddrsResponseStatus :: Lens.Lens' DeleteDeviceDefinitionResponse Lude.Int
dddrsResponseStatus = Lens.lens (responseStatus :: DeleteDeviceDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDeviceDefinitionResponse)
{-# DEPRECATED dddrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
