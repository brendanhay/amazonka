{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.UpdateDeviceDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a device definition.
module Network.AWS.Greengrass.UpdateDeviceDefinition
  ( -- * Creating a request
    UpdateDeviceDefinition (..),
    mkUpdateDeviceDefinition,

    -- ** Request lenses
    uddName,
    uddDeviceDefinitionId,

    -- * Destructuring the response
    UpdateDeviceDefinitionResponse (..),
    mkUpdateDeviceDefinitionResponse,

    -- ** Response lenses
    uddrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateDeviceDefinition' smart constructor.
data UpdateDeviceDefinition = UpdateDeviceDefinition'
  { -- | The name of the definition.
    name :: Lude.Maybe Lude.Text,
    -- | The ID of the device definition.
    deviceDefinitionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDeviceDefinition' with the minimum fields required to make a request.
--
-- * 'name' - The name of the definition.
-- * 'deviceDefinitionId' - The ID of the device definition.
mkUpdateDeviceDefinition ::
  -- | 'deviceDefinitionId'
  Lude.Text ->
  UpdateDeviceDefinition
mkUpdateDeviceDefinition pDeviceDefinitionId_ =
  UpdateDeviceDefinition'
    { name = Lude.Nothing,
      deviceDefinitionId = pDeviceDefinitionId_
    }

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uddName :: Lens.Lens' UpdateDeviceDefinition (Lude.Maybe Lude.Text)
uddName = Lens.lens (name :: UpdateDeviceDefinition -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateDeviceDefinition)
{-# DEPRECATED uddName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the device definition.
--
-- /Note:/ Consider using 'deviceDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uddDeviceDefinitionId :: Lens.Lens' UpdateDeviceDefinition Lude.Text
uddDeviceDefinitionId = Lens.lens (deviceDefinitionId :: UpdateDeviceDefinition -> Lude.Text) (\s a -> s {deviceDefinitionId = a} :: UpdateDeviceDefinition)
{-# DEPRECATED uddDeviceDefinitionId "Use generic-lens or generic-optics with 'deviceDefinitionId' instead." #-}

instance Lude.AWSRequest UpdateDeviceDefinition where
  type Rs UpdateDeviceDefinition = UpdateDeviceDefinitionResponse
  request = Req.putJSON greengrassService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateDeviceDefinitionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateDeviceDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateDeviceDefinition where
  toJSON UpdateDeviceDefinition' {..} =
    Lude.object (Lude.catMaybes [("Name" Lude..=) Lude.<$> name])

instance Lude.ToPath UpdateDeviceDefinition where
  toPath UpdateDeviceDefinition' {..} =
    Lude.mconcat
      ["/greengrass/definition/devices/", Lude.toBS deviceDefinitionId]

instance Lude.ToQuery UpdateDeviceDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateDeviceDefinitionResponse' smart constructor.
newtype UpdateDeviceDefinitionResponse = UpdateDeviceDefinitionResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDeviceDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateDeviceDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateDeviceDefinitionResponse
mkUpdateDeviceDefinitionResponse pResponseStatus_ =
  UpdateDeviceDefinitionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uddrsResponseStatus :: Lens.Lens' UpdateDeviceDefinitionResponse Lude.Int
uddrsResponseStatus = Lens.lens (responseStatus :: UpdateDeviceDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateDeviceDefinitionResponse)
{-# DEPRECATED uddrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
