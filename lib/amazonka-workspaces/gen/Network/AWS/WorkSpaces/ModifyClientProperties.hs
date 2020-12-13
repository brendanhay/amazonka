{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.ModifyClientProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the properties of the specified Amazon WorkSpaces clients.
module Network.AWS.WorkSpaces.ModifyClientProperties
  ( -- * Creating a request
    ModifyClientProperties (..),
    mkModifyClientProperties,

    -- ** Request lenses
    mcpResourceId,
    mcpClientProperties,

    -- * Destructuring the response
    ModifyClientPropertiesResponse (..),
    mkModifyClientPropertiesResponse,

    -- ** Response lenses
    mcprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkModifyClientProperties' smart constructor.
data ModifyClientProperties = ModifyClientProperties'
  { -- | The resource identifiers, in the form of directory IDs.
    resourceId :: Lude.Text,
    -- | Information about the Amazon WorkSpaces client.
    clientProperties :: ClientProperties
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyClientProperties' with the minimum fields required to make a request.
--
-- * 'resourceId' - The resource identifiers, in the form of directory IDs.
-- * 'clientProperties' - Information about the Amazon WorkSpaces client.
mkModifyClientProperties ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'clientProperties'
  ClientProperties ->
  ModifyClientProperties
mkModifyClientProperties pResourceId_ pClientProperties_ =
  ModifyClientProperties'
    { resourceId = pResourceId_,
      clientProperties = pClientProperties_
    }

-- | The resource identifiers, in the form of directory IDs.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcpResourceId :: Lens.Lens' ModifyClientProperties Lude.Text
mcpResourceId = Lens.lens (resourceId :: ModifyClientProperties -> Lude.Text) (\s a -> s {resourceId = a} :: ModifyClientProperties)
{-# DEPRECATED mcpResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | Information about the Amazon WorkSpaces client.
--
-- /Note:/ Consider using 'clientProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcpClientProperties :: Lens.Lens' ModifyClientProperties ClientProperties
mcpClientProperties = Lens.lens (clientProperties :: ModifyClientProperties -> ClientProperties) (\s a -> s {clientProperties = a} :: ModifyClientProperties)
{-# DEPRECATED mcpClientProperties "Use generic-lens or generic-optics with 'clientProperties' instead." #-}

instance Lude.AWSRequest ModifyClientProperties where
  type Rs ModifyClientProperties = ModifyClientPropertiesResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveEmpty
      ( \s h x ->
          ModifyClientPropertiesResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyClientProperties where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.ModifyClientProperties" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ModifyClientProperties where
  toJSON ModifyClientProperties' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceId" Lude..= resourceId),
            Lude.Just ("ClientProperties" Lude..= clientProperties)
          ]
      )

instance Lude.ToPath ModifyClientProperties where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyClientProperties where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkModifyClientPropertiesResponse' smart constructor.
newtype ModifyClientPropertiesResponse = ModifyClientPropertiesResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyClientPropertiesResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkModifyClientPropertiesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyClientPropertiesResponse
mkModifyClientPropertiesResponse pResponseStatus_ =
  ModifyClientPropertiesResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcprsResponseStatus :: Lens.Lens' ModifyClientPropertiesResponse Lude.Int
mcprsResponseStatus = Lens.lens (responseStatus :: ModifyClientPropertiesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyClientPropertiesResponse)
{-# DEPRECATED mcprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
