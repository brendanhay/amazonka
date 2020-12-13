{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.DeleteResourceDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a resource definition.
module Network.AWS.Greengrass.DeleteResourceDefinition
  ( -- * Creating a request
    DeleteResourceDefinition (..),
    mkDeleteResourceDefinition,

    -- ** Request lenses
    drdResourceDefinitionId,

    -- * Destructuring the response
    DeleteResourceDefinitionResponse (..),
    mkDeleteResourceDefinitionResponse,

    -- ** Response lenses
    drdrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteResourceDefinition' smart constructor.
newtype DeleteResourceDefinition = DeleteResourceDefinition'
  { -- | The ID of the resource definition.
    resourceDefinitionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteResourceDefinition' with the minimum fields required to make a request.
--
-- * 'resourceDefinitionId' - The ID of the resource definition.
mkDeleteResourceDefinition ::
  -- | 'resourceDefinitionId'
  Lude.Text ->
  DeleteResourceDefinition
mkDeleteResourceDefinition pResourceDefinitionId_ =
  DeleteResourceDefinition'
    { resourceDefinitionId =
        pResourceDefinitionId_
    }

-- | The ID of the resource definition.
--
-- /Note:/ Consider using 'resourceDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdResourceDefinitionId :: Lens.Lens' DeleteResourceDefinition Lude.Text
drdResourceDefinitionId = Lens.lens (resourceDefinitionId :: DeleteResourceDefinition -> Lude.Text) (\s a -> s {resourceDefinitionId = a} :: DeleteResourceDefinition)
{-# DEPRECATED drdResourceDefinitionId "Use generic-lens or generic-optics with 'resourceDefinitionId' instead." #-}

instance Lude.AWSRequest DeleteResourceDefinition where
  type Rs DeleteResourceDefinition = DeleteResourceDefinitionResponse
  request = Req.delete greengrassService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteResourceDefinitionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteResourceDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteResourceDefinition where
  toPath DeleteResourceDefinition' {..} =
    Lude.mconcat
      [ "/greengrass/definition/resources/",
        Lude.toBS resourceDefinitionId
      ]

instance Lude.ToQuery DeleteResourceDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteResourceDefinitionResponse' smart constructor.
newtype DeleteResourceDefinitionResponse = DeleteResourceDefinitionResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteResourceDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteResourceDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteResourceDefinitionResponse
mkDeleteResourceDefinitionResponse pResponseStatus_ =
  DeleteResourceDefinitionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdrsResponseStatus :: Lens.Lens' DeleteResourceDefinitionResponse Lude.Int
drdrsResponseStatus = Lens.lens (responseStatus :: DeleteResourceDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteResourceDefinitionResponse)
{-# DEPRECATED drdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
