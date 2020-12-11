{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.UpdateResourceDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a resource definition.
module Network.AWS.Greengrass.UpdateResourceDefinition
  ( -- * Creating a request
    UpdateResourceDefinition (..),
    mkUpdateResourceDefinition,

    -- ** Request lenses
    urdName,
    urdResourceDefinitionId,

    -- * Destructuring the response
    UpdateResourceDefinitionResponse (..),
    mkUpdateResourceDefinitionResponse,

    -- ** Response lenses
    urdrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateResourceDefinition' smart constructor.
data UpdateResourceDefinition = UpdateResourceDefinition'
  { name ::
      Lude.Maybe Lude.Text,
    resourceDefinitionId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateResourceDefinition' with the minimum fields required to make a request.
--
-- * 'name' - The name of the definition.
-- * 'resourceDefinitionId' - The ID of the resource definition.
mkUpdateResourceDefinition ::
  -- | 'resourceDefinitionId'
  Lude.Text ->
  UpdateResourceDefinition
mkUpdateResourceDefinition pResourceDefinitionId_ =
  UpdateResourceDefinition'
    { name = Lude.Nothing,
      resourceDefinitionId = pResourceDefinitionId_
    }

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdName :: Lens.Lens' UpdateResourceDefinition (Lude.Maybe Lude.Text)
urdName = Lens.lens (name :: UpdateResourceDefinition -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateResourceDefinition)
{-# DEPRECATED urdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the resource definition.
--
-- /Note:/ Consider using 'resourceDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdResourceDefinitionId :: Lens.Lens' UpdateResourceDefinition Lude.Text
urdResourceDefinitionId = Lens.lens (resourceDefinitionId :: UpdateResourceDefinition -> Lude.Text) (\s a -> s {resourceDefinitionId = a} :: UpdateResourceDefinition)
{-# DEPRECATED urdResourceDefinitionId "Use generic-lens or generic-optics with 'resourceDefinitionId' instead." #-}

instance Lude.AWSRequest UpdateResourceDefinition where
  type Rs UpdateResourceDefinition = UpdateResourceDefinitionResponse
  request = Req.putJSON greengrassService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateResourceDefinitionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateResourceDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateResourceDefinition where
  toJSON UpdateResourceDefinition' {..} =
    Lude.object (Lude.catMaybes [("Name" Lude..=) Lude.<$> name])

instance Lude.ToPath UpdateResourceDefinition where
  toPath UpdateResourceDefinition' {..} =
    Lude.mconcat
      [ "/greengrass/definition/resources/",
        Lude.toBS resourceDefinitionId
      ]

instance Lude.ToQuery UpdateResourceDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateResourceDefinitionResponse' smart constructor.
newtype UpdateResourceDefinitionResponse = UpdateResourceDefinitionResponse'
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

-- | Creates a value of 'UpdateResourceDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateResourceDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateResourceDefinitionResponse
mkUpdateResourceDefinitionResponse pResponseStatus_ =
  UpdateResourceDefinitionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdrsResponseStatus :: Lens.Lens' UpdateResourceDefinitionResponse Lude.Int
urdrsResponseStatus = Lens.lens (responseStatus :: UpdateResourceDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateResourceDefinitionResponse)
{-# DEPRECATED urdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
