{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.UpdateCoreDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a core definition.
module Network.AWS.Greengrass.UpdateCoreDefinition
  ( -- * Creating a request
    UpdateCoreDefinition (..),
    mkUpdateCoreDefinition,

    -- ** Request lenses
    uCoreDefinitionId,
    uName,

    -- * Destructuring the response
    UpdateCoreDefinitionResponse (..),
    mkUpdateCoreDefinitionResponse,

    -- ** Response lenses
    ursResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateCoreDefinition' smart constructor.
data UpdateCoreDefinition = UpdateCoreDefinition'
  { -- | The ID of the core definition.
    coreDefinitionId :: Lude.Text,
    -- | The name of the definition.
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCoreDefinition' with the minimum fields required to make a request.
--
-- * 'coreDefinitionId' - The ID of the core definition.
-- * 'name' - The name of the definition.
mkUpdateCoreDefinition ::
  -- | 'coreDefinitionId'
  Lude.Text ->
  UpdateCoreDefinition
mkUpdateCoreDefinition pCoreDefinitionId_ =
  UpdateCoreDefinition'
    { coreDefinitionId = pCoreDefinitionId_,
      name = Lude.Nothing
    }

-- | The ID of the core definition.
--
-- /Note:/ Consider using 'coreDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uCoreDefinitionId :: Lens.Lens' UpdateCoreDefinition Lude.Text
uCoreDefinitionId = Lens.lens (coreDefinitionId :: UpdateCoreDefinition -> Lude.Text) (\s a -> s {coreDefinitionId = a} :: UpdateCoreDefinition)
{-# DEPRECATED uCoreDefinitionId "Use generic-lens or generic-optics with 'coreDefinitionId' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uName :: Lens.Lens' UpdateCoreDefinition (Lude.Maybe Lude.Text)
uName = Lens.lens (name :: UpdateCoreDefinition -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateCoreDefinition)
{-# DEPRECATED uName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest UpdateCoreDefinition where
  type Rs UpdateCoreDefinition = UpdateCoreDefinitionResponse
  request = Req.putJSON greengrassService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateCoreDefinitionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateCoreDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateCoreDefinition where
  toJSON UpdateCoreDefinition' {..} =
    Lude.object (Lude.catMaybes [("Name" Lude..=) Lude.<$> name])

instance Lude.ToPath UpdateCoreDefinition where
  toPath UpdateCoreDefinition' {..} =
    Lude.mconcat
      ["/greengrass/definition/cores/", Lude.toBS coreDefinitionId]

instance Lude.ToQuery UpdateCoreDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateCoreDefinitionResponse' smart constructor.
newtype UpdateCoreDefinitionResponse = UpdateCoreDefinitionResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCoreDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateCoreDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateCoreDefinitionResponse
mkUpdateCoreDefinitionResponse pResponseStatus_ =
  UpdateCoreDefinitionResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursResponseStatus :: Lens.Lens' UpdateCoreDefinitionResponse Lude.Int
ursResponseStatus = Lens.lens (responseStatus :: UpdateCoreDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateCoreDefinitionResponse)
{-# DEPRECATED ursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
