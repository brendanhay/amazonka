{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateRegistry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing registry which is used to hold a collection of schemas. The updated properties relate to the registry, and do not modify any of the schemas within the registry.
module Network.AWS.Glue.UpdateRegistry
  ( -- * Creating a request
    UpdateRegistry (..),
    mkUpdateRegistry,

    -- ** Request lenses
    urRegistryId,
    urDescription,

    -- * Destructuring the response
    UpdateRegistryResponse (..),
    mkUpdateRegistryResponse,

    -- ** Response lenses
    urrsRegistryName,
    urrsRegistryARN,
    urrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateRegistry' smart constructor.
data UpdateRegistry = UpdateRegistry'
  { registryId :: RegistryId,
    description :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRegistry' with the minimum fields required to make a request.
--
-- * 'description' - A description of the registry. If description is not provided, this field will not be updated.
-- * 'registryId' - This is a wrapper structure that may contain the registry name and Amazon Resource Name (ARN).
mkUpdateRegistry ::
  -- | 'registryId'
  RegistryId ->
  -- | 'description'
  Lude.Text ->
  UpdateRegistry
mkUpdateRegistry pRegistryId_ pDescription_ =
  UpdateRegistry'
    { registryId = pRegistryId_,
      description = pDescription_
    }

-- | This is a wrapper structure that may contain the registry name and Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urRegistryId :: Lens.Lens' UpdateRegistry RegistryId
urRegistryId = Lens.lens (registryId :: UpdateRegistry -> RegistryId) (\s a -> s {registryId = a} :: UpdateRegistry)
{-# DEPRECATED urRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | A description of the registry. If description is not provided, this field will not be updated.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urDescription :: Lens.Lens' UpdateRegistry Lude.Text
urDescription = Lens.lens (description :: UpdateRegistry -> Lude.Text) (\s a -> s {description = a} :: UpdateRegistry)
{-# DEPRECATED urDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest UpdateRegistry where
  type Rs UpdateRegistry = UpdateRegistryResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateRegistryResponse'
            Lude.<$> (x Lude..?> "RegistryName")
            Lude.<*> (x Lude..?> "RegistryArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateRegistry where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.UpdateRegistry" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateRegistry where
  toJSON UpdateRegistry' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("RegistryId" Lude..= registryId),
            Lude.Just ("Description" Lude..= description)
          ]
      )

instance Lude.ToPath UpdateRegistry where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateRegistry where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateRegistryResponse' smart constructor.
data UpdateRegistryResponse = UpdateRegistryResponse'
  { registryName ::
      Lude.Maybe Lude.Text,
    registryARN :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRegistryResponse' with the minimum fields required to make a request.
--
-- * 'registryARN' - The Amazon Resource name (ARN) of the updated registry.
-- * 'registryName' - The name of the updated registry.
-- * 'responseStatus' - The response status code.
mkUpdateRegistryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateRegistryResponse
mkUpdateRegistryResponse pResponseStatus_ =
  UpdateRegistryResponse'
    { registryName = Lude.Nothing,
      registryARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the updated registry.
--
-- /Note:/ Consider using 'registryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrsRegistryName :: Lens.Lens' UpdateRegistryResponse (Lude.Maybe Lude.Text)
urrsRegistryName = Lens.lens (registryName :: UpdateRegistryResponse -> Lude.Maybe Lude.Text) (\s a -> s {registryName = a} :: UpdateRegistryResponse)
{-# DEPRECATED urrsRegistryName "Use generic-lens or generic-optics with 'registryName' instead." #-}

-- | The Amazon Resource name (ARN) of the updated registry.
--
-- /Note:/ Consider using 'registryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrsRegistryARN :: Lens.Lens' UpdateRegistryResponse (Lude.Maybe Lude.Text)
urrsRegistryARN = Lens.lens (registryARN :: UpdateRegistryResponse -> Lude.Maybe Lude.Text) (\s a -> s {registryARN = a} :: UpdateRegistryResponse)
{-# DEPRECATED urrsRegistryARN "Use generic-lens or generic-optics with 'registryARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrsResponseStatus :: Lens.Lens' UpdateRegistryResponse Lude.Int
urrsResponseStatus = Lens.lens (responseStatus :: UpdateRegistryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateRegistryResponse)
{-# DEPRECATED urrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
