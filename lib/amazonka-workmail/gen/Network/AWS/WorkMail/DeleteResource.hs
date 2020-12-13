{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.DeleteResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified resource.
module Network.AWS.WorkMail.DeleteResource
  ( -- * Creating a request
    DeleteResource (..),
    mkDeleteResource,

    -- ** Request lenses
    drResourceId,
    drOrganizationId,

    -- * Destructuring the response
    DeleteResourceResponse (..),
    mkDeleteResourceResponse,

    -- ** Response lenses
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkDeleteResource' smart constructor.
data DeleteResource = DeleteResource'
  { -- | The identifier of the resource to be deleted.
    resourceId :: Lude.Text,
    -- | The identifier associated with the organization from which the resource is deleted.
    organizationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteResource' with the minimum fields required to make a request.
--
-- * 'resourceId' - The identifier of the resource to be deleted.
-- * 'organizationId' - The identifier associated with the organization from which the resource is deleted.
mkDeleteResource ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'organizationId'
  Lude.Text ->
  DeleteResource
mkDeleteResource pResourceId_ pOrganizationId_ =
  DeleteResource'
    { resourceId = pResourceId_,
      organizationId = pOrganizationId_
    }

-- | The identifier of the resource to be deleted.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drResourceId :: Lens.Lens' DeleteResource Lude.Text
drResourceId = Lens.lens (resourceId :: DeleteResource -> Lude.Text) (\s a -> s {resourceId = a} :: DeleteResource)
{-# DEPRECATED drResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The identifier associated with the organization from which the resource is deleted.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drOrganizationId :: Lens.Lens' DeleteResource Lude.Text
drOrganizationId = Lens.lens (organizationId :: DeleteResource -> Lude.Text) (\s a -> s {organizationId = a} :: DeleteResource)
{-# DEPRECATED drOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Lude.AWSRequest DeleteResource where
  type Rs DeleteResource = DeleteResourceResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteResourceResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.DeleteResource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteResource where
  toJSON DeleteResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceId" Lude..= resourceId),
            Lude.Just ("OrganizationId" Lude..= organizationId)
          ]
      )

instance Lude.ToPath DeleteResource where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteResourceResponse' smart constructor.
newtype DeleteResourceResponse = DeleteResourceResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteResourceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteResourceResponse
mkDeleteResourceResponse pResponseStatus_ =
  DeleteResourceResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteResourceResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeleteResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteResourceResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
