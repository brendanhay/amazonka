{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.CreateResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon WorkMail resource.
module Network.AWS.WorkMail.CreateResource
  ( -- * Creating a request
    CreateResource (..),
    mkCreateResource,

    -- ** Request lenses
    crName,
    crType,
    crOrganizationId,

    -- * Destructuring the response
    CreateResourceResponse (..),
    mkCreateResourceResponse,

    -- ** Response lenses
    crrsResourceId,
    crrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkCreateResource' smart constructor.
data CreateResource = CreateResource'
  { -- | The name of the new resource.
    name :: Lude.Text,
    -- | The type of the new resource. The available types are @equipment@ and @room@ .
    type' :: ResourceType,
    -- | The identifier associated with the organization for which the resource is created.
    organizationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateResource' with the minimum fields required to make a request.
--
-- * 'name' - The name of the new resource.
-- * 'type'' - The type of the new resource. The available types are @equipment@ and @room@ .
-- * 'organizationId' - The identifier associated with the organization for which the resource is created.
mkCreateResource ::
  -- | 'name'
  Lude.Text ->
  -- | 'type''
  ResourceType ->
  -- | 'organizationId'
  Lude.Text ->
  CreateResource
mkCreateResource pName_ pType_ pOrganizationId_ =
  CreateResource'
    { name = pName_,
      type' = pType_,
      organizationId = pOrganizationId_
    }

-- | The name of the new resource.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crName :: Lens.Lens' CreateResource Lude.Text
crName = Lens.lens (name :: CreateResource -> Lude.Text) (\s a -> s {name = a} :: CreateResource)
{-# DEPRECATED crName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of the new resource. The available types are @equipment@ and @room@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crType :: Lens.Lens' CreateResource ResourceType
crType = Lens.lens (type' :: CreateResource -> ResourceType) (\s a -> s {type' = a} :: CreateResource)
{-# DEPRECATED crType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The identifier associated with the organization for which the resource is created.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crOrganizationId :: Lens.Lens' CreateResource Lude.Text
crOrganizationId = Lens.lens (organizationId :: CreateResource -> Lude.Text) (\s a -> s {organizationId = a} :: CreateResource)
{-# DEPRECATED crOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Lude.AWSRequest CreateResource where
  type Rs CreateResource = CreateResourceResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateResourceResponse'
            Lude.<$> (x Lude..?> "ResourceId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.CreateResource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateResource where
  toJSON CreateResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("Type" Lude..= type'),
            Lude.Just ("OrganizationId" Lude..= organizationId)
          ]
      )

instance Lude.ToPath CreateResource where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateResourceResponse' smart constructor.
data CreateResourceResponse = CreateResourceResponse'
  { -- | The identifier of the new resource.
    resourceId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateResourceResponse' with the minimum fields required to make a request.
--
-- * 'resourceId' - The identifier of the new resource.
-- * 'responseStatus' - The response status code.
mkCreateResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateResourceResponse
mkCreateResourceResponse pResponseStatus_ =
  CreateResourceResponse'
    { resourceId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier of the new resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsResourceId :: Lens.Lens' CreateResourceResponse (Lude.Maybe Lude.Text)
crrsResourceId = Lens.lens (resourceId :: CreateResourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: CreateResourceResponse)
{-# DEPRECATED crrsResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsResponseStatus :: Lens.Lens' CreateResourceResponse Lude.Int
crrsResponseStatus = Lens.lens (responseStatus :: CreateResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateResourceResponse)
{-# DEPRECATED crrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
