{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.AssociateDelegateToResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a member (user or group) to the resource's set of delegates.
module Network.AWS.WorkMail.AssociateDelegateToResource
  ( -- * Creating a request
    AssociateDelegateToResource (..),
    mkAssociateDelegateToResource,

    -- ** Request lenses
    adtrResourceId,
    adtrEntityId,
    adtrOrganizationId,

    -- * Destructuring the response
    AssociateDelegateToResourceResponse (..),
    mkAssociateDelegateToResourceResponse,

    -- ** Response lenses
    adtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkAssociateDelegateToResource' smart constructor.
data AssociateDelegateToResource = AssociateDelegateToResource'
  { -- | The resource for which members (users or groups) are associated.
    resourceId :: Lude.Text,
    -- | The member (user or group) to associate to the resource.
    entityId :: Lude.Text,
    -- | The organization under which the resource exists.
    organizationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateDelegateToResource' with the minimum fields required to make a request.
--
-- * 'resourceId' - The resource for which members (users or groups) are associated.
-- * 'entityId' - The member (user or group) to associate to the resource.
-- * 'organizationId' - The organization under which the resource exists.
mkAssociateDelegateToResource ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'entityId'
  Lude.Text ->
  -- | 'organizationId'
  Lude.Text ->
  AssociateDelegateToResource
mkAssociateDelegateToResource
  pResourceId_
  pEntityId_
  pOrganizationId_ =
    AssociateDelegateToResource'
      { resourceId = pResourceId_,
        entityId = pEntityId_,
        organizationId = pOrganizationId_
      }

-- | The resource for which members (users or groups) are associated.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adtrResourceId :: Lens.Lens' AssociateDelegateToResource Lude.Text
adtrResourceId = Lens.lens (resourceId :: AssociateDelegateToResource -> Lude.Text) (\s a -> s {resourceId = a} :: AssociateDelegateToResource)
{-# DEPRECATED adtrResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The member (user or group) to associate to the resource.
--
-- /Note:/ Consider using 'entityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adtrEntityId :: Lens.Lens' AssociateDelegateToResource Lude.Text
adtrEntityId = Lens.lens (entityId :: AssociateDelegateToResource -> Lude.Text) (\s a -> s {entityId = a} :: AssociateDelegateToResource)
{-# DEPRECATED adtrEntityId "Use generic-lens or generic-optics with 'entityId' instead." #-}

-- | The organization under which the resource exists.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adtrOrganizationId :: Lens.Lens' AssociateDelegateToResource Lude.Text
adtrOrganizationId = Lens.lens (organizationId :: AssociateDelegateToResource -> Lude.Text) (\s a -> s {organizationId = a} :: AssociateDelegateToResource)
{-# DEPRECATED adtrOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Lude.AWSRequest AssociateDelegateToResource where
  type
    Rs AssociateDelegateToResource =
      AssociateDelegateToResourceResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AssociateDelegateToResourceResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateDelegateToResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.AssociateDelegateToResource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateDelegateToResource where
  toJSON AssociateDelegateToResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceId" Lude..= resourceId),
            Lude.Just ("EntityId" Lude..= entityId),
            Lude.Just ("OrganizationId" Lude..= organizationId)
          ]
      )

instance Lude.ToPath AssociateDelegateToResource where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateDelegateToResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateDelegateToResourceResponse' smart constructor.
newtype AssociateDelegateToResourceResponse = AssociateDelegateToResourceResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateDelegateToResourceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAssociateDelegateToResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateDelegateToResourceResponse
mkAssociateDelegateToResourceResponse pResponseStatus_ =
  AssociateDelegateToResourceResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adtrrsResponseStatus :: Lens.Lens' AssociateDelegateToResourceResponse Lude.Int
adtrrsResponseStatus = Lens.lens (responseStatus :: AssociateDelegateToResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateDelegateToResourceResponse)
{-# DEPRECATED adtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
