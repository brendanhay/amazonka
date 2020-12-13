{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.DisassociateDelegateFromResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a member from the resource's set of delegates.
module Network.AWS.WorkMail.DisassociateDelegateFromResource
  ( -- * Creating a request
    DisassociateDelegateFromResource (..),
    mkDisassociateDelegateFromResource,

    -- ** Request lenses
    ddfrResourceId,
    ddfrEntityId,
    ddfrOrganizationId,

    -- * Destructuring the response
    DisassociateDelegateFromResourceResponse (..),
    mkDisassociateDelegateFromResourceResponse,

    -- ** Response lenses
    ddfrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkDisassociateDelegateFromResource' smart constructor.
data DisassociateDelegateFromResource = DisassociateDelegateFromResource'
  { -- | The identifier of the resource from which delegates' set members are removed.
    resourceId :: Lude.Text,
    -- | The identifier for the member (user, group) to be removed from the resource's delegates.
    entityId :: Lude.Text,
    -- | The identifier for the organization under which the resource exists.
    organizationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateDelegateFromResource' with the minimum fields required to make a request.
--
-- * 'resourceId' - The identifier of the resource from which delegates' set members are removed.
-- * 'entityId' - The identifier for the member (user, group) to be removed from the resource's delegates.
-- * 'organizationId' - The identifier for the organization under which the resource exists.
mkDisassociateDelegateFromResource ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'entityId'
  Lude.Text ->
  -- | 'organizationId'
  Lude.Text ->
  DisassociateDelegateFromResource
mkDisassociateDelegateFromResource
  pResourceId_
  pEntityId_
  pOrganizationId_ =
    DisassociateDelegateFromResource'
      { resourceId = pResourceId_,
        entityId = pEntityId_,
        organizationId = pOrganizationId_
      }

-- | The identifier of the resource from which delegates' set members are removed.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfrResourceId :: Lens.Lens' DisassociateDelegateFromResource Lude.Text
ddfrResourceId = Lens.lens (resourceId :: DisassociateDelegateFromResource -> Lude.Text) (\s a -> s {resourceId = a} :: DisassociateDelegateFromResource)
{-# DEPRECATED ddfrResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The identifier for the member (user, group) to be removed from the resource's delegates.
--
-- /Note:/ Consider using 'entityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfrEntityId :: Lens.Lens' DisassociateDelegateFromResource Lude.Text
ddfrEntityId = Lens.lens (entityId :: DisassociateDelegateFromResource -> Lude.Text) (\s a -> s {entityId = a} :: DisassociateDelegateFromResource)
{-# DEPRECATED ddfrEntityId "Use generic-lens or generic-optics with 'entityId' instead." #-}

-- | The identifier for the organization under which the resource exists.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfrOrganizationId :: Lens.Lens' DisassociateDelegateFromResource Lude.Text
ddfrOrganizationId = Lens.lens (organizationId :: DisassociateDelegateFromResource -> Lude.Text) (\s a -> s {organizationId = a} :: DisassociateDelegateFromResource)
{-# DEPRECATED ddfrOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Lude.AWSRequest DisassociateDelegateFromResource where
  type
    Rs DisassociateDelegateFromResource =
      DisassociateDelegateFromResourceResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisassociateDelegateFromResourceResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateDelegateFromResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "WorkMailService.DisassociateDelegateFromResource" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociateDelegateFromResource where
  toJSON DisassociateDelegateFromResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceId" Lude..= resourceId),
            Lude.Just ("EntityId" Lude..= entityId),
            Lude.Just ("OrganizationId" Lude..= organizationId)
          ]
      )

instance Lude.ToPath DisassociateDelegateFromResource where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateDelegateFromResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateDelegateFromResourceResponse' smart constructor.
newtype DisassociateDelegateFromResourceResponse = DisassociateDelegateFromResourceResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateDelegateFromResourceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisassociateDelegateFromResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateDelegateFromResourceResponse
mkDisassociateDelegateFromResourceResponse pResponseStatus_ =
  DisassociateDelegateFromResourceResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfrrsResponseStatus :: Lens.Lens' DisassociateDelegateFromResourceResponse Lude.Int
ddfrrsResponseStatus = Lens.lens (responseStatus :: DisassociateDelegateFromResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateDelegateFromResourceResponse)
{-# DEPRECATED ddfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
