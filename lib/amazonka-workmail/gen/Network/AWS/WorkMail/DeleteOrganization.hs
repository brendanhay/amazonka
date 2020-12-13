{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.DeleteOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon WorkMail organization and all underlying AWS resources managed by Amazon WorkMail as part of the organization. You can choose whether to delete the associated directory. For more information, see <https://docs.aws.amazon.com/workmail/latest/adminguide/remove_organization.html Removing an organization> in the /Amazon WorkMail Administrator Guide/ .
module Network.AWS.WorkMail.DeleteOrganization
  ( -- * Creating a request
    DeleteOrganization (..),
    mkDeleteOrganization,

    -- ** Request lenses
    dofClientToken,
    dofDeleteDirectory,
    dofOrganizationId,

    -- * Destructuring the response
    DeleteOrganizationResponse (..),
    mkDeleteOrganizationResponse,

    -- ** Response lenses
    dofrsState,
    dofrsOrganizationId,
    dofrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkDeleteOrganization' smart constructor.
data DeleteOrganization = DeleteOrganization'
  { -- | The idempotency token associated with the request.
    clientToken :: Lude.Maybe Lude.Text,
    -- | If true, deletes the AWS Directory Service directory associated with the organization.
    deleteDirectory :: Lude.Bool,
    -- | The organization ID.
    organizationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteOrganization' with the minimum fields required to make a request.
--
-- * 'clientToken' - The idempotency token associated with the request.
-- * 'deleteDirectory' - If true, deletes the AWS Directory Service directory associated with the organization.
-- * 'organizationId' - The organization ID.
mkDeleteOrganization ::
  -- | 'deleteDirectory'
  Lude.Bool ->
  -- | 'organizationId'
  Lude.Text ->
  DeleteOrganization
mkDeleteOrganization pDeleteDirectory_ pOrganizationId_ =
  DeleteOrganization'
    { clientToken = Lude.Nothing,
      deleteDirectory = pDeleteDirectory_,
      organizationId = pOrganizationId_
    }

-- | The idempotency token associated with the request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dofClientToken :: Lens.Lens' DeleteOrganization (Lude.Maybe Lude.Text)
dofClientToken = Lens.lens (clientToken :: DeleteOrganization -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: DeleteOrganization)
{-# DEPRECATED dofClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | If true, deletes the AWS Directory Service directory associated with the organization.
--
-- /Note:/ Consider using 'deleteDirectory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dofDeleteDirectory :: Lens.Lens' DeleteOrganization Lude.Bool
dofDeleteDirectory = Lens.lens (deleteDirectory :: DeleteOrganization -> Lude.Bool) (\s a -> s {deleteDirectory = a} :: DeleteOrganization)
{-# DEPRECATED dofDeleteDirectory "Use generic-lens or generic-optics with 'deleteDirectory' instead." #-}

-- | The organization ID.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dofOrganizationId :: Lens.Lens' DeleteOrganization Lude.Text
dofOrganizationId = Lens.lens (organizationId :: DeleteOrganization -> Lude.Text) (\s a -> s {organizationId = a} :: DeleteOrganization)
{-# DEPRECATED dofOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Lude.AWSRequest DeleteOrganization where
  type Rs DeleteOrganization = DeleteOrganizationResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteOrganizationResponse'
            Lude.<$> (x Lude..?> "State")
            Lude.<*> (x Lude..?> "OrganizationId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteOrganization where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.DeleteOrganization" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteOrganization where
  toJSON DeleteOrganization' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ClientToken" Lude..=) Lude.<$> clientToken,
            Lude.Just ("DeleteDirectory" Lude..= deleteDirectory),
            Lude.Just ("OrganizationId" Lude..= organizationId)
          ]
      )

instance Lude.ToPath DeleteOrganization where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteOrganization where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteOrganizationResponse' smart constructor.
data DeleteOrganizationResponse = DeleteOrganizationResponse'
  { -- | The state of the organization.
    state :: Lude.Maybe Lude.Text,
    -- | The organization ID.
    organizationId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteOrganizationResponse' with the minimum fields required to make a request.
--
-- * 'state' - The state of the organization.
-- * 'organizationId' - The organization ID.
-- * 'responseStatus' - The response status code.
mkDeleteOrganizationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteOrganizationResponse
mkDeleteOrganizationResponse pResponseStatus_ =
  DeleteOrganizationResponse'
    { state = Lude.Nothing,
      organizationId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The state of the organization.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dofrsState :: Lens.Lens' DeleteOrganizationResponse (Lude.Maybe Lude.Text)
dofrsState = Lens.lens (state :: DeleteOrganizationResponse -> Lude.Maybe Lude.Text) (\s a -> s {state = a} :: DeleteOrganizationResponse)
{-# DEPRECATED dofrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The organization ID.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dofrsOrganizationId :: Lens.Lens' DeleteOrganizationResponse (Lude.Maybe Lude.Text)
dofrsOrganizationId = Lens.lens (organizationId :: DeleteOrganizationResponse -> Lude.Maybe Lude.Text) (\s a -> s {organizationId = a} :: DeleteOrganizationResponse)
{-# DEPRECATED dofrsOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dofrsResponseStatus :: Lens.Lens' DeleteOrganizationResponse Lude.Int
dofrsResponseStatus = Lens.lens (responseStatus :: DeleteOrganizationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteOrganizationResponse)
{-# DEPRECATED dofrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
