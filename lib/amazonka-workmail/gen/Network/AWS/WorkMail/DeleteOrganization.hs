{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    doClientToken,
    doOrganizationId,
    doDeleteDirectory,

    -- * Destructuring the response
    DeleteOrganizationResponse (..),
    mkDeleteOrganizationResponse,

    -- ** Response lenses
    delrsState,
    delrsOrganizationId,
    delrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkDeleteOrganization' smart constructor.
data DeleteOrganization = DeleteOrganization'
  { clientToken ::
      Lude.Maybe Lude.Text,
    organizationId :: Lude.Text,
    deleteDirectory :: Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteOrganization' with the minimum fields required to make a request.
--
-- * 'clientToken' - The idempotency token associated with the request.
-- * 'deleteDirectory' - If true, deletes the AWS Directory Service directory associated with the organization.
-- * 'organizationId' - The organization ID.
mkDeleteOrganization ::
  -- | 'organizationId'
  Lude.Text ->
  -- | 'deleteDirectory'
  Lude.Bool ->
  DeleteOrganization
mkDeleteOrganization pOrganizationId_ pDeleteDirectory_ =
  DeleteOrganization'
    { clientToken = Lude.Nothing,
      organizationId = pOrganizationId_,
      deleteDirectory = pDeleteDirectory_
    }

-- | The idempotency token associated with the request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doClientToken :: Lens.Lens' DeleteOrganization (Lude.Maybe Lude.Text)
doClientToken = Lens.lens (clientToken :: DeleteOrganization -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: DeleteOrganization)
{-# DEPRECATED doClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The organization ID.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doOrganizationId :: Lens.Lens' DeleteOrganization Lude.Text
doOrganizationId = Lens.lens (organizationId :: DeleteOrganization -> Lude.Text) (\s a -> s {organizationId = a} :: DeleteOrganization)
{-# DEPRECATED doOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | If true, deletes the AWS Directory Service directory associated with the organization.
--
-- /Note:/ Consider using 'deleteDirectory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doDeleteDirectory :: Lens.Lens' DeleteOrganization Lude.Bool
doDeleteDirectory = Lens.lens (deleteDirectory :: DeleteOrganization -> Lude.Bool) (\s a -> s {deleteDirectory = a} :: DeleteOrganization)
{-# DEPRECATED doDeleteDirectory "Use generic-lens or generic-optics with 'deleteDirectory' instead." #-}

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
            Lude.Just ("OrganizationId" Lude..= organizationId),
            Lude.Just ("DeleteDirectory" Lude..= deleteDirectory)
          ]
      )

instance Lude.ToPath DeleteOrganization where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteOrganization where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteOrganizationResponse' smart constructor.
data DeleteOrganizationResponse = DeleteOrganizationResponse'
  { state ::
      Lude.Maybe Lude.Text,
    organizationId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DeleteOrganizationResponse' with the minimum fields required to make a request.
--
-- * 'organizationId' - The organization ID.
-- * 'responseStatus' - The response status code.
-- * 'state' - The state of the organization.
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
delrsState :: Lens.Lens' DeleteOrganizationResponse (Lude.Maybe Lude.Text)
delrsState = Lens.lens (state :: DeleteOrganizationResponse -> Lude.Maybe Lude.Text) (\s a -> s {state = a} :: DeleteOrganizationResponse)
{-# DEPRECATED delrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The organization ID.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delrsOrganizationId :: Lens.Lens' DeleteOrganizationResponse (Lude.Maybe Lude.Text)
delrsOrganizationId = Lens.lens (organizationId :: DeleteOrganizationResponse -> Lude.Maybe Lude.Text) (\s a -> s {organizationId = a} :: DeleteOrganizationResponse)
{-# DEPRECATED delrsOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delrsResponseStatus :: Lens.Lens' DeleteOrganizationResponse Lude.Int
delrsResponseStatus = Lens.lens (responseStatus :: DeleteOrganizationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteOrganizationResponse)
{-# DEPRECATED delrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
