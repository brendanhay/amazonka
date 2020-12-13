{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.UpdatePrimaryEmailAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the primary email for a user, group, or resource. The current email is moved into the list of aliases (or swapped between an existing alias and the current primary email), and the email provided in the input is promoted as the primary.
module Network.AWS.WorkMail.UpdatePrimaryEmailAddress
  ( -- * Creating a request
    UpdatePrimaryEmailAddress (..),
    mkUpdatePrimaryEmailAddress,

    -- ** Request lenses
    upeaEmail,
    upeaEntityId,
    upeaOrganizationId,

    -- * Destructuring the response
    UpdatePrimaryEmailAddressResponse (..),
    mkUpdatePrimaryEmailAddressResponse,

    -- ** Response lenses
    upearsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkUpdatePrimaryEmailAddress' smart constructor.
data UpdatePrimaryEmailAddress = UpdatePrimaryEmailAddress'
  { -- | The value of the email to be updated as primary.
    email :: Lude.Text,
    -- | The user, group, or resource to update.
    entityId :: Lude.Text,
    -- | The organization that contains the user, group, or resource to update.
    organizationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePrimaryEmailAddress' with the minimum fields required to make a request.
--
-- * 'email' - The value of the email to be updated as primary.
-- * 'entityId' - The user, group, or resource to update.
-- * 'organizationId' - The organization that contains the user, group, or resource to update.
mkUpdatePrimaryEmailAddress ::
  -- | 'email'
  Lude.Text ->
  -- | 'entityId'
  Lude.Text ->
  -- | 'organizationId'
  Lude.Text ->
  UpdatePrimaryEmailAddress
mkUpdatePrimaryEmailAddress pEmail_ pEntityId_ pOrganizationId_ =
  UpdatePrimaryEmailAddress'
    { email = pEmail_,
      entityId = pEntityId_,
      organizationId = pOrganizationId_
    }

-- | The value of the email to be updated as primary.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upeaEmail :: Lens.Lens' UpdatePrimaryEmailAddress Lude.Text
upeaEmail = Lens.lens (email :: UpdatePrimaryEmailAddress -> Lude.Text) (\s a -> s {email = a} :: UpdatePrimaryEmailAddress)
{-# DEPRECATED upeaEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The user, group, or resource to update.
--
-- /Note:/ Consider using 'entityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upeaEntityId :: Lens.Lens' UpdatePrimaryEmailAddress Lude.Text
upeaEntityId = Lens.lens (entityId :: UpdatePrimaryEmailAddress -> Lude.Text) (\s a -> s {entityId = a} :: UpdatePrimaryEmailAddress)
{-# DEPRECATED upeaEntityId "Use generic-lens or generic-optics with 'entityId' instead." #-}

-- | The organization that contains the user, group, or resource to update.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upeaOrganizationId :: Lens.Lens' UpdatePrimaryEmailAddress Lude.Text
upeaOrganizationId = Lens.lens (organizationId :: UpdatePrimaryEmailAddress -> Lude.Text) (\s a -> s {organizationId = a} :: UpdatePrimaryEmailAddress)
{-# DEPRECATED upeaOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Lude.AWSRequest UpdatePrimaryEmailAddress where
  type
    Rs UpdatePrimaryEmailAddress =
      UpdatePrimaryEmailAddressResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdatePrimaryEmailAddressResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdatePrimaryEmailAddress where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.UpdatePrimaryEmailAddress" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdatePrimaryEmailAddress where
  toJSON UpdatePrimaryEmailAddress' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Email" Lude..= email),
            Lude.Just ("EntityId" Lude..= entityId),
            Lude.Just ("OrganizationId" Lude..= organizationId)
          ]
      )

instance Lude.ToPath UpdatePrimaryEmailAddress where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdatePrimaryEmailAddress where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdatePrimaryEmailAddressResponse' smart constructor.
newtype UpdatePrimaryEmailAddressResponse = UpdatePrimaryEmailAddressResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePrimaryEmailAddressResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdatePrimaryEmailAddressResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdatePrimaryEmailAddressResponse
mkUpdatePrimaryEmailAddressResponse pResponseStatus_ =
  UpdatePrimaryEmailAddressResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upearsResponseStatus :: Lens.Lens' UpdatePrimaryEmailAddressResponse Lude.Int
upearsResponseStatus = Lens.lens (responseStatus :: UpdatePrimaryEmailAddressResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdatePrimaryEmailAddressResponse)
{-# DEPRECATED upearsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
