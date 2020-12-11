{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.RegisterToWorkMail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an existing and disabled user, group, or resource for Amazon WorkMail use by associating a mailbox and calendaring capabilities. It performs no change if the user, group, or resource is enabled and fails if the user, group, or resource is deleted. This operation results in the accumulation of costs. For more information, see <https://aws.amazon.com/workmail/pricing Pricing> . The equivalent console functionality for this operation is /Enable/ .
--
-- Users can either be created by calling the 'CreateUser' API operation or they can be synchronized from your directory. For more information, see 'DeregisterFromWorkMail' .
module Network.AWS.WorkMail.RegisterToWorkMail
  ( -- * Creating a request
    RegisterToWorkMail (..),
    mkRegisterToWorkMail,

    -- ** Request lenses
    rtwmOrganizationId,
    rtwmEntityId,
    rtwmEmail,

    -- * Destructuring the response
    RegisterToWorkMailResponse (..),
    mkRegisterToWorkMailResponse,

    -- ** Response lenses
    rtwmrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkRegisterToWorkMail' smart constructor.
data RegisterToWorkMail = RegisterToWorkMail'
  { organizationId ::
      Lude.Text,
    entityId :: Lude.Text,
    email :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterToWorkMail' with the minimum fields required to make a request.
--
-- * 'email' - The email for the user, group, or resource to be updated.
-- * 'entityId' - The identifier for the user, group, or resource to be updated.
-- * 'organizationId' - The identifier for the organization under which the user, group, or resource exists.
mkRegisterToWorkMail ::
  -- | 'organizationId'
  Lude.Text ->
  -- | 'entityId'
  Lude.Text ->
  -- | 'email'
  Lude.Text ->
  RegisterToWorkMail
mkRegisterToWorkMail pOrganizationId_ pEntityId_ pEmail_ =
  RegisterToWorkMail'
    { organizationId = pOrganizationId_,
      entityId = pEntityId_,
      email = pEmail_
    }

-- | The identifier for the organization under which the user, group, or resource exists.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmOrganizationId :: Lens.Lens' RegisterToWorkMail Lude.Text
rtwmOrganizationId = Lens.lens (organizationId :: RegisterToWorkMail -> Lude.Text) (\s a -> s {organizationId = a} :: RegisterToWorkMail)
{-# DEPRECATED rtwmOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The identifier for the user, group, or resource to be updated.
--
-- /Note:/ Consider using 'entityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmEntityId :: Lens.Lens' RegisterToWorkMail Lude.Text
rtwmEntityId = Lens.lens (entityId :: RegisterToWorkMail -> Lude.Text) (\s a -> s {entityId = a} :: RegisterToWorkMail)
{-# DEPRECATED rtwmEntityId "Use generic-lens or generic-optics with 'entityId' instead." #-}

-- | The email for the user, group, or resource to be updated.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmEmail :: Lens.Lens' RegisterToWorkMail Lude.Text
rtwmEmail = Lens.lens (email :: RegisterToWorkMail -> Lude.Text) (\s a -> s {email = a} :: RegisterToWorkMail)
{-# DEPRECATED rtwmEmail "Use generic-lens or generic-optics with 'email' instead." #-}

instance Lude.AWSRequest RegisterToWorkMail where
  type Rs RegisterToWorkMail = RegisterToWorkMailResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveEmpty
      ( \s h x ->
          RegisterToWorkMailResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterToWorkMail where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.RegisterToWorkMail" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RegisterToWorkMail where
  toJSON RegisterToWorkMail' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("OrganizationId" Lude..= organizationId),
            Lude.Just ("EntityId" Lude..= entityId),
            Lude.Just ("Email" Lude..= email)
          ]
      )

instance Lude.ToPath RegisterToWorkMail where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterToWorkMail where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRegisterToWorkMailResponse' smart constructor.
newtype RegisterToWorkMailResponse = RegisterToWorkMailResponse'
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

-- | Creates a value of 'RegisterToWorkMailResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkRegisterToWorkMailResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RegisterToWorkMailResponse
mkRegisterToWorkMailResponse pResponseStatus_ =
  RegisterToWorkMailResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmrsResponseStatus :: Lens.Lens' RegisterToWorkMailResponse Lude.Int
rtwmrsResponseStatus = Lens.lens (responseStatus :: RegisterToWorkMailResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterToWorkMailResponse)
{-# DEPRECATED rtwmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
