{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.GetInvitationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the configured values for the user enrollment invitation email template.
module Network.AWS.AlexaBusiness.GetInvitationConfiguration
  ( -- * Creating a request
    GetInvitationConfiguration (..),
    mkGetInvitationConfiguration,

    -- * Destructuring the response
    GetInvitationConfigurationResponse (..),
    mkGetInvitationConfigurationResponse,

    -- ** Response lenses
    gicrsContactEmail,
    gicrsOrganizationName,
    gicrsPrivateSkillIds,
    gicrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetInvitationConfiguration' smart constructor.
data GetInvitationConfiguration = GetInvitationConfiguration'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetInvitationConfiguration' with the minimum fields required to make a request.
mkGetInvitationConfiguration ::
  GetInvitationConfiguration
mkGetInvitationConfiguration = GetInvitationConfiguration'

instance Lude.AWSRequest GetInvitationConfiguration where
  type
    Rs GetInvitationConfiguration =
      GetInvitationConfigurationResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetInvitationConfigurationResponse'
            Lude.<$> (x Lude..?> "ContactEmail")
            Lude.<*> (x Lude..?> "OrganizationName")
            Lude.<*> (x Lude..?> "PrivateSkillIds" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetInvitationConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.GetInvitationConfiguration" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetInvitationConfiguration where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath GetInvitationConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery GetInvitationConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetInvitationConfigurationResponse' smart constructor.
data GetInvitationConfigurationResponse = GetInvitationConfigurationResponse'
  { contactEmail ::
      Lude.Maybe Lude.Text,
    organizationName ::
      Lude.Maybe Lude.Text,
    privateSkillIds ::
      Lude.Maybe
        [Lude.Text],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetInvitationConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'contactEmail' - The email ID of the organization or individual contact that the enrolled user can use.
-- * 'organizationName' - The name of the organization sending the enrollment invite to a user.
-- * 'privateSkillIds' - The list of private skill IDs that you want to recommend to the user to enable in the invitation.
-- * 'responseStatus' - The response status code.
mkGetInvitationConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetInvitationConfigurationResponse
mkGetInvitationConfigurationResponse pResponseStatus_ =
  GetInvitationConfigurationResponse'
    { contactEmail = Lude.Nothing,
      organizationName = Lude.Nothing,
      privateSkillIds = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The email ID of the organization or individual contact that the enrolled user can use.
--
-- /Note:/ Consider using 'contactEmail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gicrsContactEmail :: Lens.Lens' GetInvitationConfigurationResponse (Lude.Maybe Lude.Text)
gicrsContactEmail = Lens.lens (contactEmail :: GetInvitationConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {contactEmail = a} :: GetInvitationConfigurationResponse)
{-# DEPRECATED gicrsContactEmail "Use generic-lens or generic-optics with 'contactEmail' instead." #-}

-- | The name of the organization sending the enrollment invite to a user.
--
-- /Note:/ Consider using 'organizationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gicrsOrganizationName :: Lens.Lens' GetInvitationConfigurationResponse (Lude.Maybe Lude.Text)
gicrsOrganizationName = Lens.lens (organizationName :: GetInvitationConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {organizationName = a} :: GetInvitationConfigurationResponse)
{-# DEPRECATED gicrsOrganizationName "Use generic-lens or generic-optics with 'organizationName' instead." #-}

-- | The list of private skill IDs that you want to recommend to the user to enable in the invitation.
--
-- /Note:/ Consider using 'privateSkillIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gicrsPrivateSkillIds :: Lens.Lens' GetInvitationConfigurationResponse (Lude.Maybe [Lude.Text])
gicrsPrivateSkillIds = Lens.lens (privateSkillIds :: GetInvitationConfigurationResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {privateSkillIds = a} :: GetInvitationConfigurationResponse)
{-# DEPRECATED gicrsPrivateSkillIds "Use generic-lens or generic-optics with 'privateSkillIds' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gicrsResponseStatus :: Lens.Lens' GetInvitationConfigurationResponse Lude.Int
gicrsResponseStatus = Lens.lens (responseStatus :: GetInvitationConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetInvitationConfigurationResponse)
{-# DEPRECATED gicrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
