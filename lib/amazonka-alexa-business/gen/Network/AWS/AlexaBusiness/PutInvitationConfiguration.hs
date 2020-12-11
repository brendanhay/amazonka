{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.PutInvitationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures the email template for the user enrollment invitation with the specified attributes.
module Network.AWS.AlexaBusiness.PutInvitationConfiguration
  ( -- * Creating a request
    PutInvitationConfiguration (..),
    mkPutInvitationConfiguration,

    -- ** Request lenses
    picContactEmail,
    picPrivateSkillIds,
    picOrganizationName,

    -- * Destructuring the response
    PutInvitationConfigurationResponse (..),
    mkPutInvitationConfigurationResponse,

    -- ** Response lenses
    picrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutInvitationConfiguration' smart constructor.
data PutInvitationConfiguration = PutInvitationConfiguration'
  { contactEmail ::
      Lude.Maybe Lude.Text,
    privateSkillIds ::
      Lude.Maybe [Lude.Text],
    organizationName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutInvitationConfiguration' with the minimum fields required to make a request.
--
-- * 'contactEmail' - The email ID of the organization or individual contact that the enrolled user can use.
-- * 'organizationName' - The name of the organization sending the enrollment invite to a user.
-- * 'privateSkillIds' - The list of private skill IDs that you want to recommend to the user to enable in the invitation.
mkPutInvitationConfiguration ::
  -- | 'organizationName'
  Lude.Text ->
  PutInvitationConfiguration
mkPutInvitationConfiguration pOrganizationName_ =
  PutInvitationConfiguration'
    { contactEmail = Lude.Nothing,
      privateSkillIds = Lude.Nothing,
      organizationName = pOrganizationName_
    }

-- | The email ID of the organization or individual contact that the enrolled user can use.
--
-- /Note:/ Consider using 'contactEmail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
picContactEmail :: Lens.Lens' PutInvitationConfiguration (Lude.Maybe Lude.Text)
picContactEmail = Lens.lens (contactEmail :: PutInvitationConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {contactEmail = a} :: PutInvitationConfiguration)
{-# DEPRECATED picContactEmail "Use generic-lens or generic-optics with 'contactEmail' instead." #-}

-- | The list of private skill IDs that you want to recommend to the user to enable in the invitation.
--
-- /Note:/ Consider using 'privateSkillIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
picPrivateSkillIds :: Lens.Lens' PutInvitationConfiguration (Lude.Maybe [Lude.Text])
picPrivateSkillIds = Lens.lens (privateSkillIds :: PutInvitationConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {privateSkillIds = a} :: PutInvitationConfiguration)
{-# DEPRECATED picPrivateSkillIds "Use generic-lens or generic-optics with 'privateSkillIds' instead." #-}

-- | The name of the organization sending the enrollment invite to a user.
--
-- /Note:/ Consider using 'organizationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
picOrganizationName :: Lens.Lens' PutInvitationConfiguration Lude.Text
picOrganizationName = Lens.lens (organizationName :: PutInvitationConfiguration -> Lude.Text) (\s a -> s {organizationName = a} :: PutInvitationConfiguration)
{-# DEPRECATED picOrganizationName "Use generic-lens or generic-optics with 'organizationName' instead." #-}

instance Lude.AWSRequest PutInvitationConfiguration where
  type
    Rs PutInvitationConfiguration =
      PutInvitationConfigurationResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          PutInvitationConfigurationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutInvitationConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.PutInvitationConfiguration" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutInvitationConfiguration where
  toJSON PutInvitationConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ContactEmail" Lude..=) Lude.<$> contactEmail,
            ("PrivateSkillIds" Lude..=) Lude.<$> privateSkillIds,
            Lude.Just ("OrganizationName" Lude..= organizationName)
          ]
      )

instance Lude.ToPath PutInvitationConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery PutInvitationConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutInvitationConfigurationResponse' smart constructor.
newtype PutInvitationConfigurationResponse = PutInvitationConfigurationResponse'
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

-- | Creates a value of 'PutInvitationConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkPutInvitationConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutInvitationConfigurationResponse
mkPutInvitationConfigurationResponse pResponseStatus_ =
  PutInvitationConfigurationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
picrsResponseStatus :: Lens.Lens' PutInvitationConfigurationResponse Lude.Int
picrsResponseStatus = Lens.lens (responseStatus :: PutInvitationConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutInvitationConfigurationResponse)
{-# DEPRECATED picrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
