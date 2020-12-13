{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.UpdateGroupCertificateConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the Certificate expiry time for a group.
module Network.AWS.Greengrass.UpdateGroupCertificateConfiguration
  ( -- * Creating a request
    UpdateGroupCertificateConfiguration (..),
    mkUpdateGroupCertificateConfiguration,

    -- ** Request lenses
    ugccGroupId,
    ugccCertificateExpiryInMilliseconds,

    -- * Destructuring the response
    UpdateGroupCertificateConfigurationResponse (..),
    mkUpdateGroupCertificateConfigurationResponse,

    -- ** Response lenses
    ugccrsCertificateAuthorityExpiryInMilliseconds,
    ugccrsGroupId,
    ugccrsCertificateExpiryInMilliseconds,
    ugccrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateGroupCertificateConfiguration' smart constructor.
data UpdateGroupCertificateConfiguration = UpdateGroupCertificateConfiguration'
  { -- | The ID of the Greengrass group.
    groupId :: Lude.Text,
    -- | The amount of time remaining before the certificate expires, in milliseconds.
    certificateExpiryInMilliseconds :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGroupCertificateConfiguration' with the minimum fields required to make a request.
--
-- * 'groupId' - The ID of the Greengrass group.
-- * 'certificateExpiryInMilliseconds' - The amount of time remaining before the certificate expires, in milliseconds.
mkUpdateGroupCertificateConfiguration ::
  -- | 'groupId'
  Lude.Text ->
  UpdateGroupCertificateConfiguration
mkUpdateGroupCertificateConfiguration pGroupId_ =
  UpdateGroupCertificateConfiguration'
    { groupId = pGroupId_,
      certificateExpiryInMilliseconds = Lude.Nothing
    }

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugccGroupId :: Lens.Lens' UpdateGroupCertificateConfiguration Lude.Text
ugccGroupId = Lens.lens (groupId :: UpdateGroupCertificateConfiguration -> Lude.Text) (\s a -> s {groupId = a} :: UpdateGroupCertificateConfiguration)
{-# DEPRECATED ugccGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The amount of time remaining before the certificate expires, in milliseconds.
--
-- /Note:/ Consider using 'certificateExpiryInMilliseconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugccCertificateExpiryInMilliseconds :: Lens.Lens' UpdateGroupCertificateConfiguration (Lude.Maybe Lude.Text)
ugccCertificateExpiryInMilliseconds = Lens.lens (certificateExpiryInMilliseconds :: UpdateGroupCertificateConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {certificateExpiryInMilliseconds = a} :: UpdateGroupCertificateConfiguration)
{-# DEPRECATED ugccCertificateExpiryInMilliseconds "Use generic-lens or generic-optics with 'certificateExpiryInMilliseconds' instead." #-}

instance Lude.AWSRequest UpdateGroupCertificateConfiguration where
  type
    Rs UpdateGroupCertificateConfiguration =
      UpdateGroupCertificateConfigurationResponse
  request = Req.putJSON greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateGroupCertificateConfigurationResponse'
            Lude.<$> (x Lude..?> "CertificateAuthorityExpiryInMilliseconds")
            Lude.<*> (x Lude..?> "GroupId")
            Lude.<*> (x Lude..?> "CertificateExpiryInMilliseconds")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateGroupCertificateConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateGroupCertificateConfiguration where
  toJSON UpdateGroupCertificateConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CertificateExpiryInMilliseconds" Lude..=)
              Lude.<$> certificateExpiryInMilliseconds
          ]
      )

instance Lude.ToPath UpdateGroupCertificateConfiguration where
  toPath UpdateGroupCertificateConfiguration' {..} =
    Lude.mconcat
      [ "/greengrass/groups/",
        Lude.toBS groupId,
        "/certificateauthorities/configuration/expiry"
      ]

instance Lude.ToQuery UpdateGroupCertificateConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateGroupCertificateConfigurationResponse' smart constructor.
data UpdateGroupCertificateConfigurationResponse = UpdateGroupCertificateConfigurationResponse'
  { -- | The amount of time remaining before the certificate authority expires, in milliseconds.
    certificateAuthorityExpiryInMilliseconds :: Lude.Maybe Lude.Text,
    -- | The ID of the group certificate configuration.
    groupId :: Lude.Maybe Lude.Text,
    -- | The amount of time remaining before the certificate expires, in milliseconds.
    certificateExpiryInMilliseconds :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGroupCertificateConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'certificateAuthorityExpiryInMilliseconds' - The amount of time remaining before the certificate authority expires, in milliseconds.
-- * 'groupId' - The ID of the group certificate configuration.
-- * 'certificateExpiryInMilliseconds' - The amount of time remaining before the certificate expires, in milliseconds.
-- * 'responseStatus' - The response status code.
mkUpdateGroupCertificateConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateGroupCertificateConfigurationResponse
mkUpdateGroupCertificateConfigurationResponse pResponseStatus_ =
  UpdateGroupCertificateConfigurationResponse'
    { certificateAuthorityExpiryInMilliseconds =
        Lude.Nothing,
      groupId = Lude.Nothing,
      certificateExpiryInMilliseconds = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The amount of time remaining before the certificate authority expires, in milliseconds.
--
-- /Note:/ Consider using 'certificateAuthorityExpiryInMilliseconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugccrsCertificateAuthorityExpiryInMilliseconds :: Lens.Lens' UpdateGroupCertificateConfigurationResponse (Lude.Maybe Lude.Text)
ugccrsCertificateAuthorityExpiryInMilliseconds = Lens.lens (certificateAuthorityExpiryInMilliseconds :: UpdateGroupCertificateConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificateAuthorityExpiryInMilliseconds = a} :: UpdateGroupCertificateConfigurationResponse)
{-# DEPRECATED ugccrsCertificateAuthorityExpiryInMilliseconds "Use generic-lens or generic-optics with 'certificateAuthorityExpiryInMilliseconds' instead." #-}

-- | The ID of the group certificate configuration.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugccrsGroupId :: Lens.Lens' UpdateGroupCertificateConfigurationResponse (Lude.Maybe Lude.Text)
ugccrsGroupId = Lens.lens (groupId :: UpdateGroupCertificateConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {groupId = a} :: UpdateGroupCertificateConfigurationResponse)
{-# DEPRECATED ugccrsGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The amount of time remaining before the certificate expires, in milliseconds.
--
-- /Note:/ Consider using 'certificateExpiryInMilliseconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugccrsCertificateExpiryInMilliseconds :: Lens.Lens' UpdateGroupCertificateConfigurationResponse (Lude.Maybe Lude.Text)
ugccrsCertificateExpiryInMilliseconds = Lens.lens (certificateExpiryInMilliseconds :: UpdateGroupCertificateConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificateExpiryInMilliseconds = a} :: UpdateGroupCertificateConfigurationResponse)
{-# DEPRECATED ugccrsCertificateExpiryInMilliseconds "Use generic-lens or generic-optics with 'certificateExpiryInMilliseconds' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugccrsResponseStatus :: Lens.Lens' UpdateGroupCertificateConfigurationResponse Lude.Int
ugccrsResponseStatus = Lens.lens (responseStatus :: UpdateGroupCertificateConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateGroupCertificateConfigurationResponse)
{-# DEPRECATED ugccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
