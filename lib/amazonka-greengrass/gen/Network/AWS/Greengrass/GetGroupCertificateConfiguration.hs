{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetGroupCertificateConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current configuration for the CA used by the group.
module Network.AWS.Greengrass.GetGroupCertificateConfiguration
  ( -- * Creating a request
    GetGroupCertificateConfiguration (..),
    mkGetGroupCertificateConfiguration,

    -- ** Request lenses
    ggccGroupId,

    -- * Destructuring the response
    GetGroupCertificateConfigurationResponse (..),
    mkGetGroupCertificateConfigurationResponse,

    -- ** Response lenses
    ggccrsCertificateAuthorityExpiryInMilliseconds,
    ggccrsGroupId,
    ggccrsCertificateExpiryInMilliseconds,
    ggccrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetGroupCertificateConfiguration' smart constructor.
newtype GetGroupCertificateConfiguration = GetGroupCertificateConfiguration'
  { -- | The ID of the Greengrass group.
    groupId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGroupCertificateConfiguration' with the minimum fields required to make a request.
--
-- * 'groupId' - The ID of the Greengrass group.
mkGetGroupCertificateConfiguration ::
  -- | 'groupId'
  Lude.Text ->
  GetGroupCertificateConfiguration
mkGetGroupCertificateConfiguration pGroupId_ =
  GetGroupCertificateConfiguration' {groupId = pGroupId_}

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggccGroupId :: Lens.Lens' GetGroupCertificateConfiguration Lude.Text
ggccGroupId = Lens.lens (groupId :: GetGroupCertificateConfiguration -> Lude.Text) (\s a -> s {groupId = a} :: GetGroupCertificateConfiguration)
{-# DEPRECATED ggccGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

instance Lude.AWSRequest GetGroupCertificateConfiguration where
  type
    Rs GetGroupCertificateConfiguration =
      GetGroupCertificateConfigurationResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetGroupCertificateConfigurationResponse'
            Lude.<$> (x Lude..?> "CertificateAuthorityExpiryInMilliseconds")
            Lude.<*> (x Lude..?> "GroupId")
            Lude.<*> (x Lude..?> "CertificateExpiryInMilliseconds")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetGroupCertificateConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetGroupCertificateConfiguration where
  toPath GetGroupCertificateConfiguration' {..} =
    Lude.mconcat
      [ "/greengrass/groups/",
        Lude.toBS groupId,
        "/certificateauthorities/configuration/expiry"
      ]

instance Lude.ToQuery GetGroupCertificateConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetGroupCertificateConfigurationResponse' smart constructor.
data GetGroupCertificateConfigurationResponse = GetGroupCertificateConfigurationResponse'
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

-- | Creates a value of 'GetGroupCertificateConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'certificateAuthorityExpiryInMilliseconds' - The amount of time remaining before the certificate authority expires, in milliseconds.
-- * 'groupId' - The ID of the group certificate configuration.
-- * 'certificateExpiryInMilliseconds' - The amount of time remaining before the certificate expires, in milliseconds.
-- * 'responseStatus' - The response status code.
mkGetGroupCertificateConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetGroupCertificateConfigurationResponse
mkGetGroupCertificateConfigurationResponse pResponseStatus_ =
  GetGroupCertificateConfigurationResponse'
    { certificateAuthorityExpiryInMilliseconds =
        Lude.Nothing,
      groupId = Lude.Nothing,
      certificateExpiryInMilliseconds = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The amount of time remaining before the certificate authority expires, in milliseconds.
--
-- /Note:/ Consider using 'certificateAuthorityExpiryInMilliseconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggccrsCertificateAuthorityExpiryInMilliseconds :: Lens.Lens' GetGroupCertificateConfigurationResponse (Lude.Maybe Lude.Text)
ggccrsCertificateAuthorityExpiryInMilliseconds = Lens.lens (certificateAuthorityExpiryInMilliseconds :: GetGroupCertificateConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificateAuthorityExpiryInMilliseconds = a} :: GetGroupCertificateConfigurationResponse)
{-# DEPRECATED ggccrsCertificateAuthorityExpiryInMilliseconds "Use generic-lens or generic-optics with 'certificateAuthorityExpiryInMilliseconds' instead." #-}

-- | The ID of the group certificate configuration.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggccrsGroupId :: Lens.Lens' GetGroupCertificateConfigurationResponse (Lude.Maybe Lude.Text)
ggccrsGroupId = Lens.lens (groupId :: GetGroupCertificateConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {groupId = a} :: GetGroupCertificateConfigurationResponse)
{-# DEPRECATED ggccrsGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The amount of time remaining before the certificate expires, in milliseconds.
--
-- /Note:/ Consider using 'certificateExpiryInMilliseconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggccrsCertificateExpiryInMilliseconds :: Lens.Lens' GetGroupCertificateConfigurationResponse (Lude.Maybe Lude.Text)
ggccrsCertificateExpiryInMilliseconds = Lens.lens (certificateExpiryInMilliseconds :: GetGroupCertificateConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificateExpiryInMilliseconds = a} :: GetGroupCertificateConfigurationResponse)
{-# DEPRECATED ggccrsCertificateExpiryInMilliseconds "Use generic-lens or generic-optics with 'certificateExpiryInMilliseconds' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggccrsResponseStatus :: Lens.Lens' GetGroupCertificateConfigurationResponse Lude.Int
ggccrsResponseStatus = Lens.lens (responseStatus :: GetGroupCertificateConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetGroupCertificateConfigurationResponse)
{-# DEPRECATED ggccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
