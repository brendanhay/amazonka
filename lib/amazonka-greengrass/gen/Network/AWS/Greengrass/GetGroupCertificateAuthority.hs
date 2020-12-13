{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetGroupCertificateAuthority
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retreives the CA associated with a group. Returns the public key of the CA.
module Network.AWS.Greengrass.GetGroupCertificateAuthority
  ( -- * Creating a request
    GetGroupCertificateAuthority (..),
    mkGetGroupCertificateAuthority,

    -- ** Request lenses
    ggcaCertificateAuthorityId,
    ggcaGroupId,

    -- * Destructuring the response
    GetGroupCertificateAuthorityResponse (..),
    mkGetGroupCertificateAuthorityResponse,

    -- ** Response lenses
    ggcarsPemEncodedCertificate,
    ggcarsGroupCertificateAuthorityARN,
    ggcarsGroupCertificateAuthorityId,
    ggcarsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetGroupCertificateAuthority' smart constructor.
data GetGroupCertificateAuthority = GetGroupCertificateAuthority'
  { -- | The ID of the certificate authority.
    certificateAuthorityId :: Lude.Text,
    -- | The ID of the Greengrass group.
    groupId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGroupCertificateAuthority' with the minimum fields required to make a request.
--
-- * 'certificateAuthorityId' - The ID of the certificate authority.
-- * 'groupId' - The ID of the Greengrass group.
mkGetGroupCertificateAuthority ::
  -- | 'certificateAuthorityId'
  Lude.Text ->
  -- | 'groupId'
  Lude.Text ->
  GetGroupCertificateAuthority
mkGetGroupCertificateAuthority pCertificateAuthorityId_ pGroupId_ =
  GetGroupCertificateAuthority'
    { certificateAuthorityId =
        pCertificateAuthorityId_,
      groupId = pGroupId_
    }

-- | The ID of the certificate authority.
--
-- /Note:/ Consider using 'certificateAuthorityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggcaCertificateAuthorityId :: Lens.Lens' GetGroupCertificateAuthority Lude.Text
ggcaCertificateAuthorityId = Lens.lens (certificateAuthorityId :: GetGroupCertificateAuthority -> Lude.Text) (\s a -> s {certificateAuthorityId = a} :: GetGroupCertificateAuthority)
{-# DEPRECATED ggcaCertificateAuthorityId "Use generic-lens or generic-optics with 'certificateAuthorityId' instead." #-}

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggcaGroupId :: Lens.Lens' GetGroupCertificateAuthority Lude.Text
ggcaGroupId = Lens.lens (groupId :: GetGroupCertificateAuthority -> Lude.Text) (\s a -> s {groupId = a} :: GetGroupCertificateAuthority)
{-# DEPRECATED ggcaGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

instance Lude.AWSRequest GetGroupCertificateAuthority where
  type
    Rs GetGroupCertificateAuthority =
      GetGroupCertificateAuthorityResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetGroupCertificateAuthorityResponse'
            Lude.<$> (x Lude..?> "PemEncodedCertificate")
            Lude.<*> (x Lude..?> "GroupCertificateAuthorityArn")
            Lude.<*> (x Lude..?> "GroupCertificateAuthorityId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetGroupCertificateAuthority where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetGroupCertificateAuthority where
  toPath GetGroupCertificateAuthority' {..} =
    Lude.mconcat
      [ "/greengrass/groups/",
        Lude.toBS groupId,
        "/certificateauthorities/",
        Lude.toBS certificateAuthorityId
      ]

instance Lude.ToQuery GetGroupCertificateAuthority where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetGroupCertificateAuthorityResponse' smart constructor.
data GetGroupCertificateAuthorityResponse = GetGroupCertificateAuthorityResponse'
  { -- | The PEM encoded certificate for the group.
    pemEncodedCertificate :: Lude.Maybe Lude.Text,
    -- | The ARN of the certificate authority for the group.
    groupCertificateAuthorityARN :: Lude.Maybe Lude.Text,
    -- | The ID of the certificate authority for the group.
    groupCertificateAuthorityId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGroupCertificateAuthorityResponse' with the minimum fields required to make a request.
--
-- * 'pemEncodedCertificate' - The PEM encoded certificate for the group.
-- * 'groupCertificateAuthorityARN' - The ARN of the certificate authority for the group.
-- * 'groupCertificateAuthorityId' - The ID of the certificate authority for the group.
-- * 'responseStatus' - The response status code.
mkGetGroupCertificateAuthorityResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetGroupCertificateAuthorityResponse
mkGetGroupCertificateAuthorityResponse pResponseStatus_ =
  GetGroupCertificateAuthorityResponse'
    { pemEncodedCertificate =
        Lude.Nothing,
      groupCertificateAuthorityARN = Lude.Nothing,
      groupCertificateAuthorityId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The PEM encoded certificate for the group.
--
-- /Note:/ Consider using 'pemEncodedCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggcarsPemEncodedCertificate :: Lens.Lens' GetGroupCertificateAuthorityResponse (Lude.Maybe Lude.Text)
ggcarsPemEncodedCertificate = Lens.lens (pemEncodedCertificate :: GetGroupCertificateAuthorityResponse -> Lude.Maybe Lude.Text) (\s a -> s {pemEncodedCertificate = a} :: GetGroupCertificateAuthorityResponse)
{-# DEPRECATED ggcarsPemEncodedCertificate "Use generic-lens or generic-optics with 'pemEncodedCertificate' instead." #-}

-- | The ARN of the certificate authority for the group.
--
-- /Note:/ Consider using 'groupCertificateAuthorityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggcarsGroupCertificateAuthorityARN :: Lens.Lens' GetGroupCertificateAuthorityResponse (Lude.Maybe Lude.Text)
ggcarsGroupCertificateAuthorityARN = Lens.lens (groupCertificateAuthorityARN :: GetGroupCertificateAuthorityResponse -> Lude.Maybe Lude.Text) (\s a -> s {groupCertificateAuthorityARN = a} :: GetGroupCertificateAuthorityResponse)
{-# DEPRECATED ggcarsGroupCertificateAuthorityARN "Use generic-lens or generic-optics with 'groupCertificateAuthorityARN' instead." #-}

-- | The ID of the certificate authority for the group.
--
-- /Note:/ Consider using 'groupCertificateAuthorityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggcarsGroupCertificateAuthorityId :: Lens.Lens' GetGroupCertificateAuthorityResponse (Lude.Maybe Lude.Text)
ggcarsGroupCertificateAuthorityId = Lens.lens (groupCertificateAuthorityId :: GetGroupCertificateAuthorityResponse -> Lude.Maybe Lude.Text) (\s a -> s {groupCertificateAuthorityId = a} :: GetGroupCertificateAuthorityResponse)
{-# DEPRECATED ggcarsGroupCertificateAuthorityId "Use generic-lens or generic-optics with 'groupCertificateAuthorityId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggcarsResponseStatus :: Lens.Lens' GetGroupCertificateAuthorityResponse Lude.Int
ggcarsResponseStatus = Lens.lens (responseStatus :: GetGroupCertificateAuthorityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetGroupCertificateAuthorityResponse)
{-# DEPRECATED ggcarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
