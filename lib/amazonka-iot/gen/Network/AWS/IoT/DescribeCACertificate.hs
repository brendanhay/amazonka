{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeCACertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a registered CA certificate.
module Network.AWS.IoT.DescribeCACertificate
  ( -- * Creating a request
    DescribeCACertificate (..),
    mkDescribeCACertificate,

    -- ** Request lenses
    dCertificateId,

    -- * Destructuring the response
    DescribeCACertificateResponse (..),
    mkDescribeCACertificateResponse,

    -- ** Response lenses
    dcacfrsCertificateDescription,
    dcacfrsRegistrationConfig,
    dcacfrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the DescribeCACertificate operation.
--
-- /See:/ 'mkDescribeCACertificate' smart constructor.
newtype DescribeCACertificate = DescribeCACertificate'
  { -- | The CA certificate identifier.
    certificateId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCACertificate' with the minimum fields required to make a request.
--
-- * 'certificateId' - The CA certificate identifier.
mkDescribeCACertificate ::
  -- | 'certificateId'
  Lude.Text ->
  DescribeCACertificate
mkDescribeCACertificate pCertificateId_ =
  DescribeCACertificate' {certificateId = pCertificateId_}

-- | The CA certificate identifier.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCertificateId :: Lens.Lens' DescribeCACertificate Lude.Text
dCertificateId = Lens.lens (certificateId :: DescribeCACertificate -> Lude.Text) (\s a -> s {certificateId = a} :: DescribeCACertificate)
{-# DEPRECATED dCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

instance Lude.AWSRequest DescribeCACertificate where
  type Rs DescribeCACertificate = DescribeCACertificateResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeCACertificateResponse'
            Lude.<$> (x Lude..?> "certificateDescription")
            Lude.<*> (x Lude..?> "registrationConfig")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeCACertificate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeCACertificate where
  toPath DescribeCACertificate' {..} =
    Lude.mconcat ["/cacertificate/", Lude.toBS certificateId]

instance Lude.ToQuery DescribeCACertificate where
  toQuery = Lude.const Lude.mempty

-- | The output from the DescribeCACertificate operation.
--
-- /See:/ 'mkDescribeCACertificateResponse' smart constructor.
data DescribeCACertificateResponse = DescribeCACertificateResponse'
  { -- | The CA certificate description.
    certificateDescription :: Lude.Maybe CACertificateDescription,
    -- | Information about the registration configuration.
    registrationConfig :: Lude.Maybe RegistrationConfig,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCACertificateResponse' with the minimum fields required to make a request.
--
-- * 'certificateDescription' - The CA certificate description.
-- * 'registrationConfig' - Information about the registration configuration.
-- * 'responseStatus' - The response status code.
mkDescribeCACertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeCACertificateResponse
mkDescribeCACertificateResponse pResponseStatus_ =
  DescribeCACertificateResponse'
    { certificateDescription =
        Lude.Nothing,
      registrationConfig = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The CA certificate description.
--
-- /Note:/ Consider using 'certificateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcacfrsCertificateDescription :: Lens.Lens' DescribeCACertificateResponse (Lude.Maybe CACertificateDescription)
dcacfrsCertificateDescription = Lens.lens (certificateDescription :: DescribeCACertificateResponse -> Lude.Maybe CACertificateDescription) (\s a -> s {certificateDescription = a} :: DescribeCACertificateResponse)
{-# DEPRECATED dcacfrsCertificateDescription "Use generic-lens or generic-optics with 'certificateDescription' instead." #-}

-- | Information about the registration configuration.
--
-- /Note:/ Consider using 'registrationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcacfrsRegistrationConfig :: Lens.Lens' DescribeCACertificateResponse (Lude.Maybe RegistrationConfig)
dcacfrsRegistrationConfig = Lens.lens (registrationConfig :: DescribeCACertificateResponse -> Lude.Maybe RegistrationConfig) (\s a -> s {registrationConfig = a} :: DescribeCACertificateResponse)
{-# DEPRECATED dcacfrsRegistrationConfig "Use generic-lens or generic-optics with 'registrationConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcacfrsResponseStatus :: Lens.Lens' DescribeCACertificateResponse Lude.Int
dcacfrsResponseStatus = Lens.lens (responseStatus :: DescribeCACertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCACertificateResponse)
{-# DEPRECATED dcacfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
