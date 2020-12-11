{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateCACertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a registered CA certificate.
module Network.AWS.IoT.UpdateCACertificate
  ( -- * Creating a request
    UpdateCACertificate (..),
    mkUpdateCACertificate,

    -- ** Request lenses
    ucacRemoveAutoRegistration,
    ucacNewStatus,
    ucacRegistrationConfig,
    ucacNewAutoRegistrationStatus,
    ucacCertificateId,

    -- * Destructuring the response
    UpdateCACertificateResponse (..),
    mkUpdateCACertificateResponse,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input to the UpdateCACertificate operation.
--
-- /See:/ 'mkUpdateCACertificate' smart constructor.
data UpdateCACertificate = UpdateCACertificate'
  { removeAutoRegistration ::
      Lude.Maybe Lude.Bool,
    newStatus :: Lude.Maybe CACertificateStatus,
    registrationConfig :: Lude.Maybe RegistrationConfig,
    newAutoRegistrationStatus ::
      Lude.Maybe AutoRegistrationStatus,
    certificateId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCACertificate' with the minimum fields required to make a request.
--
-- * 'certificateId' - The CA certificate identifier.
-- * 'newAutoRegistrationStatus' - The new value for the auto registration status. Valid values are: "ENABLE" or "DISABLE".
-- * 'newStatus' - The updated status of the CA certificate.
--
-- __Note:__ The status value REGISTER_INACTIVE is deprecated and should not be used.
-- * 'registrationConfig' - Information about the registration configuration.
-- * 'removeAutoRegistration' - If true, removes auto registration.
mkUpdateCACertificate ::
  -- | 'certificateId'
  Lude.Text ->
  UpdateCACertificate
mkUpdateCACertificate pCertificateId_ =
  UpdateCACertificate'
    { removeAutoRegistration = Lude.Nothing,
      newStatus = Lude.Nothing,
      registrationConfig = Lude.Nothing,
      newAutoRegistrationStatus = Lude.Nothing,
      certificateId = pCertificateId_
    }

-- | If true, removes auto registration.
--
-- /Note:/ Consider using 'removeAutoRegistration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucacRemoveAutoRegistration :: Lens.Lens' UpdateCACertificate (Lude.Maybe Lude.Bool)
ucacRemoveAutoRegistration = Lens.lens (removeAutoRegistration :: UpdateCACertificate -> Lude.Maybe Lude.Bool) (\s a -> s {removeAutoRegistration = a} :: UpdateCACertificate)
{-# DEPRECATED ucacRemoveAutoRegistration "Use generic-lens or generic-optics with 'removeAutoRegistration' instead." #-}

-- | The updated status of the CA certificate.
--
-- __Note:__ The status value REGISTER_INACTIVE is deprecated and should not be used.
--
-- /Note:/ Consider using 'newStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucacNewStatus :: Lens.Lens' UpdateCACertificate (Lude.Maybe CACertificateStatus)
ucacNewStatus = Lens.lens (newStatus :: UpdateCACertificate -> Lude.Maybe CACertificateStatus) (\s a -> s {newStatus = a} :: UpdateCACertificate)
{-# DEPRECATED ucacNewStatus "Use generic-lens or generic-optics with 'newStatus' instead." #-}

-- | Information about the registration configuration.
--
-- /Note:/ Consider using 'registrationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucacRegistrationConfig :: Lens.Lens' UpdateCACertificate (Lude.Maybe RegistrationConfig)
ucacRegistrationConfig = Lens.lens (registrationConfig :: UpdateCACertificate -> Lude.Maybe RegistrationConfig) (\s a -> s {registrationConfig = a} :: UpdateCACertificate)
{-# DEPRECATED ucacRegistrationConfig "Use generic-lens or generic-optics with 'registrationConfig' instead." #-}

-- | The new value for the auto registration status. Valid values are: "ENABLE" or "DISABLE".
--
-- /Note:/ Consider using 'newAutoRegistrationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucacNewAutoRegistrationStatus :: Lens.Lens' UpdateCACertificate (Lude.Maybe AutoRegistrationStatus)
ucacNewAutoRegistrationStatus = Lens.lens (newAutoRegistrationStatus :: UpdateCACertificate -> Lude.Maybe AutoRegistrationStatus) (\s a -> s {newAutoRegistrationStatus = a} :: UpdateCACertificate)
{-# DEPRECATED ucacNewAutoRegistrationStatus "Use generic-lens or generic-optics with 'newAutoRegistrationStatus' instead." #-}

-- | The CA certificate identifier.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucacCertificateId :: Lens.Lens' UpdateCACertificate Lude.Text
ucacCertificateId = Lens.lens (certificateId :: UpdateCACertificate -> Lude.Text) (\s a -> s {certificateId = a} :: UpdateCACertificate)
{-# DEPRECATED ucacCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

instance Lude.AWSRequest UpdateCACertificate where
  type Rs UpdateCACertificate = UpdateCACertificateResponse
  request = Req.putJSON ioTService
  response = Res.receiveNull UpdateCACertificateResponse'

instance Lude.ToHeaders UpdateCACertificate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateCACertificate where
  toJSON UpdateCACertificate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("removeAutoRegistration" Lude..=)
              Lude.<$> removeAutoRegistration,
            ("registrationConfig" Lude..=) Lude.<$> registrationConfig
          ]
      )

instance Lude.ToPath UpdateCACertificate where
  toPath UpdateCACertificate' {..} =
    Lude.mconcat ["/cacertificate/", Lude.toBS certificateId]

instance Lude.ToQuery UpdateCACertificate where
  toQuery UpdateCACertificate' {..} =
    Lude.mconcat
      [ "newStatus" Lude.=: newStatus,
        "newAutoRegistrationStatus" Lude.=: newAutoRegistrationStatus
      ]

-- | /See:/ 'mkUpdateCACertificateResponse' smart constructor.
data UpdateCACertificateResponse = UpdateCACertificateResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCACertificateResponse' with the minimum fields required to make a request.
mkUpdateCACertificateResponse ::
  UpdateCACertificateResponse
mkUpdateCACertificateResponse = UpdateCACertificateResponse'
