{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateCACertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a registered CA certificate.
module Network.AWS.IoT.UpdateCACertificate
  ( -- * Creating a Request
    UpdateCACertificate (..),
    newUpdateCACertificate,

    -- * Request Lenses
    updateCACertificate_removeAutoRegistration,
    updateCACertificate_newStatus,
    updateCACertificate_newAutoRegistrationStatus,
    updateCACertificate_registrationConfig,
    updateCACertificate_certificateId,

    -- * Destructuring the Response
    UpdateCACertificateResponse (..),
    newUpdateCACertificateResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input to the UpdateCACertificate operation.
--
-- /See:/ 'newUpdateCACertificate' smart constructor.
data UpdateCACertificate = UpdateCACertificate'
  { -- | If true, removes auto registration.
    removeAutoRegistration :: Core.Maybe Core.Bool,
    -- | The updated status of the CA certificate.
    --
    -- __Note:__ The status value REGISTER_INACTIVE is deprecated and should
    -- not be used.
    newStatus' :: Core.Maybe CACertificateStatus,
    -- | The new value for the auto registration status. Valid values are:
    -- \"ENABLE\" or \"DISABLE\".
    newAutoRegistrationStatus' :: Core.Maybe AutoRegistrationStatus,
    -- | Information about the registration configuration.
    registrationConfig :: Core.Maybe RegistrationConfig,
    -- | The CA certificate identifier.
    certificateId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateCACertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'removeAutoRegistration', 'updateCACertificate_removeAutoRegistration' - If true, removes auto registration.
--
-- 'newStatus'', 'updateCACertificate_newStatus' - The updated status of the CA certificate.
--
-- __Note:__ The status value REGISTER_INACTIVE is deprecated and should
-- not be used.
--
-- 'newAutoRegistrationStatus'', 'updateCACertificate_newAutoRegistrationStatus' - The new value for the auto registration status. Valid values are:
-- \"ENABLE\" or \"DISABLE\".
--
-- 'registrationConfig', 'updateCACertificate_registrationConfig' - Information about the registration configuration.
--
-- 'certificateId', 'updateCACertificate_certificateId' - The CA certificate identifier.
newUpdateCACertificate ::
  -- | 'certificateId'
  Core.Text ->
  UpdateCACertificate
newUpdateCACertificate pCertificateId_ =
  UpdateCACertificate'
    { removeAutoRegistration =
        Core.Nothing,
      newStatus' = Core.Nothing,
      newAutoRegistrationStatus' = Core.Nothing,
      registrationConfig = Core.Nothing,
      certificateId = pCertificateId_
    }

-- | If true, removes auto registration.
updateCACertificate_removeAutoRegistration :: Lens.Lens' UpdateCACertificate (Core.Maybe Core.Bool)
updateCACertificate_removeAutoRegistration = Lens.lens (\UpdateCACertificate' {removeAutoRegistration} -> removeAutoRegistration) (\s@UpdateCACertificate' {} a -> s {removeAutoRegistration = a} :: UpdateCACertificate)

-- | The updated status of the CA certificate.
--
-- __Note:__ The status value REGISTER_INACTIVE is deprecated and should
-- not be used.
updateCACertificate_newStatus :: Lens.Lens' UpdateCACertificate (Core.Maybe CACertificateStatus)
updateCACertificate_newStatus = Lens.lens (\UpdateCACertificate' {newStatus'} -> newStatus') (\s@UpdateCACertificate' {} a -> s {newStatus' = a} :: UpdateCACertificate)

-- | The new value for the auto registration status. Valid values are:
-- \"ENABLE\" or \"DISABLE\".
updateCACertificate_newAutoRegistrationStatus :: Lens.Lens' UpdateCACertificate (Core.Maybe AutoRegistrationStatus)
updateCACertificate_newAutoRegistrationStatus = Lens.lens (\UpdateCACertificate' {newAutoRegistrationStatus'} -> newAutoRegistrationStatus') (\s@UpdateCACertificate' {} a -> s {newAutoRegistrationStatus' = a} :: UpdateCACertificate)

-- | Information about the registration configuration.
updateCACertificate_registrationConfig :: Lens.Lens' UpdateCACertificate (Core.Maybe RegistrationConfig)
updateCACertificate_registrationConfig = Lens.lens (\UpdateCACertificate' {registrationConfig} -> registrationConfig) (\s@UpdateCACertificate' {} a -> s {registrationConfig = a} :: UpdateCACertificate)

-- | The CA certificate identifier.
updateCACertificate_certificateId :: Lens.Lens' UpdateCACertificate Core.Text
updateCACertificate_certificateId = Lens.lens (\UpdateCACertificate' {certificateId} -> certificateId) (\s@UpdateCACertificate' {} a -> s {certificateId = a} :: UpdateCACertificate)

instance Core.AWSRequest UpdateCACertificate where
  type
    AWSResponse UpdateCACertificate =
      UpdateCACertificateResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveNull UpdateCACertificateResponse'

instance Core.Hashable UpdateCACertificate

instance Core.NFData UpdateCACertificate

instance Core.ToHeaders UpdateCACertificate where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON UpdateCACertificate where
  toJSON UpdateCACertificate' {..} =
    Core.object
      ( Core.catMaybes
          [ ("removeAutoRegistration" Core..=)
              Core.<$> removeAutoRegistration,
            ("registrationConfig" Core..=)
              Core.<$> registrationConfig
          ]
      )

instance Core.ToPath UpdateCACertificate where
  toPath UpdateCACertificate' {..} =
    Core.mconcat
      ["/cacertificate/", Core.toBS certificateId]

instance Core.ToQuery UpdateCACertificate where
  toQuery UpdateCACertificate' {..} =
    Core.mconcat
      [ "newStatus" Core.=: newStatus',
        "newAutoRegistrationStatus"
          Core.=: newAutoRegistrationStatus'
      ]

-- | /See:/ 'newUpdateCACertificateResponse' smart constructor.
data UpdateCACertificateResponse = UpdateCACertificateResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateCACertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateCACertificateResponse ::
  UpdateCACertificateResponse
newUpdateCACertificateResponse =
  UpdateCACertificateResponse'

instance Core.NFData UpdateCACertificateResponse
