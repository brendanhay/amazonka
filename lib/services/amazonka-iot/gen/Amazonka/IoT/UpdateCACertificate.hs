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
-- Module      : Amazonka.IoT.UpdateCACertificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a registered CA certificate.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions UpdateCACertificate>
-- action.
module Amazonka.IoT.UpdateCACertificate
  ( -- * Creating a Request
    UpdateCACertificate (..),
    newUpdateCACertificate,

    -- * Request Lenses
    updateCACertificate_newAutoRegistrationStatus,
    updateCACertificate_newStatus,
    updateCACertificate_registrationConfig,
    updateCACertificate_removeAutoRegistration,
    updateCACertificate_certificateId,

    -- * Destructuring the Response
    UpdateCACertificateResponse (..),
    newUpdateCACertificateResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input to the UpdateCACertificate operation.
--
-- /See:/ 'newUpdateCACertificate' smart constructor.
data UpdateCACertificate = UpdateCACertificate'
  { -- | The new value for the auto registration status. Valid values are:
    -- \"ENABLE\" or \"DISABLE\".
    newAutoRegistrationStatus' :: Prelude.Maybe AutoRegistrationStatus,
    -- | The updated status of the CA certificate.
    --
    -- __Note:__ The status value REGISTER_INACTIVE is deprecated and should
    -- not be used.
    newStatus' :: Prelude.Maybe CACertificateStatus,
    -- | Information about the registration configuration.
    registrationConfig :: Prelude.Maybe RegistrationConfig,
    -- | If true, removes auto registration.
    removeAutoRegistration :: Prelude.Maybe Prelude.Bool,
    -- | The CA certificate identifier.
    certificateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCACertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'newAutoRegistrationStatus'', 'updateCACertificate_newAutoRegistrationStatus' - The new value for the auto registration status. Valid values are:
-- \"ENABLE\" or \"DISABLE\".
--
-- 'newStatus'', 'updateCACertificate_newStatus' - The updated status of the CA certificate.
--
-- __Note:__ The status value REGISTER_INACTIVE is deprecated and should
-- not be used.
--
-- 'registrationConfig', 'updateCACertificate_registrationConfig' - Information about the registration configuration.
--
-- 'removeAutoRegistration', 'updateCACertificate_removeAutoRegistration' - If true, removes auto registration.
--
-- 'certificateId', 'updateCACertificate_certificateId' - The CA certificate identifier.
newUpdateCACertificate ::
  -- | 'certificateId'
  Prelude.Text ->
  UpdateCACertificate
newUpdateCACertificate pCertificateId_ =
  UpdateCACertificate'
    { newAutoRegistrationStatus' =
        Prelude.Nothing,
      newStatus' = Prelude.Nothing,
      registrationConfig = Prelude.Nothing,
      removeAutoRegistration = Prelude.Nothing,
      certificateId = pCertificateId_
    }

-- | The new value for the auto registration status. Valid values are:
-- \"ENABLE\" or \"DISABLE\".
updateCACertificate_newAutoRegistrationStatus :: Lens.Lens' UpdateCACertificate (Prelude.Maybe AutoRegistrationStatus)
updateCACertificate_newAutoRegistrationStatus = Lens.lens (\UpdateCACertificate' {newAutoRegistrationStatus'} -> newAutoRegistrationStatus') (\s@UpdateCACertificate' {} a -> s {newAutoRegistrationStatus' = a} :: UpdateCACertificate)

-- | The updated status of the CA certificate.
--
-- __Note:__ The status value REGISTER_INACTIVE is deprecated and should
-- not be used.
updateCACertificate_newStatus :: Lens.Lens' UpdateCACertificate (Prelude.Maybe CACertificateStatus)
updateCACertificate_newStatus = Lens.lens (\UpdateCACertificate' {newStatus'} -> newStatus') (\s@UpdateCACertificate' {} a -> s {newStatus' = a} :: UpdateCACertificate)

-- | Information about the registration configuration.
updateCACertificate_registrationConfig :: Lens.Lens' UpdateCACertificate (Prelude.Maybe RegistrationConfig)
updateCACertificate_registrationConfig = Lens.lens (\UpdateCACertificate' {registrationConfig} -> registrationConfig) (\s@UpdateCACertificate' {} a -> s {registrationConfig = a} :: UpdateCACertificate)

-- | If true, removes auto registration.
updateCACertificate_removeAutoRegistration :: Lens.Lens' UpdateCACertificate (Prelude.Maybe Prelude.Bool)
updateCACertificate_removeAutoRegistration = Lens.lens (\UpdateCACertificate' {removeAutoRegistration} -> removeAutoRegistration) (\s@UpdateCACertificate' {} a -> s {removeAutoRegistration = a} :: UpdateCACertificate)

-- | The CA certificate identifier.
updateCACertificate_certificateId :: Lens.Lens' UpdateCACertificate Prelude.Text
updateCACertificate_certificateId = Lens.lens (\UpdateCACertificate' {certificateId} -> certificateId) (\s@UpdateCACertificate' {} a -> s {certificateId = a} :: UpdateCACertificate)

instance Core.AWSRequest UpdateCACertificate where
  type
    AWSResponse UpdateCACertificate =
      UpdateCACertificateResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateCACertificateResponse'

instance Prelude.Hashable UpdateCACertificate where
  hashWithSalt _salt UpdateCACertificate' {..} =
    _salt
      `Prelude.hashWithSalt` newAutoRegistrationStatus'
      `Prelude.hashWithSalt` newStatus'
      `Prelude.hashWithSalt` registrationConfig
      `Prelude.hashWithSalt` removeAutoRegistration
      `Prelude.hashWithSalt` certificateId

instance Prelude.NFData UpdateCACertificate where
  rnf UpdateCACertificate' {..} =
    Prelude.rnf newAutoRegistrationStatus'
      `Prelude.seq` Prelude.rnf newStatus'
      `Prelude.seq` Prelude.rnf registrationConfig
      `Prelude.seq` Prelude.rnf removeAutoRegistration
      `Prelude.seq` Prelude.rnf certificateId

instance Data.ToHeaders UpdateCACertificate where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateCACertificate where
  toJSON UpdateCACertificate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("registrationConfig" Data..=)
              Prelude.<$> registrationConfig,
            ("removeAutoRegistration" Data..=)
              Prelude.<$> removeAutoRegistration
          ]
      )

instance Data.ToPath UpdateCACertificate where
  toPath UpdateCACertificate' {..} =
    Prelude.mconcat
      ["/cacertificate/", Data.toBS certificateId]

instance Data.ToQuery UpdateCACertificate where
  toQuery UpdateCACertificate' {..} =
    Prelude.mconcat
      [ "newAutoRegistrationStatus"
          Data.=: newAutoRegistrationStatus',
        "newStatus" Data.=: newStatus'
      ]

-- | /See:/ 'newUpdateCACertificateResponse' smart constructor.
data UpdateCACertificateResponse = UpdateCACertificateResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCACertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateCACertificateResponse ::
  UpdateCACertificateResponse
newUpdateCACertificateResponse =
  UpdateCACertificateResponse'

instance Prelude.NFData UpdateCACertificateResponse where
  rnf _ = ()
