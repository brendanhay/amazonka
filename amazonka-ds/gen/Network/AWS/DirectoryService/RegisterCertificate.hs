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
-- Module      : Network.AWS.DirectoryService.RegisterCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a certificate for a secure LDAP or client certificate
-- authentication.
module Network.AWS.DirectoryService.RegisterCertificate
  ( -- * Creating a Request
    RegisterCertificate (..),
    newRegisterCertificate,

    -- * Request Lenses
    registerCertificate_clientCertAuthSettings,
    registerCertificate_type,
    registerCertificate_directoryId,
    registerCertificate_certificateData,

    -- * Destructuring the Response
    RegisterCertificateResponse (..),
    newRegisterCertificateResponse,

    -- * Response Lenses
    registerCertificateResponse_certificateId,
    registerCertificateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRegisterCertificate' smart constructor.
data RegisterCertificate = RegisterCertificate'
  { -- | A @ClientCertAuthSettings@ object that contains client certificate
    -- authentication settings.
    clientCertAuthSettings :: Core.Maybe ClientCertAuthSettings,
    -- | The function that the registered certificate performs. Valid values
    -- include @ClientLDAPS@ or @ClientCertAuth@. The default value is
    -- @ClientLDAPS@.
    type' :: Core.Maybe CertificateType,
    -- | The identifier of the directory.
    directoryId :: Core.Text,
    -- | The certificate PEM string that needs to be registered.
    certificateData :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientCertAuthSettings', 'registerCertificate_clientCertAuthSettings' - A @ClientCertAuthSettings@ object that contains client certificate
-- authentication settings.
--
-- 'type'', 'registerCertificate_type' - The function that the registered certificate performs. Valid values
-- include @ClientLDAPS@ or @ClientCertAuth@. The default value is
-- @ClientLDAPS@.
--
-- 'directoryId', 'registerCertificate_directoryId' - The identifier of the directory.
--
-- 'certificateData', 'registerCertificate_certificateData' - The certificate PEM string that needs to be registered.
newRegisterCertificate ::
  -- | 'directoryId'
  Core.Text ->
  -- | 'certificateData'
  Core.Text ->
  RegisterCertificate
newRegisterCertificate
  pDirectoryId_
  pCertificateData_ =
    RegisterCertificate'
      { clientCertAuthSettings =
          Core.Nothing,
        type' = Core.Nothing,
        directoryId = pDirectoryId_,
        certificateData = pCertificateData_
      }

-- | A @ClientCertAuthSettings@ object that contains client certificate
-- authentication settings.
registerCertificate_clientCertAuthSettings :: Lens.Lens' RegisterCertificate (Core.Maybe ClientCertAuthSettings)
registerCertificate_clientCertAuthSettings = Lens.lens (\RegisterCertificate' {clientCertAuthSettings} -> clientCertAuthSettings) (\s@RegisterCertificate' {} a -> s {clientCertAuthSettings = a} :: RegisterCertificate)

-- | The function that the registered certificate performs. Valid values
-- include @ClientLDAPS@ or @ClientCertAuth@. The default value is
-- @ClientLDAPS@.
registerCertificate_type :: Lens.Lens' RegisterCertificate (Core.Maybe CertificateType)
registerCertificate_type = Lens.lens (\RegisterCertificate' {type'} -> type') (\s@RegisterCertificate' {} a -> s {type' = a} :: RegisterCertificate)

-- | The identifier of the directory.
registerCertificate_directoryId :: Lens.Lens' RegisterCertificate Core.Text
registerCertificate_directoryId = Lens.lens (\RegisterCertificate' {directoryId} -> directoryId) (\s@RegisterCertificate' {} a -> s {directoryId = a} :: RegisterCertificate)

-- | The certificate PEM string that needs to be registered.
registerCertificate_certificateData :: Lens.Lens' RegisterCertificate Core.Text
registerCertificate_certificateData = Lens.lens (\RegisterCertificate' {certificateData} -> certificateData) (\s@RegisterCertificate' {} a -> s {certificateData = a} :: RegisterCertificate)

instance Core.AWSRequest RegisterCertificate where
  type
    AWSResponse RegisterCertificate =
      RegisterCertificateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterCertificateResponse'
            Core.<$> (x Core..?> "CertificateId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RegisterCertificate

instance Core.NFData RegisterCertificate

instance Core.ToHeaders RegisterCertificate where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.RegisterCertificate" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RegisterCertificate where
  toJSON RegisterCertificate' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ClientCertAuthSettings" Core..=)
              Core.<$> clientCertAuthSettings,
            ("Type" Core..=) Core.<$> type',
            Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just
              ("CertificateData" Core..= certificateData)
          ]
      )

instance Core.ToPath RegisterCertificate where
  toPath = Core.const "/"

instance Core.ToQuery RegisterCertificate where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRegisterCertificateResponse' smart constructor.
data RegisterCertificateResponse = RegisterCertificateResponse'
  { -- | The identifier of the certificate.
    certificateId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateId', 'registerCertificateResponse_certificateId' - The identifier of the certificate.
--
-- 'httpStatus', 'registerCertificateResponse_httpStatus' - The response's http status code.
newRegisterCertificateResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RegisterCertificateResponse
newRegisterCertificateResponse pHttpStatus_ =
  RegisterCertificateResponse'
    { certificateId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the certificate.
registerCertificateResponse_certificateId :: Lens.Lens' RegisterCertificateResponse (Core.Maybe Core.Text)
registerCertificateResponse_certificateId = Lens.lens (\RegisterCertificateResponse' {certificateId} -> certificateId) (\s@RegisterCertificateResponse' {} a -> s {certificateId = a} :: RegisterCertificateResponse)

-- | The response's http status code.
registerCertificateResponse_httpStatus :: Lens.Lens' RegisterCertificateResponse Core.Int
registerCertificateResponse_httpStatus = Lens.lens (\RegisterCertificateResponse' {httpStatus} -> httpStatus) (\s@RegisterCertificateResponse' {} a -> s {httpStatus = a} :: RegisterCertificateResponse)

instance Core.NFData RegisterCertificateResponse
