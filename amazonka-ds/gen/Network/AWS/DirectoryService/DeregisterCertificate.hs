{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DirectoryService.DeregisterCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes from the system the certificate that was registered for secure
-- LDAP or client certificate authentication.
module Network.AWS.DirectoryService.DeregisterCertificate
  ( -- * Creating a Request
    DeregisterCertificate (..),
    newDeregisterCertificate,

    -- * Request Lenses
    deregisterCertificate_directoryId,
    deregisterCertificate_certificateId,

    -- * Destructuring the Response
    DeregisterCertificateResponse (..),
    newDeregisterCertificateResponse,

    -- * Response Lenses
    deregisterCertificateResponse_httpStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeregisterCertificate' smart constructor.
data DeregisterCertificate = DeregisterCertificate'
  { -- | The identifier of the directory.
    directoryId :: Prelude.Text,
    -- | The identifier of the certificate.
    certificateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeregisterCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'deregisterCertificate_directoryId' - The identifier of the directory.
--
-- 'certificateId', 'deregisterCertificate_certificateId' - The identifier of the certificate.
newDeregisterCertificate ::
  -- | 'directoryId'
  Prelude.Text ->
  -- | 'certificateId'
  Prelude.Text ->
  DeregisterCertificate
newDeregisterCertificate
  pDirectoryId_
  pCertificateId_ =
    DeregisterCertificate'
      { directoryId = pDirectoryId_,
        certificateId = pCertificateId_
      }

-- | The identifier of the directory.
deregisterCertificate_directoryId :: Lens.Lens' DeregisterCertificate Prelude.Text
deregisterCertificate_directoryId = Lens.lens (\DeregisterCertificate' {directoryId} -> directoryId) (\s@DeregisterCertificate' {} a -> s {directoryId = a} :: DeregisterCertificate)

-- | The identifier of the certificate.
deregisterCertificate_certificateId :: Lens.Lens' DeregisterCertificate Prelude.Text
deregisterCertificate_certificateId = Lens.lens (\DeregisterCertificate' {certificateId} -> certificateId) (\s@DeregisterCertificate' {} a -> s {certificateId = a} :: DeregisterCertificate)

instance Prelude.AWSRequest DeregisterCertificate where
  type
    Rs DeregisterCertificate =
      DeregisterCertificateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeregisterCertificateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeregisterCertificate

instance Prelude.NFData DeregisterCertificate

instance Prelude.ToHeaders DeregisterCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DirectoryService_20150416.DeregisterCertificate" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeregisterCertificate where
  toJSON DeregisterCertificate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DirectoryId" Prelude..= directoryId),
            Prelude.Just
              ("CertificateId" Prelude..= certificateId)
          ]
      )

instance Prelude.ToPath DeregisterCertificate where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeregisterCertificate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterCertificateResponse' smart constructor.
data DeregisterCertificateResponse = DeregisterCertificateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeregisterCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deregisterCertificateResponse_httpStatus' - The response's http status code.
newDeregisterCertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeregisterCertificateResponse
newDeregisterCertificateResponse pHttpStatus_ =
  DeregisterCertificateResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deregisterCertificateResponse_httpStatus :: Lens.Lens' DeregisterCertificateResponse Prelude.Int
deregisterCertificateResponse_httpStatus = Lens.lens (\DeregisterCertificateResponse' {httpStatus} -> httpStatus) (\s@DeregisterCertificateResponse' {} a -> s {httpStatus = a} :: DeregisterCertificateResponse)

instance Prelude.NFData DeregisterCertificateResponse
