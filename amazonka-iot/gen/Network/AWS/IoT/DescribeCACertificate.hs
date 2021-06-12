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
-- Module      : Network.AWS.IoT.DescribeCACertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a registered CA certificate.
module Network.AWS.IoT.DescribeCACertificate
  ( -- * Creating a Request
    DescribeCACertificate (..),
    newDescribeCACertificate,

    -- * Request Lenses
    describeCACertificate_certificateId,

    -- * Destructuring the Response
    DescribeCACertificateResponse (..),
    newDescribeCACertificateResponse,

    -- * Response Lenses
    describeCACertificateResponse_certificateDescription,
    describeCACertificateResponse_registrationConfig,
    describeCACertificateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DescribeCACertificate operation.
--
-- /See:/ 'newDescribeCACertificate' smart constructor.
data DescribeCACertificate = DescribeCACertificate'
  { -- | The CA certificate identifier.
    certificateId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCACertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateId', 'describeCACertificate_certificateId' - The CA certificate identifier.
newDescribeCACertificate ::
  -- | 'certificateId'
  Core.Text ->
  DescribeCACertificate
newDescribeCACertificate pCertificateId_ =
  DescribeCACertificate'
    { certificateId =
        pCertificateId_
    }

-- | The CA certificate identifier.
describeCACertificate_certificateId :: Lens.Lens' DescribeCACertificate Core.Text
describeCACertificate_certificateId = Lens.lens (\DescribeCACertificate' {certificateId} -> certificateId) (\s@DescribeCACertificate' {} a -> s {certificateId = a} :: DescribeCACertificate)

instance Core.AWSRequest DescribeCACertificate where
  type
    AWSResponse DescribeCACertificate =
      DescribeCACertificateResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCACertificateResponse'
            Core.<$> (x Core..?> "certificateDescription")
            Core.<*> (x Core..?> "registrationConfig")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeCACertificate

instance Core.NFData DescribeCACertificate

instance Core.ToHeaders DescribeCACertificate where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeCACertificate where
  toPath DescribeCACertificate' {..} =
    Core.mconcat
      ["/cacertificate/", Core.toBS certificateId]

instance Core.ToQuery DescribeCACertificate where
  toQuery = Core.const Core.mempty

-- | The output from the DescribeCACertificate operation.
--
-- /See:/ 'newDescribeCACertificateResponse' smart constructor.
data DescribeCACertificateResponse = DescribeCACertificateResponse'
  { -- | The CA certificate description.
    certificateDescription :: Core.Maybe CACertificateDescription,
    -- | Information about the registration configuration.
    registrationConfig :: Core.Maybe RegistrationConfig,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCACertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateDescription', 'describeCACertificateResponse_certificateDescription' - The CA certificate description.
--
-- 'registrationConfig', 'describeCACertificateResponse_registrationConfig' - Information about the registration configuration.
--
-- 'httpStatus', 'describeCACertificateResponse_httpStatus' - The response's http status code.
newDescribeCACertificateResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeCACertificateResponse
newDescribeCACertificateResponse pHttpStatus_ =
  DescribeCACertificateResponse'
    { certificateDescription =
        Core.Nothing,
      registrationConfig = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The CA certificate description.
describeCACertificateResponse_certificateDescription :: Lens.Lens' DescribeCACertificateResponse (Core.Maybe CACertificateDescription)
describeCACertificateResponse_certificateDescription = Lens.lens (\DescribeCACertificateResponse' {certificateDescription} -> certificateDescription) (\s@DescribeCACertificateResponse' {} a -> s {certificateDescription = a} :: DescribeCACertificateResponse)

-- | Information about the registration configuration.
describeCACertificateResponse_registrationConfig :: Lens.Lens' DescribeCACertificateResponse (Core.Maybe RegistrationConfig)
describeCACertificateResponse_registrationConfig = Lens.lens (\DescribeCACertificateResponse' {registrationConfig} -> registrationConfig) (\s@DescribeCACertificateResponse' {} a -> s {registrationConfig = a} :: DescribeCACertificateResponse)

-- | The response's http status code.
describeCACertificateResponse_httpStatus :: Lens.Lens' DescribeCACertificateResponse Core.Int
describeCACertificateResponse_httpStatus = Lens.lens (\DescribeCACertificateResponse' {httpStatus} -> httpStatus) (\s@DescribeCACertificateResponse' {} a -> s {httpStatus = a} :: DescribeCACertificateResponse)

instance Core.NFData DescribeCACertificateResponse
