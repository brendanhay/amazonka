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
-- Module      : Amazonka.WorkLink.AssociateWebsiteCertificateAuthority
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports the root certificate of a certificate authority (CA) used to
-- obtain TLS certificates used by associated websites within the company
-- network.
module Amazonka.WorkLink.AssociateWebsiteCertificateAuthority
  ( -- * Creating a Request
    AssociateWebsiteCertificateAuthority (..),
    newAssociateWebsiteCertificateAuthority,

    -- * Request Lenses
    associateWebsiteCertificateAuthority_displayName,
    associateWebsiteCertificateAuthority_fleetArn,
    associateWebsiteCertificateAuthority_certificate,

    -- * Destructuring the Response
    AssociateWebsiteCertificateAuthorityResponse (..),
    newAssociateWebsiteCertificateAuthorityResponse,

    -- * Response Lenses
    associateWebsiteCertificateAuthorityResponse_websiteCaId,
    associateWebsiteCertificateAuthorityResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkLink.Types

-- | /See:/ 'newAssociateWebsiteCertificateAuthority' smart constructor.
data AssociateWebsiteCertificateAuthority = AssociateWebsiteCertificateAuthority'
  { -- | The certificate name to display.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the fleet.
    fleetArn :: Prelude.Text,
    -- | The root certificate of the CA.
    certificate :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateWebsiteCertificateAuthority' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayName', 'associateWebsiteCertificateAuthority_displayName' - The certificate name to display.
--
-- 'fleetArn', 'associateWebsiteCertificateAuthority_fleetArn' - The ARN of the fleet.
--
-- 'certificate', 'associateWebsiteCertificateAuthority_certificate' - The root certificate of the CA.
newAssociateWebsiteCertificateAuthority ::
  -- | 'fleetArn'
  Prelude.Text ->
  -- | 'certificate'
  Prelude.Text ->
  AssociateWebsiteCertificateAuthority
newAssociateWebsiteCertificateAuthority
  pFleetArn_
  pCertificate_ =
    AssociateWebsiteCertificateAuthority'
      { displayName =
          Prelude.Nothing,
        fleetArn = pFleetArn_,
        certificate = pCertificate_
      }

-- | The certificate name to display.
associateWebsiteCertificateAuthority_displayName :: Lens.Lens' AssociateWebsiteCertificateAuthority (Prelude.Maybe Prelude.Text)
associateWebsiteCertificateAuthority_displayName = Lens.lens (\AssociateWebsiteCertificateAuthority' {displayName} -> displayName) (\s@AssociateWebsiteCertificateAuthority' {} a -> s {displayName = a} :: AssociateWebsiteCertificateAuthority)

-- | The ARN of the fleet.
associateWebsiteCertificateAuthority_fleetArn :: Lens.Lens' AssociateWebsiteCertificateAuthority Prelude.Text
associateWebsiteCertificateAuthority_fleetArn = Lens.lens (\AssociateWebsiteCertificateAuthority' {fleetArn} -> fleetArn) (\s@AssociateWebsiteCertificateAuthority' {} a -> s {fleetArn = a} :: AssociateWebsiteCertificateAuthority)

-- | The root certificate of the CA.
associateWebsiteCertificateAuthority_certificate :: Lens.Lens' AssociateWebsiteCertificateAuthority Prelude.Text
associateWebsiteCertificateAuthority_certificate = Lens.lens (\AssociateWebsiteCertificateAuthority' {certificate} -> certificate) (\s@AssociateWebsiteCertificateAuthority' {} a -> s {certificate = a} :: AssociateWebsiteCertificateAuthority)

instance
  Core.AWSRequest
    AssociateWebsiteCertificateAuthority
  where
  type
    AWSResponse AssociateWebsiteCertificateAuthority =
      AssociateWebsiteCertificateAuthorityResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateWebsiteCertificateAuthorityResponse'
            Prelude.<$> (x Core..?> "WebsiteCaId")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateWebsiteCertificateAuthority
  where
  hashWithSalt
    salt'
    AssociateWebsiteCertificateAuthority' {..} =
      salt' `Prelude.hashWithSalt` certificate
        `Prelude.hashWithSalt` fleetArn
        `Prelude.hashWithSalt` displayName

instance
  Prelude.NFData
    AssociateWebsiteCertificateAuthority
  where
  rnf AssociateWebsiteCertificateAuthority' {..} =
    Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf certificate
      `Prelude.seq` Prelude.rnf fleetArn

instance
  Core.ToHeaders
    AssociateWebsiteCertificateAuthority
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    AssociateWebsiteCertificateAuthority
  where
  toJSON AssociateWebsiteCertificateAuthority' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DisplayName" Core..=) Prelude.<$> displayName,
            Prelude.Just ("FleetArn" Core..= fleetArn),
            Prelude.Just ("Certificate" Core..= certificate)
          ]
      )

instance
  Core.ToPath
    AssociateWebsiteCertificateAuthority
  where
  toPath =
    Prelude.const
      "/associateWebsiteCertificateAuthority"

instance
  Core.ToQuery
    AssociateWebsiteCertificateAuthority
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateWebsiteCertificateAuthorityResponse' smart constructor.
data AssociateWebsiteCertificateAuthorityResponse = AssociateWebsiteCertificateAuthorityResponse'
  { -- | A unique identifier for the CA.
    websiteCaId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateWebsiteCertificateAuthorityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'websiteCaId', 'associateWebsiteCertificateAuthorityResponse_websiteCaId' - A unique identifier for the CA.
--
-- 'httpStatus', 'associateWebsiteCertificateAuthorityResponse_httpStatus' - The response's http status code.
newAssociateWebsiteCertificateAuthorityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateWebsiteCertificateAuthorityResponse
newAssociateWebsiteCertificateAuthorityResponse
  pHttpStatus_ =
    AssociateWebsiteCertificateAuthorityResponse'
      { websiteCaId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A unique identifier for the CA.
associateWebsiteCertificateAuthorityResponse_websiteCaId :: Lens.Lens' AssociateWebsiteCertificateAuthorityResponse (Prelude.Maybe Prelude.Text)
associateWebsiteCertificateAuthorityResponse_websiteCaId = Lens.lens (\AssociateWebsiteCertificateAuthorityResponse' {websiteCaId} -> websiteCaId) (\s@AssociateWebsiteCertificateAuthorityResponse' {} a -> s {websiteCaId = a} :: AssociateWebsiteCertificateAuthorityResponse)

-- | The response's http status code.
associateWebsiteCertificateAuthorityResponse_httpStatus :: Lens.Lens' AssociateWebsiteCertificateAuthorityResponse Prelude.Int
associateWebsiteCertificateAuthorityResponse_httpStatus = Lens.lens (\AssociateWebsiteCertificateAuthorityResponse' {httpStatus} -> httpStatus) (\s@AssociateWebsiteCertificateAuthorityResponse' {} a -> s {httpStatus = a} :: AssociateWebsiteCertificateAuthorityResponse)

instance
  Prelude.NFData
    AssociateWebsiteCertificateAuthorityResponse
  where
  rnf AssociateWebsiteCertificateAuthorityResponse' {..} =
    Prelude.rnf websiteCaId
      `Prelude.seq` Prelude.rnf httpStatus
