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
-- Module      : Amazonka.WorkLink.DisassociateWebsiteCertificateAuthority
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a certificate authority (CA).
module Amazonka.WorkLink.DisassociateWebsiteCertificateAuthority
  ( -- * Creating a Request
    DisassociateWebsiteCertificateAuthority (..),
    newDisassociateWebsiteCertificateAuthority,

    -- * Request Lenses
    disassociateWebsiteCertificateAuthority_fleetArn,
    disassociateWebsiteCertificateAuthority_websiteCaId,

    -- * Destructuring the Response
    DisassociateWebsiteCertificateAuthorityResponse (..),
    newDisassociateWebsiteCertificateAuthorityResponse,

    -- * Response Lenses
    disassociateWebsiteCertificateAuthorityResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkLink.Types

-- | /See:/ 'newDisassociateWebsiteCertificateAuthority' smart constructor.
data DisassociateWebsiteCertificateAuthority = DisassociateWebsiteCertificateAuthority'
  { -- | The ARN of the fleet.
    fleetArn :: Prelude.Text,
    -- | A unique identifier for the CA.
    websiteCaId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateWebsiteCertificateAuthority' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetArn', 'disassociateWebsiteCertificateAuthority_fleetArn' - The ARN of the fleet.
--
-- 'websiteCaId', 'disassociateWebsiteCertificateAuthority_websiteCaId' - A unique identifier for the CA.
newDisassociateWebsiteCertificateAuthority ::
  -- | 'fleetArn'
  Prelude.Text ->
  -- | 'websiteCaId'
  Prelude.Text ->
  DisassociateWebsiteCertificateAuthority
newDisassociateWebsiteCertificateAuthority
  pFleetArn_
  pWebsiteCaId_ =
    DisassociateWebsiteCertificateAuthority'
      { fleetArn =
          pFleetArn_,
        websiteCaId = pWebsiteCaId_
      }

-- | The ARN of the fleet.
disassociateWebsiteCertificateAuthority_fleetArn :: Lens.Lens' DisassociateWebsiteCertificateAuthority Prelude.Text
disassociateWebsiteCertificateAuthority_fleetArn = Lens.lens (\DisassociateWebsiteCertificateAuthority' {fleetArn} -> fleetArn) (\s@DisassociateWebsiteCertificateAuthority' {} a -> s {fleetArn = a} :: DisassociateWebsiteCertificateAuthority)

-- | A unique identifier for the CA.
disassociateWebsiteCertificateAuthority_websiteCaId :: Lens.Lens' DisassociateWebsiteCertificateAuthority Prelude.Text
disassociateWebsiteCertificateAuthority_websiteCaId = Lens.lens (\DisassociateWebsiteCertificateAuthority' {websiteCaId} -> websiteCaId) (\s@DisassociateWebsiteCertificateAuthority' {} a -> s {websiteCaId = a} :: DisassociateWebsiteCertificateAuthority)

instance
  Core.AWSRequest
    DisassociateWebsiteCertificateAuthority
  where
  type
    AWSResponse
      DisassociateWebsiteCertificateAuthority =
      DisassociateWebsiteCertificateAuthorityResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateWebsiteCertificateAuthorityResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateWebsiteCertificateAuthority

instance
  Prelude.NFData
    DisassociateWebsiteCertificateAuthority

instance
  Core.ToHeaders
    DisassociateWebsiteCertificateAuthority
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
    DisassociateWebsiteCertificateAuthority
  where
  toJSON DisassociateWebsiteCertificateAuthority' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FleetArn" Core..= fleetArn),
            Prelude.Just ("WebsiteCaId" Core..= websiteCaId)
          ]
      )

instance
  Core.ToPath
    DisassociateWebsiteCertificateAuthority
  where
  toPath =
    Prelude.const
      "/disassociateWebsiteCertificateAuthority"

instance
  Core.ToQuery
    DisassociateWebsiteCertificateAuthority
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateWebsiteCertificateAuthorityResponse' smart constructor.
data DisassociateWebsiteCertificateAuthorityResponse = DisassociateWebsiteCertificateAuthorityResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateWebsiteCertificateAuthorityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateWebsiteCertificateAuthorityResponse_httpStatus' - The response's http status code.
newDisassociateWebsiteCertificateAuthorityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateWebsiteCertificateAuthorityResponse
newDisassociateWebsiteCertificateAuthorityResponse
  pHttpStatus_ =
    DisassociateWebsiteCertificateAuthorityResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateWebsiteCertificateAuthorityResponse_httpStatus :: Lens.Lens' DisassociateWebsiteCertificateAuthorityResponse Prelude.Int
disassociateWebsiteCertificateAuthorityResponse_httpStatus = Lens.lens (\DisassociateWebsiteCertificateAuthorityResponse' {httpStatus} -> httpStatus) (\s@DisassociateWebsiteCertificateAuthorityResponse' {} a -> s {httpStatus = a} :: DisassociateWebsiteCertificateAuthorityResponse)

instance
  Prelude.NFData
    DisassociateWebsiteCertificateAuthorityResponse
