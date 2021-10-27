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
-- Module      : Network.AWS.WorkLink.DescribeWebsiteCertificateAuthority
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the certificate authority.
module Network.AWS.WorkLink.DescribeWebsiteCertificateAuthority
  ( -- * Creating a Request
    DescribeWebsiteCertificateAuthority (..),
    newDescribeWebsiteCertificateAuthority,

    -- * Request Lenses
    describeWebsiteCertificateAuthority_fleetArn,
    describeWebsiteCertificateAuthority_websiteCaId,

    -- * Destructuring the Response
    DescribeWebsiteCertificateAuthorityResponse (..),
    newDescribeWebsiteCertificateAuthorityResponse,

    -- * Response Lenses
    describeWebsiteCertificateAuthorityResponse_createdTime,
    describeWebsiteCertificateAuthorityResponse_certificate,
    describeWebsiteCertificateAuthorityResponse_displayName,
    describeWebsiteCertificateAuthorityResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkLink.Types

-- | /See:/ 'newDescribeWebsiteCertificateAuthority' smart constructor.
data DescribeWebsiteCertificateAuthority = DescribeWebsiteCertificateAuthority'
  { -- | The ARN of the fleet.
    fleetArn :: Prelude.Text,
    -- | A unique identifier for the certificate authority.
    websiteCaId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWebsiteCertificateAuthority' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetArn', 'describeWebsiteCertificateAuthority_fleetArn' - The ARN of the fleet.
--
-- 'websiteCaId', 'describeWebsiteCertificateAuthority_websiteCaId' - A unique identifier for the certificate authority.
newDescribeWebsiteCertificateAuthority ::
  -- | 'fleetArn'
  Prelude.Text ->
  -- | 'websiteCaId'
  Prelude.Text ->
  DescribeWebsiteCertificateAuthority
newDescribeWebsiteCertificateAuthority
  pFleetArn_
  pWebsiteCaId_ =
    DescribeWebsiteCertificateAuthority'
      { fleetArn =
          pFleetArn_,
        websiteCaId = pWebsiteCaId_
      }

-- | The ARN of the fleet.
describeWebsiteCertificateAuthority_fleetArn :: Lens.Lens' DescribeWebsiteCertificateAuthority Prelude.Text
describeWebsiteCertificateAuthority_fleetArn = Lens.lens (\DescribeWebsiteCertificateAuthority' {fleetArn} -> fleetArn) (\s@DescribeWebsiteCertificateAuthority' {} a -> s {fleetArn = a} :: DescribeWebsiteCertificateAuthority)

-- | A unique identifier for the certificate authority.
describeWebsiteCertificateAuthority_websiteCaId :: Lens.Lens' DescribeWebsiteCertificateAuthority Prelude.Text
describeWebsiteCertificateAuthority_websiteCaId = Lens.lens (\DescribeWebsiteCertificateAuthority' {websiteCaId} -> websiteCaId) (\s@DescribeWebsiteCertificateAuthority' {} a -> s {websiteCaId = a} :: DescribeWebsiteCertificateAuthority)

instance
  Core.AWSRequest
    DescribeWebsiteCertificateAuthority
  where
  type
    AWSResponse DescribeWebsiteCertificateAuthority =
      DescribeWebsiteCertificateAuthorityResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWebsiteCertificateAuthorityResponse'
            Prelude.<$> (x Core..?> "CreatedTime")
              Prelude.<*> (x Core..?> "Certificate")
              Prelude.<*> (x Core..?> "DisplayName")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeWebsiteCertificateAuthority

instance
  Prelude.NFData
    DescribeWebsiteCertificateAuthority

instance
  Core.ToHeaders
    DescribeWebsiteCertificateAuthority
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
    DescribeWebsiteCertificateAuthority
  where
  toJSON DescribeWebsiteCertificateAuthority' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FleetArn" Core..= fleetArn),
            Prelude.Just ("WebsiteCaId" Core..= websiteCaId)
          ]
      )

instance
  Core.ToPath
    DescribeWebsiteCertificateAuthority
  where
  toPath =
    Prelude.const
      "/describeWebsiteCertificateAuthority"

instance
  Core.ToQuery
    DescribeWebsiteCertificateAuthority
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeWebsiteCertificateAuthorityResponse' smart constructor.
data DescribeWebsiteCertificateAuthorityResponse = DescribeWebsiteCertificateAuthorityResponse'
  { -- | The time that the certificate authority was added.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The root certificate of the certificate authority.
    certificate :: Prelude.Maybe Prelude.Text,
    -- | The certificate name to display.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWebsiteCertificateAuthorityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTime', 'describeWebsiteCertificateAuthorityResponse_createdTime' - The time that the certificate authority was added.
--
-- 'certificate', 'describeWebsiteCertificateAuthorityResponse_certificate' - The root certificate of the certificate authority.
--
-- 'displayName', 'describeWebsiteCertificateAuthorityResponse_displayName' - The certificate name to display.
--
-- 'httpStatus', 'describeWebsiteCertificateAuthorityResponse_httpStatus' - The response's http status code.
newDescribeWebsiteCertificateAuthorityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeWebsiteCertificateAuthorityResponse
newDescribeWebsiteCertificateAuthorityResponse
  pHttpStatus_ =
    DescribeWebsiteCertificateAuthorityResponse'
      { createdTime =
          Prelude.Nothing,
        certificate = Prelude.Nothing,
        displayName = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The time that the certificate authority was added.
describeWebsiteCertificateAuthorityResponse_createdTime :: Lens.Lens' DescribeWebsiteCertificateAuthorityResponse (Prelude.Maybe Prelude.UTCTime)
describeWebsiteCertificateAuthorityResponse_createdTime = Lens.lens (\DescribeWebsiteCertificateAuthorityResponse' {createdTime} -> createdTime) (\s@DescribeWebsiteCertificateAuthorityResponse' {} a -> s {createdTime = a} :: DescribeWebsiteCertificateAuthorityResponse) Prelude.. Lens.mapping Core._Time

-- | The root certificate of the certificate authority.
describeWebsiteCertificateAuthorityResponse_certificate :: Lens.Lens' DescribeWebsiteCertificateAuthorityResponse (Prelude.Maybe Prelude.Text)
describeWebsiteCertificateAuthorityResponse_certificate = Lens.lens (\DescribeWebsiteCertificateAuthorityResponse' {certificate} -> certificate) (\s@DescribeWebsiteCertificateAuthorityResponse' {} a -> s {certificate = a} :: DescribeWebsiteCertificateAuthorityResponse)

-- | The certificate name to display.
describeWebsiteCertificateAuthorityResponse_displayName :: Lens.Lens' DescribeWebsiteCertificateAuthorityResponse (Prelude.Maybe Prelude.Text)
describeWebsiteCertificateAuthorityResponse_displayName = Lens.lens (\DescribeWebsiteCertificateAuthorityResponse' {displayName} -> displayName) (\s@DescribeWebsiteCertificateAuthorityResponse' {} a -> s {displayName = a} :: DescribeWebsiteCertificateAuthorityResponse)

-- | The response's http status code.
describeWebsiteCertificateAuthorityResponse_httpStatus :: Lens.Lens' DescribeWebsiteCertificateAuthorityResponse Prelude.Int
describeWebsiteCertificateAuthorityResponse_httpStatus = Lens.lens (\DescribeWebsiteCertificateAuthorityResponse' {httpStatus} -> httpStatus) (\s@DescribeWebsiteCertificateAuthorityResponse' {} a -> s {httpStatus = a} :: DescribeWebsiteCertificateAuthorityResponse)

instance
  Prelude.NFData
    DescribeWebsiteCertificateAuthorityResponse
