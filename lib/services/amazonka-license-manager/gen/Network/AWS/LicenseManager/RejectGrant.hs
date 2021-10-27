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
-- Module      : Network.AWS.LicenseManager.RejectGrant
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects the specified grant.
module Network.AWS.LicenseManager.RejectGrant
  ( -- * Creating a Request
    RejectGrant (..),
    newRejectGrant,

    -- * Request Lenses
    rejectGrant_grantArn,

    -- * Destructuring the Response
    RejectGrantResponse (..),
    newRejectGrantResponse,

    -- * Response Lenses
    rejectGrantResponse_status,
    rejectGrantResponse_version,
    rejectGrantResponse_grantArn,
    rejectGrantResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LicenseManager.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRejectGrant' smart constructor.
data RejectGrant = RejectGrant'
  { -- | Amazon Resource Name (ARN) of the grant.
    grantArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RejectGrant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grantArn', 'rejectGrant_grantArn' - Amazon Resource Name (ARN) of the grant.
newRejectGrant ::
  -- | 'grantArn'
  Prelude.Text ->
  RejectGrant
newRejectGrant pGrantArn_ =
  RejectGrant' {grantArn = pGrantArn_}

-- | Amazon Resource Name (ARN) of the grant.
rejectGrant_grantArn :: Lens.Lens' RejectGrant Prelude.Text
rejectGrant_grantArn = Lens.lens (\RejectGrant' {grantArn} -> grantArn) (\s@RejectGrant' {} a -> s {grantArn = a} :: RejectGrant)

instance Core.AWSRequest RejectGrant where
  type AWSResponse RejectGrant = RejectGrantResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RejectGrantResponse'
            Prelude.<$> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "Version")
            Prelude.<*> (x Core..?> "GrantArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RejectGrant

instance Prelude.NFData RejectGrant

instance Core.ToHeaders RejectGrant where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLicenseManager.RejectGrant" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RejectGrant where
  toJSON RejectGrant' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("GrantArn" Core..= grantArn)]
      )

instance Core.ToPath RejectGrant where
  toPath = Prelude.const "/"

instance Core.ToQuery RejectGrant where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRejectGrantResponse' smart constructor.
data RejectGrantResponse = RejectGrantResponse'
  { -- | Grant status.
    status :: Prelude.Maybe GrantStatus,
    -- | Grant version.
    version :: Prelude.Maybe Prelude.Text,
    -- | Grant ARN.
    grantArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RejectGrantResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'rejectGrantResponse_status' - Grant status.
--
-- 'version', 'rejectGrantResponse_version' - Grant version.
--
-- 'grantArn', 'rejectGrantResponse_grantArn' - Grant ARN.
--
-- 'httpStatus', 'rejectGrantResponse_httpStatus' - The response's http status code.
newRejectGrantResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RejectGrantResponse
newRejectGrantResponse pHttpStatus_ =
  RejectGrantResponse'
    { status = Prelude.Nothing,
      version = Prelude.Nothing,
      grantArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Grant status.
rejectGrantResponse_status :: Lens.Lens' RejectGrantResponse (Prelude.Maybe GrantStatus)
rejectGrantResponse_status = Lens.lens (\RejectGrantResponse' {status} -> status) (\s@RejectGrantResponse' {} a -> s {status = a} :: RejectGrantResponse)

-- | Grant version.
rejectGrantResponse_version :: Lens.Lens' RejectGrantResponse (Prelude.Maybe Prelude.Text)
rejectGrantResponse_version = Lens.lens (\RejectGrantResponse' {version} -> version) (\s@RejectGrantResponse' {} a -> s {version = a} :: RejectGrantResponse)

-- | Grant ARN.
rejectGrantResponse_grantArn :: Lens.Lens' RejectGrantResponse (Prelude.Maybe Prelude.Text)
rejectGrantResponse_grantArn = Lens.lens (\RejectGrantResponse' {grantArn} -> grantArn) (\s@RejectGrantResponse' {} a -> s {grantArn = a} :: RejectGrantResponse)

-- | The response's http status code.
rejectGrantResponse_httpStatus :: Lens.Lens' RejectGrantResponse Prelude.Int
rejectGrantResponse_httpStatus = Lens.lens (\RejectGrantResponse' {httpStatus} -> httpStatus) (\s@RejectGrantResponse' {} a -> s {httpStatus = a} :: RejectGrantResponse)

instance Prelude.NFData RejectGrantResponse
