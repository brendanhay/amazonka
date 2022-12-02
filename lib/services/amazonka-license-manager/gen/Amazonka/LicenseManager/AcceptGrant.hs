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
-- Module      : Amazonka.LicenseManager.AcceptGrant
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts the specified grant.
module Amazonka.LicenseManager.AcceptGrant
  ( -- * Creating a Request
    AcceptGrant (..),
    newAcceptGrant,

    -- * Request Lenses
    acceptGrant_grantArn,

    -- * Destructuring the Response
    AcceptGrantResponse (..),
    newAcceptGrantResponse,

    -- * Response Lenses
    acceptGrantResponse_status,
    acceptGrantResponse_grantArn,
    acceptGrantResponse_version,
    acceptGrantResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAcceptGrant' smart constructor.
data AcceptGrant = AcceptGrant'
  { -- | Amazon Resource Name (ARN) of the grant.
    grantArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptGrant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grantArn', 'acceptGrant_grantArn' - Amazon Resource Name (ARN) of the grant.
newAcceptGrant ::
  -- | 'grantArn'
  Prelude.Text ->
  AcceptGrant
newAcceptGrant pGrantArn_ =
  AcceptGrant' {grantArn = pGrantArn_}

-- | Amazon Resource Name (ARN) of the grant.
acceptGrant_grantArn :: Lens.Lens' AcceptGrant Prelude.Text
acceptGrant_grantArn = Lens.lens (\AcceptGrant' {grantArn} -> grantArn) (\s@AcceptGrant' {} a -> s {grantArn = a} :: AcceptGrant)

instance Core.AWSRequest AcceptGrant where
  type AWSResponse AcceptGrant = AcceptGrantResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AcceptGrantResponse'
            Prelude.<$> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "GrantArn")
            Prelude.<*> (x Data..?> "Version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AcceptGrant where
  hashWithSalt _salt AcceptGrant' {..} =
    _salt `Prelude.hashWithSalt` grantArn

instance Prelude.NFData AcceptGrant where
  rnf AcceptGrant' {..} = Prelude.rnf grantArn

instance Data.ToHeaders AcceptGrant where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLicenseManager.AcceptGrant" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AcceptGrant where
  toJSON AcceptGrant' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("GrantArn" Data..= grantArn)]
      )

instance Data.ToPath AcceptGrant where
  toPath = Prelude.const "/"

instance Data.ToQuery AcceptGrant where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAcceptGrantResponse' smart constructor.
data AcceptGrantResponse = AcceptGrantResponse'
  { -- | Grant status.
    status :: Prelude.Maybe GrantStatus,
    -- | Grant ARN.
    grantArn :: Prelude.Maybe Prelude.Text,
    -- | Grant version.
    version :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptGrantResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'acceptGrantResponse_status' - Grant status.
--
-- 'grantArn', 'acceptGrantResponse_grantArn' - Grant ARN.
--
-- 'version', 'acceptGrantResponse_version' - Grant version.
--
-- 'httpStatus', 'acceptGrantResponse_httpStatus' - The response's http status code.
newAcceptGrantResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AcceptGrantResponse
newAcceptGrantResponse pHttpStatus_ =
  AcceptGrantResponse'
    { status = Prelude.Nothing,
      grantArn = Prelude.Nothing,
      version = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Grant status.
acceptGrantResponse_status :: Lens.Lens' AcceptGrantResponse (Prelude.Maybe GrantStatus)
acceptGrantResponse_status = Lens.lens (\AcceptGrantResponse' {status} -> status) (\s@AcceptGrantResponse' {} a -> s {status = a} :: AcceptGrantResponse)

-- | Grant ARN.
acceptGrantResponse_grantArn :: Lens.Lens' AcceptGrantResponse (Prelude.Maybe Prelude.Text)
acceptGrantResponse_grantArn = Lens.lens (\AcceptGrantResponse' {grantArn} -> grantArn) (\s@AcceptGrantResponse' {} a -> s {grantArn = a} :: AcceptGrantResponse)

-- | Grant version.
acceptGrantResponse_version :: Lens.Lens' AcceptGrantResponse (Prelude.Maybe Prelude.Text)
acceptGrantResponse_version = Lens.lens (\AcceptGrantResponse' {version} -> version) (\s@AcceptGrantResponse' {} a -> s {version = a} :: AcceptGrantResponse)

-- | The response's http status code.
acceptGrantResponse_httpStatus :: Lens.Lens' AcceptGrantResponse Prelude.Int
acceptGrantResponse_httpStatus = Lens.lens (\AcceptGrantResponse' {httpStatus} -> httpStatus) (\s@AcceptGrantResponse' {} a -> s {httpStatus = a} :: AcceptGrantResponse)

instance Prelude.NFData AcceptGrantResponse where
  rnf AcceptGrantResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf grantArn
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf httpStatus
