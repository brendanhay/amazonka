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
-- Module      : Amazonka.LicenseManager.GetGrant
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets detailed information about the specified grant.
module Amazonka.LicenseManager.GetGrant
  ( -- * Creating a Request
    GetGrant (..),
    newGetGrant,

    -- * Request Lenses
    getGrant_version,
    getGrant_grantArn,

    -- * Destructuring the Response
    GetGrantResponse (..),
    newGetGrantResponse,

    -- * Response Lenses
    getGrantResponse_grant,
    getGrantResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetGrant' smart constructor.
data GetGrant = GetGrant'
  { -- | Grant version.
    version :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the grant.
    grantArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGrant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'getGrant_version' - Grant version.
--
-- 'grantArn', 'getGrant_grantArn' - Amazon Resource Name (ARN) of the grant.
newGetGrant ::
  -- | 'grantArn'
  Prelude.Text ->
  GetGrant
newGetGrant pGrantArn_ =
  GetGrant'
    { version = Prelude.Nothing,
      grantArn = pGrantArn_
    }

-- | Grant version.
getGrant_version :: Lens.Lens' GetGrant (Prelude.Maybe Prelude.Text)
getGrant_version = Lens.lens (\GetGrant' {version} -> version) (\s@GetGrant' {} a -> s {version = a} :: GetGrant)

-- | Amazon Resource Name (ARN) of the grant.
getGrant_grantArn :: Lens.Lens' GetGrant Prelude.Text
getGrant_grantArn = Lens.lens (\GetGrant' {grantArn} -> grantArn) (\s@GetGrant' {} a -> s {grantArn = a} :: GetGrant)

instance Core.AWSRequest GetGrant where
  type AWSResponse GetGrant = GetGrantResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGrantResponse'
            Prelude.<$> (x Data..?> "Grant")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetGrant where
  hashWithSalt _salt GetGrant' {..} =
    _salt `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` grantArn

instance Prelude.NFData GetGrant where
  rnf GetGrant' {..} =
    Prelude.rnf version
      `Prelude.seq` Prelude.rnf grantArn

instance Data.ToHeaders GetGrant where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSLicenseManager.GetGrant" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetGrant where
  toJSON GetGrant' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Version" Data..=) Prelude.<$> version,
            Prelude.Just ("GrantArn" Data..= grantArn)
          ]
      )

instance Data.ToPath GetGrant where
  toPath = Prelude.const "/"

instance Data.ToQuery GetGrant where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetGrantResponse' smart constructor.
data GetGrantResponse = GetGrantResponse'
  { -- | Grant details.
    grant :: Prelude.Maybe Grant,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGrantResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grant', 'getGrantResponse_grant' - Grant details.
--
-- 'httpStatus', 'getGrantResponse_httpStatus' - The response's http status code.
newGetGrantResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetGrantResponse
newGetGrantResponse pHttpStatus_ =
  GetGrantResponse'
    { grant = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Grant details.
getGrantResponse_grant :: Lens.Lens' GetGrantResponse (Prelude.Maybe Grant)
getGrantResponse_grant = Lens.lens (\GetGrantResponse' {grant} -> grant) (\s@GetGrantResponse' {} a -> s {grant = a} :: GetGrantResponse)

-- | The response's http status code.
getGrantResponse_httpStatus :: Lens.Lens' GetGrantResponse Prelude.Int
getGrantResponse_httpStatus = Lens.lens (\GetGrantResponse' {httpStatus} -> httpStatus) (\s@GetGrantResponse' {} a -> s {httpStatus = a} :: GetGrantResponse)

instance Prelude.NFData GetGrantResponse where
  rnf GetGrantResponse' {..} =
    Prelude.rnf grant
      `Prelude.seq` Prelude.rnf httpStatus
