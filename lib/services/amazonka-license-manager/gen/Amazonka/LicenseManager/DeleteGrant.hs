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
-- Module      : Amazonka.LicenseManager.DeleteGrant
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified grant.
module Amazonka.LicenseManager.DeleteGrant
  ( -- * Creating a Request
    DeleteGrant (..),
    newDeleteGrant,

    -- * Request Lenses
    deleteGrant_statusReason,
    deleteGrant_grantArn,
    deleteGrant_version,

    -- * Destructuring the Response
    DeleteGrantResponse (..),
    newDeleteGrantResponse,

    -- * Response Lenses
    deleteGrantResponse_status,
    deleteGrantResponse_grantArn,
    deleteGrantResponse_version,
    deleteGrantResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteGrant' smart constructor.
data DeleteGrant = DeleteGrant'
  { -- | The Status reason for the delete request.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the grant.
    grantArn :: Prelude.Text,
    -- | Current version of the grant.
    version :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGrant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusReason', 'deleteGrant_statusReason' - The Status reason for the delete request.
--
-- 'grantArn', 'deleteGrant_grantArn' - Amazon Resource Name (ARN) of the grant.
--
-- 'version', 'deleteGrant_version' - Current version of the grant.
newDeleteGrant ::
  -- | 'grantArn'
  Prelude.Text ->
  -- | 'version'
  Prelude.Text ->
  DeleteGrant
newDeleteGrant pGrantArn_ pVersion_ =
  DeleteGrant'
    { statusReason = Prelude.Nothing,
      grantArn = pGrantArn_,
      version = pVersion_
    }

-- | The Status reason for the delete request.
deleteGrant_statusReason :: Lens.Lens' DeleteGrant (Prelude.Maybe Prelude.Text)
deleteGrant_statusReason = Lens.lens (\DeleteGrant' {statusReason} -> statusReason) (\s@DeleteGrant' {} a -> s {statusReason = a} :: DeleteGrant)

-- | Amazon Resource Name (ARN) of the grant.
deleteGrant_grantArn :: Lens.Lens' DeleteGrant Prelude.Text
deleteGrant_grantArn = Lens.lens (\DeleteGrant' {grantArn} -> grantArn) (\s@DeleteGrant' {} a -> s {grantArn = a} :: DeleteGrant)

-- | Current version of the grant.
deleteGrant_version :: Lens.Lens' DeleteGrant Prelude.Text
deleteGrant_version = Lens.lens (\DeleteGrant' {version} -> version) (\s@DeleteGrant' {} a -> s {version = a} :: DeleteGrant)

instance Core.AWSRequest DeleteGrant where
  type AWSResponse DeleteGrant = DeleteGrantResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteGrantResponse'
            Prelude.<$> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "GrantArn")
            Prelude.<*> (x Data..?> "Version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteGrant where
  hashWithSalt _salt DeleteGrant' {..} =
    _salt `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` grantArn
      `Prelude.hashWithSalt` version

instance Prelude.NFData DeleteGrant where
  rnf DeleteGrant' {..} =
    Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf grantArn
      `Prelude.seq` Prelude.rnf version

instance Data.ToHeaders DeleteGrant where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLicenseManager.DeleteGrant" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteGrant where
  toJSON DeleteGrant' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StatusReason" Data..=) Prelude.<$> statusReason,
            Prelude.Just ("GrantArn" Data..= grantArn),
            Prelude.Just ("Version" Data..= version)
          ]
      )

instance Data.ToPath DeleteGrant where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteGrant where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteGrantResponse' smart constructor.
data DeleteGrantResponse = DeleteGrantResponse'
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
-- Create a value of 'DeleteGrantResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'deleteGrantResponse_status' - Grant status.
--
-- 'grantArn', 'deleteGrantResponse_grantArn' - Grant ARN.
--
-- 'version', 'deleteGrantResponse_version' - Grant version.
--
-- 'httpStatus', 'deleteGrantResponse_httpStatus' - The response's http status code.
newDeleteGrantResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteGrantResponse
newDeleteGrantResponse pHttpStatus_ =
  DeleteGrantResponse'
    { status = Prelude.Nothing,
      grantArn = Prelude.Nothing,
      version = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Grant status.
deleteGrantResponse_status :: Lens.Lens' DeleteGrantResponse (Prelude.Maybe GrantStatus)
deleteGrantResponse_status = Lens.lens (\DeleteGrantResponse' {status} -> status) (\s@DeleteGrantResponse' {} a -> s {status = a} :: DeleteGrantResponse)

-- | Grant ARN.
deleteGrantResponse_grantArn :: Lens.Lens' DeleteGrantResponse (Prelude.Maybe Prelude.Text)
deleteGrantResponse_grantArn = Lens.lens (\DeleteGrantResponse' {grantArn} -> grantArn) (\s@DeleteGrantResponse' {} a -> s {grantArn = a} :: DeleteGrantResponse)

-- | Grant version.
deleteGrantResponse_version :: Lens.Lens' DeleteGrantResponse (Prelude.Maybe Prelude.Text)
deleteGrantResponse_version = Lens.lens (\DeleteGrantResponse' {version} -> version) (\s@DeleteGrantResponse' {} a -> s {version = a} :: DeleteGrantResponse)

-- | The response's http status code.
deleteGrantResponse_httpStatus :: Lens.Lens' DeleteGrantResponse Prelude.Int
deleteGrantResponse_httpStatus = Lens.lens (\DeleteGrantResponse' {httpStatus} -> httpStatus) (\s@DeleteGrantResponse' {} a -> s {httpStatus = a} :: DeleteGrantResponse)

instance Prelude.NFData DeleteGrantResponse where
  rnf DeleteGrantResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf grantArn
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf httpStatus
