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
-- Module      : Amazonka.LicenseManager.CreateGrantVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of the specified grant.
module Amazonka.LicenseManager.CreateGrantVersion
  ( -- * Creating a Request
    CreateGrantVersion (..),
    newCreateGrantVersion,

    -- * Request Lenses
    createGrantVersion_sourceVersion,
    createGrantVersion_statusReason,
    createGrantVersion_allowedOperations,
    createGrantVersion_status,
    createGrantVersion_grantName,
    createGrantVersion_clientToken,
    createGrantVersion_grantArn,

    -- * Destructuring the Response
    CreateGrantVersionResponse (..),
    newCreateGrantVersionResponse,

    -- * Response Lenses
    createGrantVersionResponse_status,
    createGrantVersionResponse_grantArn,
    createGrantVersionResponse_version,
    createGrantVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateGrantVersion' smart constructor.
data CreateGrantVersion = CreateGrantVersion'
  { -- | Current version of the grant.
    sourceVersion :: Prelude.Maybe Prelude.Text,
    -- | Grant status reason.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | Allowed operations for the grant.
    allowedOperations :: Prelude.Maybe (Prelude.NonEmpty AllowedOperation),
    -- | Grant status.
    status :: Prelude.Maybe GrantStatus,
    -- | Grant name.
    grantName :: Prelude.Maybe Prelude.Text,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Text,
    -- | Amazon Resource Name (ARN) of the grant.
    grantArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGrantVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceVersion', 'createGrantVersion_sourceVersion' - Current version of the grant.
--
-- 'statusReason', 'createGrantVersion_statusReason' - Grant status reason.
--
-- 'allowedOperations', 'createGrantVersion_allowedOperations' - Allowed operations for the grant.
--
-- 'status', 'createGrantVersion_status' - Grant status.
--
-- 'grantName', 'createGrantVersion_grantName' - Grant name.
--
-- 'clientToken', 'createGrantVersion_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'grantArn', 'createGrantVersion_grantArn' - Amazon Resource Name (ARN) of the grant.
newCreateGrantVersion ::
  -- | 'clientToken'
  Prelude.Text ->
  -- | 'grantArn'
  Prelude.Text ->
  CreateGrantVersion
newCreateGrantVersion pClientToken_ pGrantArn_ =
  CreateGrantVersion'
    { sourceVersion =
        Prelude.Nothing,
      statusReason = Prelude.Nothing,
      allowedOperations = Prelude.Nothing,
      status = Prelude.Nothing,
      grantName = Prelude.Nothing,
      clientToken = pClientToken_,
      grantArn = pGrantArn_
    }

-- | Current version of the grant.
createGrantVersion_sourceVersion :: Lens.Lens' CreateGrantVersion (Prelude.Maybe Prelude.Text)
createGrantVersion_sourceVersion = Lens.lens (\CreateGrantVersion' {sourceVersion} -> sourceVersion) (\s@CreateGrantVersion' {} a -> s {sourceVersion = a} :: CreateGrantVersion)

-- | Grant status reason.
createGrantVersion_statusReason :: Lens.Lens' CreateGrantVersion (Prelude.Maybe Prelude.Text)
createGrantVersion_statusReason = Lens.lens (\CreateGrantVersion' {statusReason} -> statusReason) (\s@CreateGrantVersion' {} a -> s {statusReason = a} :: CreateGrantVersion)

-- | Allowed operations for the grant.
createGrantVersion_allowedOperations :: Lens.Lens' CreateGrantVersion (Prelude.Maybe (Prelude.NonEmpty AllowedOperation))
createGrantVersion_allowedOperations = Lens.lens (\CreateGrantVersion' {allowedOperations} -> allowedOperations) (\s@CreateGrantVersion' {} a -> s {allowedOperations = a} :: CreateGrantVersion) Prelude.. Lens.mapping Lens.coerced

-- | Grant status.
createGrantVersion_status :: Lens.Lens' CreateGrantVersion (Prelude.Maybe GrantStatus)
createGrantVersion_status = Lens.lens (\CreateGrantVersion' {status} -> status) (\s@CreateGrantVersion' {} a -> s {status = a} :: CreateGrantVersion)

-- | Grant name.
createGrantVersion_grantName :: Lens.Lens' CreateGrantVersion (Prelude.Maybe Prelude.Text)
createGrantVersion_grantName = Lens.lens (\CreateGrantVersion' {grantName} -> grantName) (\s@CreateGrantVersion' {} a -> s {grantName = a} :: CreateGrantVersion)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createGrantVersion_clientToken :: Lens.Lens' CreateGrantVersion Prelude.Text
createGrantVersion_clientToken = Lens.lens (\CreateGrantVersion' {clientToken} -> clientToken) (\s@CreateGrantVersion' {} a -> s {clientToken = a} :: CreateGrantVersion)

-- | Amazon Resource Name (ARN) of the grant.
createGrantVersion_grantArn :: Lens.Lens' CreateGrantVersion Prelude.Text
createGrantVersion_grantArn = Lens.lens (\CreateGrantVersion' {grantArn} -> grantArn) (\s@CreateGrantVersion' {} a -> s {grantArn = a} :: CreateGrantVersion)

instance Core.AWSRequest CreateGrantVersion where
  type
    AWSResponse CreateGrantVersion =
      CreateGrantVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGrantVersionResponse'
            Prelude.<$> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "GrantArn")
            Prelude.<*> (x Data..?> "Version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateGrantVersion where
  hashWithSalt _salt CreateGrantVersion' {..} =
    _salt `Prelude.hashWithSalt` sourceVersion
      `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` allowedOperations
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` grantName
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` grantArn

instance Prelude.NFData CreateGrantVersion where
  rnf CreateGrantVersion' {..} =
    Prelude.rnf sourceVersion
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf allowedOperations
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf grantName
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf grantArn

instance Data.ToHeaders CreateGrantVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLicenseManager.CreateGrantVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateGrantVersion where
  toJSON CreateGrantVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SourceVersion" Data..=) Prelude.<$> sourceVersion,
            ("StatusReason" Data..=) Prelude.<$> statusReason,
            ("AllowedOperations" Data..=)
              Prelude.<$> allowedOperations,
            ("Status" Data..=) Prelude.<$> status,
            ("GrantName" Data..=) Prelude.<$> grantName,
            Prelude.Just ("ClientToken" Data..= clientToken),
            Prelude.Just ("GrantArn" Data..= grantArn)
          ]
      )

instance Data.ToPath CreateGrantVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateGrantVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateGrantVersionResponse' smart constructor.
data CreateGrantVersionResponse = CreateGrantVersionResponse'
  { -- | Grant status.
    status :: Prelude.Maybe GrantStatus,
    -- | Grant ARN.
    grantArn :: Prelude.Maybe Prelude.Text,
    -- | New version of the grant.
    version :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGrantVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'createGrantVersionResponse_status' - Grant status.
--
-- 'grantArn', 'createGrantVersionResponse_grantArn' - Grant ARN.
--
-- 'version', 'createGrantVersionResponse_version' - New version of the grant.
--
-- 'httpStatus', 'createGrantVersionResponse_httpStatus' - The response's http status code.
newCreateGrantVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateGrantVersionResponse
newCreateGrantVersionResponse pHttpStatus_ =
  CreateGrantVersionResponse'
    { status =
        Prelude.Nothing,
      grantArn = Prelude.Nothing,
      version = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Grant status.
createGrantVersionResponse_status :: Lens.Lens' CreateGrantVersionResponse (Prelude.Maybe GrantStatus)
createGrantVersionResponse_status = Lens.lens (\CreateGrantVersionResponse' {status} -> status) (\s@CreateGrantVersionResponse' {} a -> s {status = a} :: CreateGrantVersionResponse)

-- | Grant ARN.
createGrantVersionResponse_grantArn :: Lens.Lens' CreateGrantVersionResponse (Prelude.Maybe Prelude.Text)
createGrantVersionResponse_grantArn = Lens.lens (\CreateGrantVersionResponse' {grantArn} -> grantArn) (\s@CreateGrantVersionResponse' {} a -> s {grantArn = a} :: CreateGrantVersionResponse)

-- | New version of the grant.
createGrantVersionResponse_version :: Lens.Lens' CreateGrantVersionResponse (Prelude.Maybe Prelude.Text)
createGrantVersionResponse_version = Lens.lens (\CreateGrantVersionResponse' {version} -> version) (\s@CreateGrantVersionResponse' {} a -> s {version = a} :: CreateGrantVersionResponse)

-- | The response's http status code.
createGrantVersionResponse_httpStatus :: Lens.Lens' CreateGrantVersionResponse Prelude.Int
createGrantVersionResponse_httpStatus = Lens.lens (\CreateGrantVersionResponse' {httpStatus} -> httpStatus) (\s@CreateGrantVersionResponse' {} a -> s {httpStatus = a} :: CreateGrantVersionResponse)

instance Prelude.NFData CreateGrantVersionResponse where
  rnf CreateGrantVersionResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf grantArn
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf httpStatus
