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
-- Module      : Amazonka.LicenseManager.CreateGrant
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a grant for the specified license. A grant shares the use of
-- license entitlements with specific Amazon Web Services accounts.
module Amazonka.LicenseManager.CreateGrant
  ( -- * Creating a Request
    CreateGrant (..),
    newCreateGrant,

    -- * Request Lenses
    createGrant_clientToken,
    createGrant_grantName,
    createGrant_licenseArn,
    createGrant_principals,
    createGrant_homeRegion,
    createGrant_allowedOperations,

    -- * Destructuring the Response
    CreateGrantResponse (..),
    newCreateGrantResponse,

    -- * Response Lenses
    createGrantResponse_grantArn,
    createGrantResponse_status,
    createGrantResponse_version,
    createGrantResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateGrant' smart constructor.
data CreateGrant = CreateGrant'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Text,
    -- | Grant name.
    grantName :: Prelude.Text,
    -- | Amazon Resource Name (ARN) of the license.
    licenseArn :: Prelude.Text,
    -- | The grant principals. This value should be specified as an Amazon
    -- Resource Name (ARN).
    principals :: Prelude.NonEmpty Prelude.Text,
    -- | Home Region of the grant.
    homeRegion :: Prelude.Text,
    -- | Allowed operations for the grant.
    allowedOperations :: Prelude.NonEmpty AllowedOperation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGrant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createGrant_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'grantName', 'createGrant_grantName' - Grant name.
--
-- 'licenseArn', 'createGrant_licenseArn' - Amazon Resource Name (ARN) of the license.
--
-- 'principals', 'createGrant_principals' - The grant principals. This value should be specified as an Amazon
-- Resource Name (ARN).
--
-- 'homeRegion', 'createGrant_homeRegion' - Home Region of the grant.
--
-- 'allowedOperations', 'createGrant_allowedOperations' - Allowed operations for the grant.
newCreateGrant ::
  -- | 'clientToken'
  Prelude.Text ->
  -- | 'grantName'
  Prelude.Text ->
  -- | 'licenseArn'
  Prelude.Text ->
  -- | 'principals'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'homeRegion'
  Prelude.Text ->
  -- | 'allowedOperations'
  Prelude.NonEmpty AllowedOperation ->
  CreateGrant
newCreateGrant
  pClientToken_
  pGrantName_
  pLicenseArn_
  pPrincipals_
  pHomeRegion_
  pAllowedOperations_ =
    CreateGrant'
      { clientToken = pClientToken_,
        grantName = pGrantName_,
        licenseArn = pLicenseArn_,
        principals = Lens.coerced Lens.# pPrincipals_,
        homeRegion = pHomeRegion_,
        allowedOperations =
          Lens.coerced Lens.# pAllowedOperations_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createGrant_clientToken :: Lens.Lens' CreateGrant Prelude.Text
createGrant_clientToken = Lens.lens (\CreateGrant' {clientToken} -> clientToken) (\s@CreateGrant' {} a -> s {clientToken = a} :: CreateGrant)

-- | Grant name.
createGrant_grantName :: Lens.Lens' CreateGrant Prelude.Text
createGrant_grantName = Lens.lens (\CreateGrant' {grantName} -> grantName) (\s@CreateGrant' {} a -> s {grantName = a} :: CreateGrant)

-- | Amazon Resource Name (ARN) of the license.
createGrant_licenseArn :: Lens.Lens' CreateGrant Prelude.Text
createGrant_licenseArn = Lens.lens (\CreateGrant' {licenseArn} -> licenseArn) (\s@CreateGrant' {} a -> s {licenseArn = a} :: CreateGrant)

-- | The grant principals. This value should be specified as an Amazon
-- Resource Name (ARN).
createGrant_principals :: Lens.Lens' CreateGrant (Prelude.NonEmpty Prelude.Text)
createGrant_principals = Lens.lens (\CreateGrant' {principals} -> principals) (\s@CreateGrant' {} a -> s {principals = a} :: CreateGrant) Prelude.. Lens.coerced

-- | Home Region of the grant.
createGrant_homeRegion :: Lens.Lens' CreateGrant Prelude.Text
createGrant_homeRegion = Lens.lens (\CreateGrant' {homeRegion} -> homeRegion) (\s@CreateGrant' {} a -> s {homeRegion = a} :: CreateGrant)

-- | Allowed operations for the grant.
createGrant_allowedOperations :: Lens.Lens' CreateGrant (Prelude.NonEmpty AllowedOperation)
createGrant_allowedOperations = Lens.lens (\CreateGrant' {allowedOperations} -> allowedOperations) (\s@CreateGrant' {} a -> s {allowedOperations = a} :: CreateGrant) Prelude.. Lens.coerced

instance Core.AWSRequest CreateGrant where
  type AWSResponse CreateGrant = CreateGrantResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGrantResponse'
            Prelude.<$> (x Data..?> "GrantArn")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "Version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateGrant where
  hashWithSalt _salt CreateGrant' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` grantName
      `Prelude.hashWithSalt` licenseArn
      `Prelude.hashWithSalt` principals
      `Prelude.hashWithSalt` homeRegion
      `Prelude.hashWithSalt` allowedOperations

instance Prelude.NFData CreateGrant where
  rnf CreateGrant' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf grantName
      `Prelude.seq` Prelude.rnf licenseArn
      `Prelude.seq` Prelude.rnf principals
      `Prelude.seq` Prelude.rnf homeRegion
      `Prelude.seq` Prelude.rnf allowedOperations

instance Data.ToHeaders CreateGrant where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLicenseManager.CreateGrant" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateGrant where
  toJSON CreateGrant' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ClientToken" Data..= clientToken),
            Prelude.Just ("GrantName" Data..= grantName),
            Prelude.Just ("LicenseArn" Data..= licenseArn),
            Prelude.Just ("Principals" Data..= principals),
            Prelude.Just ("HomeRegion" Data..= homeRegion),
            Prelude.Just
              ("AllowedOperations" Data..= allowedOperations)
          ]
      )

instance Data.ToPath CreateGrant where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateGrant where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateGrantResponse' smart constructor.
data CreateGrantResponse = CreateGrantResponse'
  { -- | Grant ARN.
    grantArn :: Prelude.Maybe Prelude.Text,
    -- | Grant status.
    status :: Prelude.Maybe GrantStatus,
    -- | Grant version.
    version :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGrantResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grantArn', 'createGrantResponse_grantArn' - Grant ARN.
--
-- 'status', 'createGrantResponse_status' - Grant status.
--
-- 'version', 'createGrantResponse_version' - Grant version.
--
-- 'httpStatus', 'createGrantResponse_httpStatus' - The response's http status code.
newCreateGrantResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateGrantResponse
newCreateGrantResponse pHttpStatus_ =
  CreateGrantResponse'
    { grantArn = Prelude.Nothing,
      status = Prelude.Nothing,
      version = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Grant ARN.
createGrantResponse_grantArn :: Lens.Lens' CreateGrantResponse (Prelude.Maybe Prelude.Text)
createGrantResponse_grantArn = Lens.lens (\CreateGrantResponse' {grantArn} -> grantArn) (\s@CreateGrantResponse' {} a -> s {grantArn = a} :: CreateGrantResponse)

-- | Grant status.
createGrantResponse_status :: Lens.Lens' CreateGrantResponse (Prelude.Maybe GrantStatus)
createGrantResponse_status = Lens.lens (\CreateGrantResponse' {status} -> status) (\s@CreateGrantResponse' {} a -> s {status = a} :: CreateGrantResponse)

-- | Grant version.
createGrantResponse_version :: Lens.Lens' CreateGrantResponse (Prelude.Maybe Prelude.Text)
createGrantResponse_version = Lens.lens (\CreateGrantResponse' {version} -> version) (\s@CreateGrantResponse' {} a -> s {version = a} :: CreateGrantResponse)

-- | The response's http status code.
createGrantResponse_httpStatus :: Lens.Lens' CreateGrantResponse Prelude.Int
createGrantResponse_httpStatus = Lens.lens (\CreateGrantResponse' {httpStatus} -> httpStatus) (\s@CreateGrantResponse' {} a -> s {httpStatus = a} :: CreateGrantResponse)

instance Prelude.NFData CreateGrantResponse where
  rnf CreateGrantResponse' {..} =
    Prelude.rnf grantArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf httpStatus
