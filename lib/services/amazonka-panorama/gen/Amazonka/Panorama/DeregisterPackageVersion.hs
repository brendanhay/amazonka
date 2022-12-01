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
-- Module      : Amazonka.Panorama.DeregisterPackageVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters a package version.
module Amazonka.Panorama.DeregisterPackageVersion
  ( -- * Creating a Request
    DeregisterPackageVersion (..),
    newDeregisterPackageVersion,

    -- * Request Lenses
    deregisterPackageVersion_updatedLatestPatchVersion,
    deregisterPackageVersion_ownerAccount,
    deregisterPackageVersion_packageId,
    deregisterPackageVersion_packageVersion,
    deregisterPackageVersion_patchVersion,

    -- * Destructuring the Response
    DeregisterPackageVersionResponse (..),
    newDeregisterPackageVersionResponse,

    -- * Response Lenses
    deregisterPackageVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Panorama.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeregisterPackageVersion' smart constructor.
data DeregisterPackageVersion = DeregisterPackageVersion'
  { -- | If the version was marked latest, the new version to maker as latest.
    updatedLatestPatchVersion :: Prelude.Maybe Prelude.Text,
    -- | An owner account.
    ownerAccount :: Prelude.Maybe Prelude.Text,
    -- | A package ID.
    packageId :: Prelude.Text,
    -- | A package version.
    packageVersion :: Prelude.Text,
    -- | A patch version.
    patchVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterPackageVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updatedLatestPatchVersion', 'deregisterPackageVersion_updatedLatestPatchVersion' - If the version was marked latest, the new version to maker as latest.
--
-- 'ownerAccount', 'deregisterPackageVersion_ownerAccount' - An owner account.
--
-- 'packageId', 'deregisterPackageVersion_packageId' - A package ID.
--
-- 'packageVersion', 'deregisterPackageVersion_packageVersion' - A package version.
--
-- 'patchVersion', 'deregisterPackageVersion_patchVersion' - A patch version.
newDeregisterPackageVersion ::
  -- | 'packageId'
  Prelude.Text ->
  -- | 'packageVersion'
  Prelude.Text ->
  -- | 'patchVersion'
  Prelude.Text ->
  DeregisterPackageVersion
newDeregisterPackageVersion
  pPackageId_
  pPackageVersion_
  pPatchVersion_ =
    DeregisterPackageVersion'
      { updatedLatestPatchVersion =
          Prelude.Nothing,
        ownerAccount = Prelude.Nothing,
        packageId = pPackageId_,
        packageVersion = pPackageVersion_,
        patchVersion = pPatchVersion_
      }

-- | If the version was marked latest, the new version to maker as latest.
deregisterPackageVersion_updatedLatestPatchVersion :: Lens.Lens' DeregisterPackageVersion (Prelude.Maybe Prelude.Text)
deregisterPackageVersion_updatedLatestPatchVersion = Lens.lens (\DeregisterPackageVersion' {updatedLatestPatchVersion} -> updatedLatestPatchVersion) (\s@DeregisterPackageVersion' {} a -> s {updatedLatestPatchVersion = a} :: DeregisterPackageVersion)

-- | An owner account.
deregisterPackageVersion_ownerAccount :: Lens.Lens' DeregisterPackageVersion (Prelude.Maybe Prelude.Text)
deregisterPackageVersion_ownerAccount = Lens.lens (\DeregisterPackageVersion' {ownerAccount} -> ownerAccount) (\s@DeregisterPackageVersion' {} a -> s {ownerAccount = a} :: DeregisterPackageVersion)

-- | A package ID.
deregisterPackageVersion_packageId :: Lens.Lens' DeregisterPackageVersion Prelude.Text
deregisterPackageVersion_packageId = Lens.lens (\DeregisterPackageVersion' {packageId} -> packageId) (\s@DeregisterPackageVersion' {} a -> s {packageId = a} :: DeregisterPackageVersion)

-- | A package version.
deregisterPackageVersion_packageVersion :: Lens.Lens' DeregisterPackageVersion Prelude.Text
deregisterPackageVersion_packageVersion = Lens.lens (\DeregisterPackageVersion' {packageVersion} -> packageVersion) (\s@DeregisterPackageVersion' {} a -> s {packageVersion = a} :: DeregisterPackageVersion)

-- | A patch version.
deregisterPackageVersion_patchVersion :: Lens.Lens' DeregisterPackageVersion Prelude.Text
deregisterPackageVersion_patchVersion = Lens.lens (\DeregisterPackageVersion' {patchVersion} -> patchVersion) (\s@DeregisterPackageVersion' {} a -> s {patchVersion = a} :: DeregisterPackageVersion)

instance Core.AWSRequest DeregisterPackageVersion where
  type
    AWSResponse DeregisterPackageVersion =
      DeregisterPackageVersionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeregisterPackageVersionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeregisterPackageVersion where
  hashWithSalt _salt DeregisterPackageVersion' {..} =
    _salt
      `Prelude.hashWithSalt` updatedLatestPatchVersion
      `Prelude.hashWithSalt` ownerAccount
      `Prelude.hashWithSalt` packageId
      `Prelude.hashWithSalt` packageVersion
      `Prelude.hashWithSalt` patchVersion

instance Prelude.NFData DeregisterPackageVersion where
  rnf DeregisterPackageVersion' {..} =
    Prelude.rnf updatedLatestPatchVersion
      `Prelude.seq` Prelude.rnf ownerAccount
      `Prelude.seq` Prelude.rnf packageId
      `Prelude.seq` Prelude.rnf packageVersion
      `Prelude.seq` Prelude.rnf patchVersion

instance Core.ToHeaders DeregisterPackageVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeregisterPackageVersion where
  toPath DeregisterPackageVersion' {..} =
    Prelude.mconcat
      [ "/packages/",
        Core.toBS packageId,
        "/versions/",
        Core.toBS packageVersion,
        "/patch/",
        Core.toBS patchVersion
      ]

instance Core.ToQuery DeregisterPackageVersion where
  toQuery DeregisterPackageVersion' {..} =
    Prelude.mconcat
      [ "UpdatedLatestPatchVersion"
          Core.=: updatedLatestPatchVersion,
        "OwnerAccount" Core.=: ownerAccount
      ]

-- | /See:/ 'newDeregisterPackageVersionResponse' smart constructor.
data DeregisterPackageVersionResponse = DeregisterPackageVersionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterPackageVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deregisterPackageVersionResponse_httpStatus' - The response's http status code.
newDeregisterPackageVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeregisterPackageVersionResponse
newDeregisterPackageVersionResponse pHttpStatus_ =
  DeregisterPackageVersionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deregisterPackageVersionResponse_httpStatus :: Lens.Lens' DeregisterPackageVersionResponse Prelude.Int
deregisterPackageVersionResponse_httpStatus = Lens.lens (\DeregisterPackageVersionResponse' {httpStatus} -> httpStatus) (\s@DeregisterPackageVersionResponse' {} a -> s {httpStatus = a} :: DeregisterPackageVersionResponse)

instance
  Prelude.NFData
    DeregisterPackageVersionResponse
  where
  rnf DeregisterPackageVersionResponse' {..} =
    Prelude.rnf httpStatus
