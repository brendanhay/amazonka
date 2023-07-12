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
-- Module      : Amazonka.Panorama.RegisterPackageVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a package version.
module Amazonka.Panorama.RegisterPackageVersion
  ( -- * Creating a Request
    RegisterPackageVersion (..),
    newRegisterPackageVersion,

    -- * Request Lenses
    registerPackageVersion_markLatest,
    registerPackageVersion_ownerAccount,
    registerPackageVersion_packageId,
    registerPackageVersion_packageVersion,
    registerPackageVersion_patchVersion,

    -- * Destructuring the Response
    RegisterPackageVersionResponse (..),
    newRegisterPackageVersionResponse,

    -- * Response Lenses
    registerPackageVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRegisterPackageVersion' smart constructor.
data RegisterPackageVersion = RegisterPackageVersion'
  { -- | Whether to mark the new version as the latest version.
    markLatest :: Prelude.Maybe Prelude.Bool,
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
-- Create a value of 'RegisterPackageVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'markLatest', 'registerPackageVersion_markLatest' - Whether to mark the new version as the latest version.
--
-- 'ownerAccount', 'registerPackageVersion_ownerAccount' - An owner account.
--
-- 'packageId', 'registerPackageVersion_packageId' - A package ID.
--
-- 'packageVersion', 'registerPackageVersion_packageVersion' - A package version.
--
-- 'patchVersion', 'registerPackageVersion_patchVersion' - A patch version.
newRegisterPackageVersion ::
  -- | 'packageId'
  Prelude.Text ->
  -- | 'packageVersion'
  Prelude.Text ->
  -- | 'patchVersion'
  Prelude.Text ->
  RegisterPackageVersion
newRegisterPackageVersion
  pPackageId_
  pPackageVersion_
  pPatchVersion_ =
    RegisterPackageVersion'
      { markLatest =
          Prelude.Nothing,
        ownerAccount = Prelude.Nothing,
        packageId = pPackageId_,
        packageVersion = pPackageVersion_,
        patchVersion = pPatchVersion_
      }

-- | Whether to mark the new version as the latest version.
registerPackageVersion_markLatest :: Lens.Lens' RegisterPackageVersion (Prelude.Maybe Prelude.Bool)
registerPackageVersion_markLatest = Lens.lens (\RegisterPackageVersion' {markLatest} -> markLatest) (\s@RegisterPackageVersion' {} a -> s {markLatest = a} :: RegisterPackageVersion)

-- | An owner account.
registerPackageVersion_ownerAccount :: Lens.Lens' RegisterPackageVersion (Prelude.Maybe Prelude.Text)
registerPackageVersion_ownerAccount = Lens.lens (\RegisterPackageVersion' {ownerAccount} -> ownerAccount) (\s@RegisterPackageVersion' {} a -> s {ownerAccount = a} :: RegisterPackageVersion)

-- | A package ID.
registerPackageVersion_packageId :: Lens.Lens' RegisterPackageVersion Prelude.Text
registerPackageVersion_packageId = Lens.lens (\RegisterPackageVersion' {packageId} -> packageId) (\s@RegisterPackageVersion' {} a -> s {packageId = a} :: RegisterPackageVersion)

-- | A package version.
registerPackageVersion_packageVersion :: Lens.Lens' RegisterPackageVersion Prelude.Text
registerPackageVersion_packageVersion = Lens.lens (\RegisterPackageVersion' {packageVersion} -> packageVersion) (\s@RegisterPackageVersion' {} a -> s {packageVersion = a} :: RegisterPackageVersion)

-- | A patch version.
registerPackageVersion_patchVersion :: Lens.Lens' RegisterPackageVersion Prelude.Text
registerPackageVersion_patchVersion = Lens.lens (\RegisterPackageVersion' {patchVersion} -> patchVersion) (\s@RegisterPackageVersion' {} a -> s {patchVersion = a} :: RegisterPackageVersion)

instance Core.AWSRequest RegisterPackageVersion where
  type
    AWSResponse RegisterPackageVersion =
      RegisterPackageVersionResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RegisterPackageVersionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterPackageVersion where
  hashWithSalt _salt RegisterPackageVersion' {..} =
    _salt
      `Prelude.hashWithSalt` markLatest
      `Prelude.hashWithSalt` ownerAccount
      `Prelude.hashWithSalt` packageId
      `Prelude.hashWithSalt` packageVersion
      `Prelude.hashWithSalt` patchVersion

instance Prelude.NFData RegisterPackageVersion where
  rnf RegisterPackageVersion' {..} =
    Prelude.rnf markLatest
      `Prelude.seq` Prelude.rnf ownerAccount
      `Prelude.seq` Prelude.rnf packageId
      `Prelude.seq` Prelude.rnf packageVersion
      `Prelude.seq` Prelude.rnf patchVersion

instance Data.ToHeaders RegisterPackageVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RegisterPackageVersion where
  toJSON RegisterPackageVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MarkLatest" Data..=) Prelude.<$> markLatest,
            ("OwnerAccount" Data..=) Prelude.<$> ownerAccount
          ]
      )

instance Data.ToPath RegisterPackageVersion where
  toPath RegisterPackageVersion' {..} =
    Prelude.mconcat
      [ "/packages/",
        Data.toBS packageId,
        "/versions/",
        Data.toBS packageVersion,
        "/patch/",
        Data.toBS patchVersion
      ]

instance Data.ToQuery RegisterPackageVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterPackageVersionResponse' smart constructor.
data RegisterPackageVersionResponse = RegisterPackageVersionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterPackageVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'registerPackageVersionResponse_httpStatus' - The response's http status code.
newRegisterPackageVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterPackageVersionResponse
newRegisterPackageVersionResponse pHttpStatus_ =
  RegisterPackageVersionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
registerPackageVersionResponse_httpStatus :: Lens.Lens' RegisterPackageVersionResponse Prelude.Int
registerPackageVersionResponse_httpStatus = Lens.lens (\RegisterPackageVersionResponse' {httpStatus} -> httpStatus) (\s@RegisterPackageVersionResponse' {} a -> s {httpStatus = a} :: RegisterPackageVersionResponse)

instance
  Prelude.NFData
    RegisterPackageVersionResponse
  where
  rnf RegisterPackageVersionResponse' {..} =
    Prelude.rnf httpStatus
