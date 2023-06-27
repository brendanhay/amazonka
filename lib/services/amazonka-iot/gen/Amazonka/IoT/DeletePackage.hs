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
-- Module      : Amazonka.IoT.DeletePackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific version from a software package.
--
-- __Note:__ All package versions must be deleted before deleting the
-- software package.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DeletePackageVersion>
-- action.
module Amazonka.IoT.DeletePackage
  ( -- * Creating a Request
    DeletePackage (..),
    newDeletePackage,

    -- * Request Lenses
    deletePackage_clientToken,
    deletePackage_packageName,

    -- * Destructuring the Response
    DeletePackageResponse (..),
    newDeletePackageResponse,

    -- * Response Lenses
    deletePackageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePackage' smart constructor.
data DeletePackage = DeletePackage'
  { -- | A unique case-sensitive identifier that you can provide to ensure the
    -- idempotency of the request. Don\'t reuse this client token if a new
    -- idempotent request is required.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the target package.
    packageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deletePackage_clientToken' - A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
--
-- 'packageName', 'deletePackage_packageName' - The name of the target package.
newDeletePackage ::
  -- | 'packageName'
  Prelude.Text ->
  DeletePackage
newDeletePackage pPackageName_ =
  DeletePackage'
    { clientToken = Prelude.Nothing,
      packageName = pPackageName_
    }

-- | A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
deletePackage_clientToken :: Lens.Lens' DeletePackage (Prelude.Maybe Prelude.Text)
deletePackage_clientToken = Lens.lens (\DeletePackage' {clientToken} -> clientToken) (\s@DeletePackage' {} a -> s {clientToken = a} :: DeletePackage)

-- | The name of the target package.
deletePackage_packageName :: Lens.Lens' DeletePackage Prelude.Text
deletePackage_packageName = Lens.lens (\DeletePackage' {packageName} -> packageName) (\s@DeletePackage' {} a -> s {packageName = a} :: DeletePackage)

instance Core.AWSRequest DeletePackage where
  type
    AWSResponse DeletePackage =
      DeletePackageResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeletePackageResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePackage where
  hashWithSalt _salt DeletePackage' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` packageName

instance Prelude.NFData DeletePackage where
  rnf DeletePackage' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf packageName

instance Data.ToHeaders DeletePackage where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeletePackage where
  toPath DeletePackage' {..} =
    Prelude.mconcat
      ["/packages/", Data.toBS packageName]

instance Data.ToQuery DeletePackage where
  toQuery DeletePackage' {..} =
    Prelude.mconcat ["clientToken" Data.=: clientToken]

-- | /See:/ 'newDeletePackageResponse' smart constructor.
data DeletePackageResponse = DeletePackageResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePackageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deletePackageResponse_httpStatus' - The response's http status code.
newDeletePackageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePackageResponse
newDeletePackageResponse pHttpStatus_ =
  DeletePackageResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deletePackageResponse_httpStatus :: Lens.Lens' DeletePackageResponse Prelude.Int
deletePackageResponse_httpStatus = Lens.lens (\DeletePackageResponse' {httpStatus} -> httpStatus) (\s@DeletePackageResponse' {} a -> s {httpStatus = a} :: DeletePackageResponse)

instance Prelude.NFData DeletePackageResponse where
  rnf DeletePackageResponse' {..} =
    Prelude.rnf httpStatus
