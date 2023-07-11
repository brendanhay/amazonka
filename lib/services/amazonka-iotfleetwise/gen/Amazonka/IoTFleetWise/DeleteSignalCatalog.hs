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
-- Module      : Amazonka.IoTFleetWise.DeleteSignalCatalog
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a signal catalog.
--
-- If the signal catalog is successfully deleted, Amazon Web Services IoT
-- FleetWise sends back an HTTP 200 response with an empty body.
module Amazonka.IoTFleetWise.DeleteSignalCatalog
  ( -- * Creating a Request
    DeleteSignalCatalog (..),
    newDeleteSignalCatalog,

    -- * Request Lenses
    deleteSignalCatalog_name,

    -- * Destructuring the Response
    DeleteSignalCatalogResponse (..),
    newDeleteSignalCatalogResponse,

    -- * Response Lenses
    deleteSignalCatalogResponse_httpStatus,
    deleteSignalCatalogResponse_name,
    deleteSignalCatalogResponse_arn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSignalCatalog' smart constructor.
data DeleteSignalCatalog = DeleteSignalCatalog'
  { -- | The name of the signal catalog to delete.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSignalCatalog' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteSignalCatalog_name' - The name of the signal catalog to delete.
newDeleteSignalCatalog ::
  -- | 'name'
  Prelude.Text ->
  DeleteSignalCatalog
newDeleteSignalCatalog pName_ =
  DeleteSignalCatalog' {name = pName_}

-- | The name of the signal catalog to delete.
deleteSignalCatalog_name :: Lens.Lens' DeleteSignalCatalog Prelude.Text
deleteSignalCatalog_name = Lens.lens (\DeleteSignalCatalog' {name} -> name) (\s@DeleteSignalCatalog' {} a -> s {name = a} :: DeleteSignalCatalog)

instance Core.AWSRequest DeleteSignalCatalog where
  type
    AWSResponse DeleteSignalCatalog =
      DeleteSignalCatalogResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSignalCatalogResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "name")
            Prelude.<*> (x Data..:> "arn")
      )

instance Prelude.Hashable DeleteSignalCatalog where
  hashWithSalt _salt DeleteSignalCatalog' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteSignalCatalog where
  rnf DeleteSignalCatalog' {..} = Prelude.rnf name

instance Data.ToHeaders DeleteSignalCatalog where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.DeleteSignalCatalog" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteSignalCatalog where
  toJSON DeleteSignalCatalog' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Data..= name)]
      )

instance Data.ToPath DeleteSignalCatalog where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteSignalCatalog where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSignalCatalogResponse' smart constructor.
data DeleteSignalCatalogResponse = DeleteSignalCatalogResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the deleted signal catalog.
    name :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the deleted signal catalog.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSignalCatalogResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSignalCatalogResponse_httpStatus' - The response's http status code.
--
-- 'name', 'deleteSignalCatalogResponse_name' - The name of the deleted signal catalog.
--
-- 'arn', 'deleteSignalCatalogResponse_arn' - The Amazon Resource Name (ARN) of the deleted signal catalog.
newDeleteSignalCatalogResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  DeleteSignalCatalogResponse
newDeleteSignalCatalogResponse
  pHttpStatus_
  pName_
  pArn_ =
    DeleteSignalCatalogResponse'
      { httpStatus =
          pHttpStatus_,
        name = pName_,
        arn = pArn_
      }

-- | The response's http status code.
deleteSignalCatalogResponse_httpStatus :: Lens.Lens' DeleteSignalCatalogResponse Prelude.Int
deleteSignalCatalogResponse_httpStatus = Lens.lens (\DeleteSignalCatalogResponse' {httpStatus} -> httpStatus) (\s@DeleteSignalCatalogResponse' {} a -> s {httpStatus = a} :: DeleteSignalCatalogResponse)

-- | The name of the deleted signal catalog.
deleteSignalCatalogResponse_name :: Lens.Lens' DeleteSignalCatalogResponse Prelude.Text
deleteSignalCatalogResponse_name = Lens.lens (\DeleteSignalCatalogResponse' {name} -> name) (\s@DeleteSignalCatalogResponse' {} a -> s {name = a} :: DeleteSignalCatalogResponse)

-- | The Amazon Resource Name (ARN) of the deleted signal catalog.
deleteSignalCatalogResponse_arn :: Lens.Lens' DeleteSignalCatalogResponse Prelude.Text
deleteSignalCatalogResponse_arn = Lens.lens (\DeleteSignalCatalogResponse' {arn} -> arn) (\s@DeleteSignalCatalogResponse' {} a -> s {arn = a} :: DeleteSignalCatalogResponse)

instance Prelude.NFData DeleteSignalCatalogResponse where
  rnf DeleteSignalCatalogResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
