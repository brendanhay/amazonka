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
-- Module      : Amazonka.AppIntegrationS.DeleteDataIntegration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the DataIntegration. Only DataIntegrations that don\'t have any
-- DataIntegrationAssociations can be deleted. Deleting a DataIntegration
-- also deletes the underlying Amazon AppFlow flow and service linked role.
--
-- You cannot create a DataIntegration association for a DataIntegration
-- that has been previously associated. Use a different DataIntegration, or
-- recreate the DataIntegration using the
-- <https://docs.aws.amazon.com/appintegrations/latest/APIReference/API_CreateDataIntegration.html CreateDataIntegration>
-- API.
module Amazonka.AppIntegrationS.DeleteDataIntegration
  ( -- * Creating a Request
    DeleteDataIntegration (..),
    newDeleteDataIntegration,

    -- * Request Lenses
    deleteDataIntegration_dataIntegrationIdentifier,

    -- * Destructuring the Response
    DeleteDataIntegrationResponse (..),
    newDeleteDataIntegrationResponse,

    -- * Response Lenses
    deleteDataIntegrationResponse_httpStatus,
  )
where

import Amazonka.AppIntegrationS.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDataIntegration' smart constructor.
data DeleteDataIntegration = DeleteDataIntegration'
  { -- | A unique identifier for the DataIntegration.
    dataIntegrationIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDataIntegration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataIntegrationIdentifier', 'deleteDataIntegration_dataIntegrationIdentifier' - A unique identifier for the DataIntegration.
newDeleteDataIntegration ::
  -- | 'dataIntegrationIdentifier'
  Prelude.Text ->
  DeleteDataIntegration
newDeleteDataIntegration pDataIntegrationIdentifier_ =
  DeleteDataIntegration'
    { dataIntegrationIdentifier =
        pDataIntegrationIdentifier_
    }

-- | A unique identifier for the DataIntegration.
deleteDataIntegration_dataIntegrationIdentifier :: Lens.Lens' DeleteDataIntegration Prelude.Text
deleteDataIntegration_dataIntegrationIdentifier = Lens.lens (\DeleteDataIntegration' {dataIntegrationIdentifier} -> dataIntegrationIdentifier) (\s@DeleteDataIntegration' {} a -> s {dataIntegrationIdentifier = a} :: DeleteDataIntegration)

instance Core.AWSRequest DeleteDataIntegration where
  type
    AWSResponse DeleteDataIntegration =
      DeleteDataIntegrationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDataIntegrationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDataIntegration where
  hashWithSalt _salt DeleteDataIntegration' {..} =
    _salt
      `Prelude.hashWithSalt` dataIntegrationIdentifier

instance Prelude.NFData DeleteDataIntegration where
  rnf DeleteDataIntegration' {..} =
    Prelude.rnf dataIntegrationIdentifier

instance Data.ToHeaders DeleteDataIntegration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteDataIntegration where
  toPath DeleteDataIntegration' {..} =
    Prelude.mconcat
      [ "/dataIntegrations/",
        Data.toBS dataIntegrationIdentifier
      ]

instance Data.ToQuery DeleteDataIntegration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDataIntegrationResponse' smart constructor.
data DeleteDataIntegrationResponse = DeleteDataIntegrationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDataIntegrationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDataIntegrationResponse_httpStatus' - The response's http status code.
newDeleteDataIntegrationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDataIntegrationResponse
newDeleteDataIntegrationResponse pHttpStatus_ =
  DeleteDataIntegrationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteDataIntegrationResponse_httpStatus :: Lens.Lens' DeleteDataIntegrationResponse Prelude.Int
deleteDataIntegrationResponse_httpStatus = Lens.lens (\DeleteDataIntegrationResponse' {httpStatus} -> httpStatus) (\s@DeleteDataIntegrationResponse' {} a -> s {httpStatus = a} :: DeleteDataIntegrationResponse)

instance Prelude.NFData DeleteDataIntegrationResponse where
  rnf DeleteDataIntegrationResponse' {..} =
    Prelude.rnf httpStatus
