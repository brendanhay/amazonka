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
-- Module      : Amazonka.RedshiftServerLess.GetTableRestoreStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a @TableRestoreStatus@ object.
module Amazonka.RedshiftServerLess.GetTableRestoreStatus
  ( -- * Creating a Request
    GetTableRestoreStatus (..),
    newGetTableRestoreStatus,

    -- * Request Lenses
    getTableRestoreStatus_tableRestoreRequestId,

    -- * Destructuring the Response
    GetTableRestoreStatusResponse (..),
    newGetTableRestoreStatusResponse,

    -- * Response Lenses
    getTableRestoreStatusResponse_tableRestoreStatus,
    getTableRestoreStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetTableRestoreStatus' smart constructor.
data GetTableRestoreStatus = GetTableRestoreStatus'
  { -- | The ID of the @RestoreTableFromSnapshot@ request to return status for.
    tableRestoreRequestId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTableRestoreStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableRestoreRequestId', 'getTableRestoreStatus_tableRestoreRequestId' - The ID of the @RestoreTableFromSnapshot@ request to return status for.
newGetTableRestoreStatus ::
  -- | 'tableRestoreRequestId'
  Prelude.Text ->
  GetTableRestoreStatus
newGetTableRestoreStatus pTableRestoreRequestId_ =
  GetTableRestoreStatus'
    { tableRestoreRequestId =
        pTableRestoreRequestId_
    }

-- | The ID of the @RestoreTableFromSnapshot@ request to return status for.
getTableRestoreStatus_tableRestoreRequestId :: Lens.Lens' GetTableRestoreStatus Prelude.Text
getTableRestoreStatus_tableRestoreRequestId = Lens.lens (\GetTableRestoreStatus' {tableRestoreRequestId} -> tableRestoreRequestId) (\s@GetTableRestoreStatus' {} a -> s {tableRestoreRequestId = a} :: GetTableRestoreStatus)

instance Core.AWSRequest GetTableRestoreStatus where
  type
    AWSResponse GetTableRestoreStatus =
      GetTableRestoreStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTableRestoreStatusResponse'
            Prelude.<$> (x Data..?> "tableRestoreStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTableRestoreStatus where
  hashWithSalt _salt GetTableRestoreStatus' {..} =
    _salt `Prelude.hashWithSalt` tableRestoreRequestId

instance Prelude.NFData GetTableRestoreStatus where
  rnf GetTableRestoreStatus' {..} =
    Prelude.rnf tableRestoreRequestId

instance Data.ToHeaders GetTableRestoreStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RedshiftServerless.GetTableRestoreStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetTableRestoreStatus where
  toJSON GetTableRestoreStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "tableRestoreRequestId"
                  Data..= tableRestoreRequestId
              )
          ]
      )

instance Data.ToPath GetTableRestoreStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery GetTableRestoreStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTableRestoreStatusResponse' smart constructor.
data GetTableRestoreStatusResponse = GetTableRestoreStatusResponse'
  { -- | The returned @TableRestoreStatus@ object that contains information about
    -- the status of your @RestoreTableFromSnapshot@ request.
    tableRestoreStatus :: Prelude.Maybe TableRestoreStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTableRestoreStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableRestoreStatus', 'getTableRestoreStatusResponse_tableRestoreStatus' - The returned @TableRestoreStatus@ object that contains information about
-- the status of your @RestoreTableFromSnapshot@ request.
--
-- 'httpStatus', 'getTableRestoreStatusResponse_httpStatus' - The response's http status code.
newGetTableRestoreStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTableRestoreStatusResponse
newGetTableRestoreStatusResponse pHttpStatus_ =
  GetTableRestoreStatusResponse'
    { tableRestoreStatus =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The returned @TableRestoreStatus@ object that contains information about
-- the status of your @RestoreTableFromSnapshot@ request.
getTableRestoreStatusResponse_tableRestoreStatus :: Lens.Lens' GetTableRestoreStatusResponse (Prelude.Maybe TableRestoreStatus)
getTableRestoreStatusResponse_tableRestoreStatus = Lens.lens (\GetTableRestoreStatusResponse' {tableRestoreStatus} -> tableRestoreStatus) (\s@GetTableRestoreStatusResponse' {} a -> s {tableRestoreStatus = a} :: GetTableRestoreStatusResponse)

-- | The response's http status code.
getTableRestoreStatusResponse_httpStatus :: Lens.Lens' GetTableRestoreStatusResponse Prelude.Int
getTableRestoreStatusResponse_httpStatus = Lens.lens (\GetTableRestoreStatusResponse' {httpStatus} -> httpStatus) (\s@GetTableRestoreStatusResponse' {} a -> s {httpStatus = a} :: GetTableRestoreStatusResponse)

instance Prelude.NFData GetTableRestoreStatusResponse where
  rnf GetTableRestoreStatusResponse' {..} =
    Prelude.rnf tableRestoreStatus `Prelude.seq`
      Prelude.rnf httpStatus
