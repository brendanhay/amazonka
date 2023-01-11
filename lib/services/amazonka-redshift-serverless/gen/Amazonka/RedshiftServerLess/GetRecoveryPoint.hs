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
-- Module      : Amazonka.RedshiftServerLess.GetRecoveryPoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a recovery point.
module Amazonka.RedshiftServerLess.GetRecoveryPoint
  ( -- * Creating a Request
    GetRecoveryPoint (..),
    newGetRecoveryPoint,

    -- * Request Lenses
    getRecoveryPoint_recoveryPointId,

    -- * Destructuring the Response
    GetRecoveryPointResponse (..),
    newGetRecoveryPointResponse,

    -- * Response Lenses
    getRecoveryPointResponse_recoveryPoint,
    getRecoveryPointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRecoveryPoint' smart constructor.
data GetRecoveryPoint = GetRecoveryPoint'
  { -- | The unique identifier of the recovery point to return information for.
    recoveryPointId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRecoveryPoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recoveryPointId', 'getRecoveryPoint_recoveryPointId' - The unique identifier of the recovery point to return information for.
newGetRecoveryPoint ::
  -- | 'recoveryPointId'
  Prelude.Text ->
  GetRecoveryPoint
newGetRecoveryPoint pRecoveryPointId_ =
  GetRecoveryPoint'
    { recoveryPointId =
        pRecoveryPointId_
    }

-- | The unique identifier of the recovery point to return information for.
getRecoveryPoint_recoveryPointId :: Lens.Lens' GetRecoveryPoint Prelude.Text
getRecoveryPoint_recoveryPointId = Lens.lens (\GetRecoveryPoint' {recoveryPointId} -> recoveryPointId) (\s@GetRecoveryPoint' {} a -> s {recoveryPointId = a} :: GetRecoveryPoint)

instance Core.AWSRequest GetRecoveryPoint where
  type
    AWSResponse GetRecoveryPoint =
      GetRecoveryPointResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRecoveryPointResponse'
            Prelude.<$> (x Data..?> "recoveryPoint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRecoveryPoint where
  hashWithSalt _salt GetRecoveryPoint' {..} =
    _salt `Prelude.hashWithSalt` recoveryPointId

instance Prelude.NFData GetRecoveryPoint where
  rnf GetRecoveryPoint' {..} =
    Prelude.rnf recoveryPointId

instance Data.ToHeaders GetRecoveryPoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RedshiftServerless.GetRecoveryPoint" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetRecoveryPoint where
  toJSON GetRecoveryPoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("recoveryPointId" Data..= recoveryPointId)
          ]
      )

instance Data.ToPath GetRecoveryPoint where
  toPath = Prelude.const "/"

instance Data.ToQuery GetRecoveryPoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRecoveryPointResponse' smart constructor.
data GetRecoveryPointResponse = GetRecoveryPointResponse'
  { -- | The returned recovery point object.
    recoveryPoint :: Prelude.Maybe RecoveryPoint,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRecoveryPointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recoveryPoint', 'getRecoveryPointResponse_recoveryPoint' - The returned recovery point object.
--
-- 'httpStatus', 'getRecoveryPointResponse_httpStatus' - The response's http status code.
newGetRecoveryPointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRecoveryPointResponse
newGetRecoveryPointResponse pHttpStatus_ =
  GetRecoveryPointResponse'
    { recoveryPoint =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The returned recovery point object.
getRecoveryPointResponse_recoveryPoint :: Lens.Lens' GetRecoveryPointResponse (Prelude.Maybe RecoveryPoint)
getRecoveryPointResponse_recoveryPoint = Lens.lens (\GetRecoveryPointResponse' {recoveryPoint} -> recoveryPoint) (\s@GetRecoveryPointResponse' {} a -> s {recoveryPoint = a} :: GetRecoveryPointResponse)

-- | The response's http status code.
getRecoveryPointResponse_httpStatus :: Lens.Lens' GetRecoveryPointResponse Prelude.Int
getRecoveryPointResponse_httpStatus = Lens.lens (\GetRecoveryPointResponse' {httpStatus} -> httpStatus) (\s@GetRecoveryPointResponse' {} a -> s {httpStatus = a} :: GetRecoveryPointResponse)

instance Prelude.NFData GetRecoveryPointResponse where
  rnf GetRecoveryPointResponse' {..} =
    Prelude.rnf recoveryPoint
      `Prelude.seq` Prelude.rnf httpStatus
