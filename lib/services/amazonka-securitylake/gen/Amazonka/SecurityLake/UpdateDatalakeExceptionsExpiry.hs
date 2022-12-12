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
-- Module      : Amazonka.SecurityLake.UpdateDatalakeExceptionsExpiry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the expiration period for the exception message to your preferred
-- time, and control the time-to-live (TTL) for the exception message to
-- remain. Exceptions are stored by default, for a 2 week period of time
-- from when a record was created in Security Lake.
module Amazonka.SecurityLake.UpdateDatalakeExceptionsExpiry
  ( -- * Creating a Request
    UpdateDatalakeExceptionsExpiry (..),
    newUpdateDatalakeExceptionsExpiry,

    -- * Request Lenses
    updateDatalakeExceptionsExpiry_exceptionMessageExpiry,

    -- * Destructuring the Response
    UpdateDatalakeExceptionsExpiryResponse (..),
    newUpdateDatalakeExceptionsExpiryResponse,

    -- * Response Lenses
    updateDatalakeExceptionsExpiryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newUpdateDatalakeExceptionsExpiry' smart constructor.
data UpdateDatalakeExceptionsExpiry = UpdateDatalakeExceptionsExpiry'
  { -- | The time-to-live (TTL) for the exception message to remain.
    exceptionMessageExpiry :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDatalakeExceptionsExpiry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exceptionMessageExpiry', 'updateDatalakeExceptionsExpiry_exceptionMessageExpiry' - The time-to-live (TTL) for the exception message to remain.
newUpdateDatalakeExceptionsExpiry ::
  -- | 'exceptionMessageExpiry'
  Prelude.Natural ->
  UpdateDatalakeExceptionsExpiry
newUpdateDatalakeExceptionsExpiry
  pExceptionMessageExpiry_ =
    UpdateDatalakeExceptionsExpiry'
      { exceptionMessageExpiry =
          pExceptionMessageExpiry_
      }

-- | The time-to-live (TTL) for the exception message to remain.
updateDatalakeExceptionsExpiry_exceptionMessageExpiry :: Lens.Lens' UpdateDatalakeExceptionsExpiry Prelude.Natural
updateDatalakeExceptionsExpiry_exceptionMessageExpiry = Lens.lens (\UpdateDatalakeExceptionsExpiry' {exceptionMessageExpiry} -> exceptionMessageExpiry) (\s@UpdateDatalakeExceptionsExpiry' {} a -> s {exceptionMessageExpiry = a} :: UpdateDatalakeExceptionsExpiry)

instance
  Core.AWSRequest
    UpdateDatalakeExceptionsExpiry
  where
  type
    AWSResponse UpdateDatalakeExceptionsExpiry =
      UpdateDatalakeExceptionsExpiryResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDatalakeExceptionsExpiryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateDatalakeExceptionsExpiry
  where
  hashWithSalt
    _salt
    UpdateDatalakeExceptionsExpiry' {..} =
      _salt `Prelude.hashWithSalt` exceptionMessageExpiry

instance
  Prelude.NFData
    UpdateDatalakeExceptionsExpiry
  where
  rnf UpdateDatalakeExceptionsExpiry' {..} =
    Prelude.rnf exceptionMessageExpiry

instance
  Data.ToHeaders
    UpdateDatalakeExceptionsExpiry
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDatalakeExceptionsExpiry where
  toJSON UpdateDatalakeExceptionsExpiry' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "exceptionMessageExpiry"
                  Data..= exceptionMessageExpiry
              )
          ]
      )

instance Data.ToPath UpdateDatalakeExceptionsExpiry where
  toPath =
    Prelude.const "/v1/datalake/exceptions/expiry"

instance Data.ToQuery UpdateDatalakeExceptionsExpiry where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDatalakeExceptionsExpiryResponse' smart constructor.
data UpdateDatalakeExceptionsExpiryResponse = UpdateDatalakeExceptionsExpiryResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDatalakeExceptionsExpiryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDatalakeExceptionsExpiryResponse_httpStatus' - The response's http status code.
newUpdateDatalakeExceptionsExpiryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDatalakeExceptionsExpiryResponse
newUpdateDatalakeExceptionsExpiryResponse
  pHttpStatus_ =
    UpdateDatalakeExceptionsExpiryResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateDatalakeExceptionsExpiryResponse_httpStatus :: Lens.Lens' UpdateDatalakeExceptionsExpiryResponse Prelude.Int
updateDatalakeExceptionsExpiryResponse_httpStatus = Lens.lens (\UpdateDatalakeExceptionsExpiryResponse' {httpStatus} -> httpStatus) (\s@UpdateDatalakeExceptionsExpiryResponse' {} a -> s {httpStatus = a} :: UpdateDatalakeExceptionsExpiryResponse)

instance
  Prelude.NFData
    UpdateDatalakeExceptionsExpiryResponse
  where
  rnf UpdateDatalakeExceptionsExpiryResponse' {..} =
    Prelude.rnf httpStatus
