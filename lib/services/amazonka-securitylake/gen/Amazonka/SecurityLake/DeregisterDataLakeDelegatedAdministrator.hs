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
-- Module      : Amazonka.SecurityLake.DeregisterDataLakeDelegatedAdministrator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the Amazon Security Lake delegated administrator account for the
-- organization. This API can only be called by the organization management
-- account. The organization management account cannot be the delegated
-- administrator account.
module Amazonka.SecurityLake.DeregisterDataLakeDelegatedAdministrator
  ( -- * Creating a Request
    DeregisterDataLakeDelegatedAdministrator (..),
    newDeregisterDataLakeDelegatedAdministrator,

    -- * Destructuring the Response
    DeregisterDataLakeDelegatedAdministratorResponse (..),
    newDeregisterDataLakeDelegatedAdministratorResponse,

    -- * Response Lenses
    deregisterDataLakeDelegatedAdministratorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newDeregisterDataLakeDelegatedAdministrator' smart constructor.
data DeregisterDataLakeDelegatedAdministrator = DeregisterDataLakeDelegatedAdministrator'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterDataLakeDelegatedAdministrator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeregisterDataLakeDelegatedAdministrator ::
  DeregisterDataLakeDelegatedAdministrator
newDeregisterDataLakeDelegatedAdministrator =
  DeregisterDataLakeDelegatedAdministrator'

instance
  Core.AWSRequest
    DeregisterDataLakeDelegatedAdministrator
  where
  type
    AWSResponse
      DeregisterDataLakeDelegatedAdministrator =
      DeregisterDataLakeDelegatedAdministratorResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeregisterDataLakeDelegatedAdministratorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeregisterDataLakeDelegatedAdministrator
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    DeregisterDataLakeDelegatedAdministrator
  where
  rnf _ = ()

instance
  Data.ToHeaders
    DeregisterDataLakeDelegatedAdministrator
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

instance
  Data.ToPath
    DeregisterDataLakeDelegatedAdministrator
  where
  toPath = Prelude.const "/v1/datalake/delegate"

instance
  Data.ToQuery
    DeregisterDataLakeDelegatedAdministrator
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterDataLakeDelegatedAdministratorResponse' smart constructor.
data DeregisterDataLakeDelegatedAdministratorResponse = DeregisterDataLakeDelegatedAdministratorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterDataLakeDelegatedAdministratorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deregisterDataLakeDelegatedAdministratorResponse_httpStatus' - The response's http status code.
newDeregisterDataLakeDelegatedAdministratorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeregisterDataLakeDelegatedAdministratorResponse
newDeregisterDataLakeDelegatedAdministratorResponse
  pHttpStatus_ =
    DeregisterDataLakeDelegatedAdministratorResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deregisterDataLakeDelegatedAdministratorResponse_httpStatus :: Lens.Lens' DeregisterDataLakeDelegatedAdministratorResponse Prelude.Int
deregisterDataLakeDelegatedAdministratorResponse_httpStatus = Lens.lens (\DeregisterDataLakeDelegatedAdministratorResponse' {httpStatus} -> httpStatus) (\s@DeregisterDataLakeDelegatedAdministratorResponse' {} a -> s {httpStatus = a} :: DeregisterDataLakeDelegatedAdministratorResponse)

instance
  Prelude.NFData
    DeregisterDataLakeDelegatedAdministratorResponse
  where
  rnf
    DeregisterDataLakeDelegatedAdministratorResponse' {..} =
      Prelude.rnf httpStatus
