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
-- Module      : Amazonka.Route53RecoveryReadiness.DeleteCrossAccountAuthorization
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes cross account readiness authorization.
module Amazonka.Route53RecoveryReadiness.DeleteCrossAccountAuthorization
  ( -- * Creating a Request
    DeleteCrossAccountAuthorization (..),
    newDeleteCrossAccountAuthorization,

    -- * Request Lenses
    deleteCrossAccountAuthorization_crossAccountAuthorization,

    -- * Destructuring the Response
    DeleteCrossAccountAuthorizationResponse (..),
    newDeleteCrossAccountAuthorizationResponse,

    -- * Response Lenses
    deleteCrossAccountAuthorizationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newDeleteCrossAccountAuthorization' smart constructor.
data DeleteCrossAccountAuthorization = DeleteCrossAccountAuthorization'
  { -- | The cross-account authorization.
    crossAccountAuthorization :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCrossAccountAuthorization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crossAccountAuthorization', 'deleteCrossAccountAuthorization_crossAccountAuthorization' - The cross-account authorization.
newDeleteCrossAccountAuthorization ::
  -- | 'crossAccountAuthorization'
  Prelude.Text ->
  DeleteCrossAccountAuthorization
newDeleteCrossAccountAuthorization
  pCrossAccountAuthorization_ =
    DeleteCrossAccountAuthorization'
      { crossAccountAuthorization =
          pCrossAccountAuthorization_
      }

-- | The cross-account authorization.
deleteCrossAccountAuthorization_crossAccountAuthorization :: Lens.Lens' DeleteCrossAccountAuthorization Prelude.Text
deleteCrossAccountAuthorization_crossAccountAuthorization = Lens.lens (\DeleteCrossAccountAuthorization' {crossAccountAuthorization} -> crossAccountAuthorization) (\s@DeleteCrossAccountAuthorization' {} a -> s {crossAccountAuthorization = a} :: DeleteCrossAccountAuthorization)

instance
  Core.AWSRequest
    DeleteCrossAccountAuthorization
  where
  type
    AWSResponse DeleteCrossAccountAuthorization =
      DeleteCrossAccountAuthorizationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCrossAccountAuthorizationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteCrossAccountAuthorization
  where
  hashWithSalt
    _salt
    DeleteCrossAccountAuthorization' {..} =
      _salt
        `Prelude.hashWithSalt` crossAccountAuthorization

instance
  Prelude.NFData
    DeleteCrossAccountAuthorization
  where
  rnf DeleteCrossAccountAuthorization' {..} =
    Prelude.rnf crossAccountAuthorization

instance
  Data.ToHeaders
    DeleteCrossAccountAuthorization
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

instance Data.ToPath DeleteCrossAccountAuthorization where
  toPath DeleteCrossAccountAuthorization' {..} =
    Prelude.mconcat
      [ "/crossaccountauthorizations/",
        Data.toBS crossAccountAuthorization
      ]

instance Data.ToQuery DeleteCrossAccountAuthorization where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCrossAccountAuthorizationResponse' smart constructor.
data DeleteCrossAccountAuthorizationResponse = DeleteCrossAccountAuthorizationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCrossAccountAuthorizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteCrossAccountAuthorizationResponse_httpStatus' - The response's http status code.
newDeleteCrossAccountAuthorizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteCrossAccountAuthorizationResponse
newDeleteCrossAccountAuthorizationResponse
  pHttpStatus_ =
    DeleteCrossAccountAuthorizationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteCrossAccountAuthorizationResponse_httpStatus :: Lens.Lens' DeleteCrossAccountAuthorizationResponse Prelude.Int
deleteCrossAccountAuthorizationResponse_httpStatus = Lens.lens (\DeleteCrossAccountAuthorizationResponse' {httpStatus} -> httpStatus) (\s@DeleteCrossAccountAuthorizationResponse' {} a -> s {httpStatus = a} :: DeleteCrossAccountAuthorizationResponse)

instance
  Prelude.NFData
    DeleteCrossAccountAuthorizationResponse
  where
  rnf DeleteCrossAccountAuthorizationResponse' {..} =
    Prelude.rnf httpStatus
