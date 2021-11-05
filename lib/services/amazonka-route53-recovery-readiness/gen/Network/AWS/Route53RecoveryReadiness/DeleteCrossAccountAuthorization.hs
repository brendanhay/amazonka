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
-- Module      : Network.AWS.Route53RecoveryReadiness.DeleteCrossAccountAuthorization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete cross account readiness authorization
module Network.AWS.Route53RecoveryReadiness.DeleteCrossAccountAuthorization
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53RecoveryReadiness.Types

-- | /See:/ 'newDeleteCrossAccountAuthorization' smart constructor.
data DeleteCrossAccountAuthorization = DeleteCrossAccountAuthorization'
  { -- | The cross account authorization
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
-- 'crossAccountAuthorization', 'deleteCrossAccountAuthorization_crossAccountAuthorization' - The cross account authorization
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

-- | The cross account authorization
deleteCrossAccountAuthorization_crossAccountAuthorization :: Lens.Lens' DeleteCrossAccountAuthorization Prelude.Text
deleteCrossAccountAuthorization_crossAccountAuthorization = Lens.lens (\DeleteCrossAccountAuthorization' {crossAccountAuthorization} -> crossAccountAuthorization) (\s@DeleteCrossAccountAuthorization' {} a -> s {crossAccountAuthorization = a} :: DeleteCrossAccountAuthorization)

instance
  Core.AWSRequest
    DeleteCrossAccountAuthorization
  where
  type
    AWSResponse DeleteCrossAccountAuthorization =
      DeleteCrossAccountAuthorizationResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCrossAccountAuthorizationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteCrossAccountAuthorization

instance
  Prelude.NFData
    DeleteCrossAccountAuthorization

instance
  Core.ToHeaders
    DeleteCrossAccountAuthorization
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteCrossAccountAuthorization where
  toPath DeleteCrossAccountAuthorization' {..} =
    Prelude.mconcat
      [ "/crossaccountauthorizations/",
        Core.toBS crossAccountAuthorization
      ]

instance Core.ToQuery DeleteCrossAccountAuthorization where
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
