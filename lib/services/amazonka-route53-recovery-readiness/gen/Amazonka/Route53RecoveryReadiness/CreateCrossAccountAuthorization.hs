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
-- Module      : Amazonka.Route53RecoveryReadiness.CreateCrossAccountAuthorization
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a cross-account readiness authorization. This lets you authorize
-- another account to work with Route 53 Application Recovery Controller,
-- for example, to check the readiness status of resources in a separate
-- account.
module Amazonka.Route53RecoveryReadiness.CreateCrossAccountAuthorization
  ( -- * Creating a Request
    CreateCrossAccountAuthorization (..),
    newCreateCrossAccountAuthorization,

    -- * Request Lenses
    createCrossAccountAuthorization_crossAccountAuthorization,

    -- * Destructuring the Response
    CreateCrossAccountAuthorizationResponse (..),
    newCreateCrossAccountAuthorizationResponse,

    -- * Response Lenses
    createCrossAccountAuthorizationResponse_crossAccountAuthorization,
    createCrossAccountAuthorizationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newCreateCrossAccountAuthorization' smart constructor.
data CreateCrossAccountAuthorization = CreateCrossAccountAuthorization'
  { -- | The cross-account authorization.
    crossAccountAuthorization :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCrossAccountAuthorization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crossAccountAuthorization', 'createCrossAccountAuthorization_crossAccountAuthorization' - The cross-account authorization.
newCreateCrossAccountAuthorization ::
  -- | 'crossAccountAuthorization'
  Prelude.Text ->
  CreateCrossAccountAuthorization
newCreateCrossAccountAuthorization
  pCrossAccountAuthorization_ =
    CreateCrossAccountAuthorization'
      { crossAccountAuthorization =
          pCrossAccountAuthorization_
      }

-- | The cross-account authorization.
createCrossAccountAuthorization_crossAccountAuthorization :: Lens.Lens' CreateCrossAccountAuthorization Prelude.Text
createCrossAccountAuthorization_crossAccountAuthorization = Lens.lens (\CreateCrossAccountAuthorization' {crossAccountAuthorization} -> crossAccountAuthorization) (\s@CreateCrossAccountAuthorization' {} a -> s {crossAccountAuthorization = a} :: CreateCrossAccountAuthorization)

instance
  Core.AWSRequest
    CreateCrossAccountAuthorization
  where
  type
    AWSResponse CreateCrossAccountAuthorization =
      CreateCrossAccountAuthorizationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCrossAccountAuthorizationResponse'
            Prelude.<$> (x Data..?> "crossAccountAuthorization")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateCrossAccountAuthorization
  where
  hashWithSalt
    _salt
    CreateCrossAccountAuthorization' {..} =
      _salt
        `Prelude.hashWithSalt` crossAccountAuthorization

instance
  Prelude.NFData
    CreateCrossAccountAuthorization
  where
  rnf CreateCrossAccountAuthorization' {..} =
    Prelude.rnf crossAccountAuthorization

instance
  Data.ToHeaders
    CreateCrossAccountAuthorization
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

instance Data.ToJSON CreateCrossAccountAuthorization where
  toJSON CreateCrossAccountAuthorization' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "crossAccountAuthorization"
                  Data..= crossAccountAuthorization
              )
          ]
      )

instance Data.ToPath CreateCrossAccountAuthorization where
  toPath = Prelude.const "/crossaccountauthorizations"

instance Data.ToQuery CreateCrossAccountAuthorization where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCrossAccountAuthorizationResponse' smart constructor.
data CreateCrossAccountAuthorizationResponse = CreateCrossAccountAuthorizationResponse'
  { -- | The cross-account authorization.
    crossAccountAuthorization :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCrossAccountAuthorizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crossAccountAuthorization', 'createCrossAccountAuthorizationResponse_crossAccountAuthorization' - The cross-account authorization.
--
-- 'httpStatus', 'createCrossAccountAuthorizationResponse_httpStatus' - The response's http status code.
newCreateCrossAccountAuthorizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCrossAccountAuthorizationResponse
newCreateCrossAccountAuthorizationResponse
  pHttpStatus_ =
    CreateCrossAccountAuthorizationResponse'
      { crossAccountAuthorization =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The cross-account authorization.
createCrossAccountAuthorizationResponse_crossAccountAuthorization :: Lens.Lens' CreateCrossAccountAuthorizationResponse (Prelude.Maybe Prelude.Text)
createCrossAccountAuthorizationResponse_crossAccountAuthorization = Lens.lens (\CreateCrossAccountAuthorizationResponse' {crossAccountAuthorization} -> crossAccountAuthorization) (\s@CreateCrossAccountAuthorizationResponse' {} a -> s {crossAccountAuthorization = a} :: CreateCrossAccountAuthorizationResponse)

-- | The response's http status code.
createCrossAccountAuthorizationResponse_httpStatus :: Lens.Lens' CreateCrossAccountAuthorizationResponse Prelude.Int
createCrossAccountAuthorizationResponse_httpStatus = Lens.lens (\CreateCrossAccountAuthorizationResponse' {httpStatus} -> httpStatus) (\s@CreateCrossAccountAuthorizationResponse' {} a -> s {httpStatus = a} :: CreateCrossAccountAuthorizationResponse)

instance
  Prelude.NFData
    CreateCrossAccountAuthorizationResponse
  where
  rnf CreateCrossAccountAuthorizationResponse' {..} =
    Prelude.rnf crossAccountAuthorization
      `Prelude.seq` Prelude.rnf httpStatus
