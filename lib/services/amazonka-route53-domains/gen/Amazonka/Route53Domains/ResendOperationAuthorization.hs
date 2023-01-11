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
-- Module      : Amazonka.Route53Domains.ResendOperationAuthorization
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resend the form of authorization email for this operation.
module Amazonka.Route53Domains.ResendOperationAuthorization
  ( -- * Creating a Request
    ResendOperationAuthorization (..),
    newResendOperationAuthorization,

    -- * Request Lenses
    resendOperationAuthorization_operationId,

    -- * Destructuring the Response
    ResendOperationAuthorizationResponse (..),
    newResendOperationAuthorizationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Domains.Types

-- | /See:/ 'newResendOperationAuthorization' smart constructor.
data ResendOperationAuthorization = ResendOperationAuthorization'
  { -- | Operation ID.
    operationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResendOperationAuthorization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'resendOperationAuthorization_operationId' - Operation ID.
newResendOperationAuthorization ::
  -- | 'operationId'
  Prelude.Text ->
  ResendOperationAuthorization
newResendOperationAuthorization pOperationId_ =
  ResendOperationAuthorization'
    { operationId =
        pOperationId_
    }

-- | Operation ID.
resendOperationAuthorization_operationId :: Lens.Lens' ResendOperationAuthorization Prelude.Text
resendOperationAuthorization_operationId = Lens.lens (\ResendOperationAuthorization' {operationId} -> operationId) (\s@ResendOperationAuthorization' {} a -> s {operationId = a} :: ResendOperationAuthorization)

instance Core.AWSRequest ResendOperationAuthorization where
  type
    AWSResponse ResendOperationAuthorization =
      ResendOperationAuthorizationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      ResendOperationAuthorizationResponse'

instance
  Prelude.Hashable
    ResendOperationAuthorization
  where
  hashWithSalt _salt ResendOperationAuthorization' {..} =
    _salt `Prelude.hashWithSalt` operationId

instance Prelude.NFData ResendOperationAuthorization where
  rnf ResendOperationAuthorization' {..} =
    Prelude.rnf operationId

instance Data.ToHeaders ResendOperationAuthorization where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Domains_v20140515.ResendOperationAuthorization" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ResendOperationAuthorization where
  toJSON ResendOperationAuthorization' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("OperationId" Data..= operationId)]
      )

instance Data.ToPath ResendOperationAuthorization where
  toPath = Prelude.const "/"

instance Data.ToQuery ResendOperationAuthorization where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newResendOperationAuthorizationResponse' smart constructor.
data ResendOperationAuthorizationResponse = ResendOperationAuthorizationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResendOperationAuthorizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newResendOperationAuthorizationResponse ::
  ResendOperationAuthorizationResponse
newResendOperationAuthorizationResponse =
  ResendOperationAuthorizationResponse'

instance
  Prelude.NFData
    ResendOperationAuthorizationResponse
  where
  rnf _ = ()
