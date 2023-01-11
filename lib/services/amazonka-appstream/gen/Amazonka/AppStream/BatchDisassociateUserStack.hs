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
-- Module      : Amazonka.AppStream.BatchDisassociateUserStack
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified users from the specified stacks.
module Amazonka.AppStream.BatchDisassociateUserStack
  ( -- * Creating a Request
    BatchDisassociateUserStack (..),
    newBatchDisassociateUserStack,

    -- * Request Lenses
    batchDisassociateUserStack_userStackAssociations,

    -- * Destructuring the Response
    BatchDisassociateUserStackResponse (..),
    newBatchDisassociateUserStackResponse,

    -- * Response Lenses
    batchDisassociateUserStackResponse_errors,
    batchDisassociateUserStackResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchDisassociateUserStack' smart constructor.
data BatchDisassociateUserStack = BatchDisassociateUserStack'
  { -- | The list of UserStackAssociation objects.
    userStackAssociations :: Prelude.NonEmpty UserStackAssociation
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDisassociateUserStack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userStackAssociations', 'batchDisassociateUserStack_userStackAssociations' - The list of UserStackAssociation objects.
newBatchDisassociateUserStack ::
  -- | 'userStackAssociations'
  Prelude.NonEmpty UserStackAssociation ->
  BatchDisassociateUserStack
newBatchDisassociateUserStack pUserStackAssociations_ =
  BatchDisassociateUserStack'
    { userStackAssociations =
        Lens.coerced Lens.# pUserStackAssociations_
    }

-- | The list of UserStackAssociation objects.
batchDisassociateUserStack_userStackAssociations :: Lens.Lens' BatchDisassociateUserStack (Prelude.NonEmpty UserStackAssociation)
batchDisassociateUserStack_userStackAssociations = Lens.lens (\BatchDisassociateUserStack' {userStackAssociations} -> userStackAssociations) (\s@BatchDisassociateUserStack' {} a -> s {userStackAssociations = a} :: BatchDisassociateUserStack) Prelude.. Lens.coerced

instance Core.AWSRequest BatchDisassociateUserStack where
  type
    AWSResponse BatchDisassociateUserStack =
      BatchDisassociateUserStackResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDisassociateUserStackResponse'
            Prelude.<$> (x Data..?> "errors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchDisassociateUserStack where
  hashWithSalt _salt BatchDisassociateUserStack' {..} =
    _salt `Prelude.hashWithSalt` userStackAssociations

instance Prelude.NFData BatchDisassociateUserStack where
  rnf BatchDisassociateUserStack' {..} =
    Prelude.rnf userStackAssociations

instance Data.ToHeaders BatchDisassociateUserStack where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.BatchDisassociateUserStack" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchDisassociateUserStack where
  toJSON BatchDisassociateUserStack' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "UserStackAssociations"
                  Data..= userStackAssociations
              )
          ]
      )

instance Data.ToPath BatchDisassociateUserStack where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchDisassociateUserStack where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDisassociateUserStackResponse' smart constructor.
data BatchDisassociateUserStackResponse = BatchDisassociateUserStackResponse'
  { -- | The list of UserStackAssociationError objects.
    errors :: Prelude.Maybe [UserStackAssociationError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDisassociateUserStackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errors', 'batchDisassociateUserStackResponse_errors' - The list of UserStackAssociationError objects.
--
-- 'httpStatus', 'batchDisassociateUserStackResponse_httpStatus' - The response's http status code.
newBatchDisassociateUserStackResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchDisassociateUserStackResponse
newBatchDisassociateUserStackResponse pHttpStatus_ =
  BatchDisassociateUserStackResponse'
    { errors =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of UserStackAssociationError objects.
batchDisassociateUserStackResponse_errors :: Lens.Lens' BatchDisassociateUserStackResponse (Prelude.Maybe [UserStackAssociationError])
batchDisassociateUserStackResponse_errors = Lens.lens (\BatchDisassociateUserStackResponse' {errors} -> errors) (\s@BatchDisassociateUserStackResponse' {} a -> s {errors = a} :: BatchDisassociateUserStackResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchDisassociateUserStackResponse_httpStatus :: Lens.Lens' BatchDisassociateUserStackResponse Prelude.Int
batchDisassociateUserStackResponse_httpStatus = Lens.lens (\BatchDisassociateUserStackResponse' {httpStatus} -> httpStatus) (\s@BatchDisassociateUserStackResponse' {} a -> s {httpStatus = a} :: BatchDisassociateUserStackResponse)

instance
  Prelude.NFData
    BatchDisassociateUserStackResponse
  where
  rnf BatchDisassociateUserStackResponse' {..} =
    Prelude.rnf errors
      `Prelude.seq` Prelude.rnf httpStatus
