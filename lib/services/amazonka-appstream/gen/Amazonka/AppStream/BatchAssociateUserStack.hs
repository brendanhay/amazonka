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
-- Module      : Amazonka.AppStream.BatchAssociateUserStack
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified users with the specified stacks. Users in a
-- user pool cannot be assigned to stacks with fleets that are joined to an
-- Active Directory domain.
module Amazonka.AppStream.BatchAssociateUserStack
  ( -- * Creating a Request
    BatchAssociateUserStack (..),
    newBatchAssociateUserStack,

    -- * Request Lenses
    batchAssociateUserStack_userStackAssociations,

    -- * Destructuring the Response
    BatchAssociateUserStackResponse (..),
    newBatchAssociateUserStackResponse,

    -- * Response Lenses
    batchAssociateUserStackResponse_errors,
    batchAssociateUserStackResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchAssociateUserStack' smart constructor.
data BatchAssociateUserStack = BatchAssociateUserStack'
  { -- | The list of UserStackAssociation objects.
    userStackAssociations :: Prelude.NonEmpty UserStackAssociation
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchAssociateUserStack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userStackAssociations', 'batchAssociateUserStack_userStackAssociations' - The list of UserStackAssociation objects.
newBatchAssociateUserStack ::
  -- | 'userStackAssociations'
  Prelude.NonEmpty UserStackAssociation ->
  BatchAssociateUserStack
newBatchAssociateUserStack pUserStackAssociations_ =
  BatchAssociateUserStack'
    { userStackAssociations =
        Lens.coerced Lens.# pUserStackAssociations_
    }

-- | The list of UserStackAssociation objects.
batchAssociateUserStack_userStackAssociations :: Lens.Lens' BatchAssociateUserStack (Prelude.NonEmpty UserStackAssociation)
batchAssociateUserStack_userStackAssociations = Lens.lens (\BatchAssociateUserStack' {userStackAssociations} -> userStackAssociations) (\s@BatchAssociateUserStack' {} a -> s {userStackAssociations = a} :: BatchAssociateUserStack) Prelude.. Lens.coerced

instance Core.AWSRequest BatchAssociateUserStack where
  type
    AWSResponse BatchAssociateUserStack =
      BatchAssociateUserStackResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchAssociateUserStackResponse'
            Prelude.<$> (x Data..?> "errors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchAssociateUserStack where
  hashWithSalt _salt BatchAssociateUserStack' {..} =
    _salt `Prelude.hashWithSalt` userStackAssociations

instance Prelude.NFData BatchAssociateUserStack where
  rnf BatchAssociateUserStack' {..} =
    Prelude.rnf userStackAssociations

instance Data.ToHeaders BatchAssociateUserStack where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.BatchAssociateUserStack" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchAssociateUserStack where
  toJSON BatchAssociateUserStack' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "UserStackAssociations"
                  Data..= userStackAssociations
              )
          ]
      )

instance Data.ToPath BatchAssociateUserStack where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchAssociateUserStack where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchAssociateUserStackResponse' smart constructor.
data BatchAssociateUserStackResponse = BatchAssociateUserStackResponse'
  { -- | The list of UserStackAssociationError objects.
    errors :: Prelude.Maybe [UserStackAssociationError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchAssociateUserStackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errors', 'batchAssociateUserStackResponse_errors' - The list of UserStackAssociationError objects.
--
-- 'httpStatus', 'batchAssociateUserStackResponse_httpStatus' - The response's http status code.
newBatchAssociateUserStackResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchAssociateUserStackResponse
newBatchAssociateUserStackResponse pHttpStatus_ =
  BatchAssociateUserStackResponse'
    { errors =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of UserStackAssociationError objects.
batchAssociateUserStackResponse_errors :: Lens.Lens' BatchAssociateUserStackResponse (Prelude.Maybe [UserStackAssociationError])
batchAssociateUserStackResponse_errors = Lens.lens (\BatchAssociateUserStackResponse' {errors} -> errors) (\s@BatchAssociateUserStackResponse' {} a -> s {errors = a} :: BatchAssociateUserStackResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchAssociateUserStackResponse_httpStatus :: Lens.Lens' BatchAssociateUserStackResponse Prelude.Int
batchAssociateUserStackResponse_httpStatus = Lens.lens (\BatchAssociateUserStackResponse' {httpStatus} -> httpStatus) (\s@BatchAssociateUserStackResponse' {} a -> s {httpStatus = a} :: BatchAssociateUserStackResponse)

instance
  Prelude.NFData
    BatchAssociateUserStackResponse
  where
  rnf BatchAssociateUserStackResponse' {..} =
    Prelude.rnf errors
      `Prelude.seq` Prelude.rnf httpStatus
