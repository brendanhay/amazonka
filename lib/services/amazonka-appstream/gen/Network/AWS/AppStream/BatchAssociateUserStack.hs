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
-- Module      : Network.AWS.AppStream.BatchAssociateUserStack
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified users with the specified stacks. Users in a
-- user pool cannot be assigned to stacks with fleets that are joined to an
-- Active Directory domain.
module Network.AWS.AppStream.BatchAssociateUserStack
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

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchAssociateUserStackResponse'
            Prelude.<$> (x Core..?> "errors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchAssociateUserStack

instance Prelude.NFData BatchAssociateUserStack

instance Core.ToHeaders BatchAssociateUserStack where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.BatchAssociateUserStack" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchAssociateUserStack where
  toJSON BatchAssociateUserStack' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "UserStackAssociations"
                  Core..= userStackAssociations
              )
          ]
      )

instance Core.ToPath BatchAssociateUserStack where
  toPath = Prelude.const "/"

instance Core.ToQuery BatchAssociateUserStack where
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
