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
-- Module      : Network.AWS.AppStream.BatchDisassociateUserStack
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified users from the specified stacks.
module Network.AWS.AppStream.BatchDisassociateUserStack
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

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
        Lens._Coerce Lens.# pUserStackAssociations_
    }

-- | The list of UserStackAssociation objects.
batchDisassociateUserStack_userStackAssociations :: Lens.Lens' BatchDisassociateUserStack (Prelude.NonEmpty UserStackAssociation)
batchDisassociateUserStack_userStackAssociations = Lens.lens (\BatchDisassociateUserStack' {userStackAssociations} -> userStackAssociations) (\s@BatchDisassociateUserStack' {} a -> s {userStackAssociations = a} :: BatchDisassociateUserStack) Prelude.. Lens._Coerce

instance Core.AWSRequest BatchDisassociateUserStack where
  type
    AWSResponse BatchDisassociateUserStack =
      BatchDisassociateUserStackResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDisassociateUserStackResponse'
            Prelude.<$> (x Core..?> "errors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchDisassociateUserStack

instance Prelude.NFData BatchDisassociateUserStack

instance Core.ToHeaders BatchDisassociateUserStack where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.BatchDisassociateUserStack" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchDisassociateUserStack where
  toJSON BatchDisassociateUserStack' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "UserStackAssociations"
                  Core..= userStackAssociations
              )
          ]
      )

instance Core.ToPath BatchDisassociateUserStack where
  toPath = Prelude.const "/"

instance Core.ToQuery BatchDisassociateUserStack where
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
batchDisassociateUserStackResponse_errors = Lens.lens (\BatchDisassociateUserStackResponse' {errors} -> errors) (\s@BatchDisassociateUserStackResponse' {} a -> s {errors = a} :: BatchDisassociateUserStackResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchDisassociateUserStackResponse_httpStatus :: Lens.Lens' BatchDisassociateUserStackResponse Prelude.Int
batchDisassociateUserStackResponse_httpStatus = Lens.lens (\BatchDisassociateUserStackResponse' {httpStatus} -> httpStatus) (\s@BatchDisassociateUserStackResponse' {} a -> s {httpStatus = a} :: BatchDisassociateUserStackResponse)

instance
  Prelude.NFData
    BatchDisassociateUserStackResponse
