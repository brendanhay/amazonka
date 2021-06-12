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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchAssociateUserStack' smart constructor.
data BatchAssociateUserStack = BatchAssociateUserStack'
  { -- | The list of UserStackAssociation objects.
    userStackAssociations :: Core.NonEmpty UserStackAssociation
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.NonEmpty UserStackAssociation ->
  BatchAssociateUserStack
newBatchAssociateUserStack pUserStackAssociations_ =
  BatchAssociateUserStack'
    { userStackAssociations =
        Lens._Coerce Lens.# pUserStackAssociations_
    }

-- | The list of UserStackAssociation objects.
batchAssociateUserStack_userStackAssociations :: Lens.Lens' BatchAssociateUserStack (Core.NonEmpty UserStackAssociation)
batchAssociateUserStack_userStackAssociations = Lens.lens (\BatchAssociateUserStack' {userStackAssociations} -> userStackAssociations) (\s@BatchAssociateUserStack' {} a -> s {userStackAssociations = a} :: BatchAssociateUserStack) Core.. Lens._Coerce

instance Core.AWSRequest BatchAssociateUserStack where
  type
    AWSResponse BatchAssociateUserStack =
      BatchAssociateUserStackResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchAssociateUserStackResponse'
            Core.<$> (x Core..?> "errors" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BatchAssociateUserStack

instance Core.NFData BatchAssociateUserStack

instance Core.ToHeaders BatchAssociateUserStack where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.BatchAssociateUserStack" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON BatchAssociateUserStack where
  toJSON BatchAssociateUserStack' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "UserStackAssociations"
                  Core..= userStackAssociations
              )
          ]
      )

instance Core.ToPath BatchAssociateUserStack where
  toPath = Core.const "/"

instance Core.ToQuery BatchAssociateUserStack where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newBatchAssociateUserStackResponse' smart constructor.
data BatchAssociateUserStackResponse = BatchAssociateUserStackResponse'
  { -- | The list of UserStackAssociationError objects.
    errors :: Core.Maybe [UserStackAssociationError],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Int ->
  BatchAssociateUserStackResponse
newBatchAssociateUserStackResponse pHttpStatus_ =
  BatchAssociateUserStackResponse'
    { errors =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of UserStackAssociationError objects.
batchAssociateUserStackResponse_errors :: Lens.Lens' BatchAssociateUserStackResponse (Core.Maybe [UserStackAssociationError])
batchAssociateUserStackResponse_errors = Lens.lens (\BatchAssociateUserStackResponse' {errors} -> errors) (\s@BatchAssociateUserStackResponse' {} a -> s {errors = a} :: BatchAssociateUserStackResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchAssociateUserStackResponse_httpStatus :: Lens.Lens' BatchAssociateUserStackResponse Core.Int
batchAssociateUserStackResponse_httpStatus = Lens.lens (\BatchAssociateUserStackResponse' {httpStatus} -> httpStatus) (\s@BatchAssociateUserStackResponse' {} a -> s {httpStatus = a} :: BatchAssociateUserStackResponse)

instance Core.NFData BatchAssociateUserStackResponse
