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
-- Module      : Amazonka.MechanicalTurk.DeleteHIT
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @DeleteHIT@ operation is used to delete HIT that is no longer
-- needed. Only the Requester who created the HIT can delete it.
--
-- You can only dispose of HITs that are in the @Reviewable@ state, with
-- all of their submitted assignments already either approved or rejected.
-- If you call the DeleteHIT operation on a HIT that is not in the
-- @Reviewable@ state (for example, that has not expired, or still has
-- active assignments), or on a HIT that is Reviewable but without all of
-- its submitted assignments already approved or rejected, the service will
-- return an error.
--
-- -   HITs are automatically disposed of after 120 days.
--
-- -   After you dispose of a HIT, you can no longer approve the HIT\'s
--     rejected assignments.
--
-- -   Disposed HITs are not returned in results for the ListHITs
--     operation.
--
-- -   Disposing HITs can improve the performance of operations such as
--     ListReviewableHITs and ListHITs.
module Amazonka.MechanicalTurk.DeleteHIT
  ( -- * Creating a Request
    DeleteHIT (..),
    newDeleteHIT,

    -- * Request Lenses
    deleteHIT_hITId,

    -- * Destructuring the Response
    DeleteHITResponse (..),
    newDeleteHITResponse,

    -- * Response Lenses
    deleteHITResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MechanicalTurk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteHIT' smart constructor.
data DeleteHIT = DeleteHIT'
  { -- | The ID of the HIT to be deleted.
    hITId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteHIT' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hITId', 'deleteHIT_hITId' - The ID of the HIT to be deleted.
newDeleteHIT ::
  -- | 'hITId'
  Prelude.Text ->
  DeleteHIT
newDeleteHIT pHITId_ = DeleteHIT' {hITId = pHITId_}

-- | The ID of the HIT to be deleted.
deleteHIT_hITId :: Lens.Lens' DeleteHIT Prelude.Text
deleteHIT_hITId = Lens.lens (\DeleteHIT' {hITId} -> hITId) (\s@DeleteHIT' {} a -> s {hITId = a} :: DeleteHIT)

instance Core.AWSRequest DeleteHIT where
  type AWSResponse DeleteHIT = DeleteHITResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteHITResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteHIT where
  hashWithSalt _salt DeleteHIT' {..} =
    _salt `Prelude.hashWithSalt` hITId

instance Prelude.NFData DeleteHIT where
  rnf DeleteHIT' {..} = Prelude.rnf hITId

instance Data.ToHeaders DeleteHIT where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MTurkRequesterServiceV20170117.DeleteHIT" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteHIT where
  toJSON DeleteHIT' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("HITId" Data..= hITId)]
      )

instance Data.ToPath DeleteHIT where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteHIT where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteHITResponse' smart constructor.
data DeleteHITResponse = DeleteHITResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteHITResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteHITResponse_httpStatus' - The response's http status code.
newDeleteHITResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteHITResponse
newDeleteHITResponse pHttpStatus_ =
  DeleteHITResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteHITResponse_httpStatus :: Lens.Lens' DeleteHITResponse Prelude.Int
deleteHITResponse_httpStatus = Lens.lens (\DeleteHITResponse' {httpStatus} -> httpStatus) (\s@DeleteHITResponse' {} a -> s {httpStatus = a} :: DeleteHITResponse)

instance Prelude.NFData DeleteHITResponse where
  rnf DeleteHITResponse' {..} = Prelude.rnf httpStatus
