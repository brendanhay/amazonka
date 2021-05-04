{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MechanicalTurk.DeleteHIT
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.MechanicalTurk.DeleteHIT
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteHIT' smart constructor.
data DeleteHIT = DeleteHIT'
  { -- | The ID of the HIT to be deleted.
    hITId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteHIT where
  type Rs DeleteHIT = DeleteHITResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteHITResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteHIT

instance Prelude.NFData DeleteHIT

instance Prelude.ToHeaders DeleteHIT where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "MTurkRequesterServiceV20170117.DeleteHIT" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteHIT where
  toJSON DeleteHIT' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("HITId" Prelude..= hITId)]
      )

instance Prelude.ToPath DeleteHIT where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteHIT where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteHITResponse' smart constructor.
data DeleteHITResponse = DeleteHITResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteHITResponse
