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
-- Module      : Amazonka.CloudTrail.DeleteEventDataStore
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the event data store specified by @EventDataStore@, which
-- accepts an event data store ARN. After you run @DeleteEventDataStore@,
-- the event data store enters a @PENDING_DELETION@ state, and is
-- automatically deleted after a wait period of seven days.
-- @TerminationProtectionEnabled@ must be set to @False@ on the event data
-- store; this operation cannot work if @TerminationProtectionEnabled@ is
-- @True@.
--
-- After you run @DeleteEventDataStore@ on an event data store, you cannot
-- run @ListQueries@, @DescribeQuery@, or @GetQueryResults@ on queries that
-- are using an event data store in a @PENDING_DELETION@ state. An event
-- data store in the @PENDING_DELETION@ state does not incur costs.
module Amazonka.CloudTrail.DeleteEventDataStore
  ( -- * Creating a Request
    DeleteEventDataStore (..),
    newDeleteEventDataStore,

    -- * Request Lenses
    deleteEventDataStore_eventDataStore,

    -- * Destructuring the Response
    DeleteEventDataStoreResponse (..),
    newDeleteEventDataStoreResponse,

    -- * Response Lenses
    deleteEventDataStoreResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteEventDataStore' smart constructor.
data DeleteEventDataStore = DeleteEventDataStore'
  { -- | The ARN (or the ID suffix of the ARN) of the event data store to delete.
    eventDataStore :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEventDataStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventDataStore', 'deleteEventDataStore_eventDataStore' - The ARN (or the ID suffix of the ARN) of the event data store to delete.
newDeleteEventDataStore ::
  -- | 'eventDataStore'
  Prelude.Text ->
  DeleteEventDataStore
newDeleteEventDataStore pEventDataStore_ =
  DeleteEventDataStore'
    { eventDataStore =
        pEventDataStore_
    }

-- | The ARN (or the ID suffix of the ARN) of the event data store to delete.
deleteEventDataStore_eventDataStore :: Lens.Lens' DeleteEventDataStore Prelude.Text
deleteEventDataStore_eventDataStore = Lens.lens (\DeleteEventDataStore' {eventDataStore} -> eventDataStore) (\s@DeleteEventDataStore' {} a -> s {eventDataStore = a} :: DeleteEventDataStore)

instance Core.AWSRequest DeleteEventDataStore where
  type
    AWSResponse DeleteEventDataStore =
      DeleteEventDataStoreResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteEventDataStoreResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteEventDataStore where
  hashWithSalt _salt DeleteEventDataStore' {..} =
    _salt `Prelude.hashWithSalt` eventDataStore

instance Prelude.NFData DeleteEventDataStore where
  rnf DeleteEventDataStore' {..} =
    Prelude.rnf eventDataStore

instance Core.ToHeaders DeleteEventDataStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.DeleteEventDataStore" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteEventDataStore where
  toJSON DeleteEventDataStore' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("EventDataStore" Core..= eventDataStore)
          ]
      )

instance Core.ToPath DeleteEventDataStore where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteEventDataStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEventDataStoreResponse' smart constructor.
data DeleteEventDataStoreResponse = DeleteEventDataStoreResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEventDataStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteEventDataStoreResponse_httpStatus' - The response's http status code.
newDeleteEventDataStoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteEventDataStoreResponse
newDeleteEventDataStoreResponse pHttpStatus_ =
  DeleteEventDataStoreResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteEventDataStoreResponse_httpStatus :: Lens.Lens' DeleteEventDataStoreResponse Prelude.Int
deleteEventDataStoreResponse_httpStatus = Lens.lens (\DeleteEventDataStoreResponse' {httpStatus} -> httpStatus) (\s@DeleteEventDataStoreResponse' {} a -> s {httpStatus = a} :: DeleteEventDataStoreResponse)

instance Prelude.NFData DeleteEventDataStoreResponse where
  rnf DeleteEventDataStoreResponse' {..} =
    Prelude.rnf httpStatus
