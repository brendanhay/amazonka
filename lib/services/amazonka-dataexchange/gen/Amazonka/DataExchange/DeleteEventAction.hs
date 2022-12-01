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
-- Module      : Amazonka.DataExchange.DeleteEventAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation deletes the event action.
module Amazonka.DataExchange.DeleteEventAction
  ( -- * Creating a Request
    DeleteEventAction (..),
    newDeleteEventAction,

    -- * Request Lenses
    deleteEventAction_eventActionId,

    -- * Destructuring the Response
    DeleteEventActionResponse (..),
    newDeleteEventActionResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataExchange.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteEventAction' smart constructor.
data DeleteEventAction = DeleteEventAction'
  { -- | The unique identifier for the event action.
    eventActionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEventAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventActionId', 'deleteEventAction_eventActionId' - The unique identifier for the event action.
newDeleteEventAction ::
  -- | 'eventActionId'
  Prelude.Text ->
  DeleteEventAction
newDeleteEventAction pEventActionId_ =
  DeleteEventAction' {eventActionId = pEventActionId_}

-- | The unique identifier for the event action.
deleteEventAction_eventActionId :: Lens.Lens' DeleteEventAction Prelude.Text
deleteEventAction_eventActionId = Lens.lens (\DeleteEventAction' {eventActionId} -> eventActionId) (\s@DeleteEventAction' {} a -> s {eventActionId = a} :: DeleteEventAction)

instance Core.AWSRequest DeleteEventAction where
  type
    AWSResponse DeleteEventAction =
      DeleteEventActionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteEventActionResponse'

instance Prelude.Hashable DeleteEventAction where
  hashWithSalt _salt DeleteEventAction' {..} =
    _salt `Prelude.hashWithSalt` eventActionId

instance Prelude.NFData DeleteEventAction where
  rnf DeleteEventAction' {..} =
    Prelude.rnf eventActionId

instance Core.ToHeaders DeleteEventAction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteEventAction where
  toPath DeleteEventAction' {..} =
    Prelude.mconcat
      ["/v1/event-actions/", Core.toBS eventActionId]

instance Core.ToQuery DeleteEventAction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEventActionResponse' smart constructor.
data DeleteEventActionResponse = DeleteEventActionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEventActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteEventActionResponse ::
  DeleteEventActionResponse
newDeleteEventActionResponse =
  DeleteEventActionResponse'

instance Prelude.NFData DeleteEventActionResponse where
  rnf _ = ()
