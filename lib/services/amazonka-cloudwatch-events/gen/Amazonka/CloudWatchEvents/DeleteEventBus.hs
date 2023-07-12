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
-- Module      : Amazonka.CloudWatchEvents.DeleteEventBus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified custom event bus or partner event bus. All rules
-- associated with this event bus need to be deleted. You can\'t delete
-- your account\'s default event bus.
module Amazonka.CloudWatchEvents.DeleteEventBus
  ( -- * Creating a Request
    DeleteEventBus (..),
    newDeleteEventBus,

    -- * Request Lenses
    deleteEventBus_name,

    -- * Destructuring the Response
    DeleteEventBusResponse (..),
    newDeleteEventBusResponse,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteEventBus' smart constructor.
data DeleteEventBus = DeleteEventBus'
  { -- | The name of the event bus to delete.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEventBus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteEventBus_name' - The name of the event bus to delete.
newDeleteEventBus ::
  -- | 'name'
  Prelude.Text ->
  DeleteEventBus
newDeleteEventBus pName_ =
  DeleteEventBus' {name = pName_}

-- | The name of the event bus to delete.
deleteEventBus_name :: Lens.Lens' DeleteEventBus Prelude.Text
deleteEventBus_name = Lens.lens (\DeleteEventBus' {name} -> name) (\s@DeleteEventBus' {} a -> s {name = a} :: DeleteEventBus)

instance Core.AWSRequest DeleteEventBus where
  type
    AWSResponse DeleteEventBus =
      DeleteEventBusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteEventBusResponse'

instance Prelude.Hashable DeleteEventBus where
  hashWithSalt _salt DeleteEventBus' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteEventBus where
  rnf DeleteEventBus' {..} = Prelude.rnf name

instance Data.ToHeaders DeleteEventBus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSEvents.DeleteEventBus" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteEventBus where
  toJSON DeleteEventBus' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath DeleteEventBus where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteEventBus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEventBusResponse' smart constructor.
data DeleteEventBusResponse = DeleteEventBusResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEventBusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteEventBusResponse ::
  DeleteEventBusResponse
newDeleteEventBusResponse = DeleteEventBusResponse'

instance Prelude.NFData DeleteEventBusResponse where
  rnf _ = ()
