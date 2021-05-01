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
-- Module      : Network.AWS.CloudWatchEvents.DeleteEventBus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified custom event bus or partner event bus. All rules
-- associated with this event bus need to be deleted. You can\'t delete
-- your account\'s default event bus.
module Network.AWS.CloudWatchEvents.DeleteEventBus
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

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteEventBus' smart constructor.
data DeleteEventBus = DeleteEventBus'
  { -- | The name of the event bus to delete.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteEventBus where
  type Rs DeleteEventBus = DeleteEventBusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteEventBusResponse'

instance Prelude.Hashable DeleteEventBus

instance Prelude.NFData DeleteEventBus

instance Prelude.ToHeaders DeleteEventBus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSEvents.DeleteEventBus" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteEventBus where
  toJSON DeleteEventBus' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Prelude..= name)]
      )

instance Prelude.ToPath DeleteEventBus where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteEventBus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEventBusResponse' smart constructor.
data DeleteEventBusResponse = DeleteEventBusResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteEventBusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteEventBusResponse ::
  DeleteEventBusResponse
newDeleteEventBusResponse = DeleteEventBusResponse'

instance Prelude.NFData DeleteEventBusResponse
