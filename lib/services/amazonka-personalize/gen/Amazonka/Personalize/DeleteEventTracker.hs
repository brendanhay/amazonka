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
-- Module      : Amazonka.Personalize.DeleteEventTracker
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the event tracker. Does not delete the event-interactions
-- dataset from the associated dataset group. For more information on event
-- trackers, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateEventTracker.html CreateEventTracker>.
module Amazonka.Personalize.DeleteEventTracker
  ( -- * Creating a Request
    DeleteEventTracker (..),
    newDeleteEventTracker,

    -- * Request Lenses
    deleteEventTracker_eventTrackerArn,

    -- * Destructuring the Response
    DeleteEventTrackerResponse (..),
    newDeleteEventTrackerResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteEventTracker' smart constructor.
data DeleteEventTracker = DeleteEventTracker'
  { -- | The Amazon Resource Name (ARN) of the event tracker to delete.
    eventTrackerArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEventTracker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventTrackerArn', 'deleteEventTracker_eventTrackerArn' - The Amazon Resource Name (ARN) of the event tracker to delete.
newDeleteEventTracker ::
  -- | 'eventTrackerArn'
  Prelude.Text ->
  DeleteEventTracker
newDeleteEventTracker pEventTrackerArn_ =
  DeleteEventTracker'
    { eventTrackerArn =
        pEventTrackerArn_
    }

-- | The Amazon Resource Name (ARN) of the event tracker to delete.
deleteEventTracker_eventTrackerArn :: Lens.Lens' DeleteEventTracker Prelude.Text
deleteEventTracker_eventTrackerArn = Lens.lens (\DeleteEventTracker' {eventTrackerArn} -> eventTrackerArn) (\s@DeleteEventTracker' {} a -> s {eventTrackerArn = a} :: DeleteEventTracker)

instance Core.AWSRequest DeleteEventTracker where
  type
    AWSResponse DeleteEventTracker =
      DeleteEventTrackerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteEventTrackerResponse'

instance Prelude.Hashable DeleteEventTracker where
  hashWithSalt _salt DeleteEventTracker' {..} =
    _salt `Prelude.hashWithSalt` eventTrackerArn

instance Prelude.NFData DeleteEventTracker where
  rnf DeleteEventTracker' {..} =
    Prelude.rnf eventTrackerArn

instance Data.ToHeaders DeleteEventTracker where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.DeleteEventTracker" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteEventTracker where
  toJSON DeleteEventTracker' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("eventTrackerArn" Data..= eventTrackerArn)
          ]
      )

instance Data.ToPath DeleteEventTracker where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteEventTracker where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEventTrackerResponse' smart constructor.
data DeleteEventTrackerResponse = DeleteEventTrackerResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEventTrackerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteEventTrackerResponse ::
  DeleteEventTrackerResponse
newDeleteEventTrackerResponse =
  DeleteEventTrackerResponse'

instance Prelude.NFData DeleteEventTrackerResponse where
  rnf _ = ()
