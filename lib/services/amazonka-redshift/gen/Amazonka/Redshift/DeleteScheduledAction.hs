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
-- Module      : Amazonka.Redshift.DeleteScheduledAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a scheduled action.
module Amazonka.Redshift.DeleteScheduledAction
  ( -- * Creating a Request
    DeleteScheduledAction (..),
    newDeleteScheduledAction,

    -- * Request Lenses
    deleteScheduledAction_scheduledActionName,

    -- * Destructuring the Response
    DeleteScheduledActionResponse (..),
    newDeleteScheduledActionResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteScheduledAction' smart constructor.
data DeleteScheduledAction = DeleteScheduledAction'
  { -- | The name of the scheduled action to delete.
    scheduledActionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteScheduledAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduledActionName', 'deleteScheduledAction_scheduledActionName' - The name of the scheduled action to delete.
newDeleteScheduledAction ::
  -- | 'scheduledActionName'
  Prelude.Text ->
  DeleteScheduledAction
newDeleteScheduledAction pScheduledActionName_ =
  DeleteScheduledAction'
    { scheduledActionName =
        pScheduledActionName_
    }

-- | The name of the scheduled action to delete.
deleteScheduledAction_scheduledActionName :: Lens.Lens' DeleteScheduledAction Prelude.Text
deleteScheduledAction_scheduledActionName = Lens.lens (\DeleteScheduledAction' {scheduledActionName} -> scheduledActionName) (\s@DeleteScheduledAction' {} a -> s {scheduledActionName = a} :: DeleteScheduledAction)

instance Core.AWSRequest DeleteScheduledAction where
  type
    AWSResponse DeleteScheduledAction =
      DeleteScheduledActionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull DeleteScheduledActionResponse'

instance Prelude.Hashable DeleteScheduledAction where
  hashWithSalt _salt DeleteScheduledAction' {..} =
    _salt `Prelude.hashWithSalt` scheduledActionName

instance Prelude.NFData DeleteScheduledAction where
  rnf DeleteScheduledAction' {..} =
    Prelude.rnf scheduledActionName

instance Core.ToHeaders DeleteScheduledAction where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteScheduledAction where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteScheduledAction where
  toQuery DeleteScheduledAction' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeleteScheduledAction" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "ScheduledActionName" Core.=: scheduledActionName
      ]

-- | /See:/ 'newDeleteScheduledActionResponse' smart constructor.
data DeleteScheduledActionResponse = DeleteScheduledActionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteScheduledActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteScheduledActionResponse ::
  DeleteScheduledActionResponse
newDeleteScheduledActionResponse =
  DeleteScheduledActionResponse'

instance Prelude.NFData DeleteScheduledActionResponse where
  rnf _ = ()
