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
-- Module      : Network.AWS.Redshift.DeleteScheduledAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a scheduled action.
module Network.AWS.Redshift.DeleteScheduledAction
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteScheduledAction' smart constructor.
data DeleteScheduledAction = DeleteScheduledAction'
  { -- | The name of the scheduled action to delete.
    scheduledActionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteScheduledAction where
  type
    Rs DeleteScheduledAction =
      DeleteScheduledActionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeleteScheduledActionResponse'

instance Prelude.Hashable DeleteScheduledAction

instance Prelude.NFData DeleteScheduledAction

instance Prelude.ToHeaders DeleteScheduledAction where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteScheduledAction where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteScheduledAction where
  toQuery DeleteScheduledAction' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteScheduledAction" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2012-12-01" :: Prelude.ByteString),
        "ScheduledActionName" Prelude.=: scheduledActionName
      ]

-- | /See:/ 'newDeleteScheduledActionResponse' smart constructor.
data DeleteScheduledActionResponse = DeleteScheduledActionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteScheduledActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteScheduledActionResponse ::
  DeleteScheduledActionResponse
newDeleteScheduledActionResponse =
  DeleteScheduledActionResponse'

instance Prelude.NFData DeleteScheduledActionResponse
